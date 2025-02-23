/* PHANTOM.PAS -- revised from version in UNDOCUMENTED DOS, Chapter 4.
   In particular, note use of the INT 2Fh AX=1208h and AX=120Ch functions
   in dec_SFT() and set_Owner(). This version works properly in DOS 5. */

/*$A-,B-,D+,L+,E-,F-,I-,N-,O-,R-,S-,V-*/
/*$M 2048,128,1000*/


#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <dos.h>
#include "../fujinet-rs232/sys/print.h"

#pragma pack(1)

typedef char Char;
typedef uint8_t uchar;
typedef uint32_t LONGINT;
typedef uint8_t boolean;
typedef void far * Anyptr;

enum {
  false = 0,
  true,
};

#define Registers union REGS
#define msdos(x) intdosx(&x, &x, &sr)
#define ptr(x, y) MK_FP(x, y)
#define _Escape exit
#define _SETIO(x, y) x
#define ReadOnly _A_RDONLY
#define getintvec(x, y) y = _dos_getvect(x)
#define setintvec(x, y) _dos_setvect(x, (void interrupt *) y)
#define swapvectors()
#define MEMW(x, y) *((uint16_t *) MK_FP(x, y))
#define prefixseg FP_SEG(_psp)
#define asclen(x) _fstrlen(x)

extern __segment getCS(void);
#pragma aux getCS = \
    "mov ax, cs";
extern __segment getSS(void);
#pragma aux getSS = \
    "mov ax, ss";
extern __segment getSP(void);
#pragma aux getSP = \
    "mov ax, sp";

extern uint16_t getFlags(void);
#pragma aux getFlags =	\
  "pushf"		\
  "pop ax"		\
  modify [ax]

typedef struct sig_rec {
  Char signature[8];
  unsigned short psp;
  uchar drive_no;
} sig_rec;


#define cds_id_size     10

#define cds_id          "Phantom. :\\"

#define maxfilesize     (unsigned int) 32767   /* for our 1 file */

//#define isr_code_max    102   /* offset of last byte */


/* in our ISR macine code */

typedef Char cdsidarr[cds_id_size];

/* FindFirst/Next data block - ALL DOS VERSIONS */

typedef struct {
  uchar drive_num; // drv_lett;
  Char pattern[11]; // srch_tmpl[11];
  uchar attr_mask; //srch_attr;
  unsigned short sequence, sector; //dir_entry, par_clstr;
  uchar _reserved[4]; // f1[4];
} SRCHREC, far *SRCHREC_PTR;

/* DOS System File Table entry - ALL DOS VERSIONS */

typedef struct sft_rec {
  unsigned short handle_cnt, open_mode;
  uchar attr_byte;
  unsigned short dev_info;
  Anyptr devdrv_ptr;
  LONGINT start_clstr;   /* we don't need to touch this */
  LONGINT f_time, f_size, f_pos;
  unsigned short rel_lastclstr;   /* we don't need to touch this */
  unsigned short abs_lastclstr;   /* we don't need to touch this */
  unsigned short dir_sector;   /* we don't need to touch this */
  uchar dir_entryno;   /* we don't need to touch this */
  Char fcb_fn[11];
} sft_rec;

/* DOS Current directory structure - DOS VERSION 3.xx */

typedef struct cds3_rec {
  Char curr_path[67];
  unsigned short flags;
  uchar f1[10];   /* we don't need to touch this */
  unsigned short root_ofs;
} cds3_rec;

/* DOS Current directory structure - DOS VERSION 4.xx */

typedef struct cds4_rec {
  Char curr_path[67];
  unsigned short flags;
  uchar f1[10];   /* we don't need to touch this */
  unsigned short root_ofs;
  uchar f2[7];   /* we don't need to touch this */
} cds4_rec;

/* DOS Directory entry for 'found' file - ALL DOS VERSIONS */

typedef struct {
  Char name[11]; // fname[11];
  uchar attr; // fattr;
  uchar _reserved1[10]; // f1[10];
  unsigned short time, date, start_sector; // time_lstupd, date_lstupd, start_clstr;
      /* we don't need to touch this */
  LONGINT size; //fsiz;
} DIRREC, far *DIRREC_PTR;

/* Swappable DOS Area - DOS VERSION 3.xx */

typedef struct {
  uchar f0[12];
  Anyptr curr_dta;
  uchar f1[30];
  uchar dd, mm;
  unsigned short yy_1980;
  uchar f2[96];
  Char fn1[128], fn2[128];
  SRCHREC sdb;
  DIRREC found_file;
  cds3_rec drive_cdscopy;
  Char fcb_fn1[11];
  uchar f3;
  Char fcb_fn2[11];
  uchar f4[11];
  uchar srch_attr, open_mode;
  uchar f5[48];
  Anyptr drive_cdsptr;
  uchar f6[12];
  unsigned short fn1_csofs, fn2_csofs;
  uchar f7[56];
  SRCHREC ren_srcfile;
  DIRREC ren_file;
} sda3_rec, far *sda3_ptr;

/* Swappable DOS Area - DOS VERSION 4.xx */

typedef struct {
  uchar f0[12];
  Anyptr curr_dta;
  uchar f1[32];
  uchar dd, mm;
  unsigned short yy_1980;
  uchar f2[106];
  Char fn1[128], fn2[128];
  SRCHREC sdb;
  DIRREC found_file;
  cds4_rec drive_cdscopy;
  Char fcb_fn1[11];
  uchar f3;
  Char fcb_fn2[11];
  uchar f4[11];
  uchar srch_attr, open_mode;
  uchar f5[51];
  Anyptr drive_cdsptr;
  uchar f6[12];
  unsigned short fn1_csofs, fn2_csofs;
  uchar f7[71];
  unsigned short spop_act, spop_attr, spop_mode;
  uchar f8[29];
  SRCHREC ren_srcfile;
  DIRREC ren_file;
} sda4_rec, far *sda4_ptr;

/* DOS List of lists structure - DOS VERSIONS 3.1 thru 4 */

typedef struct lol_rec {
  uchar f1[22];
  Anyptr cds;
  uchar f2[7];
  uchar last_drive;
} lol_rec;

/* This serves as a list of the function types that we support */
typedef enum {
  _inquiry, _rd, _md, _cd, _close, _commit, _read, _write, _lock, _unlock,
  _space, _setattr, _getattr, _rename, _delete, _open, _create, _ffirst,
  _fnext, _seek, _specopen, _unsupported
} fxn_type;

/* A de rigeur structure for manipulators of pointers */

#if 0
typedef struct os {
  unsigned short o, s;
} os;
#endif

typedef Char fcbfnbuf[13];

typedef Char ascbuf[128];

/* This defines a pointer to our primary Int 2Fh ISR structure */

/* A structure to contain all register values. The TP DOS registers
    type is insufficient */

typedef struct regset {
  unsigned short bp, es, ds, di, si, dx, cx, bx, ax, ss, sp, cs, ip, flags;
} regset;

#if 0
/* Our Int 2F ISR structure */
typedef uchar isr_code_buffer[isr_code_max + 1];
#endif

typedef struct {
#if 0
  isr_code_buffer ic;   /* Contains our macine code ISR stub code */
#endif
  unsigned short save_ss;   /* Stores SS on entry before stack switch */
  unsigned short save_sp;   /* Stores SP on entry before stack switch */
  unsigned short real_fl;   /* Stores flags as they were on entry */
  unsigned short save_fl;   /* Stores flags from the stack */
  unsigned short save_cs;   /* Stores return CS from the stack */
  unsigned short save_ip;   /* Stores return IP from the stack */
  boolean our_drive;   /* For ISR to either chain on or return */
} isr_rec;

typedef Char strfn[13];


/* all the calls we need to support are in the range 0..33 */

#if 0
/* The following are offsets into the ISR stub code where run time
   values must be fixed in */
#define prev_hndlr      99
#define redir_entry     49
#define our_sp_ofs      45
#define our_ss_ofs      40

/* The following offsets are known at compile time and are directly
   referenced in the ISR stub code */

#define save_ss_ofs     (isr_code_max + 1)
#define save_sp_ofs     (isr_code_max + 3)
#define save_rf_ofs     (isr_code_max + 5)
#define save_fl_ofs     (isr_code_max + 7)
#define save_cs_ofs     (isr_code_max + 9)
#define save_ip_ofs     (isr_code_max + 11)
#define our_drv_ofs     (isr_code_max + 13)
#endif

void far *prev_hndlr;

typedef Char str4[5];


static sig_rec our = {
  "PHANTOM", 0, 0
};

static Char vollab[14] = "AN ILLUS.ION\0";   /* Our Volume label */

static fxn_type fxn_map[] = {
  _inquiry, _rd, _unsupported, _md, _unsupported, _cd, _close, _commit, _read,
  _write, _lock, _unlock, _space, _unsupported, _setattr, _getattr,
  _unsupported, _rename, _unsupported, _delete, _unsupported, _unsupported,
  _open, _create, _unsupported, _unsupported, _unsupported, _ffirst, _fnext,
  _unsupported, _unsupported, _unsupported, _unsupported, _seek, _unsupported,
  _unsupported, _unsupported, _unsupported, _unsupported, _unsupported,
  _unsupported, _unsupported, _unsupported, _unsupported, _unsupported,
  _unsupported, _specopen
};
#define fxn_map_max     (sizeof(fxn_map) / sizeof(fxn_map[0]))

#if 0
/* Our ISR stub code is defined as a constant array of bytes which
   actually contains machine code as commented on the right */
/* entry: */
/* nop OR int 3          ; for debugging */
/* pushf                 ; save flags    */
/* cmp   ah,11h          ; our fxn?      */
/* jne   not_ours        ; bypass        */
/* pop   cs:real_fl      ; store act flgs*/
/* pop   cs:save_ip      ; store cs:ip   */
/* pop   cs:save_cs      ; and flags     */
/* pop   cs:save_fl      ; from stack    */

/* mov   cs:save_sp,sp   ; save stack    */
/* mov   sp,ss                           */
/* mov   cs:save_ss,sp                   */

/* mov   sp,SSEG         ; set our stack */
/* mov   ss,sp                           */
/* mov   sp,SPTR                         */

/* pushf                 ; call our      */
/* call  redir           ; intr proc.    */

/* mov   sp,cs:save_ss   ; put back      */
/* mov   ss,sp           ; caller's stack*/
/* mov   sp,cs:save_sp                   */

/* push  cs:save_fl      ; restore       */
/* push  cs:save_cs      ; restore       */
/* push  cs:save_ip      ; return addr.  */
/* push  cs:real_fl      ; save act flgs */

/* cmp cs:our_drive,0; not our drive?*/
/* je    not_ours        ; no, jump      */
/* popf                  ; yes, restore  */
/* retf  2               ; & return flags*/
/* not_ours: */
/* popf                  ; restore flags */
/* jmp   far prev_hndlr  ; pass the buck */

static isr_code_buffer isr_code = {
  0x90, 0x9c, 0x80, 0xfc, 0x11, 0x75, 0x5a, 0x2e, 0x8f, 0x6, save_rf_ofs, 0,
  0x2e, 0x8f, 0x6, save_ip_ofs, 0, 0x2e, 0x8f, 0x6, save_cs_ofs, 0, 0x2e,
  0x8f, 0x6, save_fl_ofs, 0, 0x2e, 0x89, 0x26, save_sp_ofs, 0, 0x8c, 0xd4,
  0x2e, 0x89, 0x26, save_ss_ofs, 0, 0xbc, 0, 0, 0x8e, 0xd4, 0xbc, 0, 0, 0x9c,
  0x9a, 0, 0, 0, 0, 0x2e, 0x8b, 0x26, save_ss_ofs, 0, 0x8e, 0xd4, 0x2e, 0x8b,
  0x26, save_sp_ofs, 0, 0x2e, 0xff, 0x36, save_fl_ofs, 0, 0x2e, 0xff, 0x36,
  save_cs_ofs, 0, 0x2e, 0xff, 0x36, save_ip_ofs, 0, 0x2e, 0xff, 0x36,
  save_rf_ofs, 0, 0x2e, 0x80, 0x3e, our_drv_ofs, 0, 0, 0x74, 0x4, 0x9d, 0xca,
  0x2, 0, 0x9d, 0xea, 0, 0, 0, 0
};
#endif

/* The instance of our Int 2F ISR */
static isr_rec *isr;

/* variables relating to the one allowable file.. */
static fcbfnbuf file_name;
static uchar file_buffer[maxfilesize + 1];
/*    file_opens, */
static unsigned short file_date, file_time;
static uchar file_attr;
static LONGINT file_size;

/* Our full directory structure */
static ascbuf max_path;

/* Global stuff */
static unsigned short our_sp;   /* SP to switch to on entry */
static uchar drive_no;   /* A: is 1, B: is 2, etc. */
static Char strbuf[256];   /* General purpose pascal string buffer */
static Char far *a1;   /* Pointer to an ASCIIZ string */
static Char *a2;   /* Pointer to an ASCIIZ string */
static Char drive[4];   /* Command line parameter area */
static fxn_type fxn;   /* Record of function in progress */
#if 0
static regset r;   /* Global save area for all caller's regs */
#endif
static fcbfnbuf temp_name;   /* General purpose ASCIIZ filename buffer */
static uchar iroot;   /* Index to root directory in max_path */
static uchar icur;   /* Index to current directory in max_path */
static uchar lmax;   /* Length of max_path */
static uchar ifile;   /* Index to directory in max_path with file */
static unsigned short ver;   /* full DOS version */
static Anyptr sda;   /* pointer to the Swappable Dos Area */
static lol_rec far *lol;   /* pointer to the DOS list of lists struct */
static Char h[16];


static Char *hex_(Result, inp)
Char *Result;
unsigned short inp;
{
  Result[4] = '\0';
  Result[0] = h[inp >> 12];
  Result[1] = h[(inp >> 8) & 0xf];
  Result[2] = h[(inp >> 4) & 0xf];
  Result[3] = h[inp & 0xf];
  return Result;
}


/* Fail PHANTOM, print message, exit to DOS */
static void failprog(msg)
Char *msg;
{
  _SETIO(puts(msg) >= 0, FileWriteError);
  _Escape(1);
}


/* Get DOS version, address of Swappable DOS Area, and address of
   DOS List of lists. We only run on versions of DOS >= 3.10, so
   fail otherwise */
static void get_dos_vars()
{
  Registers r;
  struct SREGS sr;

  if (_osmajor < 3 || _osmajor == 3 && _osminor < 10)
    failprog("DOS Version must be 3.10 or greater");
  r.x.ax = 0x5d06;
  msdos(r);
  sda = ptr(sr.ds, r.x.si);   /* Get SDA pointer */
  r.x.ax = 0x5200;
  msdos(r);
  lol = ptr(sr.es, r.x.bx);   /* Get LoL pointer */
}


#if 0
/* Fail the current redirector call with the supplied error number, i.e.
   set the carry flag in the returned flags, and set ax=error code */
static void fail(err)
unsigned short err;
{
  r.flags |= INTR_CF;
  r.ax = err;
}
#endif

/* Convert an 11 byte fcb style filename to ASCIIZ name.ext format */
static void fnfmfcbnm(ss, p)
Anyptr ss;
Char **p;
{
  uchar i = 0, j = 8;
  Char *s = (Char *)ss;
  boolean dot;
  void *TEMP;

  *p = temp_name;
  while (i < 8 && s[i] != ' ')
    i++;
  TEMP = s;
  memmove((*p), &TEMP, (long)i);
  while (j < 11 && s[j] != ' ')
    j++;
  TEMP = s;
  memmove((&(*p)[i+1]), &TEMP, j - 8L);
  if (j != 8) {
    (*p)[i] = '.';
    (*p)[j] = '\0';
  }
  else
    (*p)[i] = '\0';
}


/* The opposite of the above, convert an ASCIIZ name.ext filename
   into an 11 byte fcb style filename */
static void cnvt2fcb(ss, pp)
Anyptr ss, pp;
{
  uchar i = 0, j = 0;
  Char *s = (Char *)ss;
  Char *p = (Char *)pp;

  memset(p, ' ', 11L);
  while (s[i] != '\0') {
    if (s[i] == '.')
      j = 7;
    else
      p[j] = s[i];
    i++;
    j++;
  }
}

#if 0
/* Get the length of an ASCIIZ string */
static unsigned short asclen(a)
Char *a;
{
  unsigned short i = 0;

  while (i < 65535L && a[i] != '\0')
    i++;
  return i;
}
#endif


/* Set up global a1 to point to the appropriate source for the file
   or directory name parameter for this call */
static void set_fn1()
{
  switch (fxn) {

  /* For these calls, a fully qualified file/directory name is given in the
     SDA first filename field. This field, incidentally, can also be referenced
     indirectly through the SDA first filename offset field into DOS's CS. */
  case _rd:
  case _md:
  case _cd:
  case _setattr:
  case _getattr:
  case _rename:
  case _delete:
  case _open:
  case _create:
  case _ffirst:
  case _specopen:
    if (_osmajor == 3)
      a1 = ((sda3_ptr) sda)->fn1;
    else
      a1 = ((sda4_ptr) sda)->fn1;
    break;

  /* These do not need a filename. The following is valid-ish... */
  case _close:
  case _commit:
  case _read:
  case _write:
  case _seek:
    a1 = ((sft_rec *)ptr((long)r.es, (long)r.di))->fcb_fn;
    break;

  /* For findnext, an fcb style filename template is available within the
     SDA search data block field */
  case _fnext:
    if (_osmajor == 3)
      a1 = ((sda3_ptr) sda)->sdb.pattern;
    else
      a1 = ((sda4_ptr) sda)->sdb.pattern;
    break;
  }
}


/* Back up a directory level, ie go back to the previous \ in a path string */
static boolean back_1(char far *path, uchar idx)
{
  if (idx == iroot)
    return 0;
  do {
    idx--;
  } while (idx != iroot && path[idx] != '\\');
  return idx;
}


/* Check that the qualified pathname that is in a1 matches our full
   directory structure to length lsrc. If not, fail with 'Path not found' */
static boolean process_path(char far *a1, uchar lsrc)
{
  boolean Result;
  uchar isrc = 0;

  for (isrc = 0; isrc <= lsrc - 1; isrc++) {
    if (isrc > lmax || a1[isrc] != max_path[isrc]) {
      fail(3);
      return false;
    }
  }
  if (max_path[isrc] == '\\')
    return true;
  fail(3);
  return false;
}

#if 0
static LONGINT the_time()
{
  asm(" inline $b8");
  asm(" inline $0d");
  asm(" inline $12");
  asm(" inline $cd");
  asm(" inline $2f");
}
#else
static LONGINT the_time(void);
#pragma aux the_time = \
  "mov ax, 0x120d" \
  "int 0x2f" \
  modify [ax dx]
#endif

/* Change Directory - subfunction 05h */
static void cd()
{
  uchar lsrc;

  lsrc = asclen(a1);
  if (lsrc == iroot + 1)   /* Special case for root */
    lsrc--;
  if (!process_path(a1, lsrc))
    return;
  if (_osmajor == 3)   /* Copy in the new path into the CDS */
    memmove(((cds3_rec *)((sda3_ptr) sda)->drive_cdsptr)->curr_path,
	    max_path, (long)lsrc);
  else
    memmove(((cds4_rec *)((sda4_ptr) sda)->drive_cdsptr)->curr_path,
	    max_path, (long)lsrc);
  icur = lsrc;
}


/* Remove Directory - subfunction 01h */
static void rd()
{
  uchar lsrc;

  lsrc = asclen(a1);
  if (!process_path(a1, lsrc))
    return;
  if (lsrc == icur) {
    fail(5);
    return;
  }
  if (lsrc == ifile) {
    fail(5);
    return;
  }
  if (lsrc != lmax) {
    fail(5);
    return;
  }
  if (!(lmax = back_1(max_path, lmax))) {
    fail(3);
    return;
  }
  max_path[lmax+1] = '\0';
}


/* Make Directory - subfunction 03h */
static void md()
{
  uchar lsrc, isrc;

  lsrc = asclen(a1);
  isrc = lsrc;
  if (!(isrc = back_1(a1, isrc))) {
    fail(5);
    return;
  }
  if (!process_path(a1, isrc))
    return;
  if (isrc != lmax) {
    fail(5);
    return;
  }
  _fmemmove(max_path, a1, (long)lsrc);
  max_path[lsrc] = '\\';
  max_path[lsrc+1] = '\0';
  lmax = lsrc;
}

static unsigned short dec_SFT(void far *);
#pragma aux dec_SFT = \
  "mov ax, 0x1208" \
  "int 0x2f" \
  parm [es di] \
  modify [ax]

void set_Owner(void far *);
#pragma aux set_Owner = \
  "mov ax, 0x120C" \
  "int 0x2F" \
  parm [es di] \
  modify [ax]

/* Close File - subfunction 06h */
static void clsfil()
{
  sft_rec *WITH;

  /* Clear down supplied SFT entry for file */
  WITH = (sft_rec *)ptr((long)r.es, (long)r.di);
  if (dec_SFT(WITH) == 1) {
    WITH->handle_cnt = 0;
    WITH->dir_sector = 0;   /* ??? MSCDEX does it.. */
    WITH->devdrv_ptr = NULL;   /* ??? MSCDEX does it.. */
  }
  if (!(boolean)(WITH->open_mode & 3) || (boolean)(WITH->dev_info & 0x40))
    return;
  /* if new or updated file... */
  if (WITH->f_time == 0)
    file_time = the_time();
  else
    file_time = WITH->f_time;
}


/* Commit File - subfunction 07h */
static void cmmtfil()
{
  /* We support this but don't do anything... */
}


/* Read from File - subfunction 08h */
static void readfil()
{
  sft_rec *WITH;

  /* Fill the user's buffer (the DTA) from our internal; file buffer,
     and update the suplied SFT for the file */
  WITH = (sft_rec *)ptr((long)r.es, (long)r.di);
  /* if (f_pos+r.cx)>f_size then r.cx:=f_size-f_pos; */
  if (WITH->f_pos >= WITH->f_size)
    r.cx = 0;
  else if (WITH->f_pos + r.cx > WITH->f_size)
    r.cx = WITH->f_size - WITH->f_pos;
  if (_osmajor == 3)
    _fmemmove(((sda3_ptr) sda)->curr_dta, (&file_buffer[WITH->f_pos]),
	    (long)r.cx);
  else
    _fmemmove(((sda4_ptr) sda)->curr_dta, (&file_buffer[WITH->f_pos]),
	    (long)r.cx);
  WITH->f_pos += r.cx;
}


/* Write to File - subfunction 09h */
static void writfil()
{
  sft_rec *WITH;

  /* Update our internal file buffer from the user buffer (the DTA) and
     update the supplied SFT entry for the file */
  WITH = (sft_rec *)ptr((long)r.es, (long)r.di);
  if ((boolean)(file_attr & ReadOnly)) {
    fail(5);
    return;
  }
  if (WITH->f_pos + r.cx > maxfilesize)
    r.cx = maxfilesize - WITH->f_pos;
  if (_osmajor == 3)
    _fmemmove((&file_buffer[WITH->f_pos]), ((sda3_ptr) sda)->curr_dta,
	    (long)r.cx);
  else
    _fmemmove((&file_buffer[WITH->f_pos]), ((sda4_ptr) sda)->curr_dta,
	    (long)r.cx);
  WITH->f_pos += r.cx;
  if (WITH->f_pos > file_size)
    file_size = WITH->f_pos;
  WITH->f_size = file_size;
  WITH->dev_info &= ~0x40;
}


/* Get Disk Space - subfunction 0Ch */
static void dskspc()
{
  /* Our 'disk' has 1 cluster containing 1 sector of maxfilesize bytes, and ... */
  r.ax = 1;
  r.bx = 1;
  r.cx = maxfilesize + 1;
  /* ... its either all available or none! */
  r.dx = (ifile == 0);
}


/* Set File Attributes - subfunction 0Eh */
static void setfatt()
{
  uchar lsrc, isrc;

  lsrc = asclen(a1);
  isrc = lsrc;
  if (!(isrc = back_1(a1, isrc))) {
    fail(2);
    return;
  }
  if (!process_path(a1, isrc))
    return;
  if (isrc != ifile) {
    fail(2);
    return;
  }
  isrc++;
  memset(temp_name, '\0', 13L);
  _fmemmove(temp_name, (&a1[isrc]), (long)(lsrc - isrc));
  if (strncmp(temp_name, file_name, sizeof(fcbfnbuf))) {
    fail(2);
    return;
  }
  /*    if file_opens>0 then fail(5)
      else */
  file_attr = *(uchar *)ptr((long)r.ss, (long)r.sp);
}


/* Get File Attributes - subfunction 0Fh */
static void getfatt()
{
  uchar lsrc, isrc;

  lsrc = asclen(a1);
  isrc = lsrc;
  if (!(isrc = back_1(a1, isrc))) {
    fail(2);
    return;
  }
  if (!process_path(a1, isrc))
    return;
  if (isrc != ifile) {
    fail(2);
    return;
  }
  isrc++;
  memset(temp_name, '\0', 13L);
  _fmemmove(temp_name, (&a1[isrc]), (long)(lsrc - isrc));
  if (strncmp(temp_name, file_name, sizeof(fcbfnbuf))) {
    fail(2);
    return;
  }
  /*    if file_opens>0 then begin fail(5); exit; end; */
  r.ax = file_attr;
}


/* Rename File - subfunction 11h */
static void renfil()
{
  uchar lsrc, isrc;

  if (_osmajor == 3) {
    a2 = (Char *)ptr((long)r.ss, (long)((sda3_ptr) sda)->fn2_csofs);
  }
  else {
    a2 = (Char *)ptr((long)r.ss, (long)((sda4_ptr) sda)->fn2_csofs);
  }
  lsrc = asclen(a1);
  isrc = lsrc;
  if (!(isrc = back_1(a1, isrc))) {
    fail(3);
    return;
  }
  if (!process_path(a1, isrc))
    return;
  if (isrc != ifile) {
    fail(2);
    return;
  }
  isrc++;
  memset(temp_name, '\0', 13L);
  _fmemmove(temp_name, (&a1[isrc]), (long)(lsrc - isrc));
  /* Check that the current filename matches ours */
  if (strncmp(temp_name, file_name, sizeof(fcbfnbuf))) {
    fail(2);
    return;
  }
  if ((boolean)(file_attr & 0x7)) {
    fail(5);
    return;
  }
  /*    if file_opens>0 then begin fail(5); exit; end; */
  lsrc = asclen(a2);
  isrc = lsrc;
  if (!(isrc = back_1(a2, isrc))) {
    fail(3);
    return;
  }
  if (!process_path(a2, isrc))
    return;
  ifile = isrc;
  isrc++;
  /* Put in the new file name */
  memset(file_name, '\0', 13L);
  memmove(file_name, (&a2[isrc]), (long)(lsrc - isrc));
}


/* This procedure does a wildcard match from the mask onto the target, and,
   if a hit, updates the search data block and found file areas supplied */
static boolean match(char far *mask, char far *tgt,
		     SRCHREC_PTR search, DIRREC_PTR dirent,
		     uint16_t d_e, uint16_t p_c,
		     uint8_t s_a)
{
  uchar i = 0, j = 0;

  //consolef("MATCH \"%ls\" \"%ls\"\n", mask, tgt);
  if (tgt[0] == '\0' || tgt[0] == '\\')
    return false;
  while (i < 11) {
    switch (mask[i]) {
    case '?':
      if (tgt[j] == '\0' || tgt[j] == '.' || tgt[j] == '\\') {
	if (i == 8 && tgt[j] == '.')
	  j++;
	else
	  i++;
      }
      else {
	i++;
	j++;
      }
      break;

    case ' ':
      if (tgt[j] != '\0' && tgt[j] != '\\' && tgt[j] != '.')
	return false;
      i++;
      break;

    default:
      if (i == 8 && tgt[j] == '.')
	j++;
      else {
	if (tgt[j] != mask[i])
	  return false;
	i++;
	j++;
      }
      break;
    }
  }
  if (tgt[j] != '\0' && tgt[j] != '\\')
    return false;
  _fmemmove(search->pattern, mask, 11);
  search->sequence = d_e;
  search->attr_mask = s_a;
  search->sector = p_c;
  search->drive_num = drive_no | 0x80;
  i = 0;
  j = 0;
  _fmemset(dirent->name, ' ', 11);
  while (tgt[i] != '\0' && tgt[i] != '\\') {
    if (tgt[i] == '.') {
      j = 8;
      i++;
    }
    else {
      dirent->name[j] = tgt[i];
      i++;
      j++;
    }
  }
  switch (d_e) {
  case 0:
    dirent->attr = 0x8;
    break;

  case 1:
    dirent->attr = 0x10;
    break;

  case 2:
    dirent->attr = file_attr;
    break;
  }
  dirent->time = file_time;
  dirent->date = file_date;
  switch (d_e) {

  case 0:
  case 1:
    dirent->size = 0;
    break;

  case 2:
    dirent->size = file_size;
    break;
  }
  dumpHex(search, sizeof(*search), 0);
  dumpHex(dirent, sizeof(*dirent), 0);
  return true;
}


/* Delete File - subfunction 13h */
static void delfil()
{
  uchar isrc, lsrc;
  SRCHREC sdb;   /* These are dummies for the match procedure to hit */
  DIRREC der;

  lsrc = asclen(a1);
  isrc = lsrc;
  if (!(isrc = back_1(a1, isrc))) {
    fail(3);
    return;
  }
  if (!process_path(a1, isrc))
    return;
  if (isrc != ifile) {
    fail(2);
    return;
  }

  a1 += isrc + 1;
  cnvt2fcb(a1, temp_name);
  if ((file_attr & 0x1f) > 0) {
    fail(5);
    return;
  }
  if (!match(temp_name, file_name, &sdb, &der, 0, 0, 0)) {
    fail(2);
    return;
  }
  /* if file_opens=0 then */
  ifile = 0;
  /* else fail(5) */
}


/* Open Existing File - subfunction 16h */
static void opnfil()
{
  uchar isrc, lsrc;
  sft_rec *WITH;

  lsrc = asclen(a1);
  isrc = lsrc;
  if (!(isrc = back_1(a1, isrc))) {
    fail(3);
    return;
  }
  if (!process_path(a1, isrc))
    return;
  if (isrc != ifile) {
    fail(2);
    return;
  }
  isrc++;
  memset(temp_name, '\0', 13L);
  _fmemmove(temp_name, (&a1[isrc]), (long)(lsrc - isrc));
  /* Check file names match */
  if (strncmp(temp_name, file_name, sizeof(fcbfnbuf))) {
    fail(2);
    return;
  }

  /* Initialize supplied SFT entry */
  WITH = (sft_rec *)ptr((long)r.es, (long)r.di);
  file_attr = *(uchar *)ptr((long)r.ss, (long)r.sp);
  if (_osmajor == 3)
    WITH->open_mode = ((sda3_ptr) sda)->open_mode & 0x7f;
  else
    WITH->open_mode = ((sda4_ptr) sda)->open_mode & 0x7f;
  cnvt2fcb(temp_name, WITH->fcb_fn);
  /*   inc(file_opens); */
  WITH->f_size = file_size;
  WITH->f_time = file_time;
  WITH->dev_info = 0x8040L | drive_no;   /* Network drive, unwritten to */
  WITH->dir_sector = 0;
  WITH->dir_entryno = 0;
  WITH->attr_byte = file_attr;
  WITH->f_pos = 0;
  WITH->devdrv_ptr = NULL;
  set_Owner(MK_FP(r.es, r.di));
}


/* Truncate/Create File - subfunction 17h */
static void creatfil()
{
  uchar isrc, lsrc;
  sft_rec *WITH;

  lsrc = asclen(a1);
  isrc = lsrc;
  if (!(isrc = back_1(a1, isrc))) {
    fail(3);
    return;
  }
  if (!process_path(a1, isrc))
    return;

  if (ifile == 0) {
    /* Creating new file */
    ifile = isrc;
    isrc++;
    if (isrc == lsrc) {
      fail(13);
      ifile = 0;
      return;
    }
    memset(file_name, '\0', 13L);
    _fmemmove(file_name, (&a1[isrc]), (long)(lsrc - isrc));
  }
  else {
    if (ifile == isrc) {
      isrc++;
      memset(temp_name, '\0', 13L);
      _fmemmove(temp_name, (&a1[isrc]), (long)(lsrc - isrc));
      if (strncmp(temp_name, file_name, sizeof(fcbfnbuf))) {
	fail(2);
	return;
      }
      if ((boolean)(file_attr & 0x7)) {
	fail(5);
	return;
      }
      /*  if file_opens>0 then begin fail(5); exit; end; */
    }
    /* Truncate existing file */
    else
      fail(82);
  }

  /* This provokes a 'ran out of dir entries' error */

  /* Initialize supplied SFT entry */
  WITH = (sft_rec *)ptr((long)r.es, (long)r.di);
  file_attr = *(uchar *)ptr((long)r.ss, (long)r.sp);
      /* File attr is top of stack */
  WITH->open_mode = 0x1;   /* assume an open mode, none is supplied.. */
  cnvt2fcb(file_name, WITH->fcb_fn);
  /* inc(file_opens); */
  WITH->f_size = 0;
  WITH->f_pos = 0;
  file_size = 0;
  WITH->dev_info = 0x8040L | drive_no;   /* Network drive, unwritten to */
  WITH->dir_sector = 0;
  WITH->dir_entryno = 0;
  WITH->f_time = 0;
  WITH->devdrv_ptr = NULL;
  WITH->attr_byte = file_attr;
  set_Owner(MK_FP(r.es, r.di));
}


/* Special Multi-Purpose Open File - subfunction 2Eh */
static void spopnfil()
{
  uchar isrc, lsrc;
  unsigned short action, mode, result;
  sft_rec *WITH;

  lsrc = asclen(a1);
  isrc = lsrc;
  if (!(isrc = back_1(a1, isrc))) {
    fail(3);
    return;
  }
  if (!process_path(a1, isrc))
    return;
  mode = ((sda4_ptr) sda)->spop_mode & 0x7f;
  action = ((sda4_ptr) sda)->spop_act;
  /* First, check if file must or must not exist */
  if ((action & 0xf) == 0 && isrc != 0 || (action & 0xf0) == 0 && isrc == 0) {
    fail(5);
    return;
  }

  if (ifile == 0) {
    /* Creating new file */
    result = 2;
    ifile = isrc;
    isrc++;
    if (isrc == lsrc) {
      fail(13);
      ifile = 0;
      return;
    }
    memset(file_name, '\0', 13L);
    _fmemmove(file_name, (&a1[isrc]), (long)(lsrc - isrc));
  }
  else {
    if (ifile == isrc) {
      isrc++;
      memset(temp_name, '\0', 13L);
      _fmemmove(temp_name, (&a1[isrc]), (long)(lsrc - isrc));
      if (strncmp(temp_name, file_name, sizeof(fcbfnbuf))) {
	fail(82);
	return;
      }
      if ((boolean)(action & 2))
	result = 3;   /* File existed, was replaced */
      else
	result = 1;
      /* File existed, was opened */
      if ((boolean)(file_attr & 0x1) && (result == 3 || (mode & 3) > 0)) {
	fail(5);
	return;
      }  /* It's a read only file */
      if (result == 3) {   /* and (file_opens>0) */
	fail(5);
	return;
      }  /* Truncating an open file */
    }
    /* Open/Truncate existing file */
    else
      fail(5);
  }


  /* Initialize the supplied SFT entry */
  WITH = (sft_rec *)ptr((long)r.es, (long)r.di);
  if (result > 1) {
    file_attr = *(uchar *)ptr((long)r.ss, (long)r.sp);
	/* Attr is top of stack */
    WITH->f_size = 0;
    file_size = 0;
  }
  WITH->open_mode = mode;
  cnvt2fcb(file_name, WITH->fcb_fn);
  /* inc(file_opens); */
  WITH->f_pos = 0;
  WITH->f_time = 0;
  WITH->dev_info = 0x8040L | drive_no;   /* Network drive, unwritten to */
  WITH->dir_sector = 0;
  WITH->dir_entryno = 0;
  WITH->devdrv_ptr = NULL;
  WITH->attr_byte = file_attr;
  set_Owner(MK_FP(r.es, r.di));
}


/* FindFirst - subfunction 1Bh */
static void ffirst()
{
  uchar isrc;
  SRCHREC_PTR sdb;
  DIRREC_PTR der;
  uchar sa, fa;

  //consolef("FFIRST 0\n");
  isrc = asclen(a1);
  if (!(isrc = back_1(a1, isrc))) {
    fail(3);
    return;
  }
  //consolef("FFIRST 1: %i\n", isrc);
  if (!process_path(a1, isrc))
    return;
  //consolef("FFIRST 2\n");
  a2 = max_path;
  if (_osmajor == 3) {
    a1 = ((sda3_ptr) sda)->fcb_fn1;
    sdb = &((sda3_ptr) sda)->sdb;
    der = &((sda3_ptr) sda)->found_file;
    sa = ((sda3_ptr) sda)->srch_attr;
  }
  else {
    a1 = ((sda4_ptr) sda)->fcb_fn1;
    sdb = &((sda4_ptr) sda)->sdb;
    der = &((sda4_ptr) sda)->found_file;
    sa = ((sda4_ptr) sda)->srch_attr;
  }
  fa = file_attr & 0x1e;
  a2 += isrc + 1;

  //consolef("FFIRST 3 %i %i %i\n", sa, isrc, iroot);
  /* First try and match volume label, if asked for */
  if ((sa == 0x8 || ((sa & 0x8) && isrc == iroot)) &&
      match(a1, vollab, sdb, der, 0, isrc, sa))
    return;

  //consolef("FFIRST 4\n");
  /* Then try the one possible subdirectory, if asked for and if it exists */
  if ((sa & 0x10) &&
      match(a1, a2, sdb, der, 1, isrc, sa))
    return;

  //consolef("FFIRST 5\n");
  /* Finally try the one possible file, if asked for, if it exists, and if
     in this subdirectory */
  if (ifile == isrc && (fa == 0 || (sa & fa)) &&
      match(a1, file_name, sdb, der, 2, isrc, sa))
    return;
  //consolef("FFIRST 6\n");

  /* Otherwise report no more files */
  fail(18);
}


/* FindFirst - subfunction 1Bh */
static void fnext()
{
  uchar fa;
  SRCHREC_PTR sdb;
  DIRREC_PTR der;

  consolef("FNEXT 1\n");
  if (_osmajor == 3) {
    sdb = &((sda3_ptr) sda)->sdb;
    der = &((sda3_ptr) sda)->found_file;
  }
  else {
    sdb = &((sda4_ptr) sda)->sdb;
    der = &((sda4_ptr) sda)->found_file;
  }
  consolef("FNEXT 2\n");
  fa = file_attr & 0x1e;
  sdb->sequence++;
  switch (sdb->sequence) {
  case 1:
    a2 = &max_path[sdb->sector + 1];
    break;

  case 2:
    a2 = file_name;
    break;

  default:
    fail(18);
    return;
    break;
  }

  consolef("FNEXT 3\n");
  /* First try the one possible subdirectory, if it exists. FNext can never
     match a volume label */
  if (sdb->sequence == 1 && (boolean)(sdb->attr_mask & 0x10) &&
      match(a1, a2, sdb, der, sdb->sequence, sdb->sector,
	    sdb->attr_mask))
    return;

  consolef("FNEXT 4\n");
  /* Then try the one possible file, if exists, and if in this subdirectory */
  if (sdb->sequence == 1) {
    a2 = file_name;
    sdb->sequence = 2;
  }
  if (sdb->sequence == 2 && ifile == sdb->sector &&
      (fa == 0 || (boolean)(sdb->attr_mask & fa)) &&
      match(a1, a2, sdb, der, sdb->sequence, sdb->sector,
	    sdb->attr_mask))
    return;

  consolef("FNEXT 5\n");
  /* Otherwise return no more files */
  fail(18);
}


/* Seek From End Of File - subfunction 21h */
static void skfmend()
{
  LONGINT skamnt;
  sft_rec *WITH;

  skamnt = (long)r.cx * 65536L + r.dx;
  /* if file_opens=0 then begin fail(5); exit; end; */

  /* Update supplied SFT entry for file */
  WITH = (sft_rec *)ptr((long)r.es, (long)r.di);
  WITH->f_pos = WITH->f_size - skamnt;
  r.dx = ((unsigned long)WITH->f_pos) >> 16;
  r.ax = WITH->f_pos & 0xffffL;
}


static boolean call_for_us(sft_rec far *rec)
{
  const char far *path;

  if ((fxn >= _close && fxn <= _unlock) || fxn == _seek) {
    //consolef("CFU DRIVE: %c\n", (rec->dev_info & 0x1f) + 'A');
    return ((rec->dev_info & 0x1f) == drive_no);
  }
  else {
    if (fxn == _inquiry)
      return true;
    else {
      if (_osmajor == 3)
	path = ((sda3_rec far *) sda)->drive_cdsptr;
      else
	path = ((sda4_rec far *) sda)->drive_cdsptr;
      //consolef("CFU PATH \"%ls\"\n", path);
      //consolef("OUR PATH: \"%s\"\n", max_path);
      return (_fstrncmp(path, max_path, sizeof(cdsidarr)) == 0);
    }
  }
}


/* Local variables for redirector: */
struct LOC_redirector {
  unsigned short _bp;
} ;


void set_intr_retval(uint16_t);
#pragma aux set_intr_retval = \
  "mov ss:[bp+22],ax" \
  "test ax,ax"	      \
  "jnz is_err"     \
  "clc"		      \
  "jmp done"	      \
  "is_err: stc"    \
  "done:"	      \
  parm [ax]

/* This is the main entry point for the redirector. The procedure is actually
   invoked from the Int 2F ISR stub via a PUSHF and a CALL FAR IMMEDIATE
   instruction to simulate an interrupt.  That way we have many of the
   registers on the stack and DS set up for us by the TP interrupt keyword.
   This procedure saves the registers into the regset variable, assesses if
   the call is for our drive, and if so, calls the appropriate routine. On
   exit, it restores the (possibly modified) register values. */
static void interrupt redirector(union INTPACK regs)
{
  struct LOC_redirector V;

  V._bp = regs.x.bp;
  isr->our_drive = false;
  /* If we don't support the call, pretend we didn't see it...! */
  if (regs.h.ah != 0x11 || regs.h.al > fxn_map_max)
    _chain_intr(prev_hndlr);
  fxn = fxn_map[regs.h.al];
  if (fxn == _unsupported)
    _chain_intr(prev_hndlr);
  /* If the call isn't for our drive, jump out here... */
  if (!call_for_us(MK_FP(regs.x.es, regs.x.di)))
    return;
  consolef("REDIR FUNC 0x%02x/%02x\n", regs.h.ah, regs.h.al);
  /* Set up our full copy of the registers */
  isr->our_drive = true;
  _fmemmove((&r.bp), (&V._bp), sizeof(unsigned short) * 9L);
  r.ss = isr->save_ss;
  r.sp = isr->save_sp;
  r.cs = isr->save_cs;
  r.ip = isr->save_ip;
  r.flags = isr->real_fl;
  r.ax = 0;
  r.flags &= ~INTR_CF;
  set_fn1();
  switch (fxn) {

  case _inquiry:
    r.ax = 0xff;
    break;

  case _rd:
    rd();
    break;

  case _md:
    md();
    break;

  case _cd:
    cd();
    break;

  case _close:
    clsfil();
    break;

  case _commit:
    cmmtfil();
    break;

  case _read:
    readfil();
    break;

  case _write:
    writfil();
    break;

  case _space:
    dskspc();
    break;

  case _setattr:
    setfatt();
    break;

  case _lock:
  case _unlock:
    /* blank case */
    break;

  case _getattr:
    getfatt();
    break;

  case _rename:
    renfil();
    break;

  case _delete:
    delfil();
    break;

  case _open:
    opnfil();
    break;

  case _create:
    creatfil();
    break;

  case _specopen:
    spopnfil();
    break;

  case _ffirst:
    ffirst();
    break;

  case _fnext:
    fnext();
    break;

  case _seek:
    skfmend();
    break;
  }
  /* Restore the registers, including any that we have modified.. */
  _fmemmove((&V._bp), (&r.bp), sizeof(unsigned short) * 9L);
  isr->save_ss = r.ss;
  isr->save_sp = r.sp;
  isr->save_cs = r.cs;
  isr->save_ip = r.ip;
  isr->real_fl = r.flags;
  set_intr_retval(r.ax);
}

#if 0
/* This procedure sets up our ISR stub as a structure on the heap. It
   also ensures that the structure is addressed from an offset of 0 so
   that the CS overridden offsets in the ISR code line up. Finally. it
   fixes in some values which are only available to us at run time,
   either because they are variable, or because of limitations of the
   language. */
static void init_isr_code()
{
  void far *p;

  isr = (isr_rec *) malloc(sizeof(isr_rec) + 15);	// getmem(isr,sizeof(isr_rec)+15);
  isr = (isr_rec *)
    MK_FP(FP_SEG(isr)+(FP_OFF(isr) + 15) >> 4, 0);	// inc(os(isr).s,(os(isr).o+15) shr 4);
  memcpy(isr->ic, isr_code, sizeof(isr_code_buffer));	// isr^.ic:=isr_code;
  p = _dos_getvect(0x2f);				// getintvec($2f,p);
  *((void far **)&isr->ic[redir_entry]) = redirector;	// os(isr).o:=redir_entry; pointer(i^):=@redirector;
  *((uint16_t *)&isr->ic[our_ss_ofs]) = getSS();	// os(isr).o:=our_ss_ofs; word(i^):=sseg;
  *((uint16_t *)&isr->ic[our_sp_ofs]) = our_sp;		// os(isr).o:=our_sp_ofs; word(i^):=our_sp;
  *((void far **)&isr->ic[prev_hndlr]) = p;		// os(isr).o:=prev_hndlr; pointer(i^):=p;
}
#endif

#if 0
Local uchar installed_2f()
{
  /* mov ax,1100h   int 2fh */
  asm(" inline $b8");
  asm(" inline $00");
  asm(" inline $11");
  asm(" inline $cd");
  asm(" inline $2f");
}
#else
uchar installed_2f(void);
#pragma aux installed_2f = \
  "mov ax, 0x1100" \
  "int 2fh" \
  modify [ax]
#endif

/* Do our initializations */
static void init_vars()
{
  if (installed_2f() == 1)
    failprog("Not OK to install a redirector...");
  drive_no = drive[0] - '@';
  our_sp = getSP() + 0x100;
  /* file_opens:=0; */
  /* Note that the assumption is that we lost 100h bytes of stack
     on entry to main */
#if 0
  /* Initialise and fix-up the master copy of the ISR code */
  init_isr_code();
#endif
  ifile = 0;
}


/* This is where we do the initializations of the DOS structures
   that we need in order to fit the mould */
static void set_path_entry()
{
  cds3_rec far *our_cds;
  uint16_t cds_size;

  our_cds = lol->cds;
  cds_size = sizeof(cds4_rec);
  if (_osmajor == 3)
    cds_size = sizeof(cds3_rec);
  printf("CDS: 0x%08lx  size: %i\n", (uint32_t) our_cds, cds_size);
  our_cds = MK_FP(FP_SEG(our_cds), FP_OFF(our_cds) + cds_size * (drive_no - 1));
  printf("Last drive: %c:\n", lol->last_drive + 'A');
  if (drive_no > lol->last_drive)
    failprog("Drive letter higher than last drive...");

  /* Edit the Current Directory Structure for our drive */
  _fstrncpy(strbuf, our_cds->curr_path, 255);
  _SETIO(printf("Curr path is %s\n", strbuf) >= 0, FileWriteError);
  if ((our_cds->flags & 0xc000L) != 0)
    failprog("Drive already assigned.");
  our_cds->flags |= 0xc000L;   /* Network+Physical bits on ... */
  strcpy(strbuf, cds_id);
  strbuf[strlen(strbuf) - 3] = (Char)('@' + drive_no);
  _fstrcpy(our_cds->curr_path, strbuf);
  _fstrcpy(max_path, strbuf);
  our_cds->root_ofs = strlen(strbuf) - 1;
  iroot = our_cds->root_ofs;
  lmax = iroot;
}


static void tsr()
{
  uint16_t end;
  void far *heap = sbrk(0);
  uint16_t far *psp_ptr;


  end = (FP_SEG(heap) << 4) + FP_OFF(heap);
  end -= _psp << 4;
  end += 15;

  printf("Heap: 0x%08lx  PSP: 0x%04x\n", (uint32_t) heap, _psp);
  printf("CS: 0x%04x\n", getCS());
  printf("Para: %04x\n", end);
  psp_ptr = MK_FP(_psp, 0);
  printf("Top seg: %04x\n", psp_ptr[1]);
  _dos_keep(0, end >> 4);
}


static void settle_down()
{
  int idx;
  unsigned short w = 0x80;
  uint32_t far *ptr;

  /* Plug ourselves into Int 2F */
  getintvec(0x2f, prev_hndlr);
  setintvec(0x2f, redirector);
  _SETIO(printf("Phantom drive installed as %c:\n", drive[0]) >= 0,
	 FileWriteError);
  /* Find ourselves a free interrupt to call our own. Without it, future
     invocations of Phantom will not be able to unload us. */
  for (idx = 0x60; idx < 0x67; idx++) {
    ptr = MK_FP(0, idx << 2);
    if (!*ptr)
      break;
  }
  if (idx == 0x68) {
    _SETIO(printf("No user intrs available. PHANTOM not unloadable..\n") >= 0,
	   FileWriteError);
    tsr();
  }
  /* Have our new found interrupt point at the command line area of
     our PSP. Complete our signature record, put it into the command line,
     and go to sleep. */
  setintvec(idx, ptr(prefixseg, w));
  our.psp = prefixseg;
  our.drive_no = drive_no;
  *(sig_rec *)ptr(prefixseg, w) = our;
  tsr();
}


/* Find the latest Phantom installed, unplug it from the Int 2F chain if
   possible, undo the dpb chain, make the CDS reflect an invalid drive,
   and free its memory.. */
static void do_unload()
{
  long i = 0x67;
  Anyptr p;
  uint8_t far *cds;
  Registers r;
  struct SREGS sr;
  cds3_rec *WITH;

  while (i >= 0x60 &&
	 strcmp(((sig_rec *)(*(Anyptr *)ptr(0L, i << 2)))->signature,
		our.signature))
    i--;
  if (i == 0x5f) {
    _SETIO(printf("%s not found...\n", our.signature) >= 0, FileWriteError);
    _Escape(0);
  }
  getintvec(0x2f, p);
  if (p != 0)
    failprog("2F superceded...");
  setintvec(0x2f, prev_hndlr);
  getintvec(i, p);
  drive_no = ((sig_rec *)p)->drive_no;
  r.x.ax = 0x4900;
  sr.es = ((sig_rec *)p)->psp;
  msdos(r);
  if ((boolean)(getFlags() & INTR_CF))
    _SETIO(printf("Could not free main memory...\n") >= 0, FileWriteError);
  setintvec(i, NULL);
  cds = lol->cds;
  if (_osmajor == 3)
    cds += sizeof(cds3_rec) * (drive_no - 1);
  else
    cds += sizeof(cds4_rec) * (drive_no - 1);
  WITH = (cds3_rec *)cds;
  WITH->flags &= 0x3fff;
  _SETIO(printf("Drive %c: is now invalid.\n", (Char)('@' + drive_no)) >= 0,
	 FileWriteError);
}


main(argc, argv)
int argc;
Char *argv[];
{  /* MAIN */
  memcpy(h, "0123456789abcdef", sizeof(h));
  /* Check parameter count */
  if (argc != 2)
    failprog("Usage is: PHANTOM drive-letter:");
  strcpy(drive, argv[1]);
  drive[0] = toupper(drive[0]);
  /* If this is an unload request, go to it */
  if (!strcmp(drive, "-u") || !strcmp(drive, "-U")) {
    get_dos_vars();
    do_unload();
    _Escape(0);
  }
  /* Otherwise, check that it's a valid drive letter */
  if (strlen(drive) > 2 || !isupper(drive[0]) ||
      strlen(drive) == 2 && drive[1] != ':')
    failprog("Usage is: PHANTOM drive-letter:");
  /* ... and set up shop */
  init_vars();
  get_dos_vars();
  set_path_entry();
  settle_down();
  exit(EXIT_SUCCESS);
}



/* End. */
