/*	******************************************************

	PHANTOM.C - A network-redirector based XMS Ram Disk
	Copyright (c) David Maxey 1993.  All rights reserved.
	From "Undocumented DOS", 2nd edition (Addison-Wesley, 1993)

	Much of this code is explained in depth in Undocumented
	DOS, 2nd edition, chapter 8, which has a 60-page description
	of the network redirector, and an in-depth examination of
	how Phantom handles file read, open, ffirst, cd, and md.
	UndocDOS also contains a full specification for the redirector
	interface.

	07/25/93 - Drive number should be one less. AES found using
				FILES/USEFCB. Porbably same as Markun/LanMan
				problem. Search for ref: DR_TOO_HIGH

	Requires Microsoft C.  (Sorry, Borland users.)

	****************************************************** */

#include <stdlib.h>
#include <dos.h>
#include <memory.h>
#include <string.h>
#include <ctype.h>
#include <fcntl.h>
#include <bios.h>
#include <time.h>

#include "../../fujinet-rs232/sys/print.h"

/* ****************************************************
   Basic typedefs
   **************************************************** */

typedef unsigned int uint;
typedef unsigned char uchar;
typedef unsigned long ulong;

/* ****************************************************
   Constants and Macros
   **************************************************** */

#define		TRUE				1
#define		FALSE				0

#define		STACK_SIZE			1024
#define		FCARRY				0x0001
#define		SECTOR_SIZE			1024    // 1024b/sector allows for 64M of XMS
#define		FATPAGE_SIZE		128
#define		ROOTDIR_ENTRIES		128
#define		MIN_DISK_SIZE		128     // Don't load unless 128kb XMS free
#define		DEF_DISK_SIZE		0xFFFE  // Default attempted allocation is_all
#define		MAX_FXN_NO			0x2E

#ifndef MK_FP
#define MK_FP(a,b)  ((void far *)(((ulong)(a) << 16) | (b)))
#endif

#define		FREE_SECTOR_CHAIN(sec)	\
	while ((sec) != 0xFFFF) (sec) = set_next_sector((sec), 0)

#define get_sector(sec, buf)		\
	xms_copy_to_real(xms_handle, (ulong) SECTOR_SIZE * (sec), \
		SECTOR_SIZE, (uchar far *) (buf))

#define put_sector(sec, buf)		\
	xms_copy_fm_real(xms_handle, (ulong) SECTOR_SIZE * (sec), \
		SECTOR_SIZE, (uchar far *) (buf))

char *signon_string =
  "\r\n"
  "PHANTOM: A Network-Redirector Based XMS Ram Disk\r\n"
  "Copyright (c) David Maxey 1993.  All rights reserved.\r\n"
  "From \"Undocumented DOS\", 2nd edition (Addison-Wesley, 1993)\r\n";

char *usage_string =
  "Usage:\r\n"
  "    PHANTOM [-Snnnn] d:\r\n"
  " Or\r\n"
  "    PHANTOM -U\r\n"
  "\r\n"
  "where:\r\n"
  "    -Snnnn  specifies size of Ram Disk in kb of XMS\r\n"
  "    d:      specifies drive letter to use\r\n"
  "    -U      unloads the latest copy of Phantom loaded\r\n";

/* ****************************************************
   Typedefs and structures
   **************************************************** */

typedef int (far *FARPROC)(void);

#pragma pack(1)

/* XMS memory copy structure */
typedef struct {
  ulong copy_len;               /* must be EVEN */
  uint srce_hndle;              /* source handle */
  ulong srce_ofs;               /* offset in source block */
  uint dest_hndle;              /* dest handle */
  ulong dest_ofs;               /* offset in dest block */
} XMSCOPY, *XMSCOPY_PTR;

/* TSR signature and unload info structure */
typedef struct {
  uchar cmdline_len;
  char signature[10];           /* The TSR's signature string */
  uint psp;                     /* This instance's PSP */
  uchar drive_no;               /* A: is 1, B: is 2, etc. */
  uint xms_handle;              /* This instance's disk XMS handle */
  uchar far *our_handler;       /* This instance's int 2Fh handler */
  uchar far *prev_handler;      /* Previous int 2Fh handler in the chain */
} SIGREC, far *SIGREC_PTR;

/* FindFirst/Next data block - ALL DOS VERSIONS */
typedef struct {
  uchar drive_no;
  char srch_mask[11];
  uchar attr_mask;
  uint dir_entry_no;
  uint dir_sector;
  uchar f1[4];
} SRCHREC, far *SRCHREC_PTR;

/* DOS System File Table entry - ALL DOS VERSIONS */
// Some of the fields below are defined by the redirector, and differ
// from the SFT normally found under DOS
typedef struct {
  uint handle_count;
  uint open_mode;
  uchar file_attr;
  uint dev_info_word;
  uchar far *dev_drvr_ptr;
  uint start_sector;
  ulong file_time;
  long file_size;
  long file_pos;
  uint rel_sector;
  uint abs_sector;
  uint dir_sector;
  uchar dir_entry_no;
  char file_name[11];
} SFTREC, far *SFTREC_PTR;

/* DOS Current directory structure - DOS VERSION 3.xx */
typedef struct {
  char current_path[67];
  uint flags;
  uchar f1[10];
  uint root_ofs;
} V3_CDS, far *V3_CDS_PTR;

/* DOS Current directory structure - DOS VERSION 4.xx */
typedef struct {
  char current_path[67];
  uint flags;
  uchar f1[10];
  uint root_ofs;
  uchar f2[7];
} V4_CDS, far *V4_CDS_PTR;

/* DOS Directory entry for 'found' file - ALL DOS VERSIONS */
typedef struct {
  char file_name[11];
  uchar file_attr;
  uchar f1[10];
  ulong file_time;
  uint start_sector;
  long file_size;
} DIRREC, far *DIRREC_PTR;

#define		DIRREC_PER_SECTOR			(SECTOR_SIZE / sizeof(DIRREC))

/* Swappable DOS Area - DOS VERSION 3.xx */

typedef struct {
  uchar f0[12];
  uchar far *current_dta;
  uchar f1[30];
  uchar dd;
  uchar mm;
  uint yy_1980;
  uchar f2[96];
  char file_name[128];
  char file_name_2[128];
  SRCHREC srchrec;
  DIRREC dirrec;
  uchar f3[81];
  char fcb_name[11];
  uchar f4;
  char fcb_name_2[11];
  uchar f5[11];
  uchar srch_attr;
  uchar open_mode;
  uchar f6[48];
  uchar far *cdsptr;
  uchar f7[72];
  SRCHREC rename_srchrec;
  DIRREC rename_dirrec;
} V3_SDA, far *V3_SDA_PTR;

/* Swappable DOS Area - DOS VERSION 4.xx */
typedef struct {
  uchar f0[12];
  uchar far *current_dta;
  uchar f1[32];
  uchar dd;
  uchar mm;
  uint yy_1980;
  uchar f2[106];
  char file_name[128];
  char file_name_2[128];
  SRCHREC srchrec;
  DIRREC dirrec;
  uchar f3[88];
  char fcb_name[11];
  uchar f4;
  char fcb_name_2[11];
  uchar f5[11];
  uchar srch_attr;
  uchar open_mode;
  uchar f6[51];
  uchar far *cdsptr;
  uchar f7[87];
  uint action_2E;
  uint attr_2E;
  uint mode_2E;
  uchar f8[29];
  SRCHREC rename_srchrec;
  DIRREC rename_dirrec;
} V4_SDA, far *V4_SDA_PTR;

/* DOS List of lists structure - DOS VERSIONS 3.1 thru 4 */
/* We don't need much of it. */
typedef struct {
  uchar f1[22];
  V3_CDS_PTR cds_ptr;
  uchar f2[7];
  uchar last_drive;
} LOLREC, far *LOLREC_PTR;

/* DOS 4.00 and above lock/unlock region structure */
/* see lockfil() below (Thanks to Martin Westermeier.) */
typedef struct {
  ulong region_offset;
  ulong region_length;
  uchar f0[13];
  char file_name[80];           // 80 is a guess
} LOCKREC, far *LOCKREC_PTR;

/* The following structure is compiler specific, and maps
	onto the registers pushed onto the stack for an interrupt
	function. */
typedef struct {
#ifdef __BORLANDC__
  uint bp, di, si, ds, es, dx, cx, bx, ax;
#else
#ifdef __WATCOMC__
  uint gs, fs;
#endif /* __WATCOMC__ */
  uint es, ds, di, si, bp, sp, bx, dx, cx, ax;
#endif
  uint ip, cs, flags;
} ALL_REGS;

#pragma pack()

/* all the calls we need to support are in the range 0..2Eh */
/* This serves as a list of the function types that we support */
#define		_inquiry		0x00
#define		_rd			0x01
#define		_md			0x03
#define		_cd			0x05
#define		_clsfil			0x06
#define		_cmmtfil		0x07
#define		_readfil		0x08
#define		_writfil		0x09
#define		_lockfil		0x0A
#define		_unlockfil		0x0B
#define		_dskspc			0x0C
#define		_setfatt		0x0E
#define		_getfatt		0x0F
#define		_renfil			0x11
#define		_delfil			0x13
#define		_opnfil			0x16
#define		_creatfil		0x17
#define		_ffirst			0x1B
#define		_fnext			0x1C
#define		_skfmend		0x21
#define		_unknown_fxn_2D	0x2D
#define		_spopnfil		0x2E
#define		_unsupported	0xFF

typedef void (interrupt far *INTVECT) ();
typedef void (*PROC)(void);

/* ************************************************
   Global data declarations
   ************************************************ */

/* This is declared in the compiler startup code to mark the
	end of the data segment. */
extern uint end;

/* these are version independent pointers to various frequently used
	locations within the various DOS structures */
uchar far *sda_ptr;             /* ptr to SDA */
char far *current_path;         /* ptr to current path in CDS */
char far *filename_ptr;         /* ptr to 1st filename area in SDA */
char far *filename_ptr_2;       /* ptr to 2nd filename area in SDA */
char far *fcbname_ptr;          /* ptr to 1st FCB-style name in SDA */
char far *fcbname_ptr_2;        /* ptr to 2nd FCB-style name in SDA */
uchar far *srch_attr_ptr;       /* ptr to search attribute in SDA */
SRCHREC_PTR srchrec_ptr;        /* ptr to 1st Search Data Block in SDA */
SRCHREC_PTR srchrec_ptr_2;      /* ptr to 2nd Search Data Block in SDA */
DIRREC_PTR dirrec_ptr;          /* ptr to 1st found dir entry area in SDA */
DIRREC_PTR dirrec_ptr_2;        /* ptr to 1st found dir entry area in SDA */

/* Other global data items */
FARPROC xms_entrypoint = NULL;  /* obtained from Int 2fh/4310h */
#pragma aux xms_entrypoint \
  parm [ax]
ALL_REGS r;                     /* Global save area for all caller's regs */
uchar our_drive_no;             /* A: is 1, B: is 2, etc. */
char our_drive_str[3] = " :";   /* Our drive letter string */
char far *cds_path_root = "Phantom  :\\";       /* Root string for CDS */
uint cds_root_size;             /* Size of our CDS root string */
SIGREC sigrec = { 8, "PHANTOM ", 0, 0, 0 };     /* Signature record */

LOLREC_PTR lolptr;              /* pointer to List Of Lists */
uint dos_ss;                    /* DOS's saved SS at entry */
uint dos_sp;                    /* DOS's saved SP at entry */
uint our_sp;                    /* SP to switch to on entry */
uint save_sp;                   /* SP saved across internal DOS calls */
int filename_is_char_device;    /* generate_fcbname found character device name */
char our_stack[STACK_SIZE];     /* our internal stack */
uint far *stack_param_ptr;      /* ptr to word at top of stack on entry */
uint xms_handle = 0;            /* Handle of XMS allocation for disk */
uint disk_size = DEF_DISK_SIZE; /* size of XMS allocation for disk */
uint total_sectors;             /* total 1k sectors on XMS disk */
uint free_sectors;              /* unallocated sectors on XMS disk */
uchar sector_buffer[SECTOR_SIZE];       /* general purpose sector buffer */
ulong FAT_location;             /* offset within XMS allocation of disk FAT */
uint FAT_page[FATPAGE_SIZE];    /* buffer for FAT entries */
int cur_FAT_page = -1;          /* index of FAT page in buffer */
int FAT_page_dirty = FALSE;     /* Has current FAT page been updated */
uint last_sector = 0xffff;      /* last sector read into sector buffer */
INTVECT prev_int2f_vector;      /* For chaining, and restoring on unload */
int curr_fxn;                   /* Record of function in progress */

uchar fxnmap[] = {
  _inquiry,     /* 0x00h */
  _rd,  /* 0x01h */
  _unsupported, /* 0x02h */
  _md,  /* 0x03h */
  _unsupported, /* 0x04h */
  _cd,  /* 0x05h */
  _clsfil,      /* 0x06h */
  _cmmtfil,     /* 0x07h */
  _readfil,     /* 0x08h */
  _writfil,     /* 0x09h */
  _lockfil,     /* 0x0Ah */
  _unlockfil,   /* 0x0Bh */
  _dskspc,      /* 0x0Ch */
  _unsupported, /* 0x0Dh */
  _setfatt,     /* 0x0Eh */
  _getfatt,     /* 0x0Fh */
  _unsupported, /* 0x10h */
  _renfil,      /* 0x11h */
  _unsupported, /* 0x12h */
  _delfil,      /* 0x13h */
  _unsupported, /* 0x14h */
  _unsupported, /* 0x15h */
  _opnfil,      /* 0x16h */
  _creatfil,    /* 0x17h */
  _unsupported, /* 0x18h */
  _unsupported, /* 0x19h */
  _unsupported, /* 0x1Ah */
  _ffirst,      /* 0x1Bh */
  _fnext,       /* 0x1Ch */
  _unsupported, /* 0x1Dh */
  _unsupported, /* 0x1Eh */
  _unsupported, /* 0x1Fh */
  _unsupported, /* 0x20h */
  _skfmend,     /* 0x21h */
  _unsupported, /* 0x22h */
  _unsupported, /* 0x23h */
  _unsupported, /* 0x24h */
  _unsupported, /* 0x25h */
  _unsupported, /* 0x26h */
  _unsupported, /* 0x27h */
  _unsupported, /* 0x28h */
  _unsupported, /* 0x29h */
  _unsupported, /* 0x2Ah */
  _unsupported, /* 0x2Bh */
  _unsupported, /* 0x2Ch */
  _unknown_fxn_2D,      /* 0x2Dh */
  _spopnfil     /* 0x2Eh */
};

/* ------ Utility functions --------------------------------------*/

/* screen output using simple BIOS TTY to minimize library code */
/* They are here to also allow you to insert display lines for */
/* debugging/investigation */

void print_char(char c)
{
  _asm {
    mov ah, 0x0e;
    mov al, byte ptr c;
    int 0x10;
  }
}

void print_string(char far *str, int add_newline)
{
  while (*str)
    print_char(*str++);
  if (add_newline) {
    print_char('\n');
    print_char('\r');
  }
}

char *my_ltoa(ulong num)
{
  static char buf[11] = "0000000000";
  int i;
  ulong tmp;

  buf[10] = 0;
  for (i = 0; i < 10; i++) {
    tmp = num / 10;
    buf[9 - i] = (char) ('0' + (num - (10 * tmp)));
    num = tmp;
    if (i && (!num))
      break;
  }
  return (char *) &buf[9 - i];
}

char *my_hex(ulong num, int len)
{
  static char buf[9];
  static char *hex = "0123456789ABCDEF";

  buf[len] = 0;
  while (len--) {
    buf[len] = hex[num & 0xf];
    num >>= 4;
  }
  return buf;
}

/* ------------------- Internal DOS calls ------------ */

ulong dos_ftime(void)
{
  uint r_es = r.es, r_di = r.di, r_ds = r.ds;
  int result_lo, result_hi;


  _asm {
    mov save_sp, sp;             /* Save current stack pointer. */
    cli;
    mov ss, dos_ss;         /* Establish DOS's stack, current */
    mov sp, dos_sp;             /* when we got called. */
    sti;
    mov ax, 0x120d;         /* Get time/date. */
    push di;                    /* Subfunction 120C destroys di */
    push ds;                    /* It needs DS to be DOS's DS, for DOS 5.0 */
    push es;
    mov es, r_es;
    mov di, r_di;
    mov ds, r_ds;
    int 0x2F;
    xchg ax, dx;
    pop es;
    pop ds;  /* Restore DS */
    pop di;
    mov bx, ds;          /* Restore SS (same as DS) */
    cli;
    mov ss, bx;
    mov sp, save_sp;     /* and stack pointer (which we saved). */
    sti;
    mov result_lo, ax;
    mov result_hi, dx;
  }

  return (ulong) MK_FP(result_hi, result_lo);
}

void set_sft_owner(SFTREC_PTR sft)
{
  uint r_ds = r.ds;


  _asm {
    push es;
    push di;
    les di, sft;
    mov save_sp, sp;	/* Save current stack pointer. */
    cli;
    mov ss, dos_ss;     /* Establish DOS's stack, current */
    mov sp, dos_sp;     /* when we got called. */
    sti;
    mov ax, 0x120c;     /* Claim file as ours. */
    push ds;            /* It needs DS to be DOS's DS, for DOS 5.0 */
    mov ds, r_ds;
    int 0x2F;
    pop bx;		/* Restore DS */
    mov ds, bx;         /* Restore SS (same as DS) */
    cli;
    mov ss, bx;
    mov sp, save_sp;    /* and stack pointer (which we saved). */
    sti;
    pop di;
    pop es;
  }
}

// Does fcbname_ptr point to a device name?
int is_a_character_device(uint dos_ds)
{
  int result;

  _asm {
    mov ax, 0x1223;		/* Search for device name. */
    push ds;				/* It needs DS to be DOS's DS, for DOS 5.0 */
    mov ds, dos_ds;
    int 0x2F;
    pop ds;				/* Restore DS */
    jnc is_indeed;
    mov result, FALSE;
    jmp done;
 is_indeed:
    mov result, TRUE;
  done:
  }

  return result;
}

/* ------- XMS functions ------------------------------- */

/* if XMS present, store entry point in xms_entrypoint */
int xms_is_present(void)
{
  int available;

  if (xms_entrypoint)
    return TRUE;

  available = FALSE;

  _asm {
    mov ax, 0x4300;
    int 0x2f;
    cmp al, 0x80;
    je present;
    jmp done;

 present:
    mov available, TRUE;

    mov ax, 0x4310;
    int 0x2f;
    push ds;
    mov ax, seg xms_entrypoint;
    mov ds, ax;
    mov word ptr xms_entrypoint, bx;
    mov word ptr xms_entrypoint+2, es;
    pop ds;

  done:
  }

  return available;
}

/* Return size of largest free block. Return 0 if error
	Ignore the 'No return value' compiler warning for this function. */
uint xms_kb_avail(void)
{
  _asm {
    mov ah, 0x08;
  }
  return xms_entrypoint();
}

/* Allocate a chunk of XMS and return a handle */
int xms_alloc_block(uint block_size, uint *handle_ptr)
{
  int success = FALSE;
  uint handle, result;

  _asm {
    mov ah, 0x09;
    mov dx, block_size;
  }
  result = xms_entrypoint();
  _asm {
    mov handle, dx;
  }
  if (result) {
    *handle_ptr = handle;
    success = TRUE;
  }

  return success;
}

/* free XMS memory previously allocated */
int xms_free_block(uint handle)
{
  int success = FALSE;

  _asm {
    push ds;
    mov ax, seg xms_entrypoint;
    mov ds, ax;
    mov ah, 0x0A;
    mov dx, handle;

    call dword ptr xms_entrypoint;
    pop ds;
    cmp ax, 0x0000;
    je done;
    mov success, 1
  done:
  }

  return success;
}

/* Copy from XMS into real memory */
int xms_copy_to_real(uint handle, ulong ofs_in_handle, uint len, uchar far *buf)
{
  XMSCOPY xms;
  int result;

  if (len == 0)
    return TRUE;

  if (len & 1)
    return FALSE;

  xms.copy_len = (ulong) len;
  xms.srce_hndle = handle;
  xms.srce_ofs = ofs_in_handle;
  xms.dest_hndle = 0;
  xms.dest_ofs = (ulong) buf;

  _asm {
    push	es;
    push	ds;
    push	si;
    mov		ax, seg xms_entrypoint;
    mov		es, ax;
    push	ss;
    pop		ds;
#if 0
    mov		si, bp;
    add		si, offset xms;
#else
    lea		si, xms;
#endif
    mov 	ah, 0Bh;
    call	dword ptr [xms_entrypoint];
    pop		si;
    pop		ds;
    pop		es;
    cmp		ax, 0000h;
    je		nogood;
    mov result, TRUE;
    jmp done;
  nogood:
    mov result, FALSE;
  done:
  }

  return result;
}


int xms_copy_fm_real(uint handle, ulong ofs_in_handle, uint len, uchar far *buf)
{
  XMSCOPY xms;
  int result;

  if (len == 0)
    return TRUE;

  if (len & 1)
    return FALSE;

  xms.copy_len = (ulong) len;
  xms.srce_hndle = 0;
  xms.srce_ofs = (ulong) buf;
  xms.dest_hndle = handle;
  xms.dest_ofs = ofs_in_handle;

  _asm {
    push	es;
    push	ds;
    push	si;
    mov		ax, seg xms_entrypoint;
    mov		es, ax;
    push	ss;
    pop		ds;
#if 0
    mov		si, bp;
    add		si, offset xms;
#else
    lea		si, xms;
#endif
    mov 	ah, 0Bh;
    call	dword ptr [xms_entrypoint];
    pop		si;
    pop		ds;
    pop		es;
    cmp		ax, 0000h;
    je		nogood;
    mov result, TRUE;
    jmp done;
  nogood:
    mov result, FALSE;
  done:
  }

  return result;
}

/* ------ File system functions ------------------------ */

/* Fail Phantom, print message, exit to DOS */
void failprog(char *msg)
{
  if (xms_handle)
    xms_free_block(xms_handle);
  print_string((uchar far *) msg, TRUE);
  exit(1);
}

/* Check that the page of FAT entries for the supplied sector is in
	the buffer. If it isn't, go get it, but write back the currently
	buffered page first if it has been updated. */
int check_FAT_page(uint abs_sector)
{
  int page = (int) (abs_sector / FATPAGE_SIZE);

  if (page != cur_FAT_page) {
    if (FAT_page_dirty &&
        (!xms_copy_fm_real(xms_handle,
                           FAT_location + (cur_FAT_page * (FATPAGE_SIZE * 2)),
                           FATPAGE_SIZE * 2, (uchar far *) & FAT_page)))
      return FALSE;

    if (!xms_copy_to_real(xms_handle,
                          FAT_location + (page * (FATPAGE_SIZE * 2)), FATPAGE_SIZE * 2,
                          (uchar far *) & FAT_page))
      return FALSE;
    cur_FAT_page = page;
    FAT_page_dirty = FALSE;
  }
  return TRUE;
}

/* Use the FAT to find the next sector in the chain for the current
	file/directory */
uint next_FAT_sector(uint abs_sector)
{
  if (!check_FAT_page(abs_sector))
    return 0;

  return FAT_page[abs_sector - (cur_FAT_page * FATPAGE_SIZE)];
}

/* Update the FAT entry for this sector to reflect the next sector
	in the chain for the current file/directory */
uint set_next_sector(uint abs_sector, uint next_sector)
{
  uint save_sector;

  if (!check_FAT_page(abs_sector))
    return 0;

  save_sector = FAT_page[abs_sector - (cur_FAT_page * FATPAGE_SIZE)];
  FAT_page[abs_sector - (cur_FAT_page * FATPAGE_SIZE)] = next_sector;
  if (save_sector != next_sector) {
    FAT_page_dirty = TRUE;
    if (!save_sector)
      free_sectors--;
    else if (!next_sector)
      free_sectors++;
  }
  return save_sector;
}

/* Find a free sector on the disk. Use the same algorithm as
	DOS, which is to continue looking from where the last free
	sector was found and allocated. */
uint next_free_sector(void)
{
  static uint prev_sector = 0;
  uint save_sector = prev_sector;

  for (;;) {
    if (++prev_sector == total_sectors)
      prev_sector = 1;  // Sector 0 will never be free.
    if (!next_FAT_sector(prev_sector))
      break;
    if (prev_sector == save_sector)
      return 0;
  }
  return prev_sector;
}

/* Allocate the XMS memory required for the disk, partition it
   into FAT and data areas, and initialize the FAT and the root
   directory. Note that on a DOS disk, there are really three
   areas (not counting partition table and boot sector): the FAT,
   actually 2 copies of it; the root directory, which has an upper
   limit of usually 512 entries; and the data area, 'clustered' into
   up to 65535 allocation units of contiguous sectors. We do not need
   to use clusters, since a sector size of 1k gives us a potential disk
   size of 64M, which is all that one can address using XMS anyway!
   If we put the FAT in conventional memory, we would gain some speed
   at the expense of memory footprint (2k of FAT per 1024kb disk space).
*/

void set_up_xms_disk(void)
{
  ulong count, ofs;
  uint len;
  

  if (!xms_is_present())
    failprog("XMS not present.");

  if ((disk_size = min(disk_size, xms_kb_avail())) < MIN_DISK_SIZE)
    failprog("Need a minimum of 128kb XMS.");

  // The allocation is made up of n sectors and
  // n FAT entries (2 bytes each for our 16-bit FAT)
  free_sectors = total_sectors = (uint)
    (((ulong) disk_size * 1024) / (SECTOR_SIZE + 2));

  // A little wasted here, but the accurate calculation would soak up
  // TSR code space.
  if (!xms_alloc_block(disk_size, &xms_handle))
    failprog("XMS allocation error.");

  free_sectors--;
  FAT_location = (ulong) total_sectors *SECTOR_SIZE;

  // First FAT entry belongs to the root directory, which,
  // unlike DOS, we keep in the data area, and which always starts at
  // sector 0. (We do not have to worry about disk defraggers and the
  // like).
  count = (ulong) total_sectors *2;

  ofs = FAT_location;

  memset(sector_buffer, 0, sizeof(sector_buffer));
  // Claim the first sector as used for the root directory.
  sector_buffer[0] = sector_buffer[1] = (uchar) 0xff;

  while (count > 0) {
    len = (uint) min(count, SECTOR_SIZE);
    if (!xms_copy_fm_real(xms_handle, ofs, len, (uchar far *) & sector_buffer))
      failprog("XMS error.");
    count -= len;
    ofs += len;
    sector_buffer[0] = sector_buffer[1] = 0;
  }

  memset(((DIRREC *) & sector_buffer)->file_name, ' ', 11);
  memcpy(((DIRREC *) & sector_buffer)->file_name, "PHANTOM", 7);
  ((DIRREC *) & sector_buffer)->file_attr = 0x08;
  ((DIRREC *) & sector_buffer)->file_time = dos_ftime();
  if (!xms_copy_fm_real(xms_handle, 0, SECTOR_SIZE, (uchar far *) & sector_buffer))
    failprog("XMS error.");
}

/* Split the last level of the path in the filname field of the
	SDA into the FCB-style filename area, also in the SDA */

void get_fcbname_from_path(char far *path, char far *fcbname)
{
  int i;

  _fmemset(fcbname, ' ', 11);
  for (i = 0; *path; path++)
    if (*path == '.')
      i = 8;
    else
      fcbname[i++] = *path;
}

/* See whether the filename matches the mask, one character
	position at a time. A wildcard ? in tha mask matches any
	character in the filename, any other character in the mask,
	including spaces, must match exactly */

int match_to_mask(char far *mask, char far *filename)
{
  int i;

  for (i = 0; i < 11; i++)
    if ((mask[i] != filename[i]) && (mask[i] != '?'))
      return FALSE;

  return TRUE;
}

/* Find the sector number of the start of the directory entries
	for the supplied path */

int get_dir_start_sector(char far *path, uint far *abs_sector_ptr)
{
  char fcbname[11];
  uint abs_sector = 0;
  DIRREC *dr = (DIRREC *) & sector_buffer;
  char far *next_dir;
  char far *path_end = path + _fstrlen(path);
  int i;

  while (path != path_end) {
    for (next_dir = ++path; *next_dir && (*next_dir != '\\'); next_dir++);
    *next_dir = 0;
    get_fcbname_from_path(path, fcbname);

    for (;;) {
      if (!xms_copy_to_real(xms_handle, abs_sector * SECTOR_SIZE, SECTOR_SIZE, sector_buffer))
        return FALSE;
      last_sector = abs_sector;
      for (i = 0; i < DIRREC_PER_SECTOR; i++) {
        if (dr[i].file_name[0] == (char) 0xE5)
          continue;
        if (!dr[i].file_name[0])
          i = DIRREC_PER_SECTOR;
        else if (match_to_mask(dr[i].file_name, fcbname)) {
          if (!(dr[i].file_attr & 0x10))
            return FALSE;
          abs_sector = dr[i].start_sector;
          path = next_dir;
          break;
        }
      }
      if (i < DIRREC_PER_SECTOR)
        break;
      if ((abs_sector = next_FAT_sector(abs_sector)) == 0xFFFF);
      return FALSE;
    }
  }
  if (abs_sector_ptr)
    *abs_sector_ptr = abs_sector;
  return TRUE;
}

/* Get the next directory entry that matches the specified mask,
   continuing from the supplied starting position (from the previous
   find) */

int find_next_entry(char far *mask, uchar attr_mask, char far *filename,
                    uchar far *attr_ptr, ulong far *file_time_ptr,
                    uint far *start_sec_ptr, long far *file_size_ptr,
                    uint far *dir_sector_ptr, uint far *dir_entryno_ptr)
{
  DIRREC *dr = (DIRREC *) &sector_buffer;
  int i = *dir_entryno_ptr + 1;
  uint abs_sector = *dir_sector_ptr;

  for (;;) {
    if (abs_sector != last_sector)
      if (!get_sector(abs_sector, sector_buffer))
        return FALSE;
      else
        last_sector = abs_sector;
    for (; i < DIRREC_PER_SECTOR; i++) {
      if (!dr[i].file_name[0])
        return FALSE;
      if (dr[i].file_name[0] == (char) 0xE5)
        continue;
      if (match_to_mask(mask, dr[i].file_name) &&
          (!(((attr_mask == 0x08) && (!(dr[i].file_attr & 0x08))) ||
             ((dr[i].file_attr & 0x10) && (!(attr_mask & 0x10))) ||
             ((dr[i].file_attr & 0x08) && (!(attr_mask & 0x08))) ||
             ((dr[i].file_attr & 0x04) && (!(attr_mask & 0x04))) ||
             ((dr[i].file_attr & 0x02) && (!(attr_mask & 0x02)))))) {
        *dir_sector_ptr = abs_sector;
        *dir_entryno_ptr = i;
        if (filename)
          _fmemcpy(filename, dr[i].file_name, 11);
        if (attr_ptr)
          *attr_ptr = dr[i].file_attr;
        if (file_time_ptr)
          *file_time_ptr = dr[i].file_time;
        if (file_size_ptr)
          *file_size_ptr = dr[i].file_size;
        if (start_sec_ptr)
          *start_sec_ptr = dr[i].start_sector;
        return TRUE;
      }
    }
    if ((abs_sector = next_FAT_sector(abs_sector)) == 0xFFFF)
      return FALSE;
  }

}

/* Generate a new directory entry, reusing a previously deleted
   entry, using an as yet unused entry, or allocating more space
   for the sector if no entries are available in the current
   allocation for the directory */

int create_dir_entry(uint far *dir_sector_ptr, uchar far *dir_entryno_ptr,
                     char far *filename, uchar file_attr, uint start_sector, long file_size,
                     ulong file_time)
{
  uint next_sector, dir_sector = *dir_sector_ptr;
  DIRREC *dr = (DIRREC *) & sector_buffer;
  int i;

  for (;;) {
    if (dir_sector != last_sector)
      if (!get_sector(dir_sector, sector_buffer))
        return FALSE;
      else
        last_sector = dir_sector;
    for (i = 0; i < DIRREC_PER_SECTOR; i++) {
      if (dr[i].file_name[0] && (dr[i].file_name[0] != (char) 0xE5))
        continue;
      _fmemcpy(dr[i].file_name, filename, 11);
      dr[i].file_attr = file_attr;
      dr[i].file_time = file_time;
      dr[i].file_size = file_size;
      dr[i].start_sector = start_sector;
      *dir_sector_ptr = dir_sector;
      if (dir_entryno_ptr)
        *dir_entryno_ptr = (uchar) i;
      return put_sector(dir_sector, &sector_buffer);
    }
    if ((next_sector = next_FAT_sector(dir_sector)) == 0xFFFF) {
      if (!(next_sector = next_free_sector()))
        return FALSE;
      set_next_sector(dir_sector, next_sector);
      set_next_sector(next_sector, 0xFFFF);
    }
    dir_sector = next_sector;
  }
}

/* Copy the appropriate piece of data from XMS into the user buffer */

void read_data(long far *file_pos_ptr, uint *len_ptr, uchar far *buf,
               uint start_sector, uint far *last_rel_ptr, uint far *last_abs_ptr)
{
  uint start, rel_sector, abs_sector;
  uint i, count, len = *len_ptr;

  start = (uint) (*file_pos_ptr / SECTOR_SIZE);

  if (start < *last_rel_ptr) {
    rel_sector = 0;
    if ((abs_sector = start_sector) == 0xFFFF) {
      *len_ptr = 0;
      return;
    }
  }
  else {
    rel_sector = *last_rel_ptr;
    abs_sector = *last_abs_ptr;
  }

  while (len) {
    start = (uint) (*file_pos_ptr / SECTOR_SIZE);
    if (start > rel_sector) {
      if ((abs_sector = next_FAT_sector(abs_sector)) == 0xFFFF) {
        *len_ptr -= len;
        goto update_sectors;
      }
      rel_sector++;
      continue;
    }
    i = (int) (*file_pos_ptr % SECTOR_SIZE);
    count = min((uint) SECTOR_SIZE - i, len);
    if (count < SECTOR_SIZE) {
      if (!get_sector(abs_sector, &sector_buffer)) {
        *len_ptr -= len;
        goto update_sectors;
      }
      last_sector = abs_sector;
      _fmemcpy(buf, (uchar far *) & sector_buffer[i], count);
    }
    else if (!get_sector(abs_sector, buf)) {
      *len_ptr -= len;
      goto update_sectors;
    }
    len -= count;
    *file_pos_ptr += count;
    buf += count;
  }

update_sectors:
  *last_rel_ptr = rel_sector;
  *last_abs_ptr = abs_sector;
}

/* Adjust the file size, freeing up space, or allocating more
	space, if necessary */

void chop_file(long file_pos, uint far *start_sec_ptr, uint far *last_rel_ptr,
               uint far *last_abs_ptr)
{
  uint keep_sector, rel_sector, abs_sector, prev_sector = 0xFFFF;

  keep_sector = (uint) ((file_pos + SECTOR_SIZE - 1) / SECTOR_SIZE);
  abs_sector = *start_sec_ptr;

  for (rel_sector = 0; rel_sector < keep_sector; rel_sector++) {
    if (abs_sector == 0xFFFF) {
      if (!(abs_sector = next_free_sector()))
        return;
      set_next_sector(abs_sector, 0xFFFF);
      if (rel_sector)
        set_next_sector(prev_sector, abs_sector);
      else
        *start_sec_ptr = abs_sector;
    }
    abs_sector = next_FAT_sector(prev_sector = abs_sector);
  }

  if (abs_sector != 0xFFFF) {
    FREE_SECTOR_CHAIN(abs_sector);
    if (rel_sector)
      set_next_sector(prev_sector, 0xFFFF);
    else
      *start_sec_ptr = 0xFFFF;
  }

  *last_rel_ptr = rel_sector - 1;
  *last_abs_ptr = prev_sector;
}

/* Copy data from the user buffer into the appropriate location
	in XMS */

void write_data(long far *file_pos_ptr, uint *len_ptr, uchar far *buf,
                uint far *start_sec_ptr, uint far *last_rel_ptr, uint far *last_abs_ptr)
{
  uint next_sector, start, rel_sector, abs_sector;
  uint i, count, len = *len_ptr;

  start = (uint) (*file_pos_ptr / SECTOR_SIZE);

  if (start < *last_rel_ptr) {
    rel_sector = 0;
    if ((abs_sector = *start_sec_ptr) == 0xFFFF) {
      if (!(abs_sector = next_free_sector())) {
        *len_ptr = 0;
        goto update_sectors;
      }
      set_next_sector(abs_sector, 0xFFFF);
      *start_sec_ptr = abs_sector;
    }
  }
  else {
    rel_sector = *last_rel_ptr;
    abs_sector = *last_abs_ptr;
  }

  while (len) {
    start = (uint) (*file_pos_ptr / SECTOR_SIZE);
    if (start > rel_sector) {
      if ((next_sector = next_FAT_sector(abs_sector)) == 0xFFFF) {
        if (!(next_sector = next_free_sector())) {
          *len_ptr -= len;
          goto update_sectors;
        }
        set_next_sector(abs_sector, next_sector);
        set_next_sector(next_sector, 0xFFFF);
      }
      abs_sector = next_sector;
      rel_sector++;
      continue;
    }
    i = (uint) (*file_pos_ptr % SECTOR_SIZE);
    count = min((uint) SECTOR_SIZE - i, len);
    if (count < SECTOR_SIZE) {
      if (!get_sector(abs_sector, &sector_buffer))
        goto update_sectors;
      last_sector = abs_sector;
      _fmemcpy((uchar far *) & sector_buffer[i], buf, count);
      if (!put_sector(abs_sector, &sector_buffer))
        goto update_sectors;
    }
    else if (!put_sector(abs_sector, buf))
      goto update_sectors;
    len -= count;
    *file_pos_ptr += count;
    buf += count;
  }

update_sectors:
  *last_rel_ptr = rel_sector;
  *last_abs_ptr = abs_sector;
}

/* ---- Utility and startup functions --------------*/

/* Fail the current redirector call with the supplied error number, i.e.
   set the carry flag in the returned flags, and set ax=error code */

void fail(uint err)
{
  r.flags = (r.flags | FCARRY);
  r.ax = err;
}

/* Opposite of fail() ! */

void succeed(void)
{
  r.flags = (r.flags & ~FCARRY);
  r.ax = 0;
}

/* Deal with differences in DOS version once, and set up a set
	of absolute pointers */

void set_up_pointers(void)
{
  if (_osmajor == 3) {
    fcbname_ptr = ((V3_SDA_PTR) sda_ptr)->fcb_name;
    filename_ptr = ((V3_SDA_PTR) sda_ptr)->file_name + cds_root_size - 1;
    fcbname_ptr_2 = ((V3_SDA_PTR) sda_ptr)->fcb_name_2;
    filename_ptr_2 = ((V3_SDA_PTR) sda_ptr)->file_name_2 + cds_root_size - 1;
    srchrec_ptr = &((V3_SDA_PTR) sda_ptr)->srchrec;
    dirrec_ptr = &((V3_SDA_PTR) sda_ptr)->dirrec;
    srchrec_ptr_2 = &((V3_SDA_PTR) sda_ptr)->rename_srchrec;
    dirrec_ptr_2 = &((V3_SDA_PTR) sda_ptr)->rename_dirrec;
    srch_attr_ptr = &((V3_SDA_PTR) sda_ptr)->srch_attr;
  }
  else {
    fcbname_ptr = ((V4_SDA_PTR) sda_ptr)->fcb_name;
    filename_ptr = ((V4_SDA_PTR) sda_ptr)->file_name + cds_root_size - 1;
    fcbname_ptr_2 = ((V4_SDA_PTR) sda_ptr)->fcb_name_2;
    filename_ptr_2 = ((V4_SDA_PTR) sda_ptr)->file_name_2 + cds_root_size - 1;
    srchrec_ptr = &((V4_SDA_PTR) sda_ptr)->srchrec;
    dirrec_ptr = &((V4_SDA_PTR) sda_ptr)->dirrec;
    srchrec_ptr_2 = &((V4_SDA_PTR) sda_ptr)->rename_srchrec;
    dirrec_ptr_2 = &((V4_SDA_PTR) sda_ptr)->rename_dirrec;
    srch_attr_ptr = &((V4_SDA_PTR) sda_ptr)->srch_attr;
  }
}

/* This function should not be necessary. DOS usually generates an FCB
   style name in the appropriate SDA area. However, in the case of
   user input such as 'CD ..' or 'DIR ..' it leaves the fcb area all
   spaces. So this function needs to be called every time. Its other
   feature is that it uses an internal DOS call to determine whether
   the filename is a DOS character device. We will 'Access deny' any
   use of a char device explicitly directed to our drive */

void generate_fcbname(uint dos_ds)
{
  get_fcbname_from_path((char far *) (_fstrrchr(filename_ptr, '\\') + 1), fcbname_ptr);

  filename_is_char_device = is_a_character_device(dos_ds);
}

/* Does the supplied string contain a wildcard '?' */
int contains_wildcards(char far *path)
{
  int i;

  for (i = 0; i < 11; i++)
    if (path[i] == '?')
      return TRUE;
  return FALSE;
}

/* Get DOS version, address of Swappable DOS Area, and address of
	DOS List of lists. We only run on versions of DOS >= 3.10, so
	fail otherwise */

void get_dos_vars(void)
{
  uint segmnt;
  uint ofset;

  if ((_osmajor < 3) || ((_osmajor == 3) && (_osminor < 10)))
    failprog("Unsupported DOS Version");

  _asm {
    push ds;
    push es;
    mov ax, 0x5d06;     /* Get SDA pointer */
    int 0x21;
    mov segmnt, ds;
    mov ofset, si;
    pop es;
    pop ds;
  }

  sda_ptr = MK_FP(segmnt, ofset);

  _asm {
    push ds;
    push es;
    mov ax, 0x5200;     /* Get Lol pointer */
    int 0x21;
    mov segmnt, es;
    mov ofset, bx;
    pop es;
    pop ds;
  }

  lolptr = (LOLREC_PTR) MK_FP(segmnt, ofset);
}

int is_call_for_us(uint es, uint di, uint ds)
{
  uchar far *p;
  int ret = 0xFF;

  filename_is_char_device = 0;

  // Note that the first 'if' checks for the bottom 6 bits
  // of the device information word in the SFT. Values > last drive
  // relate to files not associated with drives, such as LAN Manager
  // named pipes (Thanks to Dave Markun).
  if ((curr_fxn >= _clsfil && curr_fxn <= _unlockfil)
      || (curr_fxn == _skfmend)
      || (curr_fxn == _unknown_fxn_2D)) {
    ret = ((((SFTREC_PTR) MK_FP(es, di))->dev_info_word & 0x3F)
           == our_drive_no);
  }
  else {
    if (curr_fxn == _inquiry)   // 2F/1100 -- succeed automatically
      ret = TRUE;
    else {
      if (curr_fxn == _fnext)   // Find Next
      {
        SRCHREC_PTR psrchrec;   // check search record in SDA

        if (_osmajor == 3)
          psrchrec = &(((V3_SDA_PTR) sda_ptr)->srchrec);
        else
          psrchrec = &(((V4_SDA_PTR) sda_ptr)->srchrec);
        return ((psrchrec->drive_no & (uchar) 0x40) &&
                ((psrchrec->drive_no & (uchar) 0x1F) == our_drive_no));
      }
      if (_osmajor == 3)
        p = ((V3_SDA_PTR) sda_ptr)->cdsptr;     // check CDS
      else
        p = ((V4_SDA_PTR) sda_ptr)->cdsptr;

      if (_fmemcmp(cds_path_root, p, cds_root_size) == 0) {
        // If a path is present, does it refer to a character device
        if (curr_fxn != _dskspc)
          generate_fcbname(ds);
        return TRUE;
      }
      else
        return FALSE;
    }
  }
  return ret;
}

/* Check to see that we are allowed to install */
void is_ok_to_load(void)
{
  int result;

  _asm {
    mov ax, 0x1100;
    int 0x2f;
    mov result, ax;
  }

  if (result == 1)
    failprog("Not OK to install a redirector...");
  return;
}

/* This is where we do the initializations of the DOS structures
	that we need in order to fit the mould */

void set_up_cds(void)
{
  V3_CDS_PTR our_cds_ptr;

  our_cds_ptr = lolptr->cds_ptr;
  if (_osmajor == 3)
//              our_cds_ptr = our_cds_ptr + (our_drive_no - 1);  // ref: DR_TOO_HIGH
    our_cds_ptr = our_cds_ptr + our_drive_no;
  else {
    V4_CDS_PTR t = (V4_CDS_PTR) our_cds_ptr;

//              t = t + (our_drive_no - 1);  // ref: DR_TOO_HIGH
    t = t + our_drive_no;
    our_cds_ptr = (V3_CDS_PTR) t;
  }

//      if (our_drive_no > lolptr->last_drive)  // ref: DR_TOO_HIGH
  if (our_drive_no >= lolptr->last_drive)
    failprog("Drive letter higher than last drive.");

  // Check that this drive letter is currently invalid (not in use already)
  // 0xc000 tests both physical and network bits at same time
  if ((our_cds_ptr->flags & 0xc000) != 0)
    failprog("Drive already assigned...");

  // Set Network+Physical+NotRealNetworkDrive bits on, and
  // establish our 'root'
  our_cds_ptr->flags |= 0xc080;
  cds_root_size = _fstrlen(cds_path_root);
  _fstrcpy(our_cds_ptr->current_path, cds_path_root);
  our_cds_ptr->current_path[_fstrlen(our_cds_ptr->current_path) - 3] =
//              (char) ('@'+ our_drive_no);  // ref: DR_TOO_HIGH
    (char) ('A' + our_drive_no);
  _fstrcpy(cds_path_root, our_cds_ptr->current_path);
  current_path = our_cds_ptr->current_path;
  our_cds_ptr->root_ofs = _fstrlen(our_cds_ptr->current_path) - 1;
  current_path += our_cds_ptr->root_ofs;
}

/* ----- Redirector functions ------------------*/

/* Respond that it is OK to load another redirector */
void inquiry(void)
{
  r.ax = 0x00FF;
}

/* FindNext  - subfunction 1Ch */
void fnext(void)
{
  if (!find_next_entry(srchrec_ptr->srch_mask,
                       srchrec_ptr->attr_mask, dirrec_ptr->file_name,
                       &dirrec_ptr->file_attr, &dirrec_ptr->file_time,
                       &dirrec_ptr->start_sector, &dirrec_ptr->file_size,
                       &srchrec_ptr->dir_sector, &srchrec_ptr->dir_entry_no)) {
    fail(18);
    return;
  }
}

/* Internal findnext for delete and rename processing */
uint fnext2(void)
{
  return (find_next_entry(srchrec_ptr_2->srch_mask, 0x20,
                          dirrec_ptr_2->file_name, &dirrec_ptr_2->file_attr,
                          NULL, NULL, NULL, &srchrec_ptr_2->dir_sector,
                          &srchrec_ptr_2->dir_entry_no)) ? 0 : 18;
}

/* FindFirst - subfunction 1Bh */

/* This function looks a little odd because of the embedded call to
   fnext(). This arises from the my view that findfirst is simply
   a findnext with some initialization overhead: findfirst has to
   locate the directory in which findnext is to iterate, and
   initialize the SDB state to 'point to' the first entry. It then
   gets that first entry, using findnext.
   The r.ax test at the end of the function is because, to mimic
   DOS behavior, a findfirst that finds no matching entry should
   return an error 2 (file not found), whereas a subsequent findnext
   that finds no matching entry should return error 18 (no more
   files). */

void ffirst(void)
{
  char far *path;
  int success;

  /* Special case for volume-label-only search - must be in root */
  if (path = (*srch_attr_ptr == 0x08)
      ? filename_ptr : _fstrrchr(filename_ptr, '\\'))
    *path = 0;
  success = get_dir_start_sector(filename_ptr, &srchrec_ptr->dir_sector);
  if (path)
    *path = '\\';
  if (!success) {
    fail(3);
    return;
  }

  _fmemcpy(&srchrec_ptr->srch_mask, fcbname_ptr, 11);

  srchrec_ptr->dir_entry_no = -1;
  srchrec_ptr->attr_mask = *srch_attr_ptr;
  srchrec_ptr->drive_no = (uchar) (our_drive_no | 0xC0);

  fnext();
  /* No need to check r.flags & FCARRY; if ax is 18,
     FCARRY must have been set. */
  if (r.ax == 18)
    r.ax = 2;   // make fnext error code suitable to ffirst
}

/* Internal findfirst for delete and rename processing */
uint ffirst2(void)
{
  if (!get_dir_start_sector(filename_ptr_2, &srchrec_ptr_2->dir_sector))
    return 3;

  srchrec_ptr_2->dir_entry_no = -1;
  srchrec_ptr_2->drive_no = (uchar) (our_drive_no | 0x80);

  return fnext2();
}

/* ReMove Directory - subfunction 01h */
void rd(void)
{
  /* special case for root */
  if ((*filename_ptr == '\\') && (!*(filename_ptr + 1))) {
    fail(5);
    return;
  }
  if (contains_wildcards(fcbname_ptr)) {
    fail(3);
    return;
  }
  _fstrcpy(filename_ptr_2, filename_ptr);
  *srch_attr_ptr = 0x10;

  ffirst();
  if (r.ax || (!(dirrec_ptr->file_attr & 0x10))) {
    r.ax = 3;
    return;
  }

  if (!_fstrncmp(filename_ptr_2, current_path, _fstrlen(filename_ptr_2))) {
    fail(16);
    return;
  }

  _fmemset(srchrec_ptr_2->srch_mask, '?', 11);
  srchrec_ptr_2->attr_mask = 0x3f;

  if ((r.ax = ffirst2()) == 3) {
    fail(3);
    return;
  }

  if (!r.ax) {
    fail(5);
    return;
  }

  if (!get_sector(last_sector = srchrec_ptr->dir_sector, &sector_buffer)) {
    fail(5);
    return;
  }
  ((DIRREC_PTR) & sector_buffer)[srchrec_ptr->dir_entry_no].file_name[0] = (char) 0xE5;
  if (  /* dirsector_has_entries(last_sector, &sector_buffer) && */
       (!put_sector(last_sector, &sector_buffer))) {
    fail(5);
    return;
  }

  FREE_SECTOR_CHAIN(dirrec_ptr->start_sector);
  succeed();
}

/* Make Directory - subfunction 03h */
void md(void)
{
  /* special case for root */
  if ((*filename_ptr == '\\') && (!*(filename_ptr + 1))) {
    fail(5);
    return;
  }
  // can't create dir name with * or ? in it
  if (contains_wildcards(fcbname_ptr)) {
    fail(3);
    return;
  }

  *srch_attr_ptr = 0x3f;
  ffirst();
  if (r.ax == 0)        // we need error 2 here
  {
    fail(5);
    return;
  }
  if (r.ax != 2)
    return;

  /*
     Note that although we initialize a directory sector, we actually
     needn't, since we do not create . or .. entries. This is because
     a redirector never receives requests for . or .. in ChDir - the
     absolute path is resolved by DOS before we get it. If you want
     to see dots in DIR listings, create directory entries for them
     after put_sectors. Note that you will also have to take account
     of them in RMDIR.
   */
  last_sector = 0xffff;
  memset(sector_buffer, 0, SECTOR_SIZE);
  if (!(dirrec_ptr->start_sector = next_free_sector())) {
    fail(5);
    return;
  }
  set_next_sector(dirrec_ptr->start_sector, 0xFFFF);
  last_sector = dirrec_ptr->start_sector;
  if ((!put_sector(dirrec_ptr->start_sector, &sector_buffer)) ||
      (!create_dir_entry(&srchrec_ptr->dir_sector, NULL, fcbname_ptr, 0x10,
                         dirrec_ptr->start_sector, 0, dos_ftime()))) {
    fail(5);
    return;
  }
  succeed();
}

/* Change Directory - subfunction 05h */
void cd(void)
{
  /* Special case for root */
  if ((*filename_ptr != '\\') || (*(filename_ptr + 1))) {
    if (contains_wildcards(fcbname_ptr)) {
      fail(3);
      return;
    }

    *srch_attr_ptr = 0x10;
    ffirst();
    if (r.ax || (!(dirrec_ptr->file_attr & 0x10))) {
      fail(3);
      return;
    }
  }
  _fstrcpy(current_path, filename_ptr);
}

/* Close File - subfunction 06h */
void clsfil(void)
{
  SFTREC_PTR p = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (p->handle_count)  /* If handle count not 0, decrement it */
    --p->handle_count;

  /* If writing, create/update dir entry for file */
  if (!(p->open_mode & 3))
    return;

  if (p->dir_entry_no == 0xff) {
    if (!create_dir_entry(&p->dir_sector, &p->dir_entry_no, p->file_name,
                          p->file_attr, p->start_sector, p->file_size, p->file_time))
      fail(5);
  }
  else {
    if ((last_sector != p->dir_sector) && (!get_sector(p->dir_sector, &sector_buffer)))
      fail(5);
    last_sector = p->dir_sector;
    ((DIRREC_PTR) & sector_buffer)[p->dir_entry_no].file_attr = p->file_attr;
    ((DIRREC_PTR) & sector_buffer)[p->dir_entry_no].start_sector = p->start_sector;
    ((DIRREC_PTR) & sector_buffer)[p->dir_entry_no].file_size = p->file_size;
    ((DIRREC_PTR) & sector_buffer)[p->dir_entry_no].file_time = p->file_time;
    if (!put_sector(p->dir_sector, &sector_buffer))
      fail(5);
  }
}

/* Commit File - subfunction 07h */
void cmmtfil(void)
{
  /* We support this but don't do anything... */
  return;
}

/* Read from File - subfunction 08h */
// For version that handles critical errors,
// see Undocumented DOS, 2nd edition, chapter 8
void readfil(void)
{
  SFTREC_PTR p = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (p->open_mode & 1) {
    fail(5);
    return;
  }

  if ((p->file_pos + r.cx) > p->file_size)
    r.cx = (uint) (p->file_size - p->file_pos);

  if (!r.cx)
    return;

  /* Fill caller's buffer and update the SFT for the file */
  read_data(&p->file_pos, &r.cx, ((V3_SDA_PTR) sda_ptr)->current_dta,
            p->start_sector, &p->rel_sector, &p->abs_sector);
}

/* Write to File - subfunction 09h */
void writfil(void)
{
  SFTREC_PTR p = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (!(p->open_mode & 3)) {
    fail(5);
    return;
  }

  p->file_time = dos_ftime();

  /* Take account of DOS' 0-byte-write-truncates-file rcounte */
  if (!r.cx) {
    p->file_size = p->file_pos;
    chop_file(p->file_pos, &p->start_sector, &p->rel_sector, &p->abs_sector);
    return;
  }

  /* Write from the caller's buffer and update the SFT for the file */
  write_data(&p->file_pos, &r.cx, ((V3_SDA_PTR) sda_ptr)->current_dta,
             &p->start_sector, &p->rel_sector, &p->abs_sector);
  if (p->file_pos > p->file_size)
    p->file_size = p->file_pos;
}

/* Lock file - subfunction 0Ah */

/* We support this function only to illustrate how it works. We do
	not actually honor LOCK/UNLOCK requests. The following function
	supports locking only before, and both locking/unlocking after
	DOS 4.0 */
void lockfil(void)
{
  SFTREC_PTR sft = (SFTREC_PTR) MK_FP(r.es, r.di);
  LOCKREC_PTR lockptr;
  ulong region_offset;
  ulong region_length;

  if (_osmajor > 3) {
    // In v4.0 and above, lock info is at DS:BX in a LOCKREC structure
    lockptr = (LOCKREC_PTR) MK_FP(r.ds, r.dx);
    region_offset = lockptr->region_offset;
    region_length = lockptr->region_length;
    if ((uchar) r.bx)   // if BL == 1, UNLOCK
    {
      // Call UNLOCK REGION function
    }
    else        // if BL == 0, LOCK
    {
      // Call LOCK REGION function
    }
  }
  else {
    // In v3.x, lock info is in regs and on the stack
    region_offset = ((ulong) r.cx << 16) + r.dx;
    region_length = ((ulong) r.si << 16) + *stack_param_ptr;

    // Call LOCK REGION function
  }
  return;
}

/* UnLock file - subfunction 0Bh */

/* We support this function only to illustrate how it works. The following
	function supports only unlocking before DOS 4.0 */
void unlockfil(void)
{
  SFTREC_PTR sft = (SFTREC_PTR) MK_FP(r.es, r.di);
  ulong region_offset;
  ulong region_length;

  // In v3.x, lock info is in regs and on the stack
  region_offset = ((ulong) r.cx << 16) + r.dx;
  region_length = ((ulong) r.si << 16) + *stack_param_ptr;

  // Call UNLOCK REGION function

  return;
}

/* Get Disk Space - subfunction 0Ch */
void dskspc(void)
{
  r.ax = 1;
  r.bx = total_sectors;
  r.cx = SECTOR_SIZE;
  r.dx = free_sectors;
}

/* Get File Attributes - subfunction 0Fh */
void getfatt(void)
{
  if (contains_wildcards(fcbname_ptr)) {
    fail(2);
    return;
  }

  *srch_attr_ptr = 0x3f;
  ffirst();
  if (r.ax)
    return;

  r.ax = (uint) dirrec_ptr->file_attr;
}

/* Set File Attributes - subfunction 0Eh */
void setfatt()
{
  getfatt();
  if (r.flags & FCARRY)
    return;

  if ((((uchar) * stack_param_ptr) & 0x10) ||
      (((DIRREC_PTR) sector_buffer)[srchrec_ptr->dir_entry_no].file_attr & 0x10)) {
    fail(5);
    return;
  }

  ((DIRREC_PTR) sector_buffer)[srchrec_ptr->dir_entry_no].file_attr = (uchar) * stack_param_ptr;
  if (!put_sector(last_sector, &sector_buffer)) {
    fail(5);
    return;
  }
}

/* Rename File - subfunction 11h */
void renfil(void)
{
  char far *path;
  uint ret = 0, dir_sector;
  int i = 0, j;

  *srch_attr_ptr = 0x21;
  srchrec_ptr_2->attr_mask = 0x3f;
  ffirst();
  if (r.ax)
    return;

  if (path = _fstrrchr(filename_ptr_2, '\\'))
    *path++ = 0;

  /* Keep the new name mask in fcbname_ptr_2 */
  _fmemset(fcbname_ptr_2, ' ', 11);
  for (; *path; path++)
    switch (*path) {
    case '.':
      i = 8;
      break;
    case '*':
      j = (i < 8) ? 8 : 11;
      while (i < j)
        fcbname_ptr_2[i++] = '?';
      break;
    default:
      fcbname_ptr_2[i++] = *path;
    }
  _fmemcpy(srchrec_ptr_2->srch_mask, fcbname_ptr_2, 11);
  if ((ret = ffirst2()) == 3) {
    fail(3);
    return;
  }
  else if (!ret) {
    fail(5);
    return;
  }

  ret = 0;
  dir_sector = srchrec_ptr_2->dir_sector;

  while (!r.ax) {
    for (i = 0; i < 11; i++)
      srchrec_ptr_2->srch_mask[i] = (fcbname_ptr_2[i] == '?')
        ? dirrec_ptr->file_name[i]
        : fcbname_ptr_2[i];
    if ((dirrec_ptr->file_attr & 1) || (!ffirst2()))
      ret = 5;
    else {
      if (!create_dir_entry(&dir_sector, NULL, srchrec_ptr_2->srch_mask,
                            dirrec_ptr->file_attr, dirrec_ptr->start_sector,
                            dirrec_ptr->file_size, dirrec_ptr->file_time))
        ret = 5;
      else {
        if (!get_sector(last_sector = srchrec_ptr->dir_sector, &sector_buffer)) {
          fail(5);
          return;
        }
        ((DIRREC_PTR) & sector_buffer)[srchrec_ptr->dir_entry_no].file_name[0] = (char) 0xE5;
        if (!put_sector(srchrec_ptr->dir_sector, &sector_buffer)) {
          fail(5);
          return;
        }
      }
    }
    fnext();
  }

  if (r.ax == 18)
    r.ax = ret;

  if (!r.ax)
    succeed();
  else
    fail(r.ax);
}

/* Delete File - subfunction 13h */
void delfil(void)
{
  uint ret = 0;

  *srch_attr_ptr = 0x21;
  ffirst();

  while (!r.ax) {
    if (dirrec_ptr->file_attr & 1)
      ret = 5;
    else {
      FREE_SECTOR_CHAIN(dirrec_ptr->start_sector);
      ((DIRREC_PTR) & sector_buffer)[srchrec_ptr->dir_entry_no].file_name[0] = (char) 0xE5;
      if (      /* dirsector_has_entries(last_sector, &sector_buffer) && */
           (!put_sector(last_sector, &sector_buffer))) {
        fail(5);
        return;
      }
    }
    fnext();
  }

  if (r.ax == 18)
    r.ax = ret;

  if (!r.ax)
    succeed();
  else
    fail(r.ax);
}

/* Support functions for the various file open functions below */

void init_sft(SFTREC_PTR p)
{
  /*
     Initialize the supplied SFT entry. Note the modifications to
     the open mode word in the SFT. If bit 15 is set when we receive
     it, it is an FCB open, and requires the Set FCB Owner internal
     DOS function to be called.
   */
  if (p->open_mode & 0x8000)
    /* File is being opened via FCB */
    p->open_mode |= 0x00F0;
  else
    p->open_mode &= 0x000F;

  /* Mark file as being on network drive, unwritten to */
  p->dev_info_word = (uint) (0x8040 | (uint) our_drive_no);
  p->file_pos = 0;
  p->rel_sector = 0xffff;
  p->abs_sector = 0xffff;
  p->dev_drvr_ptr = NULL;

  if (p->open_mode & 0x8000)
    set_sft_owner(p);
}

/* Note that the following function uses dirrec_ptr to supply much of
   the SFT data. This is because an open of an existing file is
   effectively a findfirst with data returned to the caller (DOS) in
   an SFT, rather than a found file directory entry buffer. So this
   function uses the knowledge that it is immediately preceded by a
   ffirst(), and that the data is avalable in dirrec_ptr. */

void fill_sft(SFTREC_PTR p, int use_found_1, int truncate)
{
  _fmemcpy(p->file_name, fcbname_ptr, 11);
  if (use_found_1) {
    p->file_attr = dirrec_ptr->file_attr;
    p->file_time = truncate ? dos_ftime() : dirrec_ptr->file_time;
    if (truncate) {
      FREE_SECTOR_CHAIN(dirrec_ptr->start_sector);
      p->start_sector = 0xFFFF;
    }
    else
      p->start_sector = dirrec_ptr->start_sector;
    p->file_size = truncate ? 0L : dirrec_ptr->file_size;
    p->dir_sector = srchrec_ptr->dir_sector;
    p->dir_entry_no = (uchar) srchrec_ptr->dir_entry_no;
  }
  else {
    p->file_attr = (uchar) * stack_param_ptr;   /* Attr is top of stack */
    p->file_time = dos_ftime();
    p->start_sector = 0xffff;
    p->file_size = 0;
    p->dir_sector = srchrec_ptr->dir_sector;
    p->dir_entry_no = 0xff;
  }
}

/* Open Existing File - subfunction 16h */
void opnfil(void)
{
  SFTREC_PTR p;

  /* locate any file for any open */

  p = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (contains_wildcards(fcbname_ptr)) {
    fail(3);
    return;
  }

  *srch_attr_ptr = 0x27;
  ffirst();
  if (!r.ax) {
    fill_sft(p, TRUE, FALSE);
    init_sft(p);
  }
}

/* Truncate/Create File - subfunction 17h */
void creatfil(void)
{
  SFTREC_PTR p = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (contains_wildcards(fcbname_ptr)) {
    fail(3);
    return;
  }

  *srch_attr_ptr = 0x3f;
  ffirst();
  if ((r.flags & FCARRY) && (r.ax != 2))
    return;

  if ((!r.ax) && (dirrec_ptr->file_attr & 0x19)) {
    fail(5);
    return;
  }

  fill_sft(p, (!r.ax), TRUE);
  init_sft(p);
  succeed();
}

/* This function is never called! DOS fiddles with position internally */
void skfmend(void)
{
  long seek_amnt;
  SFTREC_PTR p;

  /* But, just in case... */
  seek_amnt = -1L * (((long) r.cx << 16) + r.dx);
  p = (SFTREC_PTR) MK_FP(r.es, r.di);
  if (seek_amnt > p->file_size)
    seek_amnt = p->file_size;

  p->file_pos = p->file_size - seek_amnt;
  r.dx = (uint) (p->file_pos >> 16);
  r.ax = (uint) (p->file_pos & 0xFFFF);
}

void unknown_fxn_2D()
{
  r.ax = 2;
  /* Only called in v4.01, this is what MSCDEX returns */
}

/* Special Multi-Purpose Open File - subfunction 2Eh */

#define CREATE_IF_NOT_EXIST		0x10
#define OPEN_IF_EXISTS			0x01
#define REPLACE_IF_EXISTS		0x02

void special_opnfil(void)
{
  SFTREC_PTR p = (SFTREC_PTR) MK_FP(r.es, r.di);
  uint open_mode, action;

  open_mode = ((V4_SDA_PTR) sda_ptr)->mode_2E & 0x7f;
  action = ((V4_SDA_PTR) sda_ptr)->action_2E;
  p->open_mode = open_mode;

  if (contains_wildcards(fcbname_ptr)) {
    fail(3);
    return;
  }

  *srch_attr_ptr = 0x3f;
  ffirst();
  if ((r.flags & FCARRY) && (r.ax != 2))
    return;

  if (!r.ax) {
    if ((dirrec_ptr->file_attr & 0x18) ||
        ((dirrec_ptr->file_attr & 0x01) && (open_mode & 3)) || (!(action &= 0x000F))) {
      fail(5);
      return;
    }
  }
  else {
    if (!(action &= 0x00F0)) {
      fail(2);
      return;
    }
  }

  if ((!(open_mode & 3)) && r.ax) {
    fail(5);
    return;
  }

  fill_sft(p, (!r.ax), action & REPLACE_IF_EXISTS);
  init_sft(p);
  succeed();
}

/* A placeholder */
void unsupported(void)
{
  return;
}

PROC dispatch_table[] = {
  inquiry,      /* 0x00h */
  rd,   /* 0x01h */
  unsupported,  /* 0x02h */
  md,   /* 0x03h */
  unsupported,  /* 0x04h */
  cd,   /* 0x05h */
  clsfil,       /* 0x06h */
  cmmtfil,      /* 0x07h */
  readfil,      /* 0x08h */
  writfil,      /* 0x09h */
  lockfil,      /* 0x0Ah */
  unlockfil,    /* 0x0Bh */
  dskspc,       /* 0x0Ch */
  unsupported,  /* 0x0Dh */
  setfatt,      /* 0x0Eh */
  getfatt,      /* 0x0Fh */
  unsupported,  /* 0x10h */
  renfil,       /* 0x11h */
  unsupported,  /* 0x12h */
  delfil,       /* 0x13h */
  unsupported,  /* 0x14h */
  unsupported,  /* 0x15h */
  opnfil,       /* 0x16h */
  creatfil,     /* 0x17h */
  unsupported,  /* 0x18h */
  unsupported,  /* 0x19h */
  unsupported,  /* 0x1Ah */
  ffirst,       /* 0x1Bh */
  fnext,        /* 0x1Ch */
  unsupported,  /* 0x1Dh */
  unsupported,  /* 0x1Eh */
  unsupported,  /* 0x1Fh */
  unsupported,  /* 0x20h */
  skfmend,      /* 0x21h */
  unsupported,  /* 0x22h */
  unsupported,  /* 0x23h */
  unsupported,  /* 0x24h */
  unsupported,  /* 0x25h */
  unsupported,  /* 0x26h */
  unsupported,  /* 0x27h */
  unsupported,  /* 0x28h */
  unsupported,  /* 0x29h */
  unsupported,  /* 0x2Ah */
  unsupported,  /* 0x2Bh */
  unsupported,  /* 0x2Ch */
  unknown_fxn_2D,       /* 0x2Dh */
  special_opnfil        /* 0x0Eh */
};

/* -------------------------------------------------------------*/

/* This is the main entry point for the redirector. It assesses if
   the call is for our drive, and if so, calls the appropriate routine. On
   return, it restores the (possibly modified) register values. */

void interrupt far redirector(ALL_REGS entry_regs)
{
  static uint save_bp;

  _asm STI;

  if (((entry_regs.ax >> 8) != (uchar) 0x11) || ((uchar) entry_regs.ax > MAX_FXN_NO))
    goto chain_on;

  curr_fxn = fxnmap[(uchar) entry_regs.ax];

  if ((curr_fxn == _unsupported) ||
      (!is_call_for_us(entry_regs.es, entry_regs.di, entry_regs.ds)))
    goto chain_on;

  /* Set up our copy of the registers */
  r = entry_regs;

  // Save ss:sp and switch to our internal stack. We also save bp
  // so that we can get at any parameter at the top of the stack
  // (such as the file attribute passed to subfxn 17h).
  _asm mov dos_ss, ss;
  _asm mov save_bp, bp;

  stack_param_ptr = (uint far *) MK_FP(dos_ss, save_bp + sizeof(ALL_REGS));

  {
    uint my_sp = FP_OFF(our_stack) + STACK_SIZE - 2;
    
    _asm {
      mov dos_sp, sp;
      mov ax, ds;
      cli;
      mov ss, ax;   // New stack segment is in Data segment.
#if 0
      mov sp, offset our_stack + STACK_SIZE - 2;
#else
      mov sp, my_sp;
#endif
      sti;
    }
  }

  // Expect success!
  succeed();

  // Call the appropriate handling function unless we already know we
  // need to fail
  if (filename_is_char_device)
    fail(5);
  else
    dispatch_table[curr_fxn]();
  
  // Switch the stack back
  _asm {
    cli;
    mov ss, dos_ss;
    mov sp, dos_sp;

    sti;
  }

  // put the possibly changed registers back on the stack, and return
  entry_regs = r;
  return;

  // If the call wasn't for us, we chain on.
 chain_on:
  _chain_intr(prev_int2f_vector);
}

/* ---- Unload functionality --------------*/

/* Find the latest Phantom installed, unplug it from the Int 2F
	chain if possible, make the CDS reflect an invalid drive, and
	free its real and XMS memory. */

static uint ul_save_ss, ul_save_sp;
static int ul_i;

void exit_ret()
{
  _asm {
    // We should arrive back here - restore SS:SP
    mov ax, seg ul_save_ss;
    mov ds, ax;
    mov ss, ul_save_ss;
    mov sp, ul_save_sp;

    // restore the registers
    pop bp;
    pop di;
    pop si;
    pop ds;
    pop es;

    // Set current PSP back to us.
    mov bx, _psp;
    mov ah, 0x50;
    int 0x21;
  }

  _dos_setvect(ul_i, NULL);
//      our_drive_str[0] = (char) (our_drive_no + '@');  // ref: DR_TOO_HIGH
  our_drive_str[0] = (char) (our_drive_no + 'A');
  print_string(our_drive_str, FALSE);
  print_string(" is now invalid.", TRUE);
}

void unload_latest()
{
  INTVECT p_vect;
  V3_CDS_PTR cds_ptr;
  SIGREC_PTR sig_ptr;
  uint psp;

  // Note that we step backwards to allow unloading of Multiple copies
  // in reverse order to loading, so that the Int 2Fh chain remains
  // intact.
  for (ul_i = 0x66; ul_i >= 0x60; ul_i--) {
    long far *p;

    p = (long far *) MK_FP(0, ((uint) ul_i * 4));
    sig_ptr = (SIGREC_PTR) * p;
    if (_fmemcmp(sig_ptr->signature, (uchar far *) sigrec.signature,
                 sizeof(sigrec.signature)) == 0)
      break;
  }

  if (ul_i == 0x5f)
    failprog("Phantom not loaded.");

  p_vect = _dos_getvect(0x2f);

  // Check that a subsequent TSR hasn't taken over Int 2Fh
  if (sig_ptr->our_handler != (void far *) p_vect)
    failprog("Interrupt 2F has been superceded...");

  p_vect = (INTVECT) sig_ptr->prev_handler;
  _dos_setvect(0x2f, p_vect);
  p_vect = _dos_getvect(ul_i);
  psp = ((SIGREC_PTR) p_vect)->psp;
  our_drive_no = ((SIGREC_PTR) p_vect)->drive_no;

  // Free up the XMS memory
  if ((!xms_is_present()) || (!xms_free_block(((SIGREC_PTR) p_vect)->xms_handle)))
    print_string("Could not free XMS memory", TRUE);

  cds_ptr = lolptr->cds_ptr;
  if (_osmajor == 3)
//              cds_ptr += (our_drive_no - 1);  // ref: DR_TOO_HIGH
    cds_ptr += our_drive_no;
  else {
    V4_CDS_PTR t = (V4_CDS_PTR) cds_ptr;

//              t += (our_drive_no - 1);  // ref: DR_TOO_HIGH
    t += our_drive_no;
    cds_ptr = (V3_CDS_PTR) t;
  }

  // switch off the Network and Physical bits for the drive,
  // rendering it invalid.
  cds_ptr->flags = cds_ptr->flags & 0x3fff;

  // Use the recommended switch PSP and Int 4Ch method of
  // unloading the TSR (see TSRs chapter of Undocumented DOS).
  _asm {
    // Save some registers
    push es;
    push ds;
    push si;
    push di;
    push bp;

    // Set resident program's parent PSP to us.
    mov es, psp;
    mov bx, 0x16;
    mov ax, _psp;
    mov es:[di], ax;
    mov di, 0x0a;

    // Set resident program PSP return address to exit_ret;
    mov ax, offset exit_ret;

    stosw;
    mov ax, cs;

    stosw;
    mov bx, es;

    // Set current PSP to resident program
    mov ah, 0x50;
    int 0x21;

    // Save SS:SP
    mov ax, seg ul_save_ss;
    mov ds, ax;
    mov ul_save_ss, ss;
    mov ul_save_sp, sp;

    // and terminate
    mov ax, 0x4c00;
    int 0x21;
  }
}

/* ------- TSR termination routines -------- */

/* Plug into Int 2Fh, and calculate the size of the TSR to
	keep in memory. Plug into a 'user' interrupt to allow for
	unloading */

void prepare_for_tsr(void)
{
  uchar far *buf;
  int i;

  // Find ourselves a free interrupt to call our own. Without it,
  // we can still load, but a future invocation of Phantom with -U
  // will not be able to unload us.
  for (i = 0x60; i < 0x67; i++) {
    long far *p;

    p = (long far *) MK_FP(0, ((uint) i * 4));
    if (*p == 0L)
      break;
  }

  prev_int2f_vector = _dos_getvect(0x2f);
  if (i == 0x67) {
    print_string("No user intrs available. Phantom not unloadable..", TRUE);
    return;
  }

  // Our new found 'user' interrupt will point at the command line area of
  // our PSP. Complete our signature record, put it into the command line,
  // then go to sleep.

  _dos_setvect(i, (INTVECT) (buf = MK_FP(_psp, 0x80)));

  sigrec.xms_handle = xms_handle;
  sigrec.psp = _psp;
  sigrec.drive_no = our_drive_no;
  sigrec.our_handler = (void far *) redirector;
  sigrec.prev_handler = (void far *) prev_int2f_vector;
  *((SIGREC_PTR) buf) = sigrec;
}

void tsr(void)
{
  uint tsr_paras;               // Paragraphs to terminate and leave resident.
  uint highest_seg;

  _asm mov highest_seg, ds;

  tsr_paras = highest_seg + (((uint) &end) / 16) + 1 - _psp;

  // Plug ourselves into the Int 2Fh chain
  _dos_setvect(0x2f, redirector);
  _dos_keep(0, tsr_paras);
}

/* --------------------------------------------------------------------*/

int _cdecl main(uint argc, char **argv)
{
  print_string(signon_string, TRUE);

  // See what parameters we have...
  for (argv++; *argv; argv++) {
    switch (**argv) {
    case '-':
    case '/':
      (*argv)++;
      switch (toupper(**argv)) {
      case 'U':
        get_dos_vars();
        unload_latest();
        return 0;
      case 'S':
        (*argv)++;
        if (!(disk_size = atoi(*argv))) {
          print_string("Bad size parameter.", TRUE);
          return 0;
        }
        break;
      default:
        print_string("Unrecognized parameter.", TRUE);
        return 0;
      }
      break;
    default:
      our_drive_str[0] = **argv;
    }
  }

  // Otherwise, check that it's a valid drive letter
  if (our_drive_str[0] == ' ') {
    print_string(usage_string, TRUE);
    return 0;
  }

  our_drive_str[0] &= ~0x20;
//      our_drive_no = (uchar) (our_drive_str[0] - '@');  // ref: DR_TOO_HIGH
  our_drive_no = (uchar) (our_drive_str[0] - 'A');
//      if ((our_drive_no > 26) || (our_drive_no < 1))  // ref: DR_TOO_HIGH
  if (our_drive_no > 25) {
    print_string(usage_string, TRUE);
    return 0;
  }

  // Initialize XMS and alloc the 'disk space'
  set_up_xms_disk();
  is_ok_to_load();
  get_dos_vars();
  set_up_cds();
  set_up_pointers();

  // Tell the user
  print_string(my_ltoa(disk_size), FALSE);
  print_string("Kb XMS allocated.", TRUE);
  print_string("Phantom installed as ", FALSE);
  print_string(our_drive_str, TRUE);

  prepare_for_tsr();

  tsr();

  return 0;
}
