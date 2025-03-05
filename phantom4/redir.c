#include "redir.h"
#include "ramdrive.h"
#include "dosfunc.h"
#include <stdlib.h>
#include <dos.h>
#include <string.h>

#define STACK_SIZE 1024

ALL_REGS r;                     /* Global save area for all caller's regs */
uchar our_drive_no;             /* A: is 1, B: is 2, etc. */
char our_drive_str[3] = " :";   /* Our drive letter string */
char far *cds_path_root = "Phantom  :\\";       /* Root string for CDS */
uint cds_root_size;             /* Size of our CDS root string */
SIGREC sigrec = { 8, "PHANTOM ", 0, 0, 0 };     /* Signature record */
uint far *stack_param_ptr;      /* ptr to word at top of stack on entry */
int curr_fxn;                   /* Record of function in progress */
int filename_is_char_device;    /* generate_fcbname found character device name */
INTVECT prev_int2f_vector;      /* For chaining, and restoring on unload */

uint dos_ss;                    /* DOS's saved SS at entry */
uint dos_sp;                    /* DOS's saved SP at entry */
uint our_sp;                    /* SP to switch to on entry */
uint save_sp;                   /* SP saved across internal DOS calls */
char our_stack[STACK_SIZE];     /* our internal stack */

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

#ifdef __WATCOMC__
#define FCARRY                          INTR_CF
#else
#define FCARRY                          0x0001
#endif

extern __segment getSP(void);
#pragma aux getSP = \
    "mov ax, sp";
extern __segment getSS(void);
#pragma aux getSS = \
    "mov ax, ss";
extern __segment getDS(void);
#pragma aux getDS = \
    "mov ax, ds";

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

/* Does the supplied string contain a wildcard '?' */
int contains_wildcards(char far *path)
{
  int i;

  for (i = 0; i < 11; i++)
    if (path[i] == '?')
      return TRUE;
  return FALSE;
}

/* ----- Redirector functions ------------------*/

/* Respond that it is OK to load another redirector */
void inquiry(void)
{
  r.ax = 0x00FF;
}

/* Find_Next  - subfunction 1Ch */
void find_next(void)
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

/* Internal find_next for delete and rename processing */
uint fnext2(void)
{
  return (find_next_entry(srchrec_ptr_2->srch_mask, 0x20,
                          dirrec_ptr_2->file_name, &dirrec_ptr_2->file_attr,
                          NULL, NULL, NULL, &srchrec_ptr_2->dir_sector,
                          &srchrec_ptr_2->dir_entry_no)) ? 0 : 18;
}

/* Find_First - subfunction 1Bh */

/* This function looks a little odd because of the embedded call to
   find_next(). This arises from the my view that find_first is simply
   a find_next with some initialization overhead: find_first has to
   locate the directory in which find_next is to iterate, and
   initialize the SDB state to 'point to' the first entry. It then
   gets that first entry, using find_next.
   The r.ax test at the end of the function is because, to mimic
   DOS behavior, a find_first that finds no matching entry should
   return an error 2 (file not found), whereas a subsequent find_next
   that finds no matching entry should return error 18 (no more
   files). */

void find_first(void)
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

  find_next();
  /* No need to check r.flags & FCARRY; if ax is 18,
     FCARRY must have been set. */
  if (r.ax == 18)
    r.ax = 2;   // make find_next error code suitable to find_first
}

/* Internal find_first for delete and rename processing */
uint ffirst2(void)
{
  if (!get_dir_start_sector(filename_ptr_2, &srchrec_ptr_2->dir_sector))
    return 3;

  srchrec_ptr_2->dir_entry_no = -1;
  srchrec_ptr_2->drive_no = (uchar) (our_drive_no | 0x80);

  return fnext2();
}

/* ReMove Directory - subfunction 01h */
void rename_dir(void)
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

  find_first();
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

  if (!get_sector(last_sector = srchrec_ptr->dir_sector, sector_buffer)) {
    fail(5);
    return;
  }
  ((DIRREC_PTR) sector_buffer)[srchrec_ptr->dir_entry_no].file_name[0] = (char) 0xE5;
  if (  /* dirsector_has_entries(last_sector, sector_buffer) && */
       (!put_sector(last_sector, sector_buffer))) {
    fail(5);
    return;
  }

  FREE_SECTOR_CHAIN(dirrec_ptr->start_sector);
  succeed();
}

/* Make Directory - subfunction 03h */
void make_dir(void)
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
  find_first();
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
  if ((!put_sector(dirrec_ptr->start_sector, sector_buffer)) ||
      (!create_dir_entry(&srchrec_ptr->dir_sector, NULL, fcbname_ptr, 0x10,
                         dirrec_ptr->start_sector, 0, dos_ftime()))) {
    fail(5);
    return;
  }
  succeed();
}

/* Change Directory - subfunction 05h */
void chdir(void)
{
  /* Special case for root */
  if ((*filename_ptr != '\\') || (*(filename_ptr + 1))) {
    if (contains_wildcards(fcbname_ptr)) {
      fail(3);
      return;
    }

    *srch_attr_ptr = 0x10;
    find_first();
    if (r.ax || (!(dirrec_ptr->file_attr & 0x10))) {
      fail(3);
      return;
    }
  }
  _fstrcpy(current_path, filename_ptr);
}

/* Close File - subfunction 06h */
void close_file(void)
{
  SFTREC_PTR sft = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (sft->handle_count)  /* If handle count not 0, decrement it */
    --sft->handle_count;

  /* If writing, create/update dir entry for file */
  if (!(sft->open_mode & 3))
    return;

  if (sft->dir_entry_no == 0xff) {
    if (!create_dir_entry(&sft->dir_sector, &sft->dir_entry_no, sft->file_name,
                          sft->file_attr, sft->start_sector, sft->file_size, sft->file_time))
      fail(5);
  }
  else {
    if ((last_sector != sft->dir_sector) && (!get_sector(sft->dir_sector, sector_buffer)))
      fail(5);
    last_sector = sft->dir_sector;
    ((DIRREC_PTR) sector_buffer)[sft->dir_entry_no].file_attr = sft->file_attr;
    ((DIRREC_PTR) sector_buffer)[sft->dir_entry_no].start_sector = sft->start_sector;
    ((DIRREC_PTR) sector_buffer)[sft->dir_entry_no].file_size = sft->file_size;
    ((DIRREC_PTR) sector_buffer)[sft->dir_entry_no].file_time = sft->file_time;
    if (!put_sector(sft->dir_sector, sector_buffer))
      fail(5);
  }
}

/* Commit File - subfunction 07h */
void commit_file(void)
{
  /* We support this but don't do anything... */
  return;
}

/* Read from File - subfunction 08h */
// For version that handles critical errors,
// see Undocumented DOS, 2nd edition, chapter 8
void read_file(void)
{
  SFTREC_PTR sft = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (sft->open_mode & 1) {
    fail(5);
    return;
  }

  if ((sft->file_pos + r.cx) > sft->file_size)
    r.cx = (uint) (sft->file_size - sft->file_pos);

  if (!r.cx)
    return;

  /* Fill caller's buffer and update the SFT for the file */
  read_data(&sft->file_pos, &r.cx, ((V3_SDA_PTR) sda_ptr)->current_dta,
            sft->start_sector, &sft->rel_sector, &sft->abs_sector);
}

/* Write to File - subfunction 09h */
void write_file(void)
{
  SFTREC_PTR sft = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (!(sft->open_mode & 3)) {
    fail(5);
    return;
  }

  sft->file_time = dos_ftime();

  /* Take account of DOS' 0-byte-write-truncates-file rcounte */
  if (!r.cx) {
    sft->file_size = sft->file_pos;
    chop_file(sft->file_pos, &sft->start_sector, &sft->rel_sector, &sft->abs_sector);
    return;
  }

  /* Write from the caller's buffer and update the SFT for the file */
  write_data(&sft->file_pos, &r.cx, ((V3_SDA_PTR) sda_ptr)->current_dta,
             &sft->start_sector, &sft->rel_sector, &sft->abs_sector);
  if (sft->file_pos > sft->file_size)
    sft->file_size = sft->file_pos;
}

/* Lock file - subfunction 0Ah */

/* We support this function only to illustrate how it works. We do
        not actually honor LOCK/UNLOCK requests. The following function
        supports locking only before, and both locking/unlocking after
        DOS 4.0 */
void lock_file(void)
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
void unlock_file(void)
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
void disk_space(void)
{
  r.ax = 1;
  r.bx = total_sectors;
  r.cx = SECTOR_SIZE;
  r.dx = free_sectors;
}

/* Get File Attributes - subfunction 0Fh */
void get_attr(void)
{
  if (contains_wildcards(fcbname_ptr)) {
    fail(2);
    return;
  }

  *srch_attr_ptr = 0x3f;
  find_first();
  if (r.ax)
    return;

  r.ax = (uint) dirrec_ptr->file_attr;
}

/* Set File Attributes - subfunction 0Eh */
void set_attr()
{
  get_attr();
  if (r.flags & FCARRY)
    return;

  if ((((uchar) *stack_param_ptr) & 0x10) ||
      (((DIRREC_PTR) sector_buffer)[srchrec_ptr->dir_entry_no].file_attr & 0x10)) {
    fail(5);
    return;
  }

  ((DIRREC_PTR) sector_buffer)[srchrec_ptr->dir_entry_no].file_attr = (uchar) *stack_param_ptr;
  if (!put_sector(last_sector, sector_buffer)) {
    fail(5);
    return;
  }
}

/* Rename File - subfunction 11h */
void rename_file(void)
{
  char far *path;
  uint ret = 0, dir_sector;
  int i = 0, j;

  *srch_attr_ptr = 0x21;
  srchrec_ptr_2->attr_mask = 0x3f;
  find_first();
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
        if (!get_sector(last_sector = srchrec_ptr->dir_sector, sector_buffer)) {
          fail(5);
          return;
        }
        ((DIRREC_PTR) sector_buffer)[srchrec_ptr->dir_entry_no].file_name[0] = (char) 0xE5;
        if (!put_sector(srchrec_ptr->dir_sector, sector_buffer)) {
          fail(5);
          return;
        }
      }
    }
    find_next();
  }

  if (r.ax == 18)
    r.ax = ret;

  if (!r.ax)
    succeed();
  else
    fail(r.ax);
}

/* Delete File - subfunction 13h */
void delete_file(void)
{
  uint ret = 0;

  *srch_attr_ptr = 0x21;
  find_first();

  while (!r.ax) {
    if (dirrec_ptr->file_attr & 1)
      ret = 5;
    else {
      FREE_SECTOR_CHAIN(dirrec_ptr->start_sector);
      ((DIRREC_PTR) sector_buffer)[srchrec_ptr->dir_entry_no].file_name[0] = (char) 0xE5;
      if (      /* dirsector_has_entries(last_sector, sector_buffer) && */
           (!put_sector(last_sector, sector_buffer))) {
        fail(5);
        return;
      }
    }
    find_next();
  }

  if (r.ax == 18)
    r.ax = ret;

  if (!r.ax)
    succeed();
  else
    fail(r.ax);
}

/* Support functions for the various file open functions below */

void init_sft(SFTREC_PTR sft)
{
  /*
     Initialize the supplied SFT entry. Note the modifications to
     the open mode word in the SFT. If bit 15 is set when we receive
     it, it is an FCB open, and requires the Set FCB Owner internal
     DOS function to be called.
   */
  if (sft->open_mode & 0x8000)
    /* File is being opened via FCB */
    sft->open_mode |= 0x00F0;
  else
    sft->open_mode &= 0x000F;

  /* Mark file as being on network drive, unwritten to */
  sft->dev_info_word = (uint) (0x8040 | (uint) our_drive_no);
  sft->file_pos = 0;
  sft->rel_sector = 0xffff;
  sft->abs_sector = 0xffff;
  sft->dev_drvr_ptr = NULL;

  if (sft->open_mode & 0x8000)
    set_sft_owner(sft);
}

/* Note that the following function uses dirrec_ptr to supply much of
   the SFT data. This is because an open of an existing file is
   effectively a find_first with data returned to the caller (DOS) in
   an SFT, rather than a found file directory entry buffer. So this
   function uses the knowledge that it is immediately preceded by a
   find_first(), and that the data is avalable in dirrec_ptr. */

void fill_sft(SFTREC_PTR sft, int use_found_1, int truncate)
{
  _fmemcpy(sft->file_name, fcbname_ptr, 11);
  if (use_found_1) {
    sft->file_attr = dirrec_ptr->file_attr;
    sft->file_time = truncate ? dos_ftime() : dirrec_ptr->file_time;
    if (truncate) {
      FREE_SECTOR_CHAIN(dirrec_ptr->start_sector);
      sft->start_sector = 0xFFFF;
    }
    else
      sft->start_sector = dirrec_ptr->start_sector;
    sft->file_size = truncate ? 0L : dirrec_ptr->file_size;
    sft->dir_sector = srchrec_ptr->dir_sector;
    sft->dir_entry_no = (uchar) srchrec_ptr->dir_entry_no;
  }
  else {
    sft->file_attr = (uchar) *stack_param_ptr;   /* Attr is top of stack */
    sft->file_time = dos_ftime();
    sft->start_sector = 0xffff;
    sft->file_size = 0;
    sft->dir_sector = srchrec_ptr->dir_sector;
    sft->dir_entry_no = 0xff;
  }
}

/* Open Existing File - subfunction 16h */
void open_existing(void)
{
  SFTREC_PTR sft;

  /* locate any file for any open */

  sft = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (contains_wildcards(fcbname_ptr)) {
    fail(3);
    return;
  }

  *srch_attr_ptr = 0x27;
  find_first();
  if (!r.ax) {
    fill_sft(sft, TRUE, FALSE);
    init_sft(sft);
  }
}

/* Truncate/Create File - subfunction 17h */
void open_new(void)
{
  SFTREC_PTR sft = (SFTREC_PTR) MK_FP(r.es, r.di);

  if (contains_wildcards(fcbname_ptr)) {
    fail(3);
    return;
  }

  *srch_attr_ptr = 0x3f;
  find_first();
  if ((r.flags & FCARRY) && (r.ax != 2))
    return;

  if ((!r.ax) && (dirrec_ptr->file_attr & 0x19)) {
    fail(5);
    return;
  }

  fill_sft(sft, (!r.ax), TRUE);
  init_sft(sft);
  succeed();
}

/* This function is never called! DOS fiddles with position internally */
void seek_file(void)
{
  long seek_amnt;
  SFTREC_PTR sft;

  /* But, just in case... */
  seek_amnt = -1L * (((long) r.cx << 16) + r.dx);
  sft = (SFTREC_PTR) MK_FP(r.es, r.di);
  if (seek_amnt > sft->file_size)
    seek_amnt = sft->file_size;

  sft->file_pos = sft->file_size - seek_amnt;
  r.dx = (uint) (sft->file_pos >> 16);
  r.ax = (uint) (sft->file_pos & 0xFFFF);
}

void extended_attr()
{
  r.ax = 2;
  /* Only called in v4.01, this is what MSCDEX returns */
}

/* Special Multi-Purpose Open File - subfunction 2Eh */

#define CREATE_IF_NOT_EXIST             0x10
#define OPEN_IF_EXISTS                  0x01
#define REPLACE_IF_EXISTS               0x02

void open_extended(void)
{
  SFTREC_PTR sft = (SFTREC_PTR) MK_FP(r.es, r.di);
  uint open_mode, action;

  open_mode = ((V4_SDA_PTR) sda_ptr)->mode_2E & 0x7f;
  action = ((V4_SDA_PTR) sda_ptr)->action_2E;
  sft->open_mode = open_mode;

  if (contains_wildcards(fcbname_ptr)) {
    fail(3);
    return;
  }

  *srch_attr_ptr = 0x3f;
  find_first();
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

  fill_sft(sft, (!r.ax), action & REPLACE_IF_EXISTS);
  init_sft(sft);
  succeed();
}

/* A placeholder */
void unsupported(void)
{
  return;
}

typedef void (*PROC)(void);

PROC dispatch_table[] = {
  inquiry,              /* 0x00h */
  rename_dir,                   /* 0x01h */
  unsupported,          /* 0x02h */
  make_dir,                   /* 0x03h */
  unsupported,          /* 0x04h */
  chdir,                   /* 0x05h */
  close_file,               /* 0x06h */
  commit_file,              /* 0x07h */
  read_file,              /* 0x08h */
  write_file,              /* 0x09h */
  lock_file,              /* 0x0Ah */
  unlock_file,            /* 0x0Bh */
  disk_space,               /* 0x0Ch */
  unsupported,          /* 0x0Dh */
  set_attr,              /* 0x0Eh */
  get_attr,              /* 0x0Fh */
  unsupported,          /* 0x10h */
  rename_file,               /* 0x11h */
  unsupported,          /* 0x12h */
  delete_file,               /* 0x13h */
  unsupported,          /* 0x14h */
  unsupported,          /* 0x15h */
  open_existing,               /* 0x16h */
  open_new,             /* 0x17h */
  unsupported,          /* 0x18h */
  unsupported,          /* 0x19h */
  unsupported,          /* 0x1Ah */
  find_first,            /* 0x1Bh */
  find_next,             /* 0x1Ch */
  unsupported,          /* 0x1Dh */
  unsupported,          /* 0x1Eh */
  unsupported,          /* 0x1Fh */
  unsupported,          /* 0x20h */
  seek_file,              /* 0x21h */
  unsupported,          /* 0x22h */
  unsupported,          /* 0x23h */
  unsupported,          /* 0x24h */
  unsupported,          /* 0x25h */
  unsupported,          /* 0x26h */
  unsupported,          /* 0x27h */
  unsupported,          /* 0x28h */
  unsupported,          /* 0x29h */
  unsupported,          /* 0x2Ah */
  unsupported,          /* 0x2Bh */
  unsupported,          /* 0x2Ch */
  extended_attr,       /* 0x2Dh */
  open_extended         /* 0x2Eh */
};

#define MAX_FXN_NO (sizeof(dispatch_table) / sizeof(PROC))

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

int is_call_for_us(uint es, uint di, uint ds)
{
  uchar far *p;
  int ret = 0xFF;

  filename_is_char_device = 0;

  // Note that the first 'if' checks for the bottom 6 bits
  // of the device information word in the SFT. Values > last drive
  // relate to files not associated with drives, such as LAN Manager
  // named pipes (Thanks to Dave Markun).
  if ((curr_fxn >= SUBF_CLOSE && curr_fxn <= SUBF_UNLOCK)
      || (curr_fxn == SUBF_SEEK)
      || (curr_fxn == SUBF_EXTENDATTR)) {
    ret = ((((SFTREC_PTR) MK_FP(es, di))->dev_info_word & 0x3F)
           == our_drive_no);
  }
  else {
    if (curr_fxn == SUBF_INQUIRY)   // 2F/1100 -- succeed automatically
      ret = TRUE;
    else {
      if (curr_fxn == SUBF_FINDNEXT)   // Find Next
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
        if (curr_fxn != SUBF_GETDISKSPACE)
          generate_fcbname(ds);
        return TRUE;
      }
      else
        return FALSE;
    }
  }
  return ret;
}

/* -------------------------------------------------------------*/

/* This is the main entry point for the redirector. It assesses if
   the call is for our drive, and if so, calls the appropriate routine. On
   return, it restores the (possibly modified) register values. */

void interrupt far redirector(ALL_REGS entry_regs)
{
  static uint save_bp;
  uint our_ss, our_sp, cur_ss, cur_sp;

  _asm STI;

  if (((entry_regs.ax >> 8) != (uchar) 0x11) || ((uchar) entry_regs.ax > MAX_FXN_NO))
    goto chain_on;

  curr_fxn = (uchar) entry_regs.ax;

  if ((dispatch_table[curr_fxn] == unsupported) ||
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

  cur_ss = getSS();
  cur_sp = getSP();
  our_sp = (FP_OFF(our_stack) + 15) >> 4;
  our_ss = FP_SEG(our_stack) + our_sp;
  our_sp = STACK_SIZE - 2 - (((our_sp - (FP_OFF(our_stack) >> 4)) << 4)
                             - (FP_OFF(our_stack) & 0xf));

  _asm {
    mov dos_sp, sp;

    mov ax, our_ss;
    mov cx, our_sp;

    // activate new stack
    cli;
    mov ss, ax;
    mov sp, cx;
    sti;
  }

  cur_ss = getSS();
  cur_sp = getSP();

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

  cur_ss = getSS();
  cur_sp = getSP();

  // put the possibly changed registers back on the stack, and return
  entry_regs = r;
  return;

  // If the call wasn't for us, we chain on.
 chain_on:
  _chain_intr(prev_int2f_vector);
}
