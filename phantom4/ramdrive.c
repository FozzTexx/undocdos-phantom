#include "ramdrive.h"
#include "xms.h"
#include "dosfunc.h"
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>

#include "debug.h"

// FIXME - doesn't belong here
extern void failprog(char *msg);
extern void get_fcbname_from_path(char far *path, char far *fcbname);

#define         DIRREC_PER_SECTOR       (SECTOR_SIZE / sizeof(DIRREC))
#define         DEF_DISK_SIZE           0xFFFE  // Default attempted allocation is_all
#define         FATPAGE_SIZE            128
#define         MIN_DISK_SIZE           128     // Don't load unless 128kb XMS free

uint16_t xms_handle = 0;            /* Handle of XMS allocation for disk */
uint16_t disk_size = DEF_DISK_SIZE; /* size of XMS allocation for disk */
uint16_t total_sectors;             /* total 1k sectors on XMS disk */
uint16_t free_sectors;              /* unallocated sectors on XMS disk */
uint8_t sector_buffer[SECTOR_SIZE];       /* general purpose sector buffer */
uint32_t FAT_location;             /* offset within XMS allocation of disk FAT */
uint16_t FAT_page[FATPAGE_SIZE];    /* buffer for FAT entries */
int cur_FAT_page = -1;          /* index of FAT page in buffer */
int FAT_page_dirty = FALSE;     /* Has current FAT page been updated */
uint16_t last_sector = 0xffff;      /* last sector read into sector buffer */

#define get_sector(sec, buf)                                         \
  xms_copy_to_real(xms_handle, (uint32_t) SECTOR_SIZE * (sec),       \
                   SECTOR_SIZE, (uint8_t far *) (buf))

#define put_sector(sec, buf)                                         \
  xms_copy_fm_real(xms_handle, (uint32_t) SECTOR_SIZE * (sec),       \
                   SECTOR_SIZE, (uint8_t far *) (buf))

#define FREE_SECTOR_CHAIN(sec)                                       \
        while ((sec) != 0xFFFF) (sec) = set_next_sector((sec), 0)

/* Check that the page of FAT entries for the supplied sector is in
        the buffer. If it isn't, go get it, but write back the currently
        buffered page first if it has been updated. */
int check_FAT_page(uint16_t abs_sector)
{
  int page = (int) (abs_sector / FATPAGE_SIZE);

  if (page != cur_FAT_page) {
    if (FAT_page_dirty &&
        (!xms_copy_fm_real(xms_handle,
                           FAT_location + (cur_FAT_page * (FATPAGE_SIZE * 2)),
                           FATPAGE_SIZE * 2, (uint8_t far *) FAT_page)))
      return FALSE;

    if (!xms_copy_to_real(xms_handle,
                          FAT_location + (page * (FATPAGE_SIZE * 2)), FATPAGE_SIZE * 2,
                          (uint8_t far *) FAT_page))
      return FALSE;
    cur_FAT_page = page;
    FAT_page_dirty = FALSE;
  }
  return TRUE;
}

/* Use the FAT to find the next sector in the chain for the current
        file/directory */
uint16_t next_FAT_sector(uint16_t abs_sector)
{
  if (!check_FAT_page(abs_sector))
    return 0;

  return FAT_page[abs_sector - (cur_FAT_page * FATPAGE_SIZE)];
}

/* Update the FAT entry for this sector to reflect the next sector
        in the chain for the current file/directory */
uint16_t set_next_sector(uint16_t abs_sector, uint16_t next_sector)
{
  uint16_t save_sector;

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
uint16_t next_free_sector(void)
{
  static uint16_t prev_sector = 0;
  uint16_t save_sector = prev_sector;

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

void set_up_xms_disk(uint16_t req_size)
{
  uint32_t count, ofs;
  uint16_t len;


  if (!xms_is_present())
    failprog("XMS not present.");

  if (!req_size)
    req_size = DEF_DISK_SIZE;
  if ((disk_size = min(req_size, xms_kb_avail())) < MIN_DISK_SIZE)
    failprog("Need a minimum of 128kb XMS.");

  // The allocation is made up of n sectors and
  // n FAT entries (2 bytes each for our 16-bit FAT)
  free_sectors = total_sectors = (uint16_t)
    (((uint32_t) disk_size * 1024) / (SECTOR_SIZE + 2));

  // A little wasted here, but the accurate calculation would soak up
  // TSR code space.
  if (!xms_alloc_block(disk_size, &xms_handle))
    failprog("XMS allocation error.");

  free_sectors--;
  FAT_location = (uint32_t) total_sectors *SECTOR_SIZE;

  // First FAT entry belongs to the root directory, which,
  // unlike DOS, we keep in the data area, and which always starts at
  // sector 0. (We do not have to worry about disk defraggers and the
  // like).
  count = (uint32_t) total_sectors *2;

  ofs = FAT_location;

  memset(sector_buffer, 0, sizeof(sector_buffer));
  // Claim the first sector as used for the root directory.
  sector_buffer[0] = sector_buffer[1] = (uint8_t) 0xff;

  while (count > 0) {
    len = (uint16_t) min(count, SECTOR_SIZE);
    if (!xms_copy_fm_real(xms_handle, ofs, len, (uint8_t far *) sector_buffer))
      failprog("XMS error.");
    count -= len;
    ofs += len;
    sector_buffer[0] = sector_buffer[1] = 0;
  }

  memset(((DIRREC *) sector_buffer)->fcb_name, ' ', 11);
  memcpy(((DIRREC *) sector_buffer)->fcb_name, "PHANTOM", 7);
  ((DIRREC *) sector_buffer)->attr = 0x08;
  ((DIRREC *) sector_buffer)->datetime = dos_ftime();
  if (!xms_copy_fm_real(xms_handle, 0, SECTOR_SIZE, (uint8_t far *) sector_buffer))
    failprog("XMS error.");
}

/* Find the sector number of the start of the directory entries
        for the supplied path */

int get_dir_start_sector(char far *path, uint16_t far *abs_sector_ptr)
{
  char fcbname[11];
  uint16_t abs_sector = 0;
  DIRREC *dr = (DIRREC *) sector_buffer;
  char far *next_dir;
  char far *path_end = path + _fstrlen(path);
  int i;

  consolef("GET_DIR_START_SECTOR path=\"%ls\" abs_sector=%04x\n", path, *abs_sector_ptr);
  while (path != path_end) {
    for (next_dir = ++path; *next_dir && (*next_dir != '\\'); next_dir++);
    *next_dir = 0;
    get_fcbname_from_path(path, fcbname);

    for (;;) {
      if (!xms_copy_to_real(xms_handle, abs_sector * SECTOR_SIZE, SECTOR_SIZE, sector_buffer))
        return FALSE;
      last_sector = abs_sector;
      for (i = 0; i < DIRREC_PER_SECTOR; i++) {
        if (dr[i].fcb_name[0] == (char) 0xE5)
          continue;
        if (!dr[i].fcb_name[0])
          i = DIRREC_PER_SECTOR;
        else if (match_to_mask(dr[i].fcb_name, fcbname)) {
          if (!(dr[i].attr & 0x10))
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

int find_next_entry(char far *mask, uint8_t attr_mask, char far *filename,
                    uint8_t far *attr_ptr, uint32_t far *file_time_ptr,
                    uint16_t far *start_sec_ptr, uint32_t far *file_size_ptr,
                    uint16_t far *dir_sector_ptr, uint16_t far *dir_entryno_ptr)
{
  DIRREC *dr = (DIRREC *) sector_buffer;
  int idx = *dir_entryno_ptr + 1;
  uint16_t abs_sector = *dir_sector_ptr;

  consolef("FIND_NEXT_ENTRY IN mask=\"%ls\" attr_mask=%02x filename=\"%ls\" attr=%02x file_time=%08lx start_sec=%04x file_size=%08lx dir_sector=%04x dir_entryno=%04x\n", mask, attr_mask, filename, *attr_ptr, *file_time_ptr, *start_sec_ptr, *file_size_ptr, *dir_sector_ptr, *dir_entryno_ptr);
  for (;;) {
    if (abs_sector != last_sector) {
      if (!get_sector(abs_sector, sector_buffer))  {
	consolef("FIND_NEXT_ENTRY FAIL 1\n");
        return FALSE;
      }
      else
        last_sector = abs_sector;
    }
    for (; idx < DIRREC_PER_SECTOR; idx++) {
      if (!dr[idx].fcb_name[0]) {
	consolef("FIND_NEXT_ENTRY FAIL 2\n");
        return FALSE;
      }
      if (dr[idx].fcb_name[0] == (char) 0xE5)
        continue;
      if (match_to_mask(mask, dr[idx].fcb_name) &&
          (!(((attr_mask == 0x08) && (!(dr[idx].attr & 0x08))) ||
             ((dr[idx].attr & 0x10) && (!(attr_mask & 0x10))) ||
             ((dr[idx].attr & 0x08) && (!(attr_mask & 0x08))) ||
             ((dr[idx].attr & 0x04) && (!(attr_mask & 0x04))) ||
             ((dr[idx].attr & 0x02) && (!(attr_mask & 0x02)))))) {
        *dir_sector_ptr = abs_sector;
        *dir_entryno_ptr = idx;
        if (filename)
          _fmemcpy(filename, dr[idx].fcb_name, 11);
        if (attr_ptr)
          *attr_ptr = dr[idx].attr;
        if (file_time_ptr)
          *file_time_ptr = dr[idx].datetime;
        if (file_size_ptr)
          *file_size_ptr = dr[idx].size;
        if (start_sec_ptr)
          *start_sec_ptr = dr[idx].start_sector;
	consolef("FIND_NEXT_ENTRY OUT mask=\"%ls\" attr_mask=%02x filename=\"%ls\" attr=%02x file_time=%08lx start_sec=%04x file_size=%08lx dir_sector=%04x dir_entryno=%04x\n", mask, attr_mask, filename, *attr_ptr, *file_time_ptr, *start_sec_ptr, *file_size_ptr, *dir_sector_ptr, *dir_entryno_ptr);
        return TRUE;
      }
    }
    if ((abs_sector = next_FAT_sector(abs_sector)) == 0xFFFF) {
      consolef("FIND_NEXT_ENTRY FAIL 3\n");
      return FALSE;
    }
  }

  consolef("FIND_NEXT_ENTRY FAIL 4\n");
  return FALSE;
}

/* Generate a new directory entry, reusing a previously deleted
   entry, using an as yet unused entry, or allocating more space
   for the sector if no entries are available in the current
   allocation for the directory */

int create_dir_entry(uint16_t far *dir_sector_ptr, uint8_t far *dir_entryno_ptr,
                     char far *filename, uint8_t file_attr, uint16_t start_sector,
		     uint32_t file_size, uint32_t file_time)
{
  uint16_t next_sector, dir_sector = *dir_sector_ptr;
  DIRREC *dr = (DIRREC *) sector_buffer;
  int idx;

  consolef("CREATE DIR dir_sector=%04x dir_entryno=%02x filename=\"%ls\" file_attr=%02x start_sector=%04x file_size=%08lx file_time=%08lx\n", *dir_sector_ptr, *dir_entryno_ptr, filename, file_attr, start_sector, file_size, file_time);
  for (;;) {
    if (dir_sector != last_sector)
      if (!get_sector(dir_sector, sector_buffer))
        return FALSE;
      else
        last_sector = dir_sector;
    for (idx = 0; idx < DIRREC_PER_SECTOR; idx++) {
      if (dr[idx].fcb_name[0] && (dr[idx].fcb_name[0] != (char) 0xE5))
        continue;
      _fmemcpy(dr[idx].fcb_name, filename, 11);
      dr[idx].attr = file_attr;
      dr[idx].datetime = file_time;
      dr[idx].size = file_size;
      dr[idx].start_sector = start_sector;
      *dir_sector_ptr = dir_sector;
      if (dir_entryno_ptr)
        *dir_entryno_ptr = (uint8_t) idx;
      return put_sector(dir_sector, sector_buffer);
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

void read_data(uint32_t far *file_pos_ptr, uint16_t *len_ptr, uint8_t far *buf,
               uint16_t start_sector, uint16_t far *last_rel_ptr, uint16_t far *last_abs_ptr)
{
  uint16_t start, rel_sector, abs_sector;
  uint16_t i, count, len = *len_ptr;

  consolef("READ_DATA file_pos=%08lx len=%04x buf=%08lx start_sector=%04x last_rel=%04x last_abs=%04x\n", *file_pos_ptr, *len_ptr, buf, start_sector, *last_rel_ptr, *last_abs_ptr);
  start = (uint16_t) (*file_pos_ptr / SECTOR_SIZE);

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
    start = (uint16_t) (*file_pos_ptr / SECTOR_SIZE);
    if (start > rel_sector) {
      if ((abs_sector = next_FAT_sector(abs_sector)) == 0xFFFF) {
        *len_ptr -= len;
        goto update_sectors;
      }
      rel_sector++;
      continue;
    }
    i = (int) (*file_pos_ptr % SECTOR_SIZE);
    count = min((uint16_t) SECTOR_SIZE - i, len);
    if (count < SECTOR_SIZE) {
      if (!get_sector(abs_sector, sector_buffer)) {
        *len_ptr -= len;
        goto update_sectors;
      }
      last_sector = abs_sector;
      _fmemcpy(buf, (uint8_t far *) &sector_buffer[i], count);
    }
    else {
      if (!get_sector(abs_sector, buf)) {
        *len_ptr -= len;
        goto update_sectors;
      }
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

void chop_file(uint32_t file_pos, uint16_t far *start_sec_ptr, uint16_t far *last_rel_ptr,
               uint16_t far *last_abs_ptr)
{
  uint16_t keep_sector, rel_sector, abs_sector, prev_sector = 0xFFFF;

  keep_sector = (uint16_t) ((file_pos + SECTOR_SIZE - 1) / SECTOR_SIZE);
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

void write_data(uint32_t far *file_pos_ptr, uint16_t *len_ptr, uint8_t far *buf,
                uint16_t far *start_sec_ptr, uint16_t far *last_rel_ptr,
		uint16_t far *last_abs_ptr)
{
  uint16_t next_sector, start, rel_sector, abs_sector;
  uint16_t i, count, len = *len_ptr;

  start = (uint16_t) (*file_pos_ptr / SECTOR_SIZE);

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
    start = (uint16_t) (*file_pos_ptr / SECTOR_SIZE);
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
    i = (uint16_t) (*file_pos_ptr % SECTOR_SIZE);
    count = min((uint16_t) SECTOR_SIZE - i, len);
    if (count < SECTOR_SIZE) {
      if (!get_sector(abs_sector, sector_buffer))
        goto update_sectors;
      last_sector = abs_sector;
      _fmemcpy((uint8_t far *) &sector_buffer[i], buf, count);
      if (!put_sector(abs_sector, sector_buffer))
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

#ifndef DIRECT_DRIVE

#define MAX_HANDLES 8

typedef struct {
  uint16_t index, sector;
  uint8_t is_open:1;
} dir_stream;
static dir_stream dir_handles[MAX_HANDLES];

typedef struct {
  uint32_t pos, length;
  uint16_t dir_sector, start_sector, rel_sector, abs_sector;
  char fcb_name[DOS_FCBNAME_LEN];
  uint8_t is_open:1;
  uint8_t is_new:1;
} file_stream;
static file_stream file_handles[MAX_HANDLES];
static uint16_t near_count;
static char temp_fcb[DOS_FCBNAME_LEN];

void fcbitize(char far *dest, const char far *source)
{
  const char far *dot, far *ext;
  int len;


  _fmemset(dest, ' ', 11);
  dot = _fstrchr(source, '.');
  if (dot)
    ext = dot + 1;
  else {
    dot = source + _fstrlen(source);
    ext = NULL;
  }
  len = dot - source;
  _fmemcpy(dest, source, len <= 8 ? len : 8);
  if (ext) {
    len = _fstrlen(ext);
    _fmemcpy(&dest[8], ext, len <= 3 ? len : 3);
  }

  return;
}

int ram_open(char far *path, int flags)
{
  uint8_t idx;
  file_stream *fh;
  uint16_t index, attr;
  uint32_t size;
  const char far *sep;


  for (idx = 0; idx < MAX_HANDLES; idx++)
    if (!file_handles[idx].is_open)
      break;
  if (idx == MAX_HANDLES)
    return -1;

  fh = &file_handles[idx];
  memset(fh, 0, sizeof(*fh));

  index = -1;
  sep = _fstrrchr(path, '\\');
  if (!sep)
    return -1;
  fcbitize(fh->fcb_name, sep+1);
  
  if (flags != O_WRONLY) {
    fh->dir_sector = 0xffff;
    if ((path = _fstrrchr(path, '\\')))
      *path = 0;
    if (!get_dir_start_sector(path, &fh->dir_sector))
      return -1;
    if (path)
      *path = '\\';
    if (!find_next_entry(fh->fcb_name, 0x27, NULL, NULL, NULL,
			 &fh->start_sector, &fh->length, &fh->dir_sector, &index))
      return -1;
  }
  else {
    fh->start_sector = 0xffff;
    fh->is_new = 1;
  }

  fh->rel_sector = fh->abs_sector = 0xffff;
  fh->is_open = 1;
  return idx;
}

int ram_close(int fd)
{
  file_stream *fh;
  uint8_t index = -1;


  if (fd < 0 || fd >= MAX_HANDLES)
    return -1;

  fh = &file_handles[fd];
  if (!fh->is_open)
    return -1;

  dumpHex(fh, sizeof(*fh), 0);
  if (fh->is_new) {
    if (!create_dir_entry(&fh->dir_sector, &index, fh->fcb_name,
                          0 /* FIXME - DOS attr */,
			  fh->start_sector, fh->pos,
			  0 /* FIXME - current datetime */))
      return -1;
  }

  fh->is_open = 0;
  return 0;
}

int ram_read(int fd, void far *buf, uint16_t count)
{
  file_stream *fh;


  if (fd < 0 || fd >= MAX_HANDLES)
    return 0;

  fh = &file_handles[fd];
  if (!fh->is_open)
    return 0;

  if (fh->pos + count > fh->length)
    count = fh->length - fh->pos;

  if (!count)
    return count;
  
  near_count = count;
  read_data(&fh->pos, &near_count, (uint8_t far *) buf,
             fh->start_sector, &fh->rel_sector, &fh->abs_sector);
  return near_count;
}

int ram_write(int fd, const void far *buf, uint16_t count)
{
  file_stream *fh;


  if (fd < 0 || fd >= MAX_HANDLES)
    return 0;

  fh = &file_handles[fd];
  if (!fh->is_open)
    return 0;

  near_count = count;
  write_data(&fh->pos, &near_count, (uint8_t far *) buf,
             &fh->start_sector, &fh->rel_sector, &fh->abs_sector);
  return near_count;
}

uint32_t ram_seek(int fd, uint32_t offset)
{
  file_stream *fh;


  if (fd < 0 || fd >= MAX_HANDLES)
    return -1;

  fh = &file_handles[fd];
  if (!fh->is_open)
    return -1;

  fh->pos = offset;
  return fh->pos;
}

int ram_opendir(char far *name)
{
  uint8_t idx;
  dir_stream *dh;


  for (idx = 0; idx < MAX_HANDLES; idx++)
    if (!dir_handles[idx].is_open)
      break;
  if (idx == MAX_HANDLES)
    return -1;

  dh = &dir_handles[idx];
  memset(dh, 0, sizeof(*dh));
  if (!get_dir_start_sector(name, &dh->sector))
    return -1;
  
  dh->index = -1;
  dh->is_open = 1;
  return idx;
}

int ram_closedir(int dirp)
{
  if (dirp < 0 || dirp >= MAX_HANDLES)
    return -1;
  if (!dir_handles[dirp].is_open)
    return -1;
  dir_handles[dirp].is_open = 0;
  return 0;
}

DIRREC_PTR ram_readdir(int dirp)
{
  dir_stream *dh;
  DIRREC *dr = (DIRREC *) sector_buffer;


  if (dirp < 0 || dirp >= MAX_HANDLES)
    return NULL;
  dh = &dir_handles[dirp];
  if (!dh->is_open)
    return NULL;
  
  if (!find_next_entry("???????????", 0x3F, NULL, NULL, NULL, NULL, NULL,
		       &dh->sector, &dh->index))
    return NULL;

  return &dr[dh->index];
}

int ram_mkdir(char far *path)
{
  uint16_t start_sector, dir_sector = 0;
  char far *sep;


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
  if (!(start_sector = next_free_sector()))
    return -1;

  set_next_sector(start_sector, 0xFFFF);
  last_sector = start_sector;
  if ((sep = _fstrrchr(path, '\\')))
    sep++;
  else
    sep = path;
  fcbitize(temp_fcb, sep);
  if ((!put_sector(start_sector, sector_buffer)) ||
      (!create_dir_entry(&dir_sector, NULL, temp_fcb, 0x10,
                         start_sector, 0, dos_ftime())))
    return -1;

  return 0;
}

#endif
