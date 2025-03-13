#ifndef _RAMDRIVE_H
#define _RAMDRIVE_H

#include "dosdata.h"
#ifdef DIRECT_DRIVE
#include "xms.h"
#endif

#define         SECTOR_SIZE             1024    // 1024b/sector allows for 64M of XMS

extern void set_up_xms_disk(uint16_t req_size);

// FIXME - this should be static inside ramdrive.c
extern uint16_t xms_handle;

#ifdef DIRECT_DRIVE
extern uint16_t last_sector;
extern uint8_t sector_buffer[];
extern uint16_t disk_size;
extern uint16_t total_sectors;
extern uint16_t free_sectors;

extern int get_dir_start_sector(char far *path, uint16_t far *abs_sector_ptr);
extern int find_next_entry(char far *mask, uint8_t attr_mask, char far *filename,
                           uint8_t far *attr_ptr, uint32_t far *file_time_ptr,
                           uint16_t far *start_sec_ptr, uint32_t far *file_size_ptr,
                           uint16_t far *dir_sector_ptr, uint16_t far *dir_entryno_ptr);
extern uint16_t set_next_sector(uint16_t abs_sector, uint16_t next_sector);
extern uint16_t next_free_sector(void);
extern int create_dir_entry(uint16_t far *dir_sector_ptr, uint8_t far *dir_entryno_ptr,
                            char far *filename, uint8_t file_attr, uint16_t start_sector,
                            uint32_t file_size, uint32_t file_time);
extern void read_data(uint32_t far *file_pos_ptr, uint16_t *len_ptr, uint8_t far *buf,
                      uint16_t start_sector, uint16_t far *last_rel_ptr,
                      uint16_t far *last_abs_ptr);
extern void chop_file(uint32_t file_pos, uint16_t far *start_sec_ptr,
                      uint16_t far *last_rel_ptr, uint16_t far *last_abs_ptr);
extern void write_data(uint32_t far *file_pos_ptr, uint16_t *len_ptr, uint8_t far *buf,
                       uint16_t far *start_sec_ptr, uint16_t far *last_rel_ptr,
                       uint16_t far *last_abs_ptr);
#else
extern int ram_open(char far *path, int flags);
extern int ram_close(int fd);
extern int ram_read(int fd, void far *buf, uint16_t count);
extern int ram_write(int fd, const void far *buf, uint16_t count);
extern uint32_t ram_seek(int fd, uint32_t offset);
extern int ram_opendir(char far *name);
extern int ram_closedir(int dirp);
extern DIRREC_PTR ram_readdir(int dirp);
extern int ram_mkdir(char far *path);
extern int ram_stat(char far *path, DIRREC_PTR dirrec);
extern int ram_rmdir(char far *path);
extern int ram_rename(char far *old_path, char far *new_path);
#endif

#endif /* _RAMDRIVE_H */
