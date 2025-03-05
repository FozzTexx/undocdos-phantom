#ifndef _REDIR_H
#define _REDIR_H

#include "dosdata.h"

#define DOS_INT_REDIR   0x2F
#define REDIRECTOR_FUNC 0x11

enum {
  SUBF_INQUIRY          = 0x00,
  SUBF_REMOVEDIR        = 0x01,
  SUBF_MAKEDIR          = 0x03,
  SUBF_CHDIR            = 0x05,
  SUBF_CLOSE            = 0x06,
  SUBF_COMMIT           = 0x07,
  SUBF_READ             = 0x08,
  SUBF_WRITE            = 0x09,
  SUBF_LOCK             = 0x0A,
  SUBF_UNLOCK           = 0x0B,
  SUBF_GETDISKSPACE     = 0x0C,
  SUBF_SETATTR          = 0x0E,
  SUBF_GETATTR          = 0x0F,
  SUBF_RENAME           = 0x11,
  SUBF_DELETE           = 0x13,
  SUBF_OPENEXIST        = 0x16,
  SUBF_OPENCREATE       = 0x17,
  SUBF_FINDFIRST        = 0x1B,
  SUBF_FINDNEXT         = 0x1C,
  SUBF_CLOSEALL         = 0x1D,
  SUBF_DOREDIR          = 0x1E,
  SUBF_PRINTERSETUP     = 0x1F,
  SUBF_FLUSHBUFFERS     = 0x20,
  SUBF_SEEK             = 0x21,
  SUBF_PROCTERM         = 0x22,
  SUBF_QUALIFYPATH      = 0x23,
  SUBF_REDIRPRINTER     = 0x25,
  SUBF_EXTENDATTR       = 0x2D,
  SUBF_OPENEXTENDED     = 0x2E,
};

typedef void (interrupt far *INTVECT)();

extern INTVECT prev_int2f_vector;

extern uint cds_root_size;
extern uchar our_drive_no;
extern char far *cds_path_root;
extern char our_drive_str[];
extern SIGREC sigrec;
extern char far *fcbname_ptr;
extern uchar far *sda_ptr;
extern char far *current_path;
extern char far *filename_ptr;
extern char far *filename_ptr_2;
extern char far *fcbname_ptr_2;
extern uchar far *srch_attr_ptr;
extern SRCHREC_PTR srchrec_ptr;
extern SRCHREC_PTR srchrec_ptr_2;
extern DIRREC_PTR dirrec_ptr;
extern DIRREC_PTR dirrec_ptr_2;

extern void interrupt far redirector(ALL_REGS entry_regs);

#endif /* _REDIR_H */
