TARGET  = phantom3.exe
AS      = wasm -q
ASFLAGS = -0 -mt -bt=DOS
CC      = wcc -q
CFLAGS  = -0 -bt=dos -ms -osh -zu $(CPPFLAGS)
LD	= wlink OPTION quiet
LDFLAGS = &
	SYSTEM dos &
	OPTION MAP

CFILES  = phantom.c bios.c dosfunc.c xms.c ramdrive.c redir.c
OBJS = $(CFILES:.c=.obj) $(AFILES:.asm=.obj)

$(TARGET): $(OBJS)
	$(LD) $(LDFLAGS) &
	  disable 1014 &
	  name $@ &
	  file {$(OBJS)}

fnshare.obj: fnshare.c .AUTODEPEND
	$(CC) $(CFLAGS) -nt=_INIT -nc=INIT -fo=$@ $<

.c.obj: .AUTODEPEND
        $(CC) $(CFLAGS) -fo=$@ $<
.asm.obj: .AUTODEPEND
	$(AS) $(ASFLAGS) -fo=$@ $<

clean : .SYMBOLIC
	rm -f $(TARGET) *.obj *.map *.err *.sys *.com *.exe *.o
