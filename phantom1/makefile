TARGET  = phantom1
AS      = wasm -q
ASFLAGS = -mt -bt=DOS
CC      = wcc -q
CFLAGS  = -bt=dos -ms -osh $(CPPFLAGS)
LD	= wlink OPTION quiet
LDFLAGS = SYSTEM dos OPTION MAP

all: $(TARGET).exe

$(TARGET).exe: $(TARGET).obj

.obj.exe:
	$(LD) $(LDFLAGS) NAME $@ file $<
.c.obj: .AUTODEPEND
	$(CC) $(CFLAGS) -fo=$@ $<
.asm.obj: .AUTODEPEND
	$(AS) $(ASFLAGS) -fo=$@ $<

clean : .SYMBOLIC
	rm -f $(TARGET) *.obj *.map *.err *.com *.exe
