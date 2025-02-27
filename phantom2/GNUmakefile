WMAKE=wmake -h

#dosftime.obj: dosftime.asm
#	wasm -bt=DOS $<

#xms.exe: xms.c
#	wcl -q -bt=DOS $< ../../fujinet-rs232/sys/print.obj

#xms2.exe: xms2.c
#	wcl -q -bt=DOS $<

#unload.o: unload.c
#	wcc -bt=DOS $<

# stack.obj: stack.asm
# 	wasm -bt=DOS $<

all:
	@$(WMAKE) -e CPPFLAGS=-DVERSION='\"$(shell git rev-parse --short HEAD)$(shell git status --porcelain | grep -q '^[ MADRCU]' && echo '*')\"'

clean:
	@$(WMAKE) clean
