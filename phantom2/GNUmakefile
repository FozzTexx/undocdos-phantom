WMAKE=wmake -h

#dosftime.obj: dosftime.asm
#	wasm -bt=DOS $<

#xms.o: xms.c
#	wcc -bt=DOS $<

#unload.o: unload.c
#	wcc -bt=DOS $<

all:
	@$(WMAKE) -e CPPFLAGS=-DVERSION='\"$(shell git rev-parse --short HEAD)$(shell git status --porcelain | grep -q '^[ MADRCU]' && echo '*')\"'

clean:
	@$(WMAKE) clean
