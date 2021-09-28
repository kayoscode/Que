   
IDIR =include
ASM=../QueASM.exe
LD=../QueLinker.exe
EXE=../QueVM.exe

VMARGS=-m 10000000

ODIR=obj

_OBJ=main.qo utils.qo
OBJ=$(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.qo: %.qsm $(DEPS)
	$(ASM) $< $@

all: $(OBJ)
	$(LD) $@ $^

run: all
	$(EXE) all $(VMARGS)

.PHONY: clean

clean:
	rm -f $(ODIR)/*.qo *~ core $(INCDIR)/*~ 
