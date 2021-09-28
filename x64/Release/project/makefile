   
IDIR =include
ASM=../QueASM.exe
LD=../QueLinker.exe
EXE=../QueVM.exe

ODIR=obj

_OBJ=main.qo utils.qo
OBJ=$(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.qo: %.qsm $(DEPS)
	$(ASM) $< $@

all: $(OBJ)
	$(LD) $@ $^

run: all
	$(EXE) all

.PHONY: clean

clean:
	rm -f $(ODIR)/*.qo *~ core $(INCDIR)/*~ 
