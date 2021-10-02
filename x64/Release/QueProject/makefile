   
IDIR =include
CC=../QueC.exe
ASM=../QueASM.exe
LD=../QueLinker.exe
EXE=../QueVM.exe

VMARGS=-m 10000000

ODIR=obj

_QSM_OBJ=main.qo
QSM_OBJ=$(patsubst %,$(ODIR)/%,$(_QSM_OBJ))

_QUE_OBJ=fun.qo
QUE_OBJ=$(patsubst %,$(ODIR)/%,$(_QUE_OBJ))

$(ODIR)/%.qo: %.qsm $(DEPS)
	$(ASM) $< $@
	
$(ODIR)/%.qo: %.qu $(DEPS)
	$(CC) $< $@

all: $(QSM_OBJ) $(QUE_OBJ)
	$(LD) $@ $^

run: all
	$(EXE) all $(VMARGS)

.PHONY: clean

clean:
	rm -f $(ODIR)/*.qo *~ core $(INCDIR)/*~ 
