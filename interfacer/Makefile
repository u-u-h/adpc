ADPCOMPILE = ../adpcompile

GHC = ghc

all: ../config/config.mf adpc adpc-bin man2c man2lhs colecho

../config/config.mf:
	@echo Compile and run config in the config directory\! && false

include ../config/config.mf

InterfaceDTD.hs: interface.dtd
	DtdToHaskell interface.dtd InterfaceDTD.hs

LHS = Adpc.lhs Sed.lhs InterfaceDTD.hs 

adpc: $(LHS)
	$(GHC) --make Adpc.lhs -i../config -package HaXml -o $@

adpc-bin:
	ln -s adpc adpc-bin

man2c: Man2c.lhs
	$(GHC) --make Man2c.lhs -o man2c

man2lhs : Man2lhs.lhs
	$(GHC) --make Man2lhs.lhs -o man2lhs

colecho: Colecho.lhs
	$(GHC) --make Colecho.lhs -o colecho

PERMF = go+r
PERMD = go+rx

install: uninstall
	cp -d $(ADPCOMPILE) adpc adpc-bin man2c man2lhs colecho $(PREFIX)/bin
	chmod 755 $(PREFIX)/bin/adpcompile
	chmod $(PERMD) $(PREFIX)/bin/adpc-bin
	chmod $(PERMD) $(PREFIX)/bin/adpc
	chmod 755 $(PREFIX)/bin/man2c
	chmod 755 $(PREFIX)/bin/man2lhs
	chmod 755 $(PREFIX)/bin/colecho
	-mkdir $(PREFIX)/lib/adpc/
	chmod $(PERMD) $(PREFIX)/lib/adpc/
	cp -R lib/adpc/x86.libedit.a $(PREFIX)/lib/adpc/libedit.a
	chmod $(PERMF) $(PREFIX)/lib/adpc/libedit.a
	cp -R share/adpc $(PREFIX)/share
	cp share/adpc/src/* $(PREFIX)/share/adpc/lib/distr/src/
	-mkdir $(PREFIX)/share/adpc
	chmod $(PERMD) $(PREFIX)/share/adpc
	cd $(PREFIX)/share/adpc ; find . -type f | xargs chmod $(PERMF)
	cd $(PREFIX)/share/adpc ; find . -type d | xargs chmod $(PERMD)


INSTALLED = $(PREFIX)/bin/adpc-bin $(PREFIX)/bin/adpc $(PREFIX)/bin/man2c $(PREFIX)/bin/man2lhs $(PREFIX)/bin/colecho \
            $(PREFIX)/bin/adpcompile \
            $(PREFIX)/lib/adpc $(PREFIX)/share/adpc

update: all 
	sudo make install

uninstall:
	rm -rf $(INSTALLED)

clean:
	rm -f *.o *.hi InterfaceDTD.hs adpc-bin man2c man2lhs colecho
