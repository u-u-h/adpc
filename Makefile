# ------------------------------------------------------------------------------
# -- The ADP Compiler 
# -- Copyright (C) 2001-2007 Peter Steffen, Marco Ruether, Christian Lang,
# -- Georg Sauthoff
# --
# -- Send comments/bug reports to: P.Steffen <psteffen@techfak.uni-bielefeld.de>.
# -- Updates: http://www.techfak.uni-bielefeld.de/~psteffen/
# ------------------------------------------------------------------------------

VERSION := `cat VERSION`

# default target
all: config/ConfigOutput.lhs config/config.mf adpc libtab.a
	$(MAKE) -C interfacer
	$(MAKE) -C java all jar

config/ConfigOutput.lhs:
	@echo Compile and run config in the config directory\! && false

config/config.mf:
	@echo Compile and run config in the config directory\! && false

include config/config.mf

install: all
	sh config/install $(PREFIX)

# Define search-path
PATH_C = -I. -Itab/general -Itab/grasp -Itab/optimal
PATH_LHS = -i.
# Flags and tools
LDFLAGS = # -lm
CFLAGS = -O3 -Wall
CPPFLAGS = $(PATH_C)
CC = gcc
LEX = flex
DOXY = doxygen
DFILE = Doxyfile


# Haskell-source-files of the adpc
LHS = TTCombinators.lhs Constants.lhs Tools.lhs Lex.lhs Poly.lhs SM.lhs \
      PrettyPrint.lhs MathExp.lhs TLData.lhs Expr.lhs \
      Syntax.lhs StringLib.lhs Parse.hs Track.lhs Yieldsize.lhs Adptrans.lhs \
      WidthAna.lhs Dss.lhs Range.lhs TL.lhs TLFrame.lhs Depana.lhs \
      Structs.lhs IL2.lhs LatexRecs.lhs Beautify.lhs Algebras.lhs \
      Codegen.lhs Codegen1.lhs Phases.lhs Alggen.lhs AlggenParse.hs \
      Compile.lhs Main.lhs \
      CGI.lhs CGITools.lhs Pretty.lhs \
      LatexRecs_cgi.lhs Optimize_cgi.lhs Compile_cgi.lhs Alggen_cgi.lhs \
      Convert.lhs Lexer.lhs  Parse2.lhs Lift.lhs ParseMonad.lhs \
      ParseTree.lhs NewParser.hs \
      Man.lhs ListCompr.lhs \
      hsutils/Layout.lhs \
      typechecker/TC.lhs typechecker/Annotate.lhs typechecker/TypecheckTree.lhs \
      typechecker/TypecheckMonad.lhs typechecker/Typecheck.lhs

HS = hsutils/HsLayoutPre.hs hsutils/HsLex.hs hsutils/HsTokens.hs \
     hsutils/HsLexerPass1.hs hsutils/HsLexUtils.hs hsutils/L.hs

HS_TEMP = $(LHS:.lhs=.o) $(LHS:.lhs=.hi) $(HS:.hs=.o) $(HS:.hs=.hi)

HAPPY = happy
HAPPYFLAGS = -a -g -c
HAPPYFLAGSGHC = -fglasgow-exts
%.hs : %.ly
	$(HAPPY) $(HAPPYFLAGS) $(@:.hs=.ly)


# C-Object-files for the ADP-compiler
OBJECTS_C = tabulate.o hook.o tab/optimal/taboptimal_in.o \
  tab/general/asm_runtime.o tab/optimal/functions.o      \
  tab/optimal/output.o tab/general/input.o tab/general/poly_runtime.o \
  tab/general/poly_type.o tab/general/dll.o tab/general/set.o \
  tab/general/graph.o tab/general/bag.o tab/general/index_list.o \
  tab/grasp/adpc_grasp.o tab/grasp/tab_grasp.o tab/general/array_int.o \
  tab/general/time_util.o tab/general/random.o tab/grasp/asm_factory.o \
  tab/general/dep_graph.o tab/general/comb.o \
  tab/general/log.o

DEPS = $(OBJECTS_C:.o=.d)

# Target-depending Settings
ifeq ($(MAKECMDGOALS),windows)
CFLAGS += -D WINDOWS
endif

ifeq ($(MAKECMDGOALS),sparc)
OLDGCCFLAGS = -pgmc gcc-2.95 -pgml gcc-2.95 -pgma gcc-2.95
endif

# GHCMEM =  +RTS -M512m -RTS 
GHCPROFILING = # -prof -auto-all

# Build the ADP-compiler
adpc: $(LHS) $(HS)
adpc: $(OBJECTS_C)
	ghc $(GHCMEM) $(OLDGCCFLAGS) $(HAPPYFLAGSGHC) $(GHCPROFILING) -XForeignFunctionInterface -ihsutils -itypechecker -iconfig -ijava --make Main.lhs -o adpcompile -optl $(filter %.o,$^)

sparc: adpc
windows: adpc

# Build ADP-CGI for ADP website
CGI_BIN_TARGET=/vol/bibidev/adp/cgi-bin

CGIGHC = umask 007;ghc

# the ADP-CGI binaries are always compiled under sparc solaris. Therefore we use gcc-2.95 here:
adp_Compile: $(LHS) $(OBJECTS_C)
	$(CGIGHC) -pgmc gcc-2.95 -pgml gcc-2.95 -pgma gcc-2.95 -fffi --make Main.lhs -o adp_Compile -optl $(filter %.o,$^)
	install --mode 775 adp_Compile $(CGI_BIN_TARGET)

# Do not remove Haskell o- and hi-files
clean:
	rm -f $(OBJECTS_C) $(DEPS) adpcompile adpcompile.exe \
	tab/general/adpc_deserialize.o main.o hand

distclean:
	rm -rf distr adpc-$(VERSION).tar.gz

# Remove Haskell o- and hi-files
PARSERHS = AlggenParse.hs NewParser.hs  Parse.hs 
clean_all: clean distclean
	rm -f $(HS_TEMP) $(PARSERHS) adpcompile adpcompile.exe docs doxygen.log
	make -C interfacer clean
	make -C java clean
	make -C config clean

interfacer/man2lhs : interfacer/Man2lhs.lhs
	cd interfacer && ghc --make Man2lhs.lhs -o man2lhs

Man.lhs: adpc.pod interfacer/man2lhs
	pod2text -q '<>' adpc.pod > adpc.txt
	./interfacer/man2lhs '1-' adpc.txt > Man.lhs

options.tex: adpc.pod
	pod2latex adpc.pod -out tmp.tex
	sed 's/subsection\*/subsection/'  tmp.tex  > options.tex 
	rm tmp.tex

adpc.pdf: options.tex adpc.tex
	echo '\\def\\rsversion{'${VERSION}'}' > version.tex
	pdflatex adpc.tex
	cp adpc.pdf text

# C-Programm um den table design Teil 'per Hand' unabhaengig
# vom adpc mit serialisierten Daten (.i) zu testen
hand : $(OBJECTS_C) tab/general/adpc_deserialize.o main.o
	$(CC) -o $@ $^ -lm -L/vol/gnu/lib -lfl $(LDFLAGS)

########################################
# Documentation
########################################
doc:
	$(DOXY) $(DFILE)

########################################
# Tracking include file dependencies
########################################
# sun cc doesn't know the -M. options  ...
ifneq ($(filter $(CC),gcc icc),)

ifeq ($(findstring $(MAKECMDGOALS),clean clean_all),)
# work around stupid GNU make include bug
# (if make can create some non-existing include files, it should do it
#  and shut up - GNU make does not do that - it complains, but creates
#  the needed files - the -includes directive has the problem, that if
#  make can not create the included file it gives no error and shuts up)
-include $(DEPS)
endif

%.d : %.c
	@$(CC) -MM -MT "$@ $(@:.d=.o)" $(CPPFLAGS) $(CFLAGS) $< > $@
	@echo Create makefile dependencies for $<

endif


libtab.a: $(OBJECTS_C)
	ar -r $@ $^

distrib:
	@echo Call sh config/distrib PREFIX \(or -h\) for this.

