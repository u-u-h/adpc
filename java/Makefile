LHS = FrontEnd.lhs Helpers.lhs Macro.lhs Option.lhs
LHS_O = $(LHS:.lhs=.o)
LHS_HI = $(LHS:.lhs=.hi)


all:
	ghc --make FrontEnd.lhs -i../typechecker -i../hsutils  -i../config -i../ -o fe -XForeignFunctionInterface -optl ../libtab.a $(HFLAGS)


dist:
	mkdir $@

jar: dist
	javac -d dist ADPC/*.java optparse/*.java Tree/*.java
	cd dist && jar cf ../adpc.jar *
	touch jar

clean:
	rm -rf dist
	rm -f jar $(LHS_O) $(LHS_HI)
