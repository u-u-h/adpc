GHC = ghc

TEMP = config.mf ConfigOutput.lhs config
HS = Config.lhs ConfigOutput.lhs
TEMP_HS = $(HS:.lhs=.o) $(HS:.lhs=.hi)

all: config

config: Config.lhs
	$(GHC) --make $< -o $@

clean:
	rm -f $(TEMP) $(TEMP_HS)
