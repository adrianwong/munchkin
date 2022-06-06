.PHONY: default clean
.SILENT: default clean

MMC=mmc

BINDIR=bin
OUTDIR=out
SRCDIR=src

OUTFILE=munchkin

default:
	mkdir -p $(OUTDIR)
	cd $(OUTDIR) && $(MMC)				\
		-f `find ../$(SRCDIR) -name "*.m"`
	cd $(OUTDIR) && $(MMC)				\
		--make $(OUTFILE)			\
		--intermodule-optimisation		\
		--optimisation-level 5			\
		--optimise-constructor-last-call	\
		--use-grade-subdirs
	mkdir -p $(BINDIR)
	cp -p $(OUTDIR)/$(OUTFILE) $(BINDIR)

clean:
	rm -rf $(BINDIR)
	rm -rf $(OUTDIR)
