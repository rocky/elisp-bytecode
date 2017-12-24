.PHONY: info dvi html pdf ps all mostlyclean

SHELL = /bin/bash
srcdir = .
MAKEINFO = makeinfo
MAKEINFO_OPTS = --force --enable-encoding -I $(emacsdir) -I $(srcdir)
TEXI2DVI = texi2dvi
TEXI2PDF = texi2pdf
PDFTEX = pdftex
DVIPS = dvips
srcs = elisp-bytecode.texi

DVI_TARGETS = elisp-bytecode.dvi
HTML_TARGETS = elisp-bytecode.html
PDF_TARGETS = elisp-bytecode.pdf
PS_TARGETS = elisp-bytecode.ps
INDEX_TARGETS = elisp-bytecode.vrs elisp-bytecode.kys

# Standard configure variables.
srcdir = .
buildinfodir = .

all: $(INDEX_TARGETS) elisp-bytecode.info elisp-bytecode.pdf

$(INDEX_TARGETS): $(srcs)

info: $(buildinfodir)/elisp-bytecode.info
dvi: $(DVI_TARGETS)
html: $(HTML_TARGETS)
pdf: $(PDF_TARGETS)
ps: $(PS_TARGETS)

ENVADD = \
  MAKEINFO="$(MAKEINFO) $(MAKEINFO_OPTS)"

mostlyclean:
	rm -f *.aux *.log *.toc *.cp *.cps *.fn *.fns *.ky *.kys \
	  *.op *.ops *.pg *.pgs *.tp *.tps *.vr *.vrs *.info

clean: mostlyclean
	rm -f $(DVI_TARGETS) $(HTML_TARGETS) $(PDF_TARGETS) $(PS_TARGETS)


elisp-bytecode.pdf: $(srcs)
	$(ENVADD) $(TEXI2PDF) $<

clean:
