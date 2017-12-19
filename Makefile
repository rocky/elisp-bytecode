.PHONY: info dvi html pdf ps all

SHELL = /bin/bash
srcdir = .
MAKEINFO = makeinfo
MAKEINFO_OPTS = --force --enable-encoding -I $(emacsdir) -I $(srcdir)
TEXI2DVI = texi2dvi
TEXI2PDF = texi2pdf
DVIPS = dvips
srcs = elisp-bytecode.texi

DVI_TARGETS = elisp-bytecode.dvi
HTML_TARGETS = elisp-bytecode.html
PDF_TARGETS = elisp-bytecode.pdf
PS_TARGETS = elisp-bytecode.ps

# Standard configure variables.
srcdir = .
buildinfodir = .

all: elisp-bytecode.pdf

info: $(buildinfodir)/elisp-bytecode.info
dvi: $(DVI_TARGETS)
html: $(HTML_TARGETS)
pdf: $(PDF_TARGETS)
ps: $(PS_TARGETS)

ENVADD = \
  MAKEINFO="$(MAKEINFO) $(MAKEINFO_OPTS)"


elisp-bytecode.pdf: $(srcs)
	$(ENVADD) $(TEXI2PDF) $<
