.PHONY: info dvi html pdf ps all

SHELL = /bin/bash
srcdir = .
MAKEINFO = makeinfo
MAKEINFO_OPTS = --force --enable-encoding -I $(emacsdir) -I $(srcdir)
TEXI2DVI = texi2dvi
TEXI2PDF = texi2pdf
DVIPS = dvips
srcs = lap.texi

DVI_TARGETS = lap.dvi
HTML_TARGETS = lap.html
PDF_TARGETS = lap.pdf
PS_TARGETS = lap.ps

# Standard configure variables.
srcdir = .
buildinfodir = .

all: lap.pdf

info: $(buildinfodir)/lap.info
dvi: $(DVI_TARGETS)
html: $(HTML_TARGETS)
pdf: $(PDF_TARGETS)
ps: $(PS_TARGETS)

ENVADD = \
  MAKEINFO="$(MAKEINFO) $(MAKEINFO_OPTS)"


lap.pdf: $(srcs)
	$(ENVADD) $(TEXI2PDF) $<
