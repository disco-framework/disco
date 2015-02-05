
DOC_NAME  = disco-users-guide

PREVIEWER = evince2

# only create pdf documents (neither div nor ps)
LATEXMK_FLAGS = -pdf -dvi- -ps-

# filter pdflatex output with ppdflatex
LATEXMK_FLAGS += -pdflatex="ppdflatex -- %O %S"

LATEXMK_AUTOFLAGS = -pvc -e '$$pdf_previewer = "$(PREVIEWER) %O %S";'


.PHONY: all autobuild clean cleanall

all:
	latexmk $(LATEXMK_FLAGS) $(DOC_NAME)

autobuild:
	latexmk $(LATEXMK_FLAGS) $(LATEXMK_AUTOFLAGS) $(DOC_NAME)

clean:
	latexmk -c

cleanall:
	latexmk -C
