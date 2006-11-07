# Makefile for CAV paper
LATEX = latex

all: main.dvi main.ps pdf

TEXSRCS = main.tex related.tex abstract.tex introduction.tex caseExample.tex \
		  algorithm.tex evaluation.tex conclusion.tex sw.bib
FIGS = surge.c surge.ps

# Run once, then re-run until it's happy
# Input redirected from /dev/null is like hitting ^C at first error
main.dvi: $(TEXSRCS) $(FIGS)
	@echo $(LATEX) main.tex </dev/null
	@if $(LATEX) main.tex </dev/null; then \
		true; \
	else \
		stat=$$?; touch main.dvi; exit $$stat; \
	fi
	bibtex main
	@echo $(LATEX) main.tex </dev/null
	$(LATEX) main.tex </dev/null
	@while grep "Rerun to get cross" main.log; do \
		echo $(LATEX) main.tex </dev/null; \
		if $(LATEX) main.tex </dev/null; then \
			true; \
		else \
			stat=$$?; touch main.dvi; exit $$stat; \
		fi; \
	done

# generate eps from xfig's "Combined PS/LaTeX (both parts)" export output
%.eps: %.pstex_t
	./pstex-to-eps $*.pstex_t

.PHONY: ps print
print: main.ps
	gv main.ps

main.ps: main.dvi
	dvips -tletter -o main.ps main.dvi

pdf:	main.ps
	ps2pdf main.ps
clean:
	rm -f *.aux *.log main.ps *.dvi *.blg *.toc main.pdf
