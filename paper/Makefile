# Compact Protocol Paper Build

MAIN = main

all: $(MAIN).pdf

$(MAIN).pdf: $(MAIN).tex
	pdflatex $(MAIN).tex
	pdflatex $(MAIN).tex  # Run twice for references

clean:
	rm -f $(MAIN).aux $(MAIN).log $(MAIN).out $(MAIN).pdf $(MAIN).bbl $(MAIN).blg

arxiv: $(MAIN).pdf
	@echo "📦 Creating arXiv submission..."
	rm -rf arxiv-submission
	mkdir -p arxiv-submission
	cp $(MAIN).tex arxiv-submission/
	cp $(MAIN).pdf arxiv-submission/
	cd arxiv-submission && tar -czf ../compact-protocol-arxiv.tar.gz *
	@echo "✅ Created compact-protocol-arxiv.tar.gz"

.PHONY: all clean arxiv
