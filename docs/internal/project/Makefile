
html: project.html

pdf: project.pdf

open-html: html
	open project.html

project.html: project.scrbl
	scribble --dest-name project --html project.scrbl

project.pdf: project.scrbl
	scribble --dest-name project --pdf project.scrbl

clean:
	rm -f project.html project.pdf manual-fonts.css manual-racket.css manual-racket.js manual-style.css racket.css scribble-common.js scribble.css

.PHONY: html pdf open-html clean
