
html: glow-surface.html glow-sexpr.html

pdf: glow-surface.pdf glow-sexpr.pdf

open-html: html
	open glow-surface.html glow-sexpr.html

glow-surface.html: glow-surface.scrbl
	scribble --dest-name glow-surface --html glow-surface.scrbl

glow-surface.pdf: glow-surface.scrbl
	scribble --dest-name glow-surface --pdf glow-surface.scrbl

glow-sexpr.html: glow-sexpr.scrbl
	scribble --dest-name glow-sexpr --html glow-sexpr.scrbl

glow-sexpr.pdf: glow-sexpr.scrbl
	scribble --dest-name glow-sexpr --pdf glow-sexpr.scrbl

clean:
	rm -f glow-surface.html glow-surface.pdf glow-sexpr.html glow-sexpr.pdf manual-fonts.css manual-racket.css manual-racket.js manual-style.css racket.css scribble-common.js scribble.css

.PHONY: html pdf open-html clean
