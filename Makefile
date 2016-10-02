.PHONY: conv

conv:
	rm -f gen/*
	ghc --make Main.hs -o conv -XFlexibleContexts
	find * -name \*.mml -print -exec ./conv {} \;
	#find * -name \*.html -print -exec winpty inline.bat {} \;

rel: conv
	rm -rf spectral
	mkdir spectral
	find * -type d \
		-not -iname spectral \
		-and -not -path "spectral/*" \
		-and -not -path "dist/*" \
		-and -not -path "MML/*" \
		-print -exec mkdir -p spectral/{} \;
	find * -type f \
		-iname \*.html \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	find * -type f \
		-iname \*.css \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	find * -type f \
		-iname \*.js \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	find * -type f \
		-iname \*.png \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	find * -type f \
		-iname \*.jpg \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	find * -type f \
		-iname \*.svg \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	find * -type f \
		-iname \*.zip \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	du -hs spectral

rel-light: conv
	rm -rf spectral
	mkdir spectral
	find * -type d \
		-not -iname spectral \
		-and -not -path "spectral/*" \
		-and -not -path "dist/*" \
		-and -not -path "MML/*" \
		-print -exec mkdir -p spectral/{} \;
	find * -type f \
		-iname \*.html \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	find * -type f \
		-iname \*.css \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	find * -type f \
		-iname \*.js \
		-and -not -path "spectral/*" \
		-print -exec cp {} spectral/{} \;
	du -hs spectral

