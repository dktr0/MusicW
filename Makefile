build:
	cabal --ghcjs --builddir=dev-result new-build all --disable-library-profiling --disable-documentation
