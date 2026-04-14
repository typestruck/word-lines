.PHONY= update build optim

all: update build optim

js: update-js build-js

update:
	cabal --with-compiler=wasm32-wasi-ghc-9.12 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.12 --with-hsc2hs=wasm32-wasi-hsc2hs-9.12 --with-haddock=wasm32-wasi-haddock-9.12 update

repl: update
	cabal --with-compiler=wasm32-wasi-ghc-9.12 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.12 --with-hsc2hs=wasm32-wasi-hsc2hs-9.12 --with-haddock=wasm32-wasi-haddock-9.12 repl client -finteractive --repl-options='-fghci-browser -fghci-browser-port=8080'

watch:
	ghciwatch --after-startup-ghci :main --after-reload-ghci :main  --debounce 50ms --watch public/static/styles.css --watch Client/Game.hs --watch Shared/Game/View.hs --command 'cabal --with-compiler=wasm32-wasi-ghc-9.12 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.12 --with-hsc2hs=wasm32-wasi-hsc2hs-9.12 --with-haddock=wasm32-wasi-haddock-9.12 repl client -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"'

build:
	cabal --with-compiler=wasm32-wasi-ghc-9.12 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.12 --with-hsc2hs=wasm32-wasi-hsc2hs-9.12 --with-haddock=wasm32-wasi-haddock-9.12 build client
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell cabal --with-compiler=wasm32-wasi-ghc-9.12 --with-hc-pkg=wasm32-wasi-ghc-pkg-9.12 --with-hsc2hs=wasm32-wasi-hsc2hs-9.12 --with-haddock=wasm32-wasi-haddock-9.12 list-bin client | tail -n 1))
	$(shell wasm32-wasi-ghc-9.12 --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/
	mv public/client.wasm public/app.wasm
	cp Client/dictionary public/dictionary.js
	mkdir -p public/static
	mv public/*.* public/static

optim:
	wasm-opt -all -O2 public/app.wasm -o public/app.wasm
	wasm-tools strip -o public/app.wasm public/app.wasm

clean:
	rm -rf dist-newstyle public

update-js:
	cabal update --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg

build-js:
	cabal build --with-ghc=javascript-unknown-ghcjs-ghc --with-hc-pkg=javascript-unknown-ghcjs-ghc-pkg
	cp -v ./dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/*/x/app/build/app/app.jsexe/all.js .
	rm -rf public
	cp -rv static public
	bunx --bun swc ./all.js -o public/index.js
