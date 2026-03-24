.PHONY= update build optim

all: update build optim

js: update-js build-js

update:
	wasm32-wasi-cabal update

repl: update
	wasm32-wasi-cabal repl client -finteractive --repl-options='-fghci-browser -fghci-browser-port=8080'

watch:
	ghciwatch --after-startup-ghci :main --after-reload-ghci :main  --debounce 50ms --watch Client/Main.hs --watch Client/Styles.hs  --watch Client/Game.hs --command 'wasm32-wasi-cabal repl client -finteractive --repl-options="-fghci-browser -fghci-browser-port=8080"'

build:
	wasm32-wasi-cabal build client
	rm -rf public
	cp -r static public
	$(eval my_wasm=$(shell wasm32-wasi-cabal list-bin client | tail -n 1))
	$(shell wasm32-wasi-ghc --print-libdir)/post-link.mjs --input $(my_wasm) --output public/ghc_wasm_jsffi.js
	cp -v $(my_wasm) public/
	mv public/client.wasm public/app.wasm
	sed -i -e 's/\/app/static\/app/g' public/index.js
	cp Client/dictionary.js public/dictionary.js

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
