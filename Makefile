export IDRIS2 ?= idris2

.PHONY: page
page:
	pack build async-dom-docs
	cp docs/build/exec/async-dom-docs.js page/async-dom-docs.js
