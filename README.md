# async-dom: Concurrency and streaming in interactive Idris web pages

This is a framework for writing interactive web pages in Idris2 making
use of the functional streams from the
[streams](https://github.com/stefan-hoeck/idris2-streams)
library.

This is what
[rhone-js](https://github.com/stefan-hoeck/idris2-rhone-js)
tried to be and
[dom-mvc](https://github.com/stefan-hoeck/idris2-dom-mvc) didn't
even got close to.

Note: This library is currently incompatible with *dom-mvc*, because modules
of the same name are used in both libraries. I'll fix this and extract
the common modules once I get around to adjust *dom-mvc* to the
necessary changes.
