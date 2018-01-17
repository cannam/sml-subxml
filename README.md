
Simplistic Standard ML XML "parser"
===================================

https://bitbucket.org/cannam/sml-simplexml

This simplistic XML "parser", intended for handling configuration
files and small data files in simple formats, produces an in-memory
DOM tree containing element, text, and attribute data types only.

This is a very long way from being a conforming parser, and it should
only be used to support near-trivial legacy documents whose known
formats are not expected to change. The character encoding is assumed
to be UTF8 or an 8-bit ASCII-compatible encoding; wide character
encodings are not supported. Comments and processing instructions are
ignored. CDATA sections are supported, DOCTYPE declarations are
ignored, and other declarations are rejected. There is no DOM
navigation API. Testing is very minimal.

Note that although the parser is limited, it is not forgiving --
anything it can't understand is rejected with a clear error
message. But because of its super-limited design, it will both accept
many documents that are not well-formed XML, and reject many that are.

An equally simplistic serialiser is also provided.

For a proper XML parser in SML, consider fxp, mirrored at
https://github.com/cannam/fxp.

Copyright 2018 Chris Cannam.
MIT/X11 licence. See the file COPYING for details.
