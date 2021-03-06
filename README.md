
SubXml - A parser for a subset of XML, written in Standard ML
=============================================================

https://hg.sr.ht/~cannam/sml-subxml

SubXml is a parser and serialiser for a format resembling XML. It can
be used as a minimal parser for small XML configuration or interchange
files, so long as they make suitably limited use of XML and so long as
that is not expected to change.

The format supported by SubXml consists of the element, attribute,
text, CDATA, and comment syntax from XML. It differs from XML in the
following ways:

 * The only supported character encoding is UTF-8

 * Processing instructions (<? ... ?>) are ignored

 * DOCTYPE declarations are ignored; all other declarations (<! ... >)
   are rejected except for CDATA, which is handled properly

 * It follows from the above that only built-in named entities and
   character entities are decoded (but those ones are)

 * Comments appearing before or after the root element are ignored;
   other comments are included in the parsed tree

Note that although the parser is limited, it is not forgiving --
anything it can't understand is rejected with a clear error
message. But because of its super-limited design, it will both accept
many documents that are not well-formed XML, and reject many that are.

An equally simplistic serialiser is also provided.

For a proper XML parser in SML, consider fxp, mirrored at
https://github.com/cannam/fxp.

Copyright 2018-2021 Chris Cannam.
MIT/X11 licence. See the file COPYING for details.

