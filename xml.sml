
(* Simplistic Standard ML XML "parser"
   ===================================

   https://bitbucket.org/cannam/sml-simplexml

   This simplistic XML "parser", intended for handling configuration
   files and small data files in simple formats, produces an in-memory
   DOM tree containing element, text, and attribute data types only.
   
   This is a very long way from being a conforming parser, and it should
   only be used to support near-trivial legacy documents whose known
   formats are not expected to change. The character encoding is assumed
   to be UTF8 or an 8-bit encoding; wide character encodings are not
   supported. Comments and processing instructions are removed.
   Namespaces are not expanded. Other sections such as CDATA and entity
   definitions are passed through verbatim. Nothing is unescaped. There
   is no DOM navigation API. Testing is very minimal.
   
   Note that although the parser is limited, it is not forgiving --
   anything it can't understand is rejected with a clear error
   message. But because of its super-limited design, it will both accept
   many documents that are not well-formed XML, and reject many that are.
   
   An equally simplistic serialiser is also provided.
   
   Copyright 2018 Chris Cannam.

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation
   files (the "Software"), to deal in the Software without
   restriction, including without limitation the rights to use, copy,
   modify, merge, publish, distribute, sublicense, and/or sell copies
   of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR
   ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
   CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
   WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

   Except as contained in this notice, the names of Chris Cannam and
   Particular Programs Ltd shall not be used in advertising or
   otherwise to promote the sale, use or other dealings in this
   Software without prior written authorization.
*)

signature XML = sig

    datatype node = ELEMENT of { name : string, children : node list }
                  | TEXT of string
                  | CDATA of string
                  | ATTRIBUTE of string * string

    datatype document = DOCUMENT of { name : string, children : node list }

    datatype 'a result = OK of 'a
                       | ERROR of string

    val parse : string -> document result
    val serialise : document -> string
                                   
end

structure Xml = struct

    datatype node = ELEMENT of { name : string, children : node list }
                  | TEXT of string
                  | CDATA of string
                  | ATTRIBUTE of string * string

    datatype document = DOCUMENT of { name : string, children : node list }

    datatype 'a result = OK of 'a
                       | ERROR of string

    structure T = struct
        datatype token = ANGLE_L
                       | ANGLE_R
                       | ANGLE_SLASH_L
                       | SLASH_ANGLE_R
                       | EQUAL
                       | NAME of string
                       | TEXT of string
                       | CDATA of string

        fun toString t =
            case t of ANGLE_L => "<"
                    | ANGLE_R => ">"
                    | ANGLE_SLASH_L => "</"
                    | SLASH_ANGLE_R => "/>"
                    | EQUAL => "="
                    | NAME s => s
                    | TEXT s => "\"" ^ s ^ "\""
                    | CDATA s => "CDATA section"
    end

    fun error pos text = ERROR (text ^ " at character position " ^
                                Int.toString (pos - 1))
    fun tokenError pos token =
        error pos ("Unexpected token '" ^ Char.toString token ^ "'")

    val nameEnd = explode " \t\n\r\"'</>!=?"
            
    fun lexQuoted quote pos acc cc =
        let fun lexQuoted' pos text [] =
                error pos "Document ends during quoted string"
              | lexQuoted' pos text (x::xs) =
                if x = quote
                then OK (rev text, xs, pos + 1)
                else lexQuoted' (pos + 1) (x::text) xs
        in
            case lexQuoted' pos [] cc of
                OK (text, rest, newpos) =>
                lexIn newpos (T.TEXT (implode text) :: acc) rest
              | ERROR e => ERROR e
        end

    and lexName first pos acc cc =
        let fun lexName' pos text [] =
                error pos "Document ends during name"
              | lexName' pos text (x::xs) =
                if List.find (fn c => c = x) nameEnd <> NONE
                then OK (rev text, (x::xs), pos)
                else lexName' (pos + 1) (x::text) xs
        in
            case lexName' (pos-1) [] (first::cc) of
                OK ([], [], newpos) => error newpos "Document ends before name"
              | OK ([], (x::xs), newpos) => tokenError newpos x
              | OK (text, rest, newpos) =>
                lexIn newpos (T.NAME (implode text) :: acc) rest
              | ERROR e => ERROR e
        end

    and lexComment pos acc (#"-" :: #"-" :: #">" :: xs) = lexOut (pos+3) acc xs
      | lexComment pos acc (#">" :: _) = tokenError pos #">"
      | lexComment pos acc (x :: xs) = lexComment (pos+1) acc xs
      | lexComment pos acc [] = error pos "Document ends during comment"

    and lexPI pos acc (#"?" :: #">" :: xs) = lexOut (pos+2) acc xs
      | lexPI pos acc (#">" :: _) = tokenError pos #">"
      | lexPI pos acc (x :: xs) = lexPI (pos+1) acc xs
      | lexPI pos acc [] =
        error pos "Document ends during processing instruction"

    and lexCData pos acc cc =
        let fun lexCData' pos text [] =
                error pos "Document ends during CDATA section"
              | lexCData' pos text (#"]" :: #"]" :: #">" :: xs) =
                OK (rev text, xs, pos)
              | lexCData' pos text (x::xs) =
                lexCData' (pos+1) (x::text) xs
        in
            case lexCData' pos [] cc of
                OK (text, rest, newpos) =>
                lexOut newpos (T.CDATA (implode text) :: acc) rest
              | ERROR e => ERROR e
        end
              
    and lexDoctype pos acc (#">" :: xs) = lexOut (pos+1) acc xs
      | lexDoctype pos acc (x :: xs) = lexDoctype (pos+1) acc xs
      | lexDoctype pos acc [] = error pos "Document ends during DOCTYPE"

    and lexDeclaration pos acc (#"-" :: #"-" :: xs) = lexComment (pos+2) acc xs
      | lexDeclaration pos acc (#"[" :: #"C" :: #"D" :: #"A" :: #"T" :: #"A" ::
                                #"[" :: xs) = lexCData (pos+7) acc xs
      | lexDeclaration pos acc (#"D" :: #"O" :: #"C" :: #"T" :: #"Y" :: #"P" ::
                                #"E" :: xs) = lexDoctype (pos+7) acc xs
      | lexDeclaration pos acc xs = error pos "Unsupported declaration type"
            
    and lexOpen pos acc (#"/" :: xs) = lexIn (pos+1) (T.ANGLE_SLASH_L :: acc) xs
      | lexOpen pos acc (#"!" :: xs) = lexDeclaration (pos+1) acc xs
      | lexOpen pos acc (#"?" :: xs) = lexPI (pos+1) acc xs
      | lexOpen pos acc xs = lexIn pos (T.ANGLE_L :: acc) xs

    and lexSlash pos acc (#">"::xs) = lexOut (pos+1) (T.SLASH_ANGLE_R :: acc) xs
      | lexSlash pos _ (x::_) = tokenError pos x
      | lexSlash pos _ [] = error pos "Document ends before element closed"

    and lexClose pos acc xs = lexOut pos (T.ANGLE_R :: acc) xs

    and lexEqual pos acc xs = lexIn pos (T.EQUAL :: acc) xs

    and lexOut pos acc [] = OK acc
      | lexOut pos acc cc =
        let fun textOf text = T.TEXT (implode (rev text))
            fun lexOut' pos [] acc [] = OK acc
              | lexOut' pos text acc [] = OK (textOf text :: acc)
              | lexOut' pos [] acc (#"<"::xs) =
                lexOpen (pos+1) acc xs
              | lexOut' pos text acc (#"<"::xs) =
                lexOpen (pos+1) (textOf text :: acc) xs
              | lexOut' pos text acc (x::xs) =
                lexOut' (pos+1) (x::text) acc xs
        in
            lexOut' pos [] acc cc
        end
            
    and lexIn pos acc [] = error pos "Document ends inside element"
      | lexIn pos acc (#"<"::_) = tokenError pos #"<"
      | lexIn pos acc (x::xs) =
        (case x of
             #" " => lexIn | #"\t" => lexIn | #"\n" => lexIn | #"\r" => lexIn
           | #"\"" => lexQuoted x | #"'" => lexQuoted x
           | #"/" => lexSlash | #">" => lexClose | #"=" => lexEqual
           | x => lexName x) (pos+1) acc xs

    and lex str =
        case lexOut 1 [] (explode str) of
            ERROR e => ERROR e
          | OK tokens => OK (rev tokens)

    fun show [] = "end of input"
      | show (tok :: _) = T.toString tok

    fun parseError toks text = ERROR (text ^ " before " ^ show toks)

    fun parseAttribute elt name (T.EQUAL :: T.TEXT value :: xs) =
        parseNamedElement {
            name = #name elt,
            children = ATTRIBUTE (name, value) :: #children elt
        } xs
      | parseAttribute elt name toks =
        parseError toks "Expected attribute value"

    and parseContent elt (T.ANGLE_SLASH_L :: T.NAME n :: T.ANGLE_R :: xs) =
        if n = #name elt
        then OK (elt, xs)
        else parseError xs ("Element closing tag </" ^ n ^
                            "> does not match opening <" ^ #name elt ^ ">")
      | parseContent elt (T.TEXT text :: xs) =
        parseContent {
            name = #name elt,
            children = TEXT text :: #children elt
        } xs
      | parseContent elt (T.CDATA text :: xs) =
        parseContent {
            name = #name elt,
            children = CDATA text :: #children elt
        } xs
      | parseContent elt (T.ANGLE_L :: xs) =
        (case parseElement xs of
             ERROR e => ERROR e
           | OK (child, xs) => parseContent {
                                  name = #name elt,
                                  children = ELEMENT child :: #children elt
                              } xs)
      | parseContent elt (x :: xs) =
        parseError xs ("Unexpected token " ^ T.toString x)
      | parseContent elt [] =
        parseError [] ("Document ends within element \"" ^ #name elt ^ "\"")
                                     
    and parseNamedElement elt (T.SLASH_ANGLE_R :: xs) = OK (elt, xs)
      | parseNamedElement elt (T.NAME name :: xs) = parseAttribute elt name xs
      | parseNamedElement elt (T.ANGLE_R :: xs) = parseContent elt xs
      | parseNamedElement elt (x :: xs) =
        parseError xs ("Unexpected token " ^ T.toString x)
      | parseNamedElement elt [] =
        parseError [] "Document ends within tag"
                          
    and parseElement [] = ERROR "Empty element"
      | parseElement (T.NAME name :: xs) =
        (case parseNamedElement { name = name, children = [] } xs of
            ERROR e => ERROR e 
          | OK ({ name, children }, xs) =>
            OK ({ name = name, children = rev children }, xs))
      | parseElement toks = parseError toks "Expected element name"

    and parseDocument [] = ERROR "Empty document"
      | parseDocument (tok :: xs) =
        case tok of
            T.TEXT _ => parseDocument xs
          | T.ANGLE_L =>
            (case parseElement xs of
                 ERROR e => ERROR e
               | OK (elt, []) => OK (DOCUMENT elt)
               | OK (elt, (T.TEXT _ :: xs)) => OK (DOCUMENT elt)
               | OK (elt, xs) => parseError xs "Extra data after document")
          | _ => parseError xs ("Unexpected token " ^ T.toString tok)

    fun parse str =
        case lex str of
            ERROR e => ERROR e
          | OK tokens => parseDocument tokens

    fun serialiseAttributes nodes =
        case (String.concatWith
                  " "
                  (map serialiseNode (List.filter
                                          (fn ATTRIBUTE _ => true | _ => false)
                                          nodes))) of
            "" => ""
          | str => " " ^ str

    and serialiseNonAttributes nodes =
        String.concat
            (map serialiseNode (List.filter
                                    (fn ATTRIBUTE _ => false | _ => true)
                                    nodes))
                                       
    and serialiseNode (ATTRIBUTE (name, value)) =
        name ^ "=" ^ "\"" ^ value ^ "\"" (*!!!*)
      | serialiseNode (TEXT string) =
        string
      | serialiseNode (CDATA string) =
        "<![CDATA[" ^ string ^ "]]>"
      | serialiseNode (ELEMENT { name, children = [] }) =
        "<" ^ name ^ "/>"
      | serialiseNode (ELEMENT { name, children }) =
        "<" ^ name ^
        serialiseAttributes children ^ ">" ^
        serialiseNonAttributes children ^
        "</" ^ name ^ ">"
                                       
    fun serialise (DOCUMENT { name, children }) =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ^
        serialiseNode (ELEMENT { name = name, children = children })
                                 
end

