
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

signature SUBXML = sig

    datatype node = ELEMENT of { name : string, children : node list }
                  | ATTRIBUTE of string * string
                  | TEXT of string
                  | CDATA of string

    datatype document = DOCUMENT of { name : string, children : node list }

    datatype 'a result = OK of 'a
                       | ERROR of string

    val parse : string -> document result
    val serialise : document -> string
                                   
end

structure SubXml :> SUBXML = struct

    datatype node = ELEMENT of { name : string, children : node list }
                  | ATTRIBUTE of string * string
                  | TEXT of string
                  | CDATA of string

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

    structure Lex :> sig
                  val lex : string -> T.token list result
              end = struct
                      
        fun error pos text = ERROR (text ^ " at character position " ^
                                    Int.toString (pos - 1))
        fun tokenError pos token =
            error pos ("Unexpected token '" ^ Char.toString token ^ "'")

        val nameEnd = explode " \t\n\r\"'</>!=?"
                              
        fun quoted quote pos acc cc =
            let fun quoted' pos text [] =
                    error pos "Document ends during quoted string"
                  | quoted' pos text (x::xs) =
                    if x = quote
                    then OK (rev text, xs, pos + 1)
                    else quoted' (pos + 1) (x::text) xs
            in
                case quoted' pos [] cc of
                    OK (text, rest, newpos) =>
                    inside newpos (T.TEXT (implode text) :: acc) rest
                  | ERROR e => ERROR e
            end

        and name first pos acc cc =
            let fun name' pos text [] =
                    error pos "Document ends during name"
                  | name' pos text (x::xs) =
                    if List.find (fn c => c = x) nameEnd <> NONE
                    then OK (rev text, (x::xs), pos)
                    else name' (pos + 1) (x::text) xs
            in
                case name' (pos-1) [] (first::cc) of
                    OK ([], [], pos) => error pos "Document ends before name"
                  | OK ([], (x::xs), pos) => tokenError pos x
                  | OK (text, rest, pos) =>
                    inside pos (T.NAME (implode text) :: acc) rest
                  | ERROR e => ERROR e
            end

        and comment pos acc cc =
            case cc of
                #"-" :: #"-" :: #">" :: xs => outside (pos+3) acc xs
              | #">" :: _ => tokenError pos #">"
              | x :: xs => comment (pos+1) acc xs
              | [] => error pos "Document ends during comment"

        and instruction pos acc cc =
            case cc of
                #"?" :: #">" :: xs => outside (pos+2) acc xs
              | #">" :: _ => tokenError pos #">"
              | x :: xs => instruction (pos+1) acc xs
              | [] => error pos "Document ends during processing instruction"

        and cdata pos acc cc =
            let fun cdata' pos text cc =
                    case cc of
                        #"]" :: #"]" :: #">" :: xs => OK (rev text, xs, pos)
                      | x :: xs => cdata' (pos+1) (x::text) xs
                      | [] => error pos "Document ends during CDATA section"
            in
                case cdata' pos [] cc of
                    OK (text, rest, pos) =>
                    outside pos (T.CDATA (implode text) :: acc) rest
                  | ERROR e => ERROR e
            end
                
        and doctype pos acc cc =
            case cc of
                #">" :: xs => outside (pos+1) acc xs
              | x :: xs => doctype (pos+1) acc xs
              | [] => error pos "Document ends during DOCTYPE"

        and declaration pos acc cc =
            case cc of
                #"-" :: #"-" :: xs => comment (pos+2) acc xs
              | #"[" :: #"C" :: #"D" :: #"A" :: #"T" :: #"A" ::
                #"[" :: xs => cdata (pos+7) acc xs
              | #"D" :: #"O" :: #"C" :: #"T" :: #"Y" :: #"P" ::
                #"E" :: xs => doctype (pos+7) acc xs
              | [] => error pos "Document ends during declaration"
              | _ => error pos "Unsupported declaration type"

        and left pos acc cc =
            case cc of
                #"/" :: xs => inside (pos+1) (T.ANGLE_SLASH_L :: acc) xs
              | #"!" :: xs => declaration (pos+1) acc xs
              | #"?" :: xs => instruction (pos+1) acc xs
              | xs => inside pos (T.ANGLE_L :: acc) xs

        and slash pos acc cc =
            case cc of
                #">" :: xs => outside (pos+1) (T.SLASH_ANGLE_R :: acc) xs
              | x :: _ => tokenError pos x
              | [] => error pos "Document ends before element closed"

        and close pos acc xs = outside pos (T.ANGLE_R :: acc) xs

        and equal pos acc xs = inside pos (T.EQUAL :: acc) xs

        and outside pos acc [] = OK acc
          | outside pos acc cc =
            let fun textOf text = T.TEXT (implode (rev text))
                fun outside' pos [] acc [] = OK acc
                  | outside' pos text acc [] = OK (textOf text :: acc)
                  | outside' pos text acc (x::xs) =
                    case x of
                        #"<" => if text = []
                                then left (pos+1) acc xs
                                else left (pos+1) (textOf text :: acc) xs
                      | x => outside' (pos+1) (x::text) acc xs
            in
                outside' pos [] acc cc
            end
                
        and inside pos acc [] = error pos "Document ends within tag"
          | inside pos acc (#"<"::_) = tokenError pos #"<"
          | inside pos acc (x::xs) =
            (case x of
                 #" " => inside | #"\t" => inside
               | #"\n" => inside | #"\r" => inside
               | #"\"" => quoted x | #"'" => quoted x
               | #"/" => slash | #">" => close | #"=" => equal
               | x => name x) (pos+1) acc xs

        fun lex str =
            case outside 1 [] (explode str) of
                ERROR e => ERROR e
              | OK tokens => OK (rev tokens)
    end

    structure Parse :> sig
                  val parse : string -> document result
              end = struct                            
                  
        fun show [] = "end of input"
          | show (tok :: _) = T.toString tok

        fun error toks text = ERROR (text ^ ", found before " ^ show toks)

        fun attribute elt name toks =
            case toks of
                T.EQUAL :: T.TEXT value :: xs =>
                namedElement {
                    name = #name elt,
                    children = ATTRIBUTE (name, value) :: #children elt
                } xs
              | toks => error toks "Expected attribute value"

        and content elt toks =
            case toks of
                T.ANGLE_SLASH_L :: T.NAME n :: T.ANGLE_R :: xs =>
                if n = #name elt
                then OK (elt, xs)
                else error xs ("Element closing tag </" ^ n ^ "> " ^
                               "does not match opening <" ^ #name elt ^ ">")
              | T.TEXT text :: xs =>
                content {
                    name = #name elt,
                    children = TEXT text :: #children elt
                } xs
              | T.CDATA text :: xs =>
                content {
                    name = #name elt,
                    children = CDATA text :: #children elt
                } xs
              | T.ANGLE_L :: xs =>
                (case element xs of
                     ERROR e => ERROR e
                   | OK (child, xs) =>
                     content {
                         name = #name elt,
                         children = ELEMENT child :: #children elt
                     } xs)
              | x :: xs =>
                error xs ("Unexpected token " ^ T.toString x)
              | [] =>
                error [] ("Document ends within element \"" ^ #name elt ^ "\"")
                       
        and namedElement elt toks =
            case toks of
                T.SLASH_ANGLE_R :: xs => OK (elt, xs)
              | T.NAME name :: xs => attribute elt name xs
              | T.ANGLE_R :: xs => content elt xs
              | x :: xs => error xs ("Unexpected token " ^ T.toString x)
              | [] => error [] "Document ends within tag"
                       
        and element toks =
            case toks of
                [] => ERROR "Empty element"
              | T.NAME name :: xs =>
                (case namedElement { name = name, children = [] } xs of
                     ERROR e => ERROR e 
                   | OK ({ name, children }, xs) =>
                     OK ({ name = name, children = rev children }, xs))
              | toks => error toks "Expected element name"

        and document [] = ERROR "Empty document"
          | document (tok :: xs) =
            case tok of
                T.TEXT _ => document xs
              | T.ANGLE_L =>
                (case element xs of
                     ERROR e => ERROR e
                   | OK (elt, []) => OK (DOCUMENT elt)
                   | OK (elt, (T.TEXT _ :: xs)) => OK (DOCUMENT elt)
                   | OK (elt, xs) => error xs "Extra data after document")
              | _ => error xs ("Unexpected token " ^ T.toString tok)

        fun parse str =
            case Lex.lex str of
                ERROR e => ERROR e
              | OK tokens => document tokens
    end

    structure Serialise :> sig
                  val serialise : document -> string
              end = struct
                    
        fun attributes nodes =
            case (String.concatWith
                      " "
                      (map node (List.filter
                                     (fn ATTRIBUTE _ => true | _ => false)
                                     nodes))) of
                "" => ""
              | str => " " ^ str

        and nonAttributes nodes =
            String.concat
                (map node (List.filter
                               (fn ATTRIBUTE _ => false | _ => true)
                               nodes))
                
        and node n =
            case n of
                ATTRIBUTE (name, value) =>
                name ^ "=" ^ "\"" ^ value ^ "\"" (*!!!*)
              | TEXT string => string
              | CDATA string => "<![CDATA[" ^ string ^ "]]>"
              | ELEMENT { name, children = [] } => "<" ^ name ^ "/>"
              | ELEMENT { name, children } => "<" ^ name ^
                                              attributes children ^ ">" ^
                                              nonAttributes children ^
                                              "</" ^ name ^ ">"
                              
        fun serialise (DOCUMENT { name, children }) =
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ^
            node (ELEMENT { name = name, children = children })
    end

    val parse = Parse.parse
    val serialise = Serialise.serialise
                        
end

