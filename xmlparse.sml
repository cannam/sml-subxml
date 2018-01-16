
(* Take an input file and pass it through the XML parser and
   serialiser, writing the result to stdout. Used for integrity
   checks. *)

fun contents filename =
    let val stream = TextIO.openIn filename
        fun read_all str acc =
            case TextIO.inputLine str of
                SOME line => read_all str (line :: acc)
              | NONE => rev acc
        val contents = read_all stream []
        val _ = TextIO.closeIn stream
    in
        String.concat contents
    end

fun processFile filename =
    let val input = contents filename
    in
        case Xml.parse input of
            Xml.ERROR e => TextIO.output (TextIO.stdErr, "Error: " ^ e ^ "\n")
          | Xml.OK xml => print (Xml.serialise xml ^ "\n")
    end

fun usage () =
    (TextIO.output
         (TextIO.stdErr,
          "\nUsage: " ^ (CommandLine.name ()) ^
          " file.xml\n\n" ^
          "Parse the named XML file and serialise it again to stdout.\n\n");
     raise Fail "Incorrect arguments specified")

fun handleArgs args =
    case args of
        [infile] => processFile infile
      | _ => usage ()
        
fun main () =
    handleArgs (CommandLine.arguments ())

                 
