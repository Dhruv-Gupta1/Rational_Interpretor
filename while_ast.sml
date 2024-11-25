(* while_ast.sml *)
use "datatypes.sml";

structure Rat :
sig val compile : string -> DataTypes.AST
end =
    struct
    exception MyError;
    fun compile (fileName) =
    let
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
        n => if TextIO.endOfStream inStream
            then ""
            else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
        (msg,line,col) =>
        print (fileName^"["^Int.toString line^":"
        ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = MyParser.parse
        (15,
        (MyParser.makeLexer grab ),
        printError,
        ()) 
        handle MyParser.ParseError => raise MyError ; 
        val _ = TextIO.closeIn inStream;

    in 
        tree 
    end
end;