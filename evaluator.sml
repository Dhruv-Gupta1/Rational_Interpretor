val symbol_table : (string * string * string) list ref = ref [];

fun insert (a : string, b : string, c : string) =
  symbol_table := (a, b, c) :: !symbol_table;

fun find (a : string, b : string, symbol_table : (string * string) list) =
  case symbol_table of
    [] => false
  | (x, y) :: xs => if a = x andalso b = y then true else find(a, b, xs);


fun modify(a: string , b : string , c : string ) = 
  symbol_table := List.map (fn (x, y, z) => if x = a then (x, b, c) else (x, y, z)) (!symbol_table);

fun getvalue(a: string) = 
  let
    fun helper([], found) = if found then "0" else "Key not found"
      | helper((x, y, z) :: xs, found) = 
          if a = x then z
          else helper(xs, found orelse a = x)
  in
    helper(!symbol_table, false)
  end

(* val procedure_table : (string * string * string) list ref = ref []; *)


val proc_table : (string * DataTypes.BLK) list ref = ref [];

fun insertproc (a : string, b : DataTypes.BLK) =
  proc_table := (a, b) :: !proc_table;



fun findproc(a: string): DataTypes.BLK =
    let
        val result = List.find (fn (s, _) => s = a) (!proc_table)
    in
        case result of
            NONE => raise Fail ("Procedure not declared: " ^ a)
          | SOME (_, blk) => blk
    end

val tobeexecproclist : DataTypes.BLK list ref = ref [];
fun toinserttoproc (a : DataTypes.BLK) =
  tobeexecproclist := a :: !tobeexecproclist;

fun pophead () =
    case !tobeexecproclist of
        [] => raise Fail "List is empty"
      | x :: xs =>
            (tobeexecproclist := xs;
             x);


val os = TextIO.openOut "output.txt";
fun printToOutStream outstream str = let val os = outstream in TextIO.output(os,str) end;
fun getInput( message : string ) =
   ( print ( message ^ "\n") ; 
    let
        val str= valOf(TextIO.inputLine TextIO.stdIn)
    in
        String.substring ( str , 0 ,  (String.size str) -1) 
    end)

fun printValue( valueToBePrinted ) = printToOutStream os ( Int.toString valueToBePrinted)





signature ASTeval =
sig
  (* val inputFile : string *)
  val ASTTree : DataTypes.AST
end

structure Asteval :> ASTeval =
struct
(* val inputFile = getInput("Enter file name ") *)
val ASTTree = Rat.compile "prog.rat"
val DataTypes.PROG (block) = ASTTree

fun procedureexecute ([]) = ()
  |procedureexecute (hd::tl) =
  let
    val DataTypes.PROCDEF (identifier, block) = hd
    val DataTypes.IDEN(idenstring) = identifier
    val _ = insertproc(idenstring, block)
    val _ = TextIO.output (os, idenstring^"\n");
  in
    procedureexecute(tl)
  end
fun ratdeclarationonst([]) = []
  | ratdeclarationonst(hd::tl) = 
    let
      val DataTypes.IDEN(idenstring_ratdec) = hd
      val _ = insert(idenstring_ratdec,"rat", "0")
      (* val _ = TextIO.output (os, idenstring_ratdec^"\n"); *)
    in
      ratdeclarationonst(tl)
    end
fun intdeclarationonst([]) = []
  | intdeclarationonst(hd::tl) = 
    let
      val DataTypes.IDEN(idenstring_intdec) = hd
      val _ = insert(idenstring_intdec,"int", "0")
      (* val _ = TextIO.output (os, idenstring_intdec^"\n"); *)
    in
      intdeclarationonst(tl)
    end
  
fun booldeclarationonst([]) = []
  | booldeclarationonst(hd::tl) = 
    let
      val DataTypes.IDEN(idenstring_booldec) = hd
      val _ = insert(idenstring_booldec,"bool", "false")
      (* val _ = TextIO.output (os, idenstring_booldec^"\n"); *)
    in
      booldeclarationonst(tl)
    end
fun checkType(str: string) : string =
  let
      val len = String.size str
      val lastChar = String.sub (str, len - 1)
  in
      if lastChar = #")" then
          "rat"
      else if str = "true" orelse str = "false" then
          "bool"
      else
          "int"
  end;
fun evalexpression( exp ) = 
  let 
    val returnexp =
      case exp of 
        DataTypes.AND(exp1,exp2) => 
          let
            val bool1str = evalexpression(exp1)
            val bool1 = ( bool1str = "true" )
            val bool2str = evalexpression(exp2)
            val bool2 = ( bool2str = "true" )
          in
            Bool.toString(bool1 andalso bool2)
          end
      | DataTypes.TRUE =>
          let
          in 
            "true"
          end
      | DataTypes.FALSE =>
          let
          in 
            "false"
          end
      | DataTypes.OR(exp1,exp2) =>
          let
            val bool1str = evalexpression(exp1)
            val bool1 = ( bool1str = "true" )
            val bool2str = evalexpression(exp2)
            val bool2 = ( bool2str = "true" )
          in 
            Bool.toString(bool1 orelse bool2)
          end
      | DataTypes.NOT(exp1) =>
          let
            val bool1str = evalexpression(exp1)
            val bool1 = ( bool1str = "true" )
          in 
            Bool.toString(not bool1)
          end
      | DataTypes.bigint(exp1) =>
          let
          in
            exp1
          end
      | DataTypes.PLUS(exp1,exp2) =>
          let
            val int1str = evalexpression(exp1)
            val int1 = Bigint.from_string(int1str)
            val int2str = evalexpression(exp2)
            val int2 = Bigint.from_string(int2str)
          in
            Bigint.to_string(Bigint.add int1 int2)
          end
      | DataTypes.MINUS(exp1,exp2) =>
          let
            val int1str = evalexpression(exp1)
            val int1 = Bigint.from_string(int1str)
            val int2str = evalexpression(exp2)
            val int2 = Bigint.from_string(int2str)
          in
            Bigint.to_string(Bigint.subtract int1 int2)
          end
      | DataTypes.TIMES(exp1,exp2) =>
          let
            val int1str = evalexpression(exp1)
            val int1 = Bigint.from_string(int1str)
            val int2str = evalexpression(exp2)
            val int2 = Bigint.from_string(int2str)
          in
            Bigint.to_string(Bigint.multiply int1 int2)
          end
      | DataTypes.DIV(exp1,exp2) =>
          let
            val int1str = evalexpression(exp1)
            val int1 = Bigint.from_string(int1str)
            val int2str = evalexpression(exp2)
            val int2 = Bigint.from_string(int2str)
          in
            Bigint.to_string(#1 (Bigint.divmod int1 int2))
          end
      | DataTypes.MOD(exp1,exp2) =>
          let
            val int1str = evalexpression(exp1)
            val int1 = Bigint.from_string(int1str)
            val int2str = evalexpression(exp2)
            val int2 = Bigint.from_string(int2str)
          in
            Bigint.to_string(#2 (Bigint.divmod int1 int2))
          end
      | DataTypes.decimal(exp1) =>
          let
          in
            exp1
          end
      | DataTypes.ADDRAT(exp1,exp2) =>
          let
            val rat1str = evalexpression(exp1)
            val rat2str = evalexpression(exp2)
            val rat1 = Rationals.fromDecimal(rat1str)
            val rat2 = Rationals.fromDecimal(rat2str)
          in
            Rationals.toDecimal(Rationals.add(rat1,rat2))
          end
      | DataTypes.SUBRAT(exp1,exp2) =>
          let
            val rat1str = evalexpression(exp1)
            val rat2str = evalexpression(exp2)
            val rat1 = Rationals.fromDecimal(rat1str)
            val rat2 = Rationals.fromDecimal(rat2str)
          in
            Rationals.toDecimal(Rationals.subtract(rat1,rat2))
          end
      | DataTypes.MULRAT(exp1,exp2) =>
          let
            val rat1str = evalexpression(exp1)
            val rat2str = evalexpression(exp2)
            val rat1 = Rationals.fromDecimal(rat1str)
            val rat2 = Rationals.fromDecimal(rat2str)
          in
            Rationals.toDecimal(Rationals.multiply(rat1,rat2))
          end
      | DataTypes.DIVRAT(exp1,exp2) =>
          let
            val rat1str = evalexpression(exp1)
            val rat2str = evalexpression(exp2)
            val rat1 = Rationals.fromDecimal(rat1str)
            val rat2 = Rationals.fromDecimal(rat2str)
            val SOME a = Rationals.divide(rat1,rat2)
          in
            Rationals.toDecimal(a)
          end
      | DataTypes.MAKERAT(exp1,exp2) =>
          let
            val int1str = evalexpression(exp1)
            val int1 = Bigint.from_string(int1str)
            val int2str = evalexpression(exp2)
            val int2 = Bigint.from_string(int2str)
            val SOME a = Rationals.make_rat(int1,int2)
          in
            Rationals.toDecimal(a)
          end
      | DataTypes.INVERSERAT(exp1) =>
          let
            val rat1str = evalexpression(exp1)
            val rat1 = Rationals.fromDecimal(rat1str)
            val SOME a = Rationals.inverse(rat1)
          in
            Rationals.toDecimal(a)
          end
      | DataTypes.IDVAR(exp1) =>
          let
            val DataTypes.IDEN(idenstring) = exp1
            val ans = getvalue(idenstring) 
          in
            ans
          end
      | DataTypes.EQ(exp1,exp2) =>
          let
            val exp1str = evalexpression(exp1)
            val exp2str = evalexpression(exp2)
          in
            Bool.toString(exp1str = exp2str)
          end
      | DataTypes.NEQ(exp1,exp2) =>
          let
            val exp1str = evalexpression(exp1)
            val exp2str = evalexpression(exp2)
          in
            Bool.toString(exp1str <> exp2str)
          end
      | DataTypes.LT(exp1,exp2) =>
          let
            val exp1str = evalexpression(exp1)
            val exp2str = evalexpression(exp2)
            val ans = 
            if checkType(exp1str) = "rat" andalso checkType(exp2str) = "rat" then
                let
                    val rat1 = Rationals.fromDecimal(exp1str)
                    val rat2 = Rationals.fromDecimal(exp2str)
                in
                    Bool.toString(Rationals.less(rat1,rat2))
                end
            else
                let
                    val int1 = Bigint.from_string(exp1str)
                    val int2 = Bigint.from_string(exp2str)
                in
                    Bool.toString(not (Bigint.geq int1 int2 ))
                end
          in
            ans
          end
      | DataTypes.LEQ(exp1,exp2) =>
          let
            val exp1str = evalexpression(exp1)
            val exp2str = evalexpression(exp2)
            val ans = 
            if checkType(exp1str) = "rat" andalso checkType(exp2str) = "rat" then
                let
                    val rat1 = Rationals.fromDecimal(exp1str)
                    val rat2 = Rationals.fromDecimal(exp2str)
                in
                    Bool.toString((Rationals.less(rat1,rat2)) orelse (exp1str = exp2str))
                end
            else
                let
                    val int1 = Bigint.from_string(exp1str)
                    val int2 = Bigint.from_string(exp2str)
                in
                    Bool.toString((not (Bigint.geq int1 int2 )) orelse (exp1str = exp2str))
                end
          in
            ans
          end
      | DataTypes.GT(exp1,exp2) =>
          let
            val exp1str = evalexpression(exp1)
            val exp2str = evalexpression(exp2)
            val ans = 
            if checkType(exp1str) = "rat" andalso checkType(exp2str) = "rat" then
                let
                    val rat1 = Rationals.fromDecimal(exp1str)
                    val rat2 = Rationals.fromDecimal(exp2str)
                in
                    Bool.toString((not (Rationals.less(rat1,rat2))) andalso (exp1str <> exp2str))
                end
            else
                let
                    val int1 = Bigint.from_string(exp1str)
                    val int2 = Bigint.from_string(exp2str)
                in
                    Bool.toString(not (Bigint.geq int1 int2 ))
                end
          in
            ans
          end
      | DataTypes.GEQ(exp1,exp2) =>
          let
            val exp1str = evalexpression(exp1)
            val exp2str = evalexpression(exp2)
            val ans = 
            if checkType(exp1str) = "rat" andalso checkType(exp2str) = "rat" then
                let
                    val rat1 = Rationals.fromDecimal(exp1str)
                    val rat2 = Rationals.fromDecimal(exp2str)
                in
                    Bool.toString(not (Rationals.less(rat1,rat2)))
                end
            else
                let
                    val int1 = Bigint.from_string(exp1str)
                    val int2 = Bigint.from_string(exp2str)
                in
                    Bool.toString((Bigint.geq int1 int2 ))
                end
          in
            ans
          end
  in      
    returnexp
  end


fun evaluatecommandlist([]) = []
  | evaluatecommandlist(hd::tl) = 
      let
        val _ =
          case hd of 
            DataTypes.CMDASSIGN(cmdassign) => 
              let
                val DataTypes.ASSIGNCMD(identifier_cmd,exp_cmd) = cmdassign
                val DataTypes.IDEN(idenstring_cmd) = identifier_cmd
                val _ = modify(idenstring_cmd, checkType(idenstring_cmd), evalexpression(exp_cmd))
              in
                ()
              end
          | DataTypes.CMDPRINT(cmdprint) => 
              let
                val DataTypes.PRINTCMD(exp_cmd) = cmdprint
                val final = evalexpression(exp_cmd)
                val _ = TextIO.output (os, final^"\n")
              in
                ()
              end
          | DataTypes.CMDITE(cmdite) =>
              let
                val DataTypes.ITECMD(exp_cmd,commandlist_cmd1,commandlist_cmd2) = cmdite
                val bool = evalexpression(exp_cmd)
                val DataTypes.CMDLST (commandlist1) = commandlist_cmd1
                val DataTypes.CMDLST (commandlist2) = commandlist_cmd2
                val _ = 
                  if (bool = "true") then  evaluatecommandlist(commandlist1)
                  else evaluatecommandlist(commandlist2)
              in
                ()
              end
          | DataTypes.CMDWH(cmdwhile) =>
              let
                val DataTypes.WHCMD(exp_cmd,commandlist_cmd) = cmdwhile
                val DataTypes.CMDLST (commandlist) = commandlist_cmd
                fun while_handle(exp,cseq) = 
                  if evalexpression(exp) = "true" then
                    let
                      val _ = evaluatecommandlist(cseq)
                    in
                      while_handle(exp,cseq)
                    end
                  else
                    ()
                val _ = while_handle(exp_cmd,commandlist)
              in
                ()
              end
          | DataTypes.CMDREAD(cmdread) =>
              let
                val DataTypes.READCMD(identifier_cmd) = cmdread
                val DataTypes.IDEN(idenstring_cmd) = identifier_cmd
                val ident  = getInput("enter the identifier's value:")
                val _ = modify(idenstring_cmd, checkType(idenstring_cmd), ident)
              in
                ()
              end
          | DataTypes.CMDCALL(cmdcall) =>
            let
              val DataTypes.CALLCMD(ident_call) = cmdcall
              val DataTypes.IDEN(idenstring_call) = ident_call
              val blocky = findproc(idenstring_call)
              val _ = toinserttoproc(blocky)
            in
              ()
            end
      in
        evaluatecommandlist(tl)
      end;




fun runblock( block) =
  let
    val DataTypes.BLK (declarations, commands) = block
    val DataTypes.CMDLST (commandlist) = commands
    val DataTypes.DECSEQ (vardec, procdec) = declarations
    val vardectrue = case vardec of SOME v => v | NONE => DataTypes.VARDEC (NONE,NONE,NONE)
    val procdectrue = case procdec of SOME p => p | NONE => DataTypes.PROCDEC []
    val DataTypes.PROCDEC (procdefinationlist) = procdectrue
    val _ = procedureexecute(procdefinationlist)
    



    val DataTypes.VARDEC(ratvardec,intvardec,boolvardec)= vardectrue
    val ratvardectrue = case ratvardec of SOME r => r | NONE => DataTypes.RatVarDec []
    val intvardectrue = case intvardec of SOME i => i | NONE => DataTypes.IntVarDec []
    val boolvardectrue = case boolvardec of SOME b => b | NONE => DataTypes.BoolVarDec []


    val DataTypes.RatVarDec (Ratdeclarationslist) = ratvardectrue
    val DataTypes.IntVarDec (Intdeclarationslist) = intvardectrue
    val DataTypes.BoolVarDec (Booldeclarationslist) = boolvardectrue
    val _ = ratdeclarationonst(Ratdeclarationslist)
    val _ = intdeclarationonst(Intdeclarationslist)
    val _ = booldeclarationonst(Booldeclarationslist)
    val _ = evaluatecommandlist(commandlist)



    fun procwork([]) = []
      | procwork(hd::tl) = 
        let
            val x = pophead()
            val _ = runblock(x)
        in
          procwork(tl)
        end
      
  val _ = procwork(!tobeexecproclist)



  in
    ()
  end

val _ = runblock(block)
val closeStream = TextIO.closeOut os;
end;