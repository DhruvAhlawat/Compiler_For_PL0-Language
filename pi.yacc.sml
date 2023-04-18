functor PiLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Pi_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\002\000\006\000\023\000\005\000\034\000\004\000\000\000\
\\001\000\002\000\015\000\015\000\014\000\016\000\013\000\017\000\012\000\
\\021\000\011\000\027\000\010\000\000\000\
\\001\000\002\000\017\000\017\000\012\000\021\000\011\000\000\000\
\\001\000\003\000\053\000\006\000\053\000\019\000\053\000\025\000\053\000\
\\026\000\053\000\028\000\053\000\029\000\053\000\030\000\053\000\
\\031\000\053\000\032\000\053\000\033\000\053\000\035\000\053\000\000\000\
\\001\000\003\000\054\000\006\000\054\000\019\000\054\000\025\000\054\000\
\\026\000\054\000\028\000\054\000\029\000\054\000\030\000\054\000\
\\031\000\054\000\032\000\054\000\033\000\054\000\035\000\054\000\000\000\
\\001\000\003\000\055\000\006\000\055\000\019\000\055\000\025\000\055\000\
\\026\000\055\000\028\000\055\000\029\000\055\000\030\000\055\000\
\\031\000\055\000\032\000\055\000\033\000\055\000\035\000\055\000\000\000\
\\001\000\003\000\056\000\006\000\056\000\019\000\056\000\025\000\056\000\
\\026\000\056\000\028\000\056\000\029\000\056\000\030\000\056\000\
\\031\000\056\000\032\000\056\000\033\000\056\000\035\000\056\000\000\000\
\\001\000\003\000\057\000\025\000\057\000\026\000\057\000\035\000\057\000\000\000\
\\001\000\003\000\058\000\025\000\021\000\026\000\058\000\035\000\058\000\000\000\
\\001\000\003\000\059\000\025\000\059\000\026\000\059\000\035\000\059\000\000\000\
\\001\000\003\000\060\000\006\000\028\000\025\000\060\000\026\000\060\000\
\\035\000\060\000\000\000\
\\001\000\003\000\061\000\006\000\028\000\025\000\061\000\026\000\061\000\
\\035\000\061\000\000\000\
\\001\000\003\000\062\000\006\000\028\000\025\000\062\000\026\000\062\000\
\\035\000\062\000\000\000\
\\001\000\003\000\063\000\006\000\028\000\025\000\063\000\026\000\063\000\
\\035\000\063\000\000\000\
\\001\000\003\000\064\000\006\000\028\000\025\000\064\000\026\000\064\000\
\\035\000\064\000\000\000\
\\001\000\003\000\065\000\006\000\028\000\025\000\065\000\026\000\065\000\
\\035\000\065\000\000\000\
\\001\000\003\000\066\000\025\000\066\000\026\000\066\000\035\000\066\000\000\000\
\\001\000\003\000\067\000\025\000\067\000\026\000\067\000\035\000\067\000\000\000\
\\001\000\003\000\068\000\025\000\068\000\026\000\068\000\035\000\068\000\000\000\
\\001\000\003\000\069\000\019\000\069\000\000\000\
\\001\000\003\000\070\000\006\000\028\000\019\000\070\000\000\000\
\\001\000\003\000\071\000\019\000\071\000\000\000\
\\001\000\003\000\033\000\000\000\
\\001\000\003\000\044\000\025\000\021\000\026\000\020\000\000\000\
\\001\000\003\000\045\000\006\000\028\000\000\000\
\\001\000\003\000\045\000\006\000\028\000\028\000\027\000\029\000\026\000\
\\030\000\025\000\031\000\024\000\032\000\023\000\033\000\022\000\000\000\
\\001\000\006\000\028\000\028\000\027\000\029\000\026\000\030\000\025\000\
\\031\000\024\000\032\000\023\000\033\000\022\000\000\000\
\\001\000\019\000\000\000\020\000\000\000\000\000\
\\001\000\019\000\052\000\020\000\052\000\000\000\
\\001\000\019\000\007\000\000\000\
\\001\000\019\000\046\000\000\000\
\\001\000\019\000\049\000\000\000\
\\001\000\025\000\021\000\026\000\020\000\035\000\019\000\000\000\
\\001\000\036\000\047\000\000\000\
\\001\000\037\000\050\000\000\000\
\"
val actionRowNumbers =
"\000\000\029\000\001\000\002\000\
\\000\000\028\000\032\000\026\000\
\\001\000\005\000\004\000\017\000\
\\018\000\001\000\020\000\002\000\
\\022\000\000\000\001\000\001\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\009\000\
\\023\000\025\000\024\000\021\000\
\\030\000\008\000\007\000\010\000\
\\011\000\015\000\014\000\013\000\
\\012\000\003\000\016\000\006\000\
\\033\000\000\000\031\000\034\000\
\\019\000\027\000"
val gotoT =
"\
\\002\000\049\000\004\000\001\000\000\000\
\\000\000\
\\001\000\007\000\006\000\006\000\000\000\
\\001\000\014\000\000\000\
\\004\000\016\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\007\000\006\000\027\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\029\000\006\000\028\000\000\000\
\\000\000\
\\001\000\030\000\000\000\
\\000\000\
\\004\000\032\000\000\000\
\\001\000\007\000\006\000\033\000\000\000\
\\001\000\007\000\006\000\034\000\000\000\
\\001\000\035\000\000\000\
\\001\000\036\000\000\000\
\\001\000\037\000\000\000\
\\001\000\038\000\000\000\
\\001\000\039\000\000\000\
\\001\000\040\000\000\000\
\\001\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 50
val numrules = 20
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = string
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | DECI of unit ->  (string) | NUMBA of unit ->  (string)
 | ID of unit ->  (string)
 | commandSequence of unit ->  (COMMAND list)
 | boolExp of unit ->  (BOOLEXP) | command of unit ->  (COMMAND)
 | exp of unit ->  (EXP)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 19) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "LPAREN"
  | (T 2) => "RPAREN"
  | (T 3) => "DIV"
  | (T 4) => "MUL"
  | (T 5) => "ADD"
  | (T 6) => "SUB"
  | (T 7) => "MOD"
  | (T 8) => "LBRACE"
  | (T 9) => "RBRACE"
  | (T 10) => "RATADD"
  | (T 11) => "RATSUB"
  | (T 12) => "RATMUL"
  | (T 13) => "RATDIV"
  | (T 14) => "FALSE"
  | (T 15) => "TRUE"
  | (T 16) => "NUMBA"
  | (T 17) => "DISPLAY"
  | (T 18) => "EOL"
  | (T 19) => "EOF"
  | (T 20) => "DECI"
  | (T 21) => "SHOWDECIMAL"
  | (T 22) => "PRINT"
  | (T 23) => "READ"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "NOT"
  | (T 27) => "LT"
  | (T 28) => "LEQ"
  | (T 29) => "GT"
  | (T 30) => "GEQ"
  | (T 31) => "NEQ"
  | (T 32) => "EQ"
  | (T 33) => "IF"
  | (T 34) => "THEN"
  | (T 35) => "ELSE"
  | (T 36) => "FI"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30)
 $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23)
 $$ (T 22) $$ (T 21) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, EOL1right)) :: ( _, ( MlyValue.command 
command1, command1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  (command as command1) = command1
 ()
 in (runCMD(command))
end; ()))
 in ( LrTable.NT 1, ( result, command1left, EOL1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (add(exp1, exp2))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.NUMBA NUMBA1, NUMBA1left, NUMBA1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (NUMBA
 as NUMBA1) = NUMBA1 ()
 in (
rat(valOf(Rational.make_rat(BigInt.getBigInt(NUMBA), BigInt.getBigInt("1"))))
)
end)
 in ( LrTable.NT 0, ( result, NUMBA1left, NUMBA1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.DECI DECI1, DECI1left, DECI1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (DECI
 as DECI1) = DECI1 ()
 in (rat(Rational.fromDecimal(DECI)))
end)
 in ( LrTable.NT 0, ( result, DECI1left, DECI1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _ ::
 ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671)) =>
 let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (andOp(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _ ::
 ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671)) =>
 let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (orOp(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.boolExp boolExp1, _, boolExp1right)) :: ( _,
 ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.boolExp
 (fn _ => let val  (boolExp as boolExp1) = boolExp1 ()
 in (notOp(boolExp))
end)
 in ( LrTable.NT 5, ( result, NOT1left, boolExp1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (eq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (neq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (lt(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (leq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (gt(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (geq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.boolExp 
boolExp1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.boolExp (fn _ => let val  (boolExp as boolExp1
) = boolExp1 ()
 in (boolExp)
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.boolExp (fn _ => (TRUE))
 in ( LrTable.NT 5, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 16, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.boolExp (fn _ => (FALSE))
 in ( LrTable.NT 5, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 17, ( ( _, ( _, _, FI1right)) :: _ :: ( _, ( MlyValue.command 
command2, _, _)) :: _ :: _ :: ( _, ( MlyValue.command command1, _, _))
 :: _ :: ( _, ( MlyValue.boolExp boolExp1, _, _)) :: ( _, ( _, IF1left
, _)) :: rest671)) => let val  result = MlyValue.command (fn _ => let
 val  (boolExp as boolExp1) = boolExp1 ()
 val  command1 = command1 ()
 val  command2 = command2 ()
 in (ConditionalCMD(boolExp, command1, command2))
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
PRINT1left, _)) :: rest671)) => let val  result = MlyValue.command (fn
 _ => let val  (exp as exp1) = exp1 ()
 in (PrintCMD(exp))
end)
 in ( LrTable.NT 3, ( result, PRINT1left, exp1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.command 
command1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.command (fn _ => let val  (command as command1
) = command1 ()
 in (command)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Pi_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RATADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun RATSUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RATMUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun RATDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun NUMBA (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.NUMBA (fn () => i),p1,p2))
fun DISPLAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun DECI (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.DECI (fn () => i),p1,p2))
fun SHOWDECIMAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
end
end
