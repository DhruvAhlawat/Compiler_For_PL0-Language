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
\\001\000\002\000\006\000\010\000\113\000\023\000\005\000\034\000\004\000\000\000\
\\001\000\002\000\006\000\010\000\072\000\023\000\005\000\034\000\004\000\000\000\
\\001\000\002\000\006\000\023\000\005\000\034\000\004\000\000\000\
\\001\000\002\000\015\000\015\000\014\000\016\000\013\000\017\000\012\000\
\\021\000\011\000\027\000\010\000\000\000\
\\001\000\002\000\016\000\000\000\
\\001\000\002\000\051\000\017\000\012\000\021\000\011\000\000\000\
\\001\000\003\000\084\000\004\000\037\000\005\000\036\000\006\000\084\000\
\\007\000\084\000\008\000\033\000\011\000\084\000\012\000\084\000\
\\013\000\030\000\014\000\029\000\025\000\084\000\026\000\084\000\
\\028\000\084\000\029\000\084\000\030\000\084\000\031\000\084\000\
\\032\000\084\000\033\000\084\000\035\000\084\000\000\000\
\\001\000\003\000\085\000\004\000\037\000\005\000\036\000\006\000\085\000\
\\007\000\085\000\008\000\033\000\011\000\085\000\012\000\085\000\
\\013\000\030\000\014\000\029\000\025\000\085\000\026\000\085\000\
\\028\000\085\000\029\000\085\000\030\000\085\000\031\000\085\000\
\\032\000\085\000\033\000\085\000\035\000\085\000\000\000\
\\001\000\003\000\086\000\004\000\037\000\005\000\036\000\006\000\086\000\
\\007\000\086\000\008\000\033\000\011\000\086\000\012\000\086\000\
\\013\000\030\000\014\000\029\000\025\000\086\000\026\000\086\000\
\\028\000\086\000\029\000\086\000\030\000\086\000\031\000\086\000\
\\032\000\086\000\033\000\086\000\035\000\086\000\000\000\
\\001\000\003\000\087\000\004\000\037\000\005\000\036\000\006\000\087\000\
\\007\000\087\000\008\000\033\000\011\000\087\000\012\000\087\000\
\\013\000\030\000\014\000\029\000\025\000\087\000\026\000\087\000\
\\028\000\087\000\029\000\087\000\030\000\087\000\031\000\087\000\
\\032\000\087\000\033\000\087\000\035\000\087\000\000\000\
\\001\000\003\000\088\000\004\000\088\000\005\000\088\000\006\000\088\000\
\\007\000\088\000\008\000\088\000\011\000\088\000\012\000\088\000\
\\013\000\088\000\014\000\088\000\025\000\088\000\026\000\088\000\
\\028\000\088\000\029\000\088\000\030\000\088\000\031\000\088\000\
\\032\000\088\000\033\000\088\000\035\000\088\000\000\000\
\\001\000\003\000\089\000\004\000\089\000\005\000\089\000\006\000\089\000\
\\007\000\089\000\008\000\089\000\011\000\089\000\012\000\089\000\
\\013\000\089\000\014\000\089\000\025\000\089\000\026\000\089\000\
\\028\000\089\000\029\000\089\000\030\000\089\000\031\000\089\000\
\\032\000\089\000\033\000\089\000\035\000\089\000\000\000\
\\001\000\003\000\090\000\004\000\090\000\005\000\090\000\006\000\090\000\
\\007\000\090\000\008\000\090\000\011\000\090\000\012\000\090\000\
\\013\000\090\000\014\000\090\000\025\000\090\000\026\000\090\000\
\\028\000\090\000\029\000\090\000\030\000\090\000\031\000\090\000\
\\032\000\090\000\033\000\090\000\035\000\090\000\000\000\
\\001\000\003\000\091\000\004\000\091\000\005\000\091\000\006\000\091\000\
\\007\000\091\000\008\000\091\000\011\000\091\000\012\000\091\000\
\\013\000\091\000\014\000\091\000\025\000\091\000\026\000\091\000\
\\028\000\091\000\029\000\091\000\030\000\091\000\031\000\091\000\
\\032\000\091\000\033\000\091\000\035\000\091\000\000\000\
\\001\000\003\000\092\000\004\000\092\000\005\000\092\000\006\000\092\000\
\\007\000\092\000\008\000\092\000\011\000\092\000\012\000\092\000\
\\013\000\092\000\014\000\092\000\025\000\092\000\026\000\092\000\
\\028\000\092\000\029\000\092\000\030\000\092\000\031\000\092\000\
\\032\000\092\000\033\000\092\000\035\000\092\000\000\000\
\\001\000\003\000\093\000\004\000\093\000\005\000\093\000\006\000\093\000\
\\007\000\093\000\008\000\093\000\011\000\093\000\012\000\093\000\
\\013\000\093\000\014\000\093\000\025\000\093\000\026\000\093\000\
\\028\000\093\000\029\000\093\000\030\000\093\000\031\000\093\000\
\\032\000\093\000\033\000\093\000\035\000\093\000\000\000\
\\001\000\003\000\094\000\004\000\094\000\005\000\094\000\006\000\094\000\
\\007\000\094\000\008\000\094\000\011\000\094\000\012\000\094\000\
\\013\000\094\000\014\000\094\000\025\000\094\000\026\000\094\000\
\\028\000\094\000\029\000\094\000\030\000\094\000\031\000\094\000\
\\032\000\094\000\033\000\094\000\035\000\094\000\000\000\
\\001\000\003\000\095\000\004\000\095\000\005\000\095\000\006\000\095\000\
\\007\000\095\000\008\000\095\000\011\000\095\000\012\000\095\000\
\\013\000\095\000\014\000\095\000\025\000\095\000\026\000\095\000\
\\028\000\095\000\029\000\095\000\030\000\095\000\031\000\095\000\
\\032\000\095\000\033\000\095\000\035\000\095\000\000\000\
\\001\000\003\000\096\000\025\000\096\000\026\000\096\000\032\000\096\000\
\\033\000\096\000\035\000\096\000\000\000\
\\001\000\003\000\097\000\025\000\022\000\026\000\097\000\032\000\097\000\
\\033\000\097\000\035\000\097\000\000\000\
\\001\000\003\000\098\000\025\000\022\000\026\000\021\000\032\000\098\000\
\\033\000\098\000\035\000\098\000\000\000\
\\001\000\003\000\099\000\004\000\037\000\005\000\036\000\006\000\035\000\
\\007\000\034\000\008\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\025\000\099\000\026\000\099\000\
\\032\000\099\000\033\000\099\000\035\000\099\000\000\000\
\\001\000\003\000\100\000\004\000\037\000\005\000\036\000\006\000\035\000\
\\007\000\034\000\008\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\025\000\100\000\026\000\100\000\
\\032\000\100\000\033\000\100\000\035\000\100\000\000\000\
\\001\000\003\000\101\000\004\000\037\000\005\000\036\000\006\000\035\000\
\\007\000\034\000\008\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\025\000\101\000\026\000\101\000\
\\032\000\101\000\033\000\101\000\035\000\101\000\000\000\
\\001\000\003\000\102\000\004\000\037\000\005\000\036\000\006\000\035\000\
\\007\000\034\000\008\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\025\000\102\000\026\000\102\000\
\\032\000\102\000\033\000\102\000\035\000\102\000\000\000\
\\001\000\003\000\103\000\004\000\037\000\005\000\036\000\006\000\035\000\
\\007\000\034\000\008\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\025\000\103\000\026\000\103\000\
\\032\000\103\000\033\000\103\000\035\000\103\000\000\000\
\\001\000\003\000\104\000\004\000\037\000\005\000\036\000\006\000\035\000\
\\007\000\034\000\008\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\025\000\104\000\026\000\104\000\
\\032\000\104\000\033\000\104\000\035\000\104\000\000\000\
\\001\000\003\000\105\000\025\000\105\000\026\000\105\000\032\000\105\000\
\\033\000\105\000\035\000\105\000\000\000\
\\001\000\003\000\106\000\025\000\106\000\026\000\106\000\032\000\106\000\
\\033\000\106\000\035\000\106\000\000\000\
\\001\000\003\000\107\000\025\000\107\000\026\000\107\000\032\000\107\000\
\\033\000\107\000\035\000\107\000\000\000\
\\001\000\003\000\108\000\025\000\022\000\026\000\021\000\032\000\020\000\
\\035\000\108\000\000\000\
\\001\000\003\000\109\000\025\000\022\000\026\000\021\000\033\000\109\000\
\\035\000\109\000\000\000\
\\001\000\003\000\114\000\019\000\114\000\000\000\
\\001\000\003\000\115\000\019\000\115\000\000\000\
\\001\000\003\000\116\000\019\000\116\000\000\000\
\\001\000\003\000\117\000\019\000\117\000\000\000\
\\001\000\003\000\043\000\000\000\
\\001\000\003\000\066\000\025\000\022\000\026\000\021\000\032\000\020\000\
\\033\000\019\000\000\000\
\\001\000\003\000\067\000\004\000\037\000\005\000\036\000\006\000\035\000\
\\007\000\034\000\008\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\000\000\
\\001\000\003\000\067\000\004\000\037\000\005\000\036\000\006\000\035\000\
\\007\000\034\000\008\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\028\000\028\000\029\000\027\000\
\\030\000\026\000\031\000\025\000\032\000\024\000\033\000\023\000\000\000\
\\001\000\003\000\068\000\025\000\022\000\026\000\021\000\032\000\020\000\
\\033\000\019\000\000\000\
\\001\000\003\000\069\000\004\000\037\000\005\000\036\000\006\000\035\000\
\\007\000\034\000\008\000\033\000\011\000\032\000\012\000\031\000\
\\013\000\030\000\014\000\029\000\028\000\028\000\029\000\027\000\
\\030\000\026\000\031\000\025\000\032\000\024\000\033\000\023\000\000\000\
\\001\000\004\000\037\000\005\000\036\000\006\000\035\000\007\000\034\000\
\\008\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\014\000\029\000\028\000\028\000\029\000\027\000\030\000\026\000\
\\031\000\025\000\032\000\024\000\033\000\023\000\000\000\
\\001\000\009\000\045\000\000\000\
\\001\000\010\000\112\000\000\000\
\\001\000\010\000\079\000\000\000\
\\001\000\019\000\000\000\020\000\000\000\000\000\
\\001\000\019\000\083\000\020\000\083\000\000\000\
\\001\000\019\000\007\000\000\000\
\\001\000\019\000\075\000\000\000\
\\001\000\019\000\080\000\000\000\
\\001\000\025\000\022\000\026\000\021\000\032\000\020\000\033\000\019\000\
\\035\000\018\000\000\000\
\\001\000\036\000\110\000\037\000\110\000\000\000\
\\001\000\036\000\111\000\037\000\111\000\000\000\
\\001\000\036\000\070\000\000\000\
\\001\000\037\000\076\000\000\000\
\"
val actionRowNumbers =
"\002\000\048\000\003\000\004\000\
\\002\000\047\000\051\000\042\000\
\\003\000\016\000\015\000\028\000\
\\029\000\003\000\003\000\036\000\
\\043\000\003\000\003\000\003\000\
\\003\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\020\000\037\000\039\000\040\000\
\\041\000\035\000\054\000\001\000\
\\030\000\031\000\019\000\018\000\
\\021\000\005\000\022\000\026\000\
\\025\000\024\000\023\000\013\000\
\\011\000\009\000\007\000\014\000\
\\008\000\006\000\010\000\012\000\
\\027\000\017\000\034\000\033\000\
\\043\000\049\000\053\000\038\000\
\\055\000\000\000\032\000\045\000\
\\050\000\052\000\000\000\044\000\
\\046\000"
val gotoT =
"\
\\002\000\080\000\004\000\001\000\000\000\
\\000\000\
\\001\000\007\000\006\000\006\000\000\000\
\\000\000\
\\004\000\015\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\007\000\006\000\036\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\038\000\006\000\037\000\000\000\
\\001\000\040\000\006\000\039\000\000\000\
\\000\000\
\\007\000\042\000\000\000\
\\001\000\007\000\006\000\044\000\000\000\
\\001\000\007\000\006\000\045\000\000\000\
\\001\000\007\000\006\000\046\000\000\000\
\\001\000\007\000\006\000\047\000\000\000\
\\001\000\048\000\000\000\
\\001\000\050\000\000\000\
\\001\000\051\000\000\000\
\\001\000\052\000\000\000\
\\001\000\053\000\000\000\
\\001\000\054\000\000\000\
\\001\000\055\000\000\000\
\\001\000\056\000\000\000\
\\001\000\057\000\000\000\
\\001\000\058\000\000\000\
\\001\000\059\000\000\000\
\\001\000\060\000\000\000\
\\001\000\061\000\000\000\
\\001\000\062\000\000\000\
\\001\000\063\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\069\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\071\000\000\000\
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
\\007\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\076\000\008\000\075\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\076\000\008\000\079\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 81
val numrules = 35
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
 | ID of unit ->  (string) | comSeqInBrace of unit ->  (COMMANDSEQ)
 | commandSeq of unit ->  (COMMANDSEQ) | boolExp of unit ->  (BOOLEXP)
 | command of unit ->  (COMMAND) | exp of unit ->  (EXP)
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
 in (int(add(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (rat(add(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (int(sub(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (rat(sub(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (int(mul(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (rat(mul(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (int(divOp(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (rat(divOp(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (int(modOp(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.NUMBA NUMBA1, NUMBA1left, NUMBA1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (NUMBA
 as NUMBA1) = NUMBA1 ()
 in (intType(BigInt.getBigInt(NUMBA)))
end)
 in ( LrTable.NT 0, ( result, NUMBA1left, NUMBA1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.DECI DECI1, DECI1left, DECI1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (DECI
 as DECI1) = DECI1 ()
 in (ratType(Rational.fromDecimal(DECI)))
end)
 in ( LrTable.NT 0, ( result, DECI1left, DECI1right), rest671)
end
|  ( 12, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _
 :: ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671))
 => let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (andOp(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _
 :: ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671))
 => let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (orOp(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 15, ( ( _, ( MlyValue.boolExp boolExp1, _, boolExp1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  (boolExp as boolExp1) = boolExp1 ()
 in (notOp(boolExp))
end)
 in ( LrTable.NT 5, ( result, NOT1left, boolExp1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (eq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (neq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (lt(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (leq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (gt(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (geq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.boolExp 
boolExp1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.boolExp (fn _ => let val  (boolExp as boolExp1
) = boolExp1 ()
 in (boolExp)
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 23, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.boolExp (fn _ => (TRUE))
 in ( LrTable.NT 5, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 24, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.boolExp (fn _ => (FALSE))
 in ( LrTable.NT 5, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _
 :: ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671))
 => let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (beq(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _
 :: ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671))
 => let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (bneq(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 27, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( 
MlyValue.comSeqInBrace comSeqInBrace1, _, _)) :: _ :: ( _, ( 
MlyValue.command command1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: 
rest671)) => let val  result = MlyValue.commandSeq (fn _ => let val  (
command as command1) = command1 ()
 val  (comSeqInBrace as comSeqInBrace1) = comSeqInBrace1 ()
 in (cons(command,comSeqInBrace))
end)
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 28, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _))
 :: rest671)) => let val  result = MlyValue.commandSeq (fn _ => (empty
))
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.comSeqInBrace comSeqInBrace1, _, 
comSeqInBrace1right)) :: _ :: ( _, ( MlyValue.command command1, 
command1left, _)) :: rest671)) => let val  result = 
MlyValue.comSeqInBrace (fn _ => let val  (command as command1) = 
command1 ()
 val  (comSeqInBrace as comSeqInBrace1) = comSeqInBrace1 ()
 in (cons(command, comSeqInBrace))
end)
 in ( LrTable.NT 7, ( result, command1left, comSeqInBrace1right), 
rest671)
end
|  ( 30, ( rest671)) => let val  result = MlyValue.comSeqInBrace (fn _
 => (empty))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 31, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.commandSeq 
commandSeq2, _, _)) :: _ :: ( _, ( MlyValue.commandSeq commandSeq1, _,
 _)) :: _ :: ( _, ( MlyValue.boolExp boolExp1, _, _)) :: ( _, ( _, 
IF1left, _)) :: rest671)) => let val  result = MlyValue.command (fn _
 => let val  (boolExp as boolExp1) = boolExp1 ()
 val  commandSeq1 = commandSeq1 ()
 val  commandSeq2 = commandSeq2 ()
 in (ConditionalCMD(boolExp, commandSeq1, commandSeq2))
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let val  result
 = MlyValue.command (fn _ => let val  (exp as exp1) = exp1 ()
 in (PrintCMD(exp))
end)
 in ( LrTable.NT 3, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.boolExp 
boolExp1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let
 val  result = MlyValue.command (fn _ => let val  (boolExp as boolExp1
) = boolExp1 ()
 in (PrintBool(boolExp))
end)
 in ( LrTable.NT 3, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 34, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.command 
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
