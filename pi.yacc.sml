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

val scopeNumber = ref ~1;

fun printScopeNumbers(block(a,b,c)) = 
    let
        fun psn([]) = ()
        |   psn(x::xs) = (print(Int.toString(x)^"\n"); psn(xs));
    in
        psn(c)
    end;


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\020\000\009\000\164\000\023\000\018\000\034\000\017\000\000\000\
\\001\000\001\000\020\000\009\000\019\000\023\000\018\000\034\000\017\000\000\000\
\\001\000\001\000\020\000\023\000\018\000\034\000\017\000\000\000\
\\001\000\001\000\036\000\015\000\035\000\016\000\034\000\017\000\033\000\
\\021\000\032\000\027\000\031\000\000\000\
\\001\000\001\000\037\000\000\000\
\\001\000\001\000\087\000\017\000\033\000\021\000\032\000\000\000\
\\001\000\002\000\135\000\003\000\067\000\004\000\066\000\005\000\135\000\
\\006\000\135\000\007\000\063\000\011\000\135\000\012\000\135\000\
\\013\000\060\000\014\000\059\000\025\000\135\000\026\000\135\000\
\\028\000\135\000\029\000\135\000\030\000\135\000\031\000\135\000\
\\032\000\135\000\033\000\135\000\035\000\135\000\000\000\
\\001\000\002\000\136\000\003\000\067\000\004\000\066\000\005\000\136\000\
\\006\000\136\000\007\000\063\000\011\000\136\000\012\000\136\000\
\\013\000\060\000\014\000\059\000\025\000\136\000\026\000\136\000\
\\028\000\136\000\029\000\136\000\030\000\136\000\031\000\136\000\
\\032\000\136\000\033\000\136\000\035\000\136\000\000\000\
\\001\000\002\000\137\000\003\000\067\000\004\000\066\000\005\000\137\000\
\\006\000\137\000\007\000\063\000\011\000\137\000\012\000\137\000\
\\013\000\060\000\014\000\059\000\025\000\137\000\026\000\137\000\
\\028\000\137\000\029\000\137\000\030\000\137\000\031\000\137\000\
\\032\000\137\000\033\000\137\000\035\000\137\000\000\000\
\\001\000\002\000\138\000\003\000\067\000\004\000\066\000\005\000\138\000\
\\006\000\138\000\007\000\063\000\011\000\138\000\012\000\138\000\
\\013\000\060\000\014\000\059\000\025\000\138\000\026\000\138\000\
\\028\000\138\000\029\000\138\000\030\000\138\000\031\000\138\000\
\\032\000\138\000\033\000\138\000\035\000\138\000\000\000\
\\001\000\002\000\139\000\003\000\139\000\004\000\139\000\005\000\139\000\
\\006\000\139\000\007\000\139\000\011\000\139\000\012\000\139\000\
\\013\000\139\000\014\000\139\000\025\000\139\000\026\000\139\000\
\\028\000\139\000\029\000\139\000\030\000\139\000\031\000\139\000\
\\032\000\139\000\033\000\139\000\035\000\139\000\000\000\
\\001\000\002\000\140\000\003\000\140\000\004\000\140\000\005\000\140\000\
\\006\000\140\000\007\000\140\000\011\000\140\000\012\000\140\000\
\\013\000\140\000\014\000\140\000\025\000\140\000\026\000\140\000\
\\028\000\140\000\029\000\140\000\030\000\140\000\031\000\140\000\
\\032\000\140\000\033\000\140\000\035\000\140\000\000\000\
\\001\000\002\000\141\000\003\000\141\000\004\000\141\000\005\000\141\000\
\\006\000\141\000\007\000\141\000\011\000\141\000\012\000\141\000\
\\013\000\141\000\014\000\141\000\025\000\141\000\026\000\141\000\
\\028\000\141\000\029\000\141\000\030\000\141\000\031\000\141\000\
\\032\000\141\000\033\000\141\000\035\000\141\000\000\000\
\\001\000\002\000\142\000\003\000\142\000\004\000\142\000\005\000\142\000\
\\006\000\142\000\007\000\142\000\011\000\142\000\012\000\142\000\
\\013\000\142\000\014\000\142\000\025\000\142\000\026\000\142\000\
\\028\000\142\000\029\000\142\000\030\000\142\000\031\000\142\000\
\\032\000\142\000\033\000\142\000\035\000\142\000\000\000\
\\001\000\002\000\143\000\003\000\143\000\004\000\143\000\005\000\143\000\
\\006\000\143\000\007\000\143\000\011\000\143\000\012\000\143\000\
\\013\000\143\000\014\000\143\000\025\000\143\000\026\000\143\000\
\\028\000\143\000\029\000\143\000\030\000\143\000\031\000\143\000\
\\032\000\143\000\033\000\143\000\035\000\143\000\000\000\
\\001\000\002\000\144\000\003\000\144\000\004\000\144\000\005\000\144\000\
\\006\000\144\000\007\000\144\000\011\000\144\000\012\000\144\000\
\\013\000\144\000\014\000\144\000\025\000\144\000\026\000\144\000\
\\028\000\144\000\029\000\144\000\030\000\144\000\031\000\144\000\
\\032\000\144\000\033\000\144\000\035\000\144\000\000\000\
\\001\000\002\000\145\000\003\000\145\000\004\000\145\000\005\000\145\000\
\\006\000\145\000\007\000\145\000\011\000\145\000\012\000\145\000\
\\013\000\145\000\014\000\145\000\025\000\145\000\026\000\145\000\
\\028\000\145\000\029\000\145\000\030\000\145\000\031\000\145\000\
\\032\000\145\000\033\000\145\000\035\000\145\000\000\000\
\\001\000\002\000\146\000\003\000\146\000\004\000\146\000\005\000\146\000\
\\006\000\146\000\007\000\146\000\011\000\146\000\012\000\146\000\
\\013\000\146\000\014\000\146\000\025\000\146\000\026\000\146\000\
\\028\000\146\000\029\000\146\000\030\000\146\000\031\000\146\000\
\\032\000\146\000\033\000\146\000\035\000\146\000\000\000\
\\001\000\002\000\147\000\025\000\147\000\026\000\147\000\032\000\147\000\
\\033\000\147\000\035\000\147\000\000\000\
\\001\000\002\000\148\000\025\000\052\000\026\000\148\000\032\000\148\000\
\\033\000\148\000\035\000\148\000\000\000\
\\001\000\002\000\149\000\025\000\052\000\026\000\051\000\032\000\149\000\
\\033\000\149\000\035\000\149\000\000\000\
\\001\000\002\000\150\000\003\000\067\000\004\000\066\000\005\000\065\000\
\\006\000\064\000\007\000\063\000\011\000\062\000\012\000\061\000\
\\013\000\060\000\014\000\059\000\025\000\150\000\026\000\150\000\
\\032\000\150\000\033\000\150\000\035\000\150\000\000\000\
\\001\000\002\000\151\000\003\000\067\000\004\000\066\000\005\000\065\000\
\\006\000\064\000\007\000\063\000\011\000\062\000\012\000\061\000\
\\013\000\060\000\014\000\059\000\025\000\151\000\026\000\151\000\
\\032\000\151\000\033\000\151\000\035\000\151\000\000\000\
\\001\000\002\000\152\000\003\000\067\000\004\000\066\000\005\000\065\000\
\\006\000\064\000\007\000\063\000\011\000\062\000\012\000\061\000\
\\013\000\060\000\014\000\059\000\025\000\152\000\026\000\152\000\
\\032\000\152\000\033\000\152\000\035\000\152\000\000\000\
\\001\000\002\000\153\000\003\000\067\000\004\000\066\000\005\000\065\000\
\\006\000\064\000\007\000\063\000\011\000\062\000\012\000\061\000\
\\013\000\060\000\014\000\059\000\025\000\153\000\026\000\153\000\
\\032\000\153\000\033\000\153\000\035\000\153\000\000\000\
\\001\000\002\000\154\000\003\000\067\000\004\000\066\000\005\000\065\000\
\\006\000\064\000\007\000\063\000\011\000\062\000\012\000\061\000\
\\013\000\060\000\014\000\059\000\025\000\154\000\026\000\154\000\
\\032\000\154\000\033\000\154\000\035\000\154\000\000\000\
\\001\000\002\000\155\000\003\000\067\000\004\000\066\000\005\000\065\000\
\\006\000\064\000\007\000\063\000\011\000\062\000\012\000\061\000\
\\013\000\060\000\014\000\059\000\025\000\155\000\026\000\155\000\
\\032\000\155\000\033\000\155\000\035\000\155\000\000\000\
\\001\000\002\000\156\000\025\000\156\000\026\000\156\000\032\000\156\000\
\\033\000\156\000\035\000\156\000\000\000\
\\001\000\002\000\157\000\025\000\157\000\026\000\157\000\032\000\157\000\
\\033\000\157\000\035\000\157\000\000\000\
\\001\000\002\000\158\000\025\000\158\000\026\000\158\000\032\000\158\000\
\\033\000\158\000\035\000\158\000\000\000\
\\001\000\002\000\159\000\025\000\052\000\026\000\051\000\032\000\050\000\
\\035\000\159\000\000\000\
\\001\000\002\000\160\000\025\000\052\000\026\000\051\000\033\000\160\000\
\\035\000\160\000\000\000\
\\001\000\002\000\165\000\019\000\165\000\000\000\
\\001\000\002\000\166\000\019\000\166\000\000\000\
\\001\000\002\000\167\000\019\000\167\000\000\000\
\\001\000\002\000\168\000\019\000\168\000\000\000\
\\001\000\002\000\073\000\000\000\
\\001\000\002\000\102\000\025\000\052\000\026\000\051\000\032\000\050\000\
\\033\000\049\000\000\000\
\\001\000\002\000\103\000\003\000\067\000\004\000\066\000\005\000\065\000\
\\006\000\064\000\007\000\063\000\011\000\062\000\012\000\061\000\
\\013\000\060\000\014\000\059\000\000\000\
\\001\000\002\000\103\000\003\000\067\000\004\000\066\000\005\000\065\000\
\\006\000\064\000\007\000\063\000\011\000\062\000\012\000\061\000\
\\013\000\060\000\014\000\059\000\028\000\058\000\029\000\057\000\
\\030\000\056\000\031\000\055\000\032\000\054\000\033\000\053\000\000\000\
\\001\000\002\000\104\000\025\000\052\000\026\000\051\000\032\000\050\000\
\\033\000\049\000\000\000\
\\001\000\002\000\105\000\003\000\067\000\004\000\066\000\005\000\065\000\
\\006\000\064\000\007\000\063\000\011\000\062\000\012\000\061\000\
\\013\000\060\000\014\000\059\000\028\000\058\000\029\000\057\000\
\\030\000\056\000\031\000\055\000\032\000\054\000\033\000\053\000\000\000\
\\001\000\003\000\067\000\004\000\066\000\005\000\065\000\006\000\064\000\
\\007\000\063\000\011\000\062\000\012\000\061\000\013\000\060\000\
\\014\000\059\000\028\000\058\000\029\000\057\000\030\000\056\000\
\\031\000\055\000\032\000\054\000\033\000\053\000\000\000\
\\001\000\008\000\118\000\000\000\
\\001\000\008\000\119\000\000\000\
\\001\000\008\000\120\000\042\000\014\000\000\000\
\\001\000\008\000\122\000\042\000\122\000\000\000\
\\001\000\008\000\123\000\039\000\123\000\040\000\123\000\042\000\123\000\000\000\
\\001\000\008\000\124\000\038\000\007\000\039\000\124\000\040\000\124\000\
\\042\000\124\000\000\000\
\\001\000\008\000\125\000\040\000\125\000\042\000\125\000\000\000\
\\001\000\008\000\126\000\039\000\011\000\040\000\126\000\042\000\126\000\000\000\
\\001\000\008\000\127\000\042\000\127\000\000\000\
\\001\000\008\000\128\000\040\000\022\000\042\000\128\000\000\000\
\\001\000\008\000\009\000\000\000\
\\001\000\009\000\163\000\000\000\
\\001\000\009\000\079\000\000\000\
\\001\000\010\000\015\000\000\000\
\\001\000\010\000\023\000\000\000\
\\001\000\010\000\025\000\000\000\
\\001\000\010\000\039\000\000\000\
\\001\000\010\000\045\000\000\000\
\\001\000\010\000\077\000\000\000\
\\001\000\010\000\107\000\000\000\
\\001\000\019\000\000\000\020\000\000\000\000\000\
\\001\000\019\000\116\000\020\000\116\000\000\000\
\\001\000\019\000\117\000\020\000\117\000\000\000\
\\001\000\019\000\121\000\000\000\
\\001\000\019\000\129\000\000\000\
\\001\000\019\000\130\000\041\000\075\000\000\000\
\\001\000\019\000\131\000\000\000\
\\001\000\019\000\132\000\041\000\027\000\000\000\
\\001\000\019\000\133\000\000\000\
\\001\000\019\000\134\000\041\000\041\000\000\000\
\\001\000\019\000\161\000\020\000\161\000\036\000\161\000\037\000\161\000\000\000\
\\001\000\019\000\162\000\020\000\162\000\036\000\162\000\037\000\162\000\000\000\
\\001\000\019\000\024\000\000\000\
\\001\000\019\000\028\000\000\000\
\\001\000\019\000\044\000\000\000\
\\001\000\019\000\076\000\000\000\
\\001\000\019\000\080\000\000\000\
\\001\000\019\000\106\000\000\000\
\\001\000\025\000\052\000\026\000\051\000\032\000\050\000\033\000\049\000\
\\035\000\048\000\000\000\
\\001\000\036\000\110\000\000\000\
\\001\000\037\000\114\000\000\000\
\"
val actionRowNumbers =
"\048\000\053\000\050\000\045\000\
\\064\000\056\000\065\000\001\000\
\\052\000\057\000\075\000\043\000\
\\058\000\070\000\076\000\003\000\
\\004\000\074\000\002\000\046\000\
\\059\000\072\000\045\000\048\000\
\\077\000\060\000\000\000\081\000\
\\042\000\003\000\016\000\015\000\
\\028\000\029\000\003\000\003\000\
\\036\000\068\000\078\000\061\000\
\\044\000\066\000\047\000\070\000\
\\055\000\079\000\053\000\003\000\
\\003\000\003\000\003\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\005\000\005\000\
\\005\000\005\000\020\000\037\000\
\\039\000\040\000\041\000\035\000\
\\080\000\062\000\049\000\072\000\
\\069\000\073\000\000\000\082\000\
\\030\000\031\000\019\000\018\000\
\\021\000\005\000\022\000\026\000\
\\025\000\024\000\023\000\013\000\
\\011\000\009\000\007\000\014\000\
\\008\000\006\000\010\000\012\000\
\\027\000\017\000\034\000\033\000\
\\051\000\068\000\071\000\054\000\
\\053\000\038\000\067\000\083\000\
\\032\000\063\000"
val gotoT =
"\
\\002\000\113\000\003\000\004\000\009\000\003\000\010\000\002\000\
\\018\000\001\000\000\000\
\\007\000\006\000\000\000\
\\011\000\008\000\000\000\
\\016\000\011\000\017\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\014\000\000\000\
\\012\000\019\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\024\000\000\000\
\\000\000\
\\001\000\028\000\006\000\027\000\000\000\
\\000\000\
\\000\000\
\\004\000\036\000\000\000\
\\000\000\
\\000\000\
\\014\000\038\000\000\000\
\\016\000\040\000\017\000\010\000\000\000\
\\003\000\041\000\009\000\003\000\010\000\002\000\018\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\045\000\008\000\044\000\000\000\
\\000\000\
\\000\000\
\\001\000\028\000\006\000\066\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\068\000\006\000\067\000\000\000\
\\001\000\070\000\006\000\069\000\000\000\
\\000\000\
\\015\000\072\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\076\000\000\000\
\\000\000\
\\000\000\
\\007\000\079\000\000\000\
\\001\000\028\000\006\000\080\000\000\000\
\\001\000\028\000\006\000\081\000\000\000\
\\001\000\028\000\006\000\082\000\000\000\
\\001\000\028\000\006\000\083\000\000\000\
\\001\000\084\000\000\000\
\\001\000\086\000\000\000\
\\001\000\087\000\000\000\
\\001\000\088\000\000\000\
\\001\000\089\000\000\000\
\\001\000\090\000\000\000\
\\001\000\091\000\000\000\
\\001\000\092\000\000\000\
\\001\000\093\000\000\000\
\\001\000\094\000\000\000\
\\001\000\095\000\000\000\
\\001\000\096\000\000\000\
\\001\000\097\000\000\000\
\\001\000\098\000\000\000\
\\001\000\099\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\106\000\000\000\
\\000\000\
\\000\000\
\\004\000\045\000\008\000\107\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\109\000\000\000\
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
\\000\000\
\\015\000\110\000\000\000\
\\000\000\
\\000\000\
\\007\000\111\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 114
val numrules = 53
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
 | IDENT of unit ->  (string) | DeclarationSeq of unit ->  (DECSEQ)
 | ProcDef of unit ->  (PROCDEF) | ProcDecls of unit ->  (PROCDECLS)
 | BoolIDlist of unit ->  (VARDECSEC)
 | IntIDlist of unit ->  (VARDECSEC)
 | RatIDlist of unit ->  (VARDECSEC)
 | BoolVarDecls of unit ->  (VARDECSEC)
 | IntVarDecls of unit ->  (VARDECSEC)
 | RatVarDecls of unit ->  (VARDECSEC)
 | VarDecls of unit ->  (VARDECSEC)
 | comSeqInBrace of unit ->  (COMMANDSEQ)
 | commandSeq of unit ->  (COMMANDSEQ) | boolExp of unit ->  (BOOLEXP)
 | command of unit ->  (COMMAND) | Block of unit ->  (BLOCK)
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
fn (T 0) => "LPAREN"
  | (T 1) => "RPAREN"
  | (T 2) => "DIV"
  | (T 3) => "MUL"
  | (T 4) => "ADD"
  | (T 5) => "SUB"
  | (T 6) => "MOD"
  | (T 7) => "LBRACE"
  | (T 8) => "RBRACE"
  | (T 9) => "IDENT"
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
  | (T 37) => "RATIONAL"
  | (T 38) => "INTEGER"
  | (T 39) => "BOOLEAN"
  | (T 40) => "COMMA"
  | (T 41) => "PROCEDURE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35)
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (fileName):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Block Block1, Block1left, Block1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  (
Block as Block1) = Block1 ()
 in (scopeNumber := ~1; printScopeNumbers(Block))
end; ()))
 in ( LrTable.NT 1, ( result, Block1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.commandSeq commandSeq1, _, commandSeq1right)
) :: ( _, ( MlyValue.DeclarationSeq DeclarationSeq1, 
DeclarationSeq1left, _)) :: rest671)) => let val  result = 
MlyValue.Block (fn _ => let val  (DeclarationSeq as DeclarationSeq1) =
 DeclarationSeq1 ()
 val  (commandSeq as commandSeq1) = commandSeq1 ()
 in (
scopeNumber := !scopeNumber + 1; block(DeclarationSeq, commandSeq, !scopeNumber:: getChildrenScopes(DeclarationSeq))
)
end)
 in ( LrTable.NT 2, ( result, DeclarationSeq1left, commandSeq1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.ProcDecls ProcDecls1, _, ProcDecls1right))
 :: ( _, ( MlyValue.VarDecls VarDecls1, VarDecls1left, _)) :: rest671)
) => let val  result = MlyValue.DeclarationSeq (fn _ => let val  (
VarDecls as VarDecls1) = VarDecls1 ()
 val  (ProcDecls as ProcDecls1) = ProcDecls1 ()
 in (decSeq(VarDecls, ProcDecls))
end)
 in ( LrTable.NT 17, ( result, VarDecls1left, ProcDecls1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.ProcDecls ProcDecls1, _, ProcDecls1right))
 :: _ :: ( _, ( MlyValue.ProcDef ProcDef1, ProcDef1left, _)) :: 
rest671)) => let val  result = MlyValue.ProcDecls (fn _ => let val  (
ProcDef as ProcDef1) = ProcDef1 ()
 val  (ProcDecls as ProcDecls1) = ProcDecls1 ()
 in (procDecls(ProcDef, ProcDecls))
end)
 in ( LrTable.NT 15, ( result, ProcDef1left, ProcDecls1right), rest671
)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.ProcDecls (fn _ => (
emptyDec))
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( MlyValue.Block Block1, _, Block1right)) :: ( _, ( 
MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, PROCEDURE1left, _)) :: 
rest671)) => let val  result = MlyValue.ProcDef (fn _ => let val  (
IDENT as IDENT1) = IDENT1 ()
 val  (Block as Block1) = Block1 ()
 in ( procDef(IDENT,Block) )
end)
 in ( LrTable.NT 16, ( result, PROCEDURE1left, Block1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.BoolVarDecls BoolVarDecls1, _, 
BoolVarDecls1right)) :: ( _, ( MlyValue.IntVarDecls IntVarDecls1, _, _
)) :: ( _, ( MlyValue.RatVarDecls RatVarDecls1, RatVarDecls1left, _))
 :: rest671)) => let val  result = MlyValue.VarDecls (fn _ => let val 
 (RatVarDecls as RatVarDecls1) = RatVarDecls1 ()
 val  (IntVarDecls as IntVarDecls1) = IntVarDecls1 ()
 val  (BoolVarDecls as BoolVarDecls1) = BoolVarDecls1 ()
 in (RatVarDecls @ IntVarDecls @ BoolVarDecls)
end)
 in ( LrTable.NT 8, ( result, RatVarDecls1left, BoolVarDecls1right), 
rest671)
end
|  ( 7, ( ( _, ( _, _, EOL1right)) :: ( _, ( MlyValue.RatIDlist 
RatIDlist1, _, _)) :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _
, RATIONAL1left, _)) :: rest671)) => let val  result = 
MlyValue.RatVarDecls (fn _ => let val  IDENT1 = IDENT1 ()
 val  (RatIDlist as RatIDlist1) = RatIDlist1 ()
 in (rational(IDENT1) :: RatIDlist)
end)
 in ( LrTable.NT 9, ( result, RATIONAL1left, EOL1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.RatVarDecls (fn _ =>
 ([]))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( _, _, EOL1right)) :: ( _, ( MlyValue.IntIDlist 
IntIDlist1, _, _)) :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _
, INTEGER1left, _)) :: rest671)) => let val  result = 
MlyValue.IntVarDecls (fn _ => let val  IDENT1 = IDENT1 ()
 val  (IntIDlist as IntIDlist1) = IntIDlist1 ()
 in (integer(IDENT1) :: IntIDlist)
end)
 in ( LrTable.NT 10, ( result, INTEGER1left, EOL1right), rest671)
end
|  ( 10, ( rest671)) => let val  result = MlyValue.IntVarDecls (fn _
 => ([]))
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 11, ( ( _, ( _, _, EOL1right)) :: ( _, ( MlyValue.BoolIDlist 
BoolIDlist1, _, _)) :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, (
 _, BOOLEAN1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolVarDecls (fn _ => let val  IDENT1 = IDENT1 ()
 val  (BoolIDlist as BoolIDlist1) = BoolIDlist1 ()
 in (boolean(IDENT1) :: BoolIDlist)
end)
 in ( LrTable.NT 11, ( result, BOOLEAN1left, EOL1right), rest671)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.BoolVarDecls (fn _
 => ([]))
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( MlyValue.BoolIDlist BoolIDlist1, _, BoolIDlist1right
)) :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, COMMA1left, _)
) :: rest671)) => let val  result = MlyValue.BoolIDlist (fn _ => let
 val  IDENT1 = IDENT1 ()
 val  (BoolIDlist as BoolIDlist1) = BoolIDlist1 ()
 in (boolean(IDENT1) :: BoolIDlist)
end)
 in ( LrTable.NT 14, ( result, COMMA1left, BoolIDlist1right), rest671)

end
|  ( 14, ( rest671)) => let val  result = MlyValue.BoolIDlist (fn _ =>
 ([]))
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 15, ( ( _, ( MlyValue.RatIDlist RatIDlist1, _, RatIDlist1right))
 :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, COMMA1left, _))
 :: rest671)) => let val  result = MlyValue.RatIDlist (fn _ => let
 val  IDENT1 = IDENT1 ()
 val  (RatIDlist as RatIDlist1) = RatIDlist1 ()
 in (rational(IDENT1) :: RatIDlist)
end)
 in ( LrTable.NT 12, ( result, COMMA1left, RatIDlist1right), rest671)

end
|  ( 16, ( rest671)) => let val  result = MlyValue.RatIDlist (fn _ =>
 ([]))
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.IntIDlist IntIDlist1, _, IntIDlist1right))
 :: ( _, ( MlyValue.IDENT IDENT1, _, _)) :: ( _, ( _, COMMA1left, _))
 :: rest671)) => let val  result = MlyValue.IntIDlist (fn _ => let
 val  IDENT1 = IDENT1 ()
 val  (IntIDlist as IntIDlist1) = IntIDlist1 ()
 in (integer(IDENT1) :: IntIDlist)
end)
 in ( LrTable.NT 13, ( result, COMMA1left, IntIDlist1right), rest671)

end
|  ( 18, ( rest671)) => let val  result = MlyValue.IntIDlist (fn _ =>
 ([]))
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (int(add(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (rat(add(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (int(sub(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (rat(sub(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (int(mul(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (rat(mul(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (int(divOp(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (rat(divOp(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (int(modOp(exp1, exp2)))
end)
 in ( LrTable.NT 0, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.NUMBA NUMBA1, NUMBA1left, NUMBA1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (NUMBA
 as NUMBA1) = NUMBA1 ()
 in (intType(BigInt.getBigInt(NUMBA)))
end)
 in ( LrTable.NT 0, ( result, NUMBA1left, NUMBA1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.DECI DECI1, DECI1left, DECI1right)) :: 
rest671)) => let val  result = MlyValue.exp (fn _ => let val  (DECI
 as DECI1) = DECI1 ()
 in (ratType(Rational.fromDecimal(DECI)))
end)
 in ( LrTable.NT 0, ( result, DECI1left, DECI1right), rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _
 :: ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671))
 => let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (andOp(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 32, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _
 :: ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671))
 => let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (orOp(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.boolExp boolExp1, _, boolExp1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  (boolExp as boolExp1) = boolExp1 ()
 in (notOp(boolExp))
end)
 in ( LrTable.NT 5, ( result, NOT1left, boolExp1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (eq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (neq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (lt(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (leq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (gt(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.boolExp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (geq(exp1, exp2))
end)
 in ( LrTable.NT 5, ( result, exp1left, exp2right), rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.boolExp 
boolExp1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.boolExp (fn _ => let val  (boolExp as boolExp1
) = boolExp1 ()
 in (boolExp)
end)
 in ( LrTable.NT 5, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 41, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.boolExp (fn _ => (TRUE))
 in ( LrTable.NT 5, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 42, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.boolExp (fn _ => (FALSE))
 in ( LrTable.NT 5, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _
 :: ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671))
 => let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (beq(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 44, ( ( _, ( MlyValue.boolExp boolExp2, _, boolExp2right)) :: _
 :: ( _, ( MlyValue.boolExp boolExp1, boolExp1left, _)) :: rest671))
 => let val  result = MlyValue.boolExp (fn _ => let val  boolExp1 = 
boolExp1 ()
 val  boolExp2 = boolExp2 ()
 in (bneq(boolExp1, boolExp2))
end)
 in ( LrTable.NT 5, ( result, boolExp1left, boolExp2right), rest671)

end
|  ( 45, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( 
MlyValue.comSeqInBrace comSeqInBrace1, _, _)) :: _ :: ( _, ( 
MlyValue.command command1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: 
rest671)) => let val  result = MlyValue.commandSeq (fn _ => let val  (
command as command1) = command1 ()
 val  (comSeqInBrace as comSeqInBrace1) = comSeqInBrace1 ()
 in (cons(command,comSeqInBrace))
end)
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 46, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _))
 :: rest671)) => let val  result = MlyValue.commandSeq (fn _ => (empty
))
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.comSeqInBrace comSeqInBrace1, _, 
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
|  ( 48, ( rest671)) => let val  result = MlyValue.comSeqInBrace (fn _
 => (empty))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 49, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.commandSeq 
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
|  ( 50, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let val  result
 = MlyValue.command (fn _ => let val  (exp as exp1) = exp1 ()
 in (PrintCMD(exp))
end)
 in ( LrTable.NT 3, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 51, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.boolExp 
boolExp1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let
 val  result = MlyValue.command (fn _ => let val  (boolExp as boolExp1
) = boolExp1 ()
 in (PrintBool(boolExp))
end)
 in ( LrTable.NT 3, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 52, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.command 
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
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun IDENT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.IDENT (fn () => i),p1,p2))
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
fun RATIONAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun INTEGER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLEAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
end
end
