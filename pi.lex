structure T = Tokens 

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) token

type lexarg = string
type arg = lexarg
val lin = ref 1;
val col = ref 0; (*lin and col are not really used, hence not updated*)
val eof = fn fileName => T.EOF (!lin,!col);

fun containsDecimal(x:string) =
    let
        val s = explode x;
        fun cd([]) = false
        |   cd(a::b) = if(a = #".") then true else cd(b);
    in
        cd(s)
    end;


%%
%full
%header (functor PiLexFun(structure Tokens: Pi_TOKENS));
%arg (fileName:string);

alpha = [A-Za-z];
num = [0-9.~()];
allAscii = . | "\n";
comments = "(*" {allAscii}* "*)";
integer=[~]?[0-9]+;
deci = [~]?[0-9]*\.[0-9]+ | [~]?[0-9]*\.[0-9]*\([0-9]+\);
alphanum = [0-9a-zA-Z];
spaces = [\ \t];
%%
".+." => (T.RATADD(!lin,!col));
".-." => (T.RATSUB(!lin,!col));
".*." => (T.RATMUL(!lin,!col));
"./." => (T.RATDIV(!lin,!col));
{spaces} => (continue());
"(" => (T.LPAREN(!lin,!col));
")" => (T.RPAREN(!lin,!col));
"{" => (T.LBRACE(!lin,!col));
"}" => (T.RBRACE(!lin,!col));
"," => (T.COMMA(!lin,!col));


"procedure" => (T.PROCEDURE(!lin,!col));
"rational" => (T.RATIONAL(!lin,!col));
"integer" => (T.INTEGER(!lin,!col));
"boolean" => (T.BOOLEAN(!lin,!col));
":=" => (T.ASSIGN(!lin,!col));

"&&" => (T.AND(!lin,!col));
"||" => (T.OR(!lin,!col));
"!" => (T.NOT(!lin,!col));
"=" => (T.EQ(!lin,!col));
">=" => (T.GEQ(!lin,!col));
"<=" => (T.LEQ(!lin,!col));
">" => (T.GT(!lin,!col));
"<" => (T.LT(!lin,!col));
"<>" => (T.NEQ(!lin,!col));

"while" => (T.WHILE(!lin,!col));
"do" => (T.DO(!lin,!col));
"od" => (T.OD(!lin,!col));
"read" => (T.READ(!lin,!col));
"if" => (T.IF(!lin,!col));
"fi" => (T.FI(!lin,!col));
"then" => (T.THEN(!lin,!col));
"else" => (T.ELSE(!lin,!col));
"print" => (T.PRINT(!lin,!col));

"false" => (T.FALSE(!lin,!col));
"true" => (T.TRUE(!lin,!col));

"\n"  => (lin:= !lin + 1; continue());
";" => (T.EOL(!lin,!col));  
"/" => (T.DIV(!lin,!col));
"*" => (T.MUL(!lin,!col));
"+" => (T.ADD(!lin,!col));
"-" => (T.SUB(!lin,!col));
"%" => (T.MOD(!lin,!col));

{integer} => (T.NUMBA(yytext,!lin,!col));
{deci} => (T.DECI(yytext,!lin,!col));
{alpha}{alphanum}* => (T.IDENT(yytext,!lin,!col));
{comments} => (continue());
. => (print("unmatched character typed "^yytext^"\n"); continue());