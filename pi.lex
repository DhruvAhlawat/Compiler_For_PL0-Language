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
".+." => (col := !col + 3;T.RATADD(!lin,!col));
".-." => (col := !col + 3;T.RATSUB(!lin,!col));
".*." => (col := !col + 3;T.RATMUL(!lin,!col));
"./." => (col := !col + 3;T.RATDIV(!lin,!col));
{spaces} => (continue());
"(" => (col := !col + 1;T.LPAREN(!lin,!col));
")" => (col := !col + 1;T.RPAREN(!lin,!col));
"{" => (col := !col + 1; T.LBRACE(!lin,!col));
"}" => (col := !col + 1;T.RBRACE(!lin,!col));
"," => (col := !col + 1;T.COMMA(!lin,!col));


"procedure" => (T.PROCEDURE(!lin,!col));
"rational" => (T.RATIONAL(!lin,!col));
"integer" => (T.INTEGER(!lin,!col));
"boolean" => (T.BOOLEAN(!lin,!col));
":=" => (T.ASSIGN(!lin,!col));

"&&" => (col := !col + 2;T.AND(!lin,!col));
"||" => (col := !col + 2;T.OR(!lin,!col));
"!" => (col := !col + 1;T.NOT(!lin,!col));
"=" => (col := !col + 1;T.EQ(!lin,!col));
">=" => (col := !col + 2;T.GEQ(!lin,!col));
"<=" => (col := !col + 2;T.LEQ(!lin,!col));
">" => (col := !col + 1;T.GT(!lin,!col));
"<" => (col := !col + 1;T.LT(!lin,!col));
"<>" => (col := !col + 2;T.NEQ(!lin,!col));

"while" => (col := !col + 5;T.WHILE(!lin,!col));
"do" => (col := !col + 2;T.DO(!lin,!col));
"od" => (col := !col + 2;T.OD(!lin,!col));
"read" => (col := !col + 4;T.READ(!lin,!col));
"if" => (col := !col + 2;T.IF(!lin,!col));
"fi" => (col := !col + 2;T.FI(!lin,!col));
"then" => (col := !col + 4;T.THEN(!lin,!col));
"else" => (col := !col + 4;T.ELSE(!lin,!col));
"print" => (col := !col + 5;T.PRINT(!lin,!col));
"call" => (col := !col + 4;T.CALL(!lin,!col));


"false" => (col := !col + 5;T.FALSE(!lin,!col));
"true" => (col := !col + 4;T.TRUE(!lin,!col));

"\n"  => (lin:= !lin + 1; continue());
";" => (col := !col + 1;T.EOL(!lin,!col));  
"/" => (col := !col + 1;T.DIV(!lin,!col));
"*" => (col := !col + 1;T.MUL(!lin,!col));
"+" => (col := !col + 1;T.ADD(!lin,!col));
"-" => (col := !col + 1;T.SUB(!lin,!col));
"%" => (col := !col + 1;T.MOD(!lin,!col));

{integer} => (col := !col + String.size(yytext); T.NUMBA(yytext,!lin,!col));
{deci} => (col := !col + String.size(yytext); T.DECI(yytext,!lin,!col));
{alpha}{alphanum}* => (col := !col + String.size(yytext); T.IDENT(yytext,!lin,!col));
{comments} => (continue());
. => (print("unmatched character typed "^yytext^"\n"); continue());