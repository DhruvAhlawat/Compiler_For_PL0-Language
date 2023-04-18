open DataTypes

%%
%name Pi
%term ID of string | LPAREN | RPAREN | DIV | MUL | ADD | SUB | MOD | LBRACE | RBRACE
| RATADD | RATSUB | RATMUL | RATDIV | FALSE | TRUE
| NUMBA of string | DISPLAY | EOL | EOF | DECI of string | SHOWDECIMAL | PRINT | READ 
| AND | OR | NOT | LT | LEQ | GT | GEQ | NEQ | EQ
| IF | THEN | ELSE | FI

%nonterm exp of EXP  
| Program | Block | command of COMMAND | WhileCmd | boolExp of BOOLEXP | commandSequence of COMMAND list  


%pos int
%eop EOL EOF
%noshift EOF
%nonassoc EOF DISPLAY 

%nodefault
%verbose
%arg (fileName) : string
%start Program

%left IF THEN ELSE FI
%left OR
%left AND 
%left SHOWDECIMAL
%left ADD SUB RATADD RATSUB
%left DIV MUL RATMUL RATDIV MOD
%left LPAREN RPAREN EOL
%nonassoc EQ
%nonassoc NOT
%nonassoc LT LEQ GT GEQ NEQ
%left LBRACE RBRACE


%%
Program: command EOL (runCMD(command))

exp : exp ADD exp (add(exp1, exp2))
    | NUMBA  (rat(valOf(Rational.make_rat(BigInt.getBigInt(NUMBA), BigInt.getBigInt("1")))))
    | DECI   (rat(Rational.fromDecimal(DECI)))
    | LPAREN exp RPAREN (exp)

boolExp: boolExp AND boolExp (andOp(boolExp1, boolExp2))
        | boolExp OR boolExp (orOp(boolExp1, boolExp2))
        | NOT boolExp (notOp(boolExp))
        | exp EQ exp (eq(exp1, exp2))
        | exp NEQ exp (neq(exp1, exp2))
        | exp LT exp (lt(exp1, exp2))
        | exp LEQ exp (leq(exp1, exp2))
        | exp GT exp (gt(exp1, exp2))
        | exp GEQ exp (geq(exp1, exp2))
        | LPAREN boolExp RPAREN (boolExp)
        | TRUE (TRUE)
        | FALSE (FALSE)




        (*commandSeq : LBRACE commandSequence  RBRACE (commandSequence)

        commandSequence : command EOL commandSequence (command :: commandSequence1)
                        | command EOL ([command])
                        |   ([])*)


command : IF boolExp THEN command EOL ELSE command EOL FI (ConditionalCMD(boolExp, command1, command2))
|   PRINT exp (PrintCMD(exp))
|   LPAREN command RPAREN (command)

