open DataTypes

val scopeNumber = ref ~1;

fun printScopeNumbers(block(decSeq(_,a),b,c,d)) = 
    let
        fun psn([]) = ()
        |   psn(x::xs) = (print(Int.toString(x)^" "); psn(xs));
        fun helper(emptyDec) = []
        |   helper(procDecls(procDef(f,b),h))  = (print(f); printScopeNumbers(b); helper(h))
    in
        (print(":"^(Int.toString(c))^" = "); psn(!d); print("\n"); helper(a))
    end;



%%
%name Pi
%term LPAREN | RPAREN | DIV | MUL | ADD | SUB | MOD | LBRACE | RBRACE | IDENT of string | VAR | INVERSE
| RATADD | RATSUB | RATMUL | RATDIV | FALSE | TRUE
| NUMBA of string | DISPLAY | EOL | EOF | DECI of string | PRINT | READ 
| AND | OR | NOT | LT | LEQ | GT | GEQ | NEQ | EQ 
| IF | THEN | ELSE | FI | RATIONAL | INTEGER | BOOLEAN | COMMA | PROCEDURE | ASSIGN | CALL | WHILE | DO | OD 
| TODECIMAL | SHOWDECIMAL | SHOWRAT | RAT | MAKE_RAT | FROMDECIMAL

%nonterm exp of EXP  
| Program | Block of BLOCK| command of COMMAND | WhileCmd | commandSeq of COMMANDSEQ | comSeqInBrace of COMMANDSEQ
| VarDecls of VARDECSEC | RatVarDecls of VARDECSEC | IntVarDecls of VARDECSEC  | BoolVarDecls of VARDECSEC 
| RatIDlist of VARDECSEC | IntIDlist of VARDECSEC | BoolIDlist of VARDECSEC
| ProcDecls of PROCDECLS | ProcDef of PROCDEF| DeclarationSeq of DECSEQ


%pos int
%eop EOL EOF
%noshift EOF
%nonassoc EOF DISPLAY 

%nodefault
%verbose
%arg (fileName) : string
%start Program

%left 
%left PRINT READ CALL 
%nonassoc ASSIGN
%nonassoc EQ
%left LT LEQ GT GEQ NEQ
%nonassoc NOT TRUE FALSE
%left IF THEN ELSE FI 
%left OR
%left AND 
%left SHOWDECIMAL
%left ADD SUB RATADD RATSUB
%left DIV MUL RATMUL RATDIV MOD
%left LPAREN RPAREN EOL
%left PROCEDURE IDENT 
%left LBRACE RBRACE
%left RAT MAKE_RAT INVERSE
%nonassoc RATIONAL INTEGER BOOLEAN COMMA


%%
Program : Block (assignBlockScopes(Block, [])  ; scopeNumber := ~1; runBlock(Block))

Block : DeclarationSeq commandSeq (scopeNumber := !scopeNumber + 1; block(DeclarationSeq, commandSeq, !scopeNumber, ref []))
     

DeclarationSeq : VarDecls ProcDecls (decSeq(VarDecls, ProcDecls)) 

ProcDecls : ProcDef EOL ProcDecls (procDecls(ProcDef, ProcDecls))
        | (emptyDec)
ProcDef : PROCEDURE IDENT Block ( procDef(IDENT,Block) )

VarDecls : RatVarDecls IntVarDecls BoolVarDecls (RatVarDecls @ IntVarDecls @ BoolVarDecls) 

RatVarDecls : RATIONAL IDENT RatIDlist EOL(rational(IDENT1) :: RatIDlist)
|   ([])
IntVarDecls : INTEGER IDENT IntIDlist EOL(integer(IDENT1) :: IntIDlist)
|   ([])
BoolVarDecls : BOOLEAN IDENT BoolIDlist EOL (boolean(IDENT1) :: BoolIDlist)
|   ([])
BoolIDlist : COMMA IDENT BoolIDlist (boolean(IDENT1) :: BoolIDlist)
| ([])
RatIDlist : COMMA IDENT RatIDlist (rational(IDENT1) :: RatIDlist)
| ([])
IntIDlist : COMMA IDENT IntIDlist (integer(IDENT1) :: IntIDlist)
| ([])


exp : exp ADD exp (int(add(exp1, exp2)))
    | exp RATADD exp (rat(add(exp1, exp2)))
    | exp SUB exp (int(sub(exp1, exp2)))
    | exp RATSUB exp (rat(sub(exp1, exp2)))
    | exp MUL exp (int(mul(exp1, exp2)))
    | exp RATMUL exp (rat(mul(exp1, exp2)))
    | exp DIV exp (int(divOp(exp1, exp2)))
    | exp RATDIV exp (rat(divOp(exp1, exp2)))
    | exp MOD exp (int(modOp(exp1, exp2)))
    | INVERSE exp (rat(inverse(exp)))
    | NUMBA (intType(BigInt.getBigInt(NUMBA)))
    | MAKE_RAT exp exp (rat(makeRat(exp1, exp2)))
    | MAKE_RAT LPAREN exp COMMA exp RPAREN (rat(makeRat(exp1,exp2)))
    | FROMDECIMAL DECI (ratType(Rational.fromDecimal(DECI)))
    | FROMDECIMAL LPAREN DECI RPAREN (ratType(Rational.fromDecimal(DECI)))
    | RAT exp (rat(makeRat(exp1, intType(BigInt.getBigInt("1")))))
    | DECI (ratType(Rational.fromDecimal(DECI)))
    | LPAREN exp RPAREN (exp)
    | IDENT (var(IDENT))
    | VAR IDENT (var(IDENT))
    | exp AND exp (bool(andOp(exp1, exp2)))
    | exp OR exp (bool(orOp(exp1, exp2)))
    | NOT exp (bool(notOp(exp)))
    | exp EQ exp (bool(eq(exp1, exp2)))
    | exp NEQ exp (bool(neq(exp1, exp2)))
    | exp LT exp (bool(lt(exp1, exp2)))
    | exp LEQ exp (bool(leq(exp1, exp2)))
    | exp GT exp (bool(gt(exp1, exp2)))
    | exp GEQ exp (bool(geq(exp1, exp2)))
    | TRUE (boolType(true))
    | FALSE (boolType(false))


    

        (*boolExp: boolExp AND boolExp (andOp(boolExp1, boolExp2))
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
                | boolExp EQ boolExp (beq(boolExp1, boolExp2))
                | boolExp NEQ boolExp (bneq(boolExp1, boolExp2))*)


commandSeq : LBRACE command EOL comSeqInBrace RBRACE (cons(command,comSeqInBrace))
            |   LBRACE RBRACE (empty)

comSeqInBrace : command EOL comSeqInBrace (cons(command, comSeqInBrace))
           |    (empty)

command : IF exp THEN commandSeq ELSE commandSeq FI (ConditionalCMD(exp, commandSeq1, commandSeq2))
        |   PRINT LPAREN exp RPAREN (PrintCMD(exp))
        |   PRINT LPAREN TODECIMAL exp RPAREN (PrintDecCMD(exp))
        |   PRINT LPAREN SHOWDECIMAL exp RPAREN (PrintDecCMD(exp))
        |   PRINT LPAREN SHOWRAT exp RPAREN (PrintCMD(exp))
        |   LPAREN command RPAREN (command)
        |   IDENT ASSIGN exp (AssignCMD(IDENT, exp))
        |   WHILE exp DO commandSeq OD (WhileCMD(exp, commandSeq))
        |   CALL IDENT (CallCMD(IDENT))
        |   READ LPAREN IDENT RPAREN (ReadCMD(IDENT))

