(* use "rational.sml"; *)
structure DataTypes =
struct
    datatype EXP= 
                  add of EXP * EXP | sub of EXP * EXP | mul of EXP * EXP | divOp of EXP * EXP 
                | MOD of EXP * EXP 
                | ID of string | INT of int | rat of Rational.rational
                | STRING of string | CHAR of char | BOOL of bool
    
    datatype BOOLEXP = eq of EXP*EXP | neq of EXP*EXP | andOp of BOOLEXP*BOOLEXP 
        | orOp of BOOLEXP*BOOLEXP | notOp of BOOLEXP | TRUE | FALSE | lt of EXP * EXP 
        | gt of EXP * EXP | leq of EXP * EXP | geq of EXP * EXP 
    (*datatype COMMANDSEQ = COMMAND list*)
    datatype COMMANDSEQ = empty | cons of COMMAND * COMMANDSEQ
    and  COMMAND =  PrintCMD of EXP | ConditionalCMD of BOOLEXP * COMMANDSEQ * COMMANDSEQ

    (* datatype curType = INT of BigInt.bigint |
                          RAT of Rational.rational |
                          BOOL of bool |
                          NONE *)
    


    exception divisionByZeroError;

    fun eval(rat a) = a
        |   eval(add(a,b)) = Rational.add(eval(a),eval(b))
        |   eval(sub(a,b)) = Rational.subtract(eval(a),eval(b))
        |   eval(mul(a,b)) = Rational.multiply(eval(a),eval(b))
        |   eval(divOp(a,b)) = let val c = Rational.divide(eval(a),eval(b)); in if(c = NONE) then raise divisionByZeroError else valOf(c) end
    and evalBool(TRUE) = true
        |   evalBool(FALSE) = false
        |   evalBool(eq(a,b)) = (Rational.equal(eval(a),eval(b)))
        |   evalBool(neq(a,b)) = not (evalBool(eq(a,b)))
        |   evalBool(lt(a,b)) = Rational.less(eval(a),eval(b))
        |   evalBool(leq(a,b)) = let val aval = eval(a); val bval = eval(b); in (Rational.less(aval,bval)) orelse (Rational.equal(aval,bval)) end
        |   evalBool(gt(a,b)) = not (evalBool(leq(a,b)))
        |   evalBool(geq(a,b)) = not (evalBool(lt(a,b)))
        |   evalBool(orOp(a,b)) = (evalBool(a)) orelse (evalBool(b))
        |   evalBool(andOp(a,b)) = (evalBool(a)) andalso (evalBool(b))
    and runCMD(PrintCMD(a)) = print(Rational.showRat(eval(a)))
        |   runCMD(ConditionalCMD(a,b,c)) = if(evalBool(a)) then runCMDSeq(b) else runCMDSeq(c)    
    
    and runCMDSeq(empty) = ()
        |   runCMDSeq(cons(h,t)) = (runCMD(h); runCMDSeq(t)); (*runs it in sequence, but in the reverse order currently*)






end;

