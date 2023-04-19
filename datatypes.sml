(* use "rational.sml"; *)
structure DataTypes =
struct
    datatype EXP= 
                  add of EXP * EXP | sub of EXP * EXP | mul of EXP * EXP | divOp of EXP * EXP 
                |   ratadd of EXP * EXP | ratsub of EXP * EXP | ratmul of EXP * EXP | ratdivOp of EXP * EXP
                | modOp of EXP * EXP    
                | int of EXP |  rat of EXP 
                (* | bool of EXP | eq of EXP*EXP | neq of EXP*EXP | andOp of EXP*EXP | lt of EXP*EXP *)
                | ID of string | intType of BigInt.bigint | ratType of Rational.rational
                | STRING of string | CHAR of char | boolType of bool;
    
    datatype BOOLEXP = eq of EXP*EXP | neq of EXP*EXP | andOp of BOOLEXP*BOOLEXP 
        | orOp of BOOLEXP*BOOLEXP | notOp of BOOLEXP | TRUE | FALSE | lt of EXP * EXP 
        | gt of EXP * EXP | leq of EXP * EXP | geq of EXP * EXP | beq of BOOLEXP * BOOLEXP | bneq of BOOLEXP * BOOLEXP;

    datatype COMMANDSEQ = empty | cons of COMMAND * COMMANDSEQ
    and  COMMAND =  PrintCMD of EXP | ConditionalCMD of BOOLEXP * COMMANDSEQ * COMMANDSEQ | PrintBool of BOOLEXP

    (* type PROCDEF = string*COMMANDSEQ *)

    (* datatype curType = integer | rational | boolean  *)
    
    datatype VARDEC = boolean of string | rational of string | integer of string
    type VARDECSEC = VARDEC list

    datatype BLOCK = block of DECSEQ * COMMANDSEQ
    and DECSEQ = decSeq of VARDECSEC * PROCDECLS 
    and PROCDECLS =  emptyDec | procDecls of PROCDEF * PROCDECLS 
    and PROCDEF = procDef of string * BLOCK

    exception typeMismatched;
    exception divisionByZeroError;
    
    fun Add (intType(a),intType(b)) = intType(BigInt.add(a,b)) 
        |   Add (_,_) = raise typeMismatched;
        fun ratAdd (ratType(a),ratType(b)) = ratType(Rational.add(a,b))
        |   ratAdd (_,_) = raise typeMismatched;
        fun Sub (intType(a),intType(b)) = intType(BigInt.sub(a,b))
        |   Sub (_,_) = raise typeMismatched;
        fun ratSub (ratType(a),ratType(b)) = ratType(Rational.subtract(a,b))
        |   ratSub (_,_) = raise typeMismatched;
        fun Mul (intType(a),intType(b)) = intType(BigInt.mult(a,b))
        |   Mul (_,_) = raise typeMismatched;
        fun ratMul (ratType(a),ratType(b)) = ratType(Rational.multiply(a,b))
        |   ratMul (_,_) = raise typeMismatched;
        fun Div (intType(a),intType(b)) = intType(#1 (BigInt.divide(a,b)))
        |   Div (_,_) = raise typeMismatched;
        fun ratDiv (ratType(a),ratType(b)) = ratType(valOf (Rational.divide(a,b)))
        |   ratDiv (_,_) = raise typeMismatched;
        fun Mod (intType(a),intType(b)) = intType(#2 (BigInt.divide(a,b)))
        |   Mod (_,_) = raise typeMismatched;

    fun lessThan(a,b) = 
        case (a,b) of
            (intType(a),intType(b)) =>  (BigInt.leq(a,b) = 1)
        |   (ratType(a),ratType(b)) =>  Rational.less(a,b)
        |   _ => raise typeMismatched
    fun greaterThan(a,b) = 
        case (a,b) of
            (intType(a),intType(b)) =>  (BigInt.leq(a,b) = 0)
        |   (ratType(a),ratType(b)) =>  (not (Rational.less(a,b))) andalso (not (Rational.equal(a,b)))
        |   _ => raise typeMismatched
    fun isEqual(a,b) = 
        case (a,b) of
            (intType(a),intType(b)) =>  (BigInt.leq(a,b) = 2)
        |   (ratType(a),ratType(b)) =>  ((Rational.equal(a,b)))
        |   _ => raise typeMismatched
   
        


    fun eval((intType a) | int(intType a)) = intType (a)
        |   eval((ratType a) | rat(ratType a)) = ratType (a)
        |   eval(int(add(a,b))) = Add(eval(a),eval(b))
        |   eval(rat(add(a,b))) = ratAdd(eval(a),eval(b))
        |   eval(rat(sub(a,b))) = ratSub(eval(a),eval(b))
        |   eval(rat(mul(a,b))) = ratMul(eval(a),eval(b))
        |   eval(rat(divOp(a,b))) = ratDiv(eval(a),eval(b))
        |   eval(int(sub(a,b))) = Sub(eval(a),eval(b))
        |   eval(int(mul(a,b))) = Mul(eval(a),eval(b))
        |   eval(int(divOp(a,b))) = Div(eval(a),eval(b))
        |   eval(int(modOp(a,b))) = Mod(eval(a),eval(b))
        (* |   eval((boolType a) | bool(boolType a)) = boolType (a)
        |   eval(bool(lt(a,b))) = boolType(lessThan(eval(a),eval(b))) *)
    
   

    fun evalBool(TRUE) = true
        |   evalBool(FALSE) = false
        |   evalBool(eq(a,b)) =  isEqual(eval(a),eval(b))
        |   evalBool(neq(a,b)) = not (eval(a) = eval(b))
        |   evalBool(andOp(a,b)) = (evalBool(a) andalso evalBool(b))
        |   evalBool(orOp(a,b)) = (evalBool(a) orelse evalBool(b))
        |   evalBool(notOp(a)) = (not (evalBool(a)))
        |   evalBool(lt(a,b)) = lessThan(eval(a),eval(b))
        |   evalBool(gt(a,b)) = greaterThan(eval(a),eval(b))
        |   evalBool(leq(a,b)) =  let val A = eval(a); val B = eval(b); in (lessThan(A,B) orelse (A = B)) end
        |   evalBool(geq(a,b)) =  not (lessThan(eval(a),eval(b)))
        |   evalBool(beq(a,b)) = (evalBool(a) = evalBool(b))
        |   evalBool(bneq(a,b)) = not (evalBool(a) = evalBool(b))


    
    
    
    fun runCMD(PrintCMD(a)) = 
        let 
            val b = eval(a); 
            fun printHelper(intType(a)) = print(implode (a))
            |   printHelper(ratType(a)) = print(Rational.showRat(a))
            |   printHelper(boolType(a)) = print(Bool.toString(a))
            in 
                (printHelper(b); print("\n")) (*printing a new line for ease of viewing printed text*) (*perhaps gotta CHANGE since no newline prints were mentioned in assighnment*)
            end
    |   runCMD(PrintBool(a)) = (print(Bool.toString(evalBool(a))); print("\n"))
    |   runCMD(ConditionalCMD(a,b,c)) = if (evalBool(a)) then runCMDSeq(b) else runCMDSeq(c)

    and runCMDSeq(empty) = ()
    |   runCMDSeq(cons(a,b)) = (runCMD(a); runCMDSeq(b))

end;
