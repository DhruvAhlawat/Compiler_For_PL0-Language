(* val a = DataTypes.intType(BigInt.getBigInt("1"));
val b = DataTypes.intType(BigInt.getBigInt("2"));
val c = DataTypes.eval(DataTypes.int(DataTypes.add(a,b)));

DataTypes.eval(DataTypes.int(DataTypes.add(a,c)));

(* val a = DataTypes.intType(BigInt.getBigInt("1")); *)

       
(*    
    (* fun evalInt(int a) = int(a)
    |   evalInt(rat a) = raise typeMismatched
    |   evalInt(add(a,b)) = int(BigInt.add(evalInt(a),evalInt(b)))
    |   evalInt(sub(a,b)) = int(BigInt.sub(evalInt(a),evalInt(b)))
    |   evalInt(mul(a,b)) = int(BigInt.mult(evalInt(a),evalInt(b)))
    |   evalInt(divOp(a,b)) = int(#1 (BigInt.divide(evalInt(a),evalInt(b))))
    |   evalInt(modOp(a,b)) = int(#2 (BigInt.divide(evalInt(a),evalInt(b))))
     *)
    (* fun evalRat(int a) = raise typeMismatched
    |   evalRat(rat a) = a
    |   evalRat(ratadd(a,b)) = Rational.add(evalRat(a),evalRat(b))
    |   evalRat(ratsub(a,b)) = Rational.subtract(evalRat(a),evalRat(b))
    |   evalRat(ratmul(a,b)) = Rational.multiply(evalRat(a),evalRat(b))
    |   evalRat(ratdivOp(a,b)) = 
        let 
            val c = Rational.divide(evalRat(a),evalRat(b));
            fun doRatDiv(SOME(a)) = a
            |   doRatDiv(NONE) = raise divisionByZeroError;
        in
            doRatDiv(c)
        end;  *)
    
    (* fun evalInt((int a, INT)) = (int(a),INT)
    | evalInt((rat a, INT)) = raise typeMismatched
    | evalInt(add(a,b),INT) = 
    let 
        val int(c) = #1 (evalInt(a,INT));
        val int(d) = #1 (evalInt(b,INT));
            in
        (int(BigInt.add(c,d)),INT)
    end; *)
    (* fun evalRat((rat a, RAT)) = (rat(a), RAT)
    | evalRat((int a, INT)) = raise typeMismatched

    fun eval((a,INT)) = evalInt((a,INT))
    | eval((a,RAT)) = evalRat((a,RAT)) *)

    

    (* fun eval(rat a) = a
        |   eval(add(a,b)) = Rational.add(eval(a),eval(b))
        |   eval(sub(a,b)) = Rational.subtract(eval(a),eval(b))
        |   eval(mul(a,b)) = Rational.multiply(eval(a),eval(b))
        |   eval(divOp(a,b)) = let val c = Rational.divide(eval(a),eval(b)); in if(c = NONE) then raise divisionByZeroError else valOf(c) end
         *)
    (* fun evalBool(TRUE) = true
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
        |   runCMDSeq(cons(h,t)) = (runCMD(h); runCMDSeq(t)); runs it in sequence, but in the reverse order currently *) *)