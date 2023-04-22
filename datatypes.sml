(* use "rational.sml"; *)
(* open SymbolTable *)
(* use "symbolTable.sml" *)
val cmdline = TextIO.stdIn; 

structure DataTypes =
struct
    datatype EXP= 
                  add of EXP * EXP | sub of EXP * EXP | mul of EXP * EXP | divOp of EXP * EXP 
                |   ratadd of EXP * EXP | ratsub of EXP * EXP | ratmul of EXP * EXP | ratdivOp of EXP * EXP
                | modOp of EXP * EXP | makeRat of EXP * EXP | inverse of EXP
                | int of EXP |  rat of EXP | bool of EXP | neg of EXP
                (* | bool of EXP | eq of EXP*EXP | neq of EXP*EXP | andOp of EXP*EXP | lt of EXP*EXP *)
                | var of string | intType of BigInt.bigint | ratType of Rational.rational | boolType of bool 
                | eq of EXP*EXP | neq of EXP*EXP | andOp of EXP*EXP 
                | orOp of EXP*EXP | notOp of EXP | TRUE | FALSE | lt of EXP * EXP 
                | gt of EXP * EXP | leq of EXP * EXP | geq of EXP * EXP 
            

    datatype COMMANDSEQ = empty | cons of COMMAND * COMMANDSEQ
    and  COMMAND =  PrintCMD of EXP | ConditionalCMD of EXP * COMMANDSEQ * COMMANDSEQ 
    | AssignCMD of string * EXP | AssignBoolCMD  of string * EXP | WhileCMD of EXP * COMMANDSEQ | CallCMD of string 
    | PrintDecCMD of EXP | ReadCMD of string

    (* type PROCDEF = string*COMMANDSEQ *)

    (* datatype curType = integer | rational | boolean  *)
    
    datatype VARDEC = boolean of string | rational of string | integer of string
    type VARDECSEC = VARDEC list

    datatype BLOCK = block of DECSEQ * COMMANDSEQ * int * int list ref (*the int list is for the list of parent nodes*)
    and DECSEQ = decSeq of VARDECSEC * PROCDECLS 
    and PROCDECLS =  emptyDec | procDecls of PROCDEF * PROCDECLS 
    and PROCDEF = procDef of string * BLOCK 

(*Symbol table stuff*)
    type VarSymbol = EXP * int      (*EXP denotes the type of the variable along with its value, whereas int stores the scope of the variable*)
    type ProcSymbol = PROCDEF * int (*EXP denotes the type of the variable along with its value, whereas int stores the scope of the variable*)
    (* type VarBool = BOOLEXP * int    BOOLEXP denotes the type of the variable along with its value, whereas int stores the scope of the variable *)
    exception VariableNotDeclared;
    exception VarIncorrectTypeOrNotDeclared;
    val varMap : (string, VarSymbol list) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(50, Fail "not declared")
    val procMap : (string, ProcSymbol list) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(50, Fail "not declared")
    (* val boolMap : (string, bool) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(50, Fail "not declared") *)


    fun declareProc (name:string, proc:PROCDEF, scope:int) = 
        let
            val procList = 
            case HashTable.find procMap name of
                NONE => []
                | SOME l => l
        in
            HashTable.insert procMap  (name, ((proc, scope):ProcSymbol )::procList)
        end
    fun undeclareProc (name:string, proc:PROCDEF, scope:int) = 
        let
            val procList = 
            case HashTable.find procMap name of
                NONE => []
                | SOME l => l
        in
            HashTable.insert procMap  (name, tl(procList))
        end

    fun declareVar (name:string, exp:EXP, scope:int) = 
        let
            val varList = 
            case HashTable.find varMap name of
                NONE => []
                | SOME l => l
        in
            HashTable.insert varMap  (name, (exp, scope)::varList)
        end
    
    fun undeclareVar (name:string, exp:EXP, scope:int) = 
        let
            val varList = 
            case HashTable.find varMap name of
                NONE => []
                | SOME l => l
        in
            HashTable.insert varMap  (name, tl(varList))
        end


    (*This function removes the previous instance of the variable in the symbol table so we can append the new value to it*)
    (* fun declareBool (name:string, exp:BOOLEXP, scope:int) = 
        let
            val varList = 
            case HashTable.find boolMap name of
                NONE => []
                | SOME l => l
        in
            HashTable.insert boolMap  (name, (exp, scope)::varList)
        end

    fun undeclareBool (name:string, exp:BOOLEXP, scope:int) = 
        let
            val varList = 
            case HashTable.find boolMap name of
                NONE => []
                | SOME l => l
        in
            HashTable.insert boolMap  (name, tl(varList))
        end *)
    
    fun findAndRemovePreviousVarValue(a, b) = 
        let 
            fun findMatchingScope(((b,a):VarSymbol):: t, L , reqScope:int) = if(reqScope = a) then SOME((L@t, b))
            else
                findMatchingScope(t, (b,a) :: L, reqScope)
            |   findMatchingScope([], L, reqScope) = NONE
        in
            findMatchingScope(a, [], b)
        end

   
    
    fun getVarVal(name:string, scopes:int list) = 
        let
            val varList = HashTable.lookup varMap name;
            fun getVarExp(a, b) = 
                let 
                    fun findMatchingScope(((b,a):VarSymbol):: t, reqScope:int) = if(reqScope = a) then SOME(b)
                    else
                        findMatchingScope(t, reqScope)
                    |   findMatchingScope([], reqScope) = NONE
                in
                    findMatchingScope(a, b)
                end
            fun checkAllScopes([]) = raise VariableNotDeclared
            |   checkAllScopes((scope::t)) = 
                case getVarExp(varList, scope) of
                    NONE => checkAllScopes(t)
                    | SOME exp => exp
        in
            checkAllScopes(scopes)
        end


    fun assignVar (name:string, exp:EXP, scopes :int list) =
        let
            val varList = HashTable.lookup varMap name; (*returns an exception if the variable has not been declared*)
            fun checkAllScopes([]) = raise VariableNotDeclared
            |   checkAllScopes((scope::t)) = 
                case findAndRemovePreviousVarValue(varList, scope) of
                    NONE => checkAllScopes(t)
                    | SOME (l, prevExp) => (  case (prevExp, exp) of 
                            (intType(_), intType(_)) => HashTable.insert varMap (name, (exp, scope)::l)
                            | (ratType(_), ratType(_)) => HashTable.insert varMap (name, (exp, scope)::l)
                            | (boolType(_), boolType(_)) => HashTable.insert varMap (name, (exp, scope)::l)
                            | (_,_) => raise   VarIncorrectTypeOrNotDeclared)   
                            (*if we found a matching scope, then its previous value would have been deleted so we insert a new exp,scope in l and put it in the hashmap*)
                    
        in
            checkAllScopes(scopes)
        end


    fun declareVariables(L, scopeNumber) =
        let
            fun helper([]) = ()
            |   helper(rational(a) :: h) = (declareVar(a, ratType(Rational.fromDecimal("0.0(0)")), scopeNumber); helper(h))
            |   helper(integer(a) :: h) = (declareVar(a, intType(BigInt.getBigInt("0")), scopeNumber); helper(h))
            |   helper(boolean(a) :: h) = (declareVar(a, boolType(false), scopeNumber); helper(h))
        in
            helper(L)
        end;
    fun undeclareVariables(L, scopeNumber) =
        let
            fun helper([]) = ()
            |   helper(rational(a) :: h) = (undeclareVar(a, ratType(Rational.fromDecimal("0.0(0)")), scopeNumber); helper(h))
            |   helper(integer(a) :: h) = (undeclareVar(a, intType(BigInt.getBigInt("0")), scopeNumber); helper(h))
            |   helper(boolean(a) :: h) = (undeclareVar(a, boolType(false), scopeNumber); helper(h))
        in
            helper(L)
        end;
        
    fun declareProcedures(emptyDec, scopeNumber) = () (*do nothing incase no procedures declared*)
        |   declareProcedures(procDecls(procDef(f,b),h), scopeNumber) = 
            (declareProc(f, procDef(f,b), scopeNumber); declareProcedures(h, scopeNumber)) 
    fun undeclareProcedures(emptyDec, scopeNumer) = ()
    |   undeclareProcedures(procDecls(procDef(f,b),h), scopeNumber) = 
        (undeclareProc(f, procDef(f,b), scopeNumber); undeclareProcedures(h, scopeNumber))
(*end of symbolTable Stuff*)

    


    fun assignBlockScopes(block(decSeq(_,a),_,curScope,L), parentScopes) = 
        let
            fun helper(emptyDec) = []
            |   helper(procDecls(procDef(f,b),h))  = (assignBlockScopes(b,curScope::parentScopes); helper(h))
        in
            (L := parentScopes; helper(a))
        end;
        


    exception typeMismatched;
    exception divisionByZeroError;
    exception procedureNotDeclared;
    
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
        |   (boolType(a),boolType(b)) =>  (a = b)
        |   _ => raise typeMismatched
   
        
    fun getBoolOut(boolType(a)) = a
    |   getBoolOut(_) = raise typeMismatched;

    fun negate(a) =
        case a of
            ratType(a) => ratType(Rational.neg(a))
        |   intType(a) => intType(BigInt.neg(a))
    fun eval((intType a, c) | (int(intType a),c)) = intType (a)
        |   eval((ratType a, c) | (rat(ratType a),c)) = ratType (a)
        |   eval(int(add(a,b)),c) = Add(eval(a,c),eval(b,c))
        |   eval(rat(add(a,b)),c) = ratAdd(eval(a,c),eval(b,c))
        |   eval(rat(sub(a,b)),c) = ratSub(eval(a,c),eval(b,c))
        |   eval(rat(mul(a,b)),c) = ratMul(eval(a,c),eval(b,c))
        |   eval(rat(divOp(a,b)),c) = ratDiv(eval(a,c),eval(b,c))
        |   eval(int(sub(a,b)),c) = Sub(eval(a,c),eval(b,c))
        |   eval(int(mul(a,b)),c) = Mul(eval(a,c),eval(b,c))
        |   eval(int(divOp(a,b)),c) = Div(eval(a,c),eval(b,c))
        |   eval(int(modOp(a,b)),c) = Mod(eval(a,c),eval(b,c))
        |   eval(var(a),c) = getVarVal(a,c)
        |   eval(boolType(false),c) = boolType(false)
        |   eval(bool(boolType(a)),c) = boolType(a)
        |   eval(boolType(true),c) = boolType(true)
        |   eval(bool(eq(a,b)),c) = boolType(isEqual(eval(a,c),eval(b,c)))
        |   eval(bool(neq(a,b)),c) = boolType(not (isEqual(eval(a,c),eval(b,c))))
        |   eval(bool(andOp(a,b)),c) = boolType(getBoolOut(eval(a,c)) andalso getBoolOut(eval(b,c)))
        |   eval(bool(orOp(a,b)),c) = boolType(getBoolOut(eval(a,c)) orelse getBoolOut(eval(b,c)))
        |   eval(bool(notOp(a)),c) = boolType(not (getBoolOut(eval(a,c))))
        |   eval(bool(lt(a,b)),c) = boolType(lessThan(eval(a,c),eval(b,c)))
        |   eval(bool(gt(a,b)),c) = boolType(greaterThan(eval(a,c),eval(b,c)))
        |   eval(bool(leq(a,b)),c) = boolType(not (greaterThan(eval(a,c),eval(b,c))))
        |   eval(bool(geq(a,b)),c) = boolType(not (lessThan(eval(a,c),eval(b,c))))
        |   eval(rat(makeRat(a,b)),c) = 
            let 
                val aVal = eval(a,c)
                val bVal = eval(b,c)
            in
                case (aVal,bVal) of
                    (intType(a),intType(b)) => (ratType(valOf(Rational.make_rat(a,b)))) 
                |   _ => raise typeMismatched
            end
        |   eval(rat(inverse(a)), c) = 
            let
                val b = eval(a,c);
            in
                case b of 
                intType(d) => ratType(valOf(Rational.make_rat(BigInt.getBigInt("1"),d)))
                | ratType(d) => ratType(valOf(Rational.inverse(d)))
            end
        |   eval(neg(a),c) = negate(eval(a,c))
            
       
            
        (* |   eval((boolType a) | bool(boolType a)) = boolType (a)
        |   eval(bool(lt(a,b))) = boolType(lessThan(eval(a),eval(b))) *)
    fun getRidOfTrailingNewLine(str) = 
        let
            val strList = explode str
            fun removeTrailingNewLine([]) = []
            |   removeTrailingNewLine([#"\n"]) = []
            |   removeTrailingNewLine([x]) = [x]
            |   removeTrailingNewLine(x::y::rest) = x::removeTrailingNewLine(y::rest)
        in
            implode(removeTrailingNewLine(strList))
        end


    
    fun CallProcedure(name:string, parentScopes) = 
        let
            val procNamedList = HashTable.lookup procMap name
            fun findProc(procList, scope) = 
            case procList of
                [] => NONE
                | (proc, procScope)::rest => if (procScope = scope) then SOME(proc) else findProc(rest, scope)
            val curScopeList = parentScopes;

            fun checkAllScopes([]) = raise procedureNotDeclared
            |   checkAllScopes(cur :: rest) = 
                case findProc(procNamedList, cur) of
                    NONE => checkAllScopes(rest)
                    | SOME(proc) => proc
            
            val procDef(procName, blk) = checkAllScopes(curScopeList); (*this returns the procedure defined in the smallest enclosing scope*)
        in
            runBlock(blk)
        end
    
    and runCMD(PrintCMD(a), scopes) = 
        let 
            val b = eval(a,scopes); 
            fun printHelper(intType(a)) = print(implode (a))
            |   printHelper(ratType(a)) = print(Rational.showRat(a))
            |   printHelper(boolType(a)) = print(Bool.toString(a))
            in 
                (printHelper(b); print("\n")) (*printing a new line for ease of viewing printed text*) (*perhaps gotta CHANGE since no newline prints were mentioned in assighnment*)
            end
        |   runCMD(PrintDecCMD(a), scopes) =
            let 
                val b = eval(a,scopes); 
            in
            case b of 
                ratType(c) => (print(Rational.toDecimal(c)); print("\n"))
                |   _ => raise typeMismatched 
            end
        (* |   runCMD(PrintBool(a), scopes) = (print(Bool.toString(evalBool(a,scopes))); print("\n")) *)
        |   runCMD(ConditionalCMD(a,b,c), scopes) = if (getBoolOut(eval(a, scopes))) then runCMDSeq(b, scopes) else runCMDSeq(c, scopes)
        |   runCMD(AssignCMD(a,b), scopes) = assignVar(a, eval(b,scopes), scopes)
        |   runCMD(WhileCMD(a,b), scopes) = if(getBoolOut(eval(a,scopes))) then (runCMDSeq(b, scopes); runCMD(WhileCMD(a,b), scopes)) else ()
        |   runCMD(CallCMD(a), scopes) = CallProcedure(a, scopes)
        |   runCMD(ReadCMD(a),scopes) = 
            let 
                val SOME(value) = (print(a^":= ") ;TextIO.inputLine(TextIO.stdIn))
                val readValue = getRidOfTrailingNewLine(value);
            in
                (((assignVar(a, intType(BigInt.getBigInt(readValue)), scopes)) handle _ => 
                (assignVar(a, ratType(Rational.fromDecimal(readValue)), scopes))) handle _ => 
                (assignVar(a, boolType(valOf(Bool.fromString(readValue))), scopes))) handle _ => raise typeMismatched
            end

    and runCMDSeq(empty, scopes) = ()
        |   runCMDSeq(cons(a,b), scopes) = ((runCMD(a, scopes)) handle typeMismatched => (print("raised typeMismatched. \n please type check your code again. \n");raise typeMismatched); runCMDSeq(b, scopes))

    and runBlock(block(decSeq(a,b),c,curScope,parentScopes)) = 
        let
            
        in
            (declareVariables(a,curScope); declareProcedures(b,curScope); 
            runCMDSeq(c,curScope::(!parentScopes)); undeclareVariables(a,curScope); undeclareProcedures(b,curScope))
        end;
end;

