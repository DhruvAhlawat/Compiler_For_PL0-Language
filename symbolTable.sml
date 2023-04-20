open HashTable
structure SymbolTable =
struct
    type VarSymbol = DataTypes.EXP * int(*EXP denotes the type of the variable along with its value, whereas int stores the scope of the variable*)
    type ProcSymbol = DataTypes.PROCDEF * int (*EXP denotes the type of the variable along with its value, whereas int stores the scope of the variable*)

    exception VariableNotDeclared;
    exception VarIncorrectTypeOrNotDeclared;
    val varMap : (string, VarSymbol list) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(50, Fail "not declared")
    val procMap : (string, ProcSymbol list) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(50, Fail "not declared")

    fun declareVar (name:string, exp:DataTypes.EXP, scope:int) = 
    let
        val varList = 
        case HashTable.find varMap name of
            NONE => []
            | SOME l => l
    in
        HashTable.insert varMap  (name, (exp, scope)::varList)
    end

    (*This function removes the previous instance of the variable in the symbol table so we can append the new value to it*)
    fun findAndRemovePreviousVarValue(a, b) = 
    let 
        fun findMatchingScope(((b,a):VarSymbol):: t, L , reqScope:int) = if(reqScope = a) then SOME((L@t, b))
        else
            findMatchingScope(t, (b,a) :: L, reqScope)
        |   findMatchingScope([], L, reqScope) = NONE
    in
        findMatchingScope(a, [], b)
    end


    fun assignVar (name:string, exp:DataTypes.EXP, scopes :int list) =
    let
        val varList = HashTable.lookup varMap name; (*returns an exception if the variable has not been declared*)
        fun checkAllScopes([]) = raise VariableNotDeclared
        |   checkAllScopes((scope::t)) = 
            case findAndRemovePreviousVarValue(varList, scope) of
                NONE => checkAllScopes(t)
                | SOME (l, prevExp) => (  case (prevExp, exp) of 
                        (DataTypes.intType(_), DataTypes.intType(_)) => HashTable.insert varMap (name, (exp, scope)::l)
                        | (DataTypes.ratType(_), DataTypes.ratType(_)) => HashTable.insert varMap (name, (exp, scope)::l)
                        | (_,_) => raise   VarIncorrectTypeOrNotDeclared)   
                        (*if we found a matching scope, then its previous value would have been deleted so we insert a new exp,scope in l and put it in the hashmap*)
                
    in
        checkAllScopes(scopes)
    end
end;
    