open HashTable
open DataTypes
type VarSymbol = DataTypes.EXP * int(*EXP denotes the type of the variable along with its value, whereas int stores the scope of the variable*)
type ProcSymbol = DataTypes.PROCDEF * int (*EXP denotes the type of the variable along with its value, whereas int stores the scope of the variable*)


val varMap : (string, VarSymbol list) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(50, Fail "not declared")
val procMap : (string, ProcSymbol list) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)(50, Fail "not declared")

fun insertVar (name:string, exp:DataTypes.EXP, scope:int) = 
let
    val varList = 
    case HashTable.find varMap name of
        NONE => []
        | SOME l => l
in
    HashTable.insert varMap  (name, (exp, scope)::varList)
end