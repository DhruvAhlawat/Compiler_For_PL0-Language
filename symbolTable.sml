open HashTable
open DataTypes

type VarSymbol = DataTypes.EXP * int (*EXP denotes the type of the variable along with its value, whereas int stores the scope of the variable*)
type ProcSymbol = DataTypes.EXP * int (*EXP denotes the type of the variable along with its value, whereas int stores the scope of the variable*)
val hashmap : (string,int list) HashTable.hash_table = HashTable.mkTable(HashString.hashString, op=)