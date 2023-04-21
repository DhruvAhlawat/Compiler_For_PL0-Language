functor PiLexFun(structure Tokens: Pi_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

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




      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
#[([(#"\^@",#"\b",1),
(#"\v",#"\^_",1),
(#"\"",#"$",1),
(#"'",#"'",1),
(#"?",#"@",1),
(#"[",#"`",1),
(#"\127",#"\255",1),
(#"\t",#"\t",2),
(#" ",#" ",2),
(#"\n",#"\n",3),
(#"!",#"!",4),
(#"%",#"%",5),
(#"&",#"&",6),
(#"(",#"(",7),
(#")",#")",8),
(#"*",#"*",9),
(#"+",#"+",10),
(#",",#",",11),
(#"-",#"-",12),
(#".",#".",13),
(#"/",#"/",14),
(#"0",#"9",15),
(#":",#":",16),
(#";",#";",17),
(#"<",#"<",18),
(#"=",#"=",19),
(#">",#">",20),
(#"A",#"Z",21),
(#"a",#"a",21),
(#"g",#"h",21),
(#"j",#"l",21),
(#"n",#"n",21),
(#"q",#"q",21),
(#"u",#"u",21),
(#"x",#"z",21),
(#"b",#"b",22),
(#"c",#"c",23),
(#"d",#"d",24),
(#"e",#"e",25),
(#"f",#"f",26),
(#"i",#"i",27),
(#"m",#"m",28),
(#"o",#"o",29),
(#"p",#"p",30),
(#"r",#"r",31),
(#"s",#"s",32),
(#"t",#"t",33),
(#"v",#"v",34),
(#"w",#"w",35),
(#"{",#"{",36),
(#"|",#"|",37),
(#"}",#"}",38),
(#"~",#"~",39)], []), ([], [55]), ([], [4, 55]), ([], [44]), ([], [23, 55]), ([], [50, 55]), ([(#"&",#"&",160)], [55]), ([(#"*",#"*",157)], [5, 55]), ([], [6, 55]), ([], [47, 55]), ([], [48, 55]), ([], [9, 55]), ([], [49, 55]), ([(#"(",#"(",42),
(#"*",#"*",149),
(#"+",#"+",150),
(#"-",#"-",151),
(#"/",#"/",152),
(#"0",#"9",43)], [55]), ([], [46, 55]), ([(#".",#".",40),
(#"0",#"9",41)], [51, 55]), ([(#"=",#"=",148)], [55]), ([], [45, 55]), ([(#"=",#"=",146),
(#">",#">",147)], [28, 55]), ([], [24, 55]), ([(#"=",#"=",145)], [27, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"n",47),
(#"p",#"z",47),
(#"o",#"o",139)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",136)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"n",47),
(#"p",#"z",47),
(#"o",#"o",135)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",132)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"e",47),
(#"g",#"h",47),
(#"j",#"q",47),
(#"s",#"z",47),
(#"f",#"f",120),
(#"i",#"i",121),
(#"r",#"r",122)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"e",47),
(#"g",#"m",47),
(#"o",#"z",47),
(#"f",#"f",108),
(#"n",#"n",109)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",101)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"c",47),
(#"e",#"z",47),
(#"d",#"d",100)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"q",47),
(#"s",#"z",47),
(#"r",#"r",89)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"d",47),
(#"f",#"z",47),
(#"a",#"a",79),
(#"e",#"e",80)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"g",47),
(#"i",#"z",47),
(#"h",#"h",66)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"g",47),
(#"i",#"n",47),
(#"p",#"s",47),
(#"u",#"z",47),
(#"h",#"h",54),
(#"o",#"o",55),
(#"t",#"t",56)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",52)], [53, 55]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"g",47),
(#"i",#"z",47),
(#"h",#"h",48)], [53, 55]), ([], [7, 55]), ([(#"|",#"|",46)], [55]), ([], [8, 55]), ([(#".",#".",40),
(#"0",#"9",41)], [55]), ([(#"(",#"(",42),
(#"0",#"9",43)], []), ([(#".",#".",40),
(#"0",#"9",41)], [51]), ([(#"0",#"9",44)], []), ([(#"(",#"(",42),
(#"0",#"9",43)], [52]), ([(#")",#")",45),
(#"0",#"9",44)], []), ([], [52]), ([], [22]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"h",47),
(#"j",#"z",47),
(#"i",#"i",49)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",50)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",51)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [30, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"q",47),
(#"s",#"z",47),
(#"r",#"r",53)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [43, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",64)], [53]), ([(#"0",#"9",47),
(#"A",#"C",47),
(#"E",#"Z",47),
(#"a",#"z",47),
(#"D",#"D",57)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [42, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",58)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"b",47),
(#"d",#"z",47),
(#"c",#"c",59)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"h",47),
(#"j",#"z",47),
(#"i",#"i",60)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"l",47),
(#"n",#"z",47),
(#"m",#"m",61)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",62)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",63)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [15, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"m",47),
(#"o",#"z",47),
(#"n",#"n",65)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [36, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"n",47),
(#"p",#"z",47),
(#"o",#"o",67)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"v",47),
(#"x",#"z",47),
(#"w",#"w",68)], [53]), ([(#"0",#"9",47),
(#"A",#"C",47),
(#"E",#"Q",47),
(#"S",#"Z",47),
(#"a",#"z",47),
(#"D",#"D",69),
(#"R",#"R",70)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",73)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",71)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"s",47),
(#"u",#"z",47),
(#"t",#"t",72)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [19, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"b",47),
(#"d",#"z",47),
(#"c",#"c",74)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"h",47),
(#"j",#"z",47),
(#"i",#"i",75)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"l",47),
(#"n",#"z",47),
(#"m",#"m",76)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",77)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",78)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [20, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"s",47),
(#"u",#"z",47),
(#"t",#"t",83)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",81)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"c",47),
(#"e",#"z",47),
(#"d",#"d",82)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [33, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"h",47),
(#"j",#"z",47),
(#"i",#"i",84)], [17, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"n",47),
(#"p",#"z",47),
(#"o",#"o",85)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"m",47),
(#"o",#"z",47),
(#"n",#"n",86)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",87)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",88)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [11, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"h",47),
(#"j",#"n",47),
(#"p",#"z",47),
(#"i",#"i",90),
(#"o",#"o",91)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"m",47),
(#"o",#"z",47),
(#"n",#"n",98)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"b",47),
(#"d",#"z",47),
(#"c",#"c",92)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",93)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"c",47),
(#"e",#"z",47),
(#"d",#"d",94)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"t",47),
(#"v",#"z",47),
(#"u",#"u",95)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"q",47),
(#"s",#"z",47),
(#"r",#"r",96)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",97)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [10, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"s",47),
(#"u",#"z",47),
(#"t",#"t",99)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [38, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [32, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"j",47),
(#"l",#"z",47),
(#"k",#"k",102)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",103)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47),
(#"_",#"_",104)], [53]), ([(#"r",#"r",105)], []), ([(#"a",#"a",106)], []), ([(#"t",#"t",107)], []), ([], [18]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [34, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"s",47),
(#"u",#"u",47),
(#"w",#"z",47),
(#"t",#"t",110),
(#"v",#"v",111)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",116)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",112)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"q",47),
(#"s",#"z",47),
(#"r",#"r",113)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"r",47),
(#"t",#"z",47),
(#"s",#"s",114)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",115)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [40, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"f",47),
(#"h",#"z",47),
(#"g",#"g",117)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",118)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"q",47),
(#"s",#"z",47),
(#"r",#"r",119)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [12, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [41, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [35, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"n",47),
(#"p",#"z",47),
(#"o",#"o",123)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"l",47),
(#"n",#"z",47),
(#"m",#"m",124)], [53]), ([(#"0",#"9",47),
(#"A",#"C",47),
(#"E",#"Z",47),
(#"a",#"z",47),
(#"D",#"D",125)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",126)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"b",47),
(#"d",#"z",47),
(#"c",#"c",127)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"h",47),
(#"j",#"z",47),
(#"i",#"i",128)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"l",47),
(#"n",#"z",47),
(#"m",#"m",129)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",130)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",131)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [16, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"r",47),
(#"t",#"z",47),
(#"s",#"s",133)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",134)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [37, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [31, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",137)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",138)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [39, 53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"n",47),
(#"p",#"z",47),
(#"o",#"o",140)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"k",47),
(#"m",#"z",47),
(#"l",#"l",141)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"d",47),
(#"f",#"z",47),
(#"e",#"e",142)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"b",#"z",47),
(#"a",#"a",143)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"m",47),
(#"o",#"z",47),
(#"n",#"n",144)], [53]), ([(#"0",#"9",47),
(#"A",#"Z",47),
(#"a",#"z",47)], [13, 53]), ([], [25]), ([], [26]), ([], [29]), ([], [14]), ([(#".",#".",156)], []), ([(#".",#".",155)], []), ([(#".",#".",154)], []), ([(#".",#".",153)], []), ([], [3]), ([], [1]), ([], [0]), ([], [2]), ([(#"\^@",#")",157),
(#"+",#"\255",157),
(#"*",#"*",158)], []), ([(#"\^@",#"(",157),
(#"+",#"\255",157),
(#")",#")",159),
(#"*",#"*",158)], []), ([(#"\^@",#")",157),
(#"+",#"\255",157),
(#"*",#"*",158)], [54]), ([], [21])]
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as (fileName:string)) () = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 3;T.RATADD(!lin,!col)))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 3;T.RATSUB(!lin,!col)))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 3;T.RATMUL(!lin,!col)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 3;T.RATDIV(!lin,!col)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.LPAREN(!lin,!col)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.RPAREN(!lin,!col)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1; T.LBRACE(!lin,!col)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.RBRACE(!lin,!col)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.COMMA(!lin,!col)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.PROCEDURE(!lin,!col)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RATIONAL(!lin,!col)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.INTEGER(!lin,!col)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.BOOLEAN(!lin,!col)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.ASSIGN(!lin,!col)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.TODECIMAL(!lin,!col)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.FROMDECIMAL(!lin,!col)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RAT(!lin,!col)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.MAKE_RAT(!lin,!col)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.SHOWRAT(!lin,!col)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.SHOWDECIMAL(!lin,!col)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.AND(!lin,!col)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.OR(!lin,!col)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.NOT(!lin,!col)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.EQ(!lin,!col)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.GEQ(!lin,!col)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.LEQ(!lin,!col)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.GT(!lin,!col)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.LT(!lin,!col)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.NEQ(!lin,!col)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 5;T.WHILE(!lin,!col)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.DO(!lin,!col)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.OD(!lin,!col)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 4;T.READ(!lin,!col)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.IF(!lin,!col)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.FI(!lin,!col)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 4;T.THEN(!lin,!col)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 4;T.ELSE(!lin,!col)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 5;T.PRINT(!lin,!col)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 4;T.CALL(!lin,!col)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 7;T.INVERSE(!lin,!col)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.FALSE(!lin,!col)))
fun yyAction42 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 2;T.TRUE(!lin,!col)))
fun yyAction43 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 3;T.VAR(!lin,!col)))
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lin:= !lin + 1; continue()))
fun yyAction45 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.EOL(!lin,!col)))
fun yyAction46 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.DIV(!lin,!col)))
fun yyAction47 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.MUL(!lin,!col)))
fun yyAction48 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.ADD(!lin,!col)))
fun yyAction49 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.SUB(!lin,!col)))
fun yyAction50 (strm, lastMatch : yymatch) = (yystrm := strm;
      (col := !col + 1;T.MOD(!lin,!col)))
fun yyAction51 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col := !col + String.size(yytext); T.NUMBA(yytext,!lin,!col))
      end
fun yyAction52 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col := !col + String.size(yytext); T.DECI(yytext,!lin,!col))
      end
fun yyAction53 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (col := !col + String.size(yytext); T.IDENT(yytext,!lin,!col))
      end
fun yyAction54 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction55 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print("unmatched character typed "^yytext^"\n"); continue())
      end
val yyactTable = Vector.fromList([yyAction0, yyAction1, yyAction2, yyAction3,
  yyAction4, yyAction5, yyAction6, yyAction7, yyAction8, yyAction9, yyAction10,
  yyAction11, yyAction12, yyAction13, yyAction14, yyAction15, yyAction16,
  yyAction17, yyAction18, yyAction19, yyAction20, yyAction21, yyAction22,
  yyAction23, yyAction24, yyAction25, yyAction26, yyAction27, yyAction28,
  yyAction29, yyAction30, yyAction31, yyAction32, yyAction33, yyAction34,
  yyAction35, yyAction36, yyAction37, yyAction38, yyAction39, yyAction40,
  yyAction41, yyAction42, yyAction43, yyAction44, yyAction45, yyAction46,
  yyAction47, yyAction48, yyAction49, yyAction50, yyAction51, yyAction52,
  yyAction53, yyAction54, yyAction55])
in
  if yyInput.eof(!(yystrm))
    then UserDeclarations.eof(yyarg)
    else (case (!(yyss))
       of INITIAL => yygo yyactTable (0, !(yystrm), yyNO_MATCH)
      (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
