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
Vector.fromList []
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
      (T.RATADD(!lin,!col)))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RATSUB(!lin,!col)))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RATMUL(!lin,!col)))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RATDIV(!lin,!col)))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LPAREN(!lin,!col)))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RPAREN(!lin,!col)))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LBRACE(!lin,!col)))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RBRACE(!lin,!col)))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RATIONAL(!lin,!col)))
fun yyAction10 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.INTEGER(!lin,!col)))
fun yyAction11 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.BOOLEAN(!lin,!col)))
fun yyAction12 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.AND(!lin,!col)))
fun yyAction13 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.OR(!lin,!col)))
fun yyAction14 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.NOT(!lin,!col)))
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.EQ(!lin,!col)))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.GEQ(!lin,!col)))
fun yyAction17 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LEQ(!lin,!col)))
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.GT(!lin,!col)))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LT(!lin,!col)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.NEQ(!lin,!col)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.IF(!lin,!col)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.FI(!lin,!col)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.THEN(!lin,!col)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.ELSE(!lin,!col)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.PRINT(!lin,!col)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.FALSE(!lin,!col)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.TRUE(!lin,!col)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lin:= !lin + 1; continue()))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.EOL(!lin,!col)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.DIV(!lin,!col)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.MUL(!lin,!col)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.ADD(!lin,!col)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.SUB(!lin,!col)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.MOD(!lin,!col)))
fun yyAction35 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.NUMBA(yytext,!lin,!col))
      end
fun yyAction36 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.DECI(yytext,!lin,!col))
      end
fun yyAction37 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (T.ID(yytext,!lin,!col))
      end
fun yyAction38 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (print("unmatched character typed "^yytext^"\n"); continue())
      end
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yystuck(lastMatch)
            else if inp < #"*"
              then if inp = #")"
                  then yyQ36(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ35(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ35(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ35(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ35(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #")"
              then yyAction36(strm, yyNO_MATCH)
            else if inp < #")"
              then if inp = #"("
                  then yyQ33(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
                  else yyAction36(strm, yyNO_MATCH)
            else if inp = #"0"
              then yyQ34(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
            else if inp < #"0"
              then yyAction36(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ34(strm', yyMATCH(strm, yyAction36, yyNO_MATCH))
              else yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #")"
              then yystuck(lastMatch)
            else if inp < #")"
              then if inp = #"("
                  then yyQ33(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"0"
              then yyQ34(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ34(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ31(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ32(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ31(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ32(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"|"
              then yyQ37(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction27(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
                  else yyAction27(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction27, yyNO_MATCH))
              else yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"e"
              then yyQ42(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"u"
              then yyQ41(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"u"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction23(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
                  else yyAction23(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction23, yyNO_MATCH))
              else yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"n"
              then yyQ44(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"n"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"e"
              then yyQ43(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"r"
              then yyQ40(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"r"
              then if inp = #"h"
                  then yyQ39(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction9(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
                  else yyAction9(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction9, yyNO_MATCH))
              else yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"l"
              then yyQ51(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"b"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ50(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"n"
              then yyQ49(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"n"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"o"
              then yyQ48(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"i"
              then yyQ47(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"i"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"t"
              then yyQ46(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"t"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"b"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ45(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction25(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
                  else yyAction25(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction25, yyNO_MATCH))
              else yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"t"
              then yyQ55(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"t"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"n"
              then yyQ54(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"n"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"i"
              then yyQ53(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"i"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"r"
              then yyQ52(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"r"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ62 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction10(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
                  else yyAction10(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction10, yyNO_MATCH))
              else yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ61 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"r"
              then yyQ62(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"r"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ60 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"e"
              then yyQ61(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ59 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"g"
              then yyQ60(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"g"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"e"
              then yyQ59(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"t"
              then yyQ58(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"t"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction21(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
                  else yyAction21(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction21, yyNO_MATCH))
              else yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"n"
              then yyQ57(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"n"
              then if inp = #"f"
                  then yyQ56(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ64 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction22(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
                  else yyAction22(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction22, yyNO_MATCH))
              else yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ67 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction26(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
                  else yyAction26(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction26, yyNO_MATCH))
              else yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ66 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"e"
              then yyQ67(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ65 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"s"
              then yyQ66(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"s"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ63 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"l"
              then yyQ65(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"a"
              then yyQ63(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"a"
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp = #"A"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"A"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"j"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"j"
              then if inp = #"i"
                  then yyQ64(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ70 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction24(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
                  else yyAction24(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction24, yyNO_MATCH))
              else yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ69 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"e"
              then yyQ70(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ68 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"s"
              then yyQ69(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"s"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"l"
              then yyQ68(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ76 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction11(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
                  else yyAction11(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction11, yyNO_MATCH))
              else yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ75 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"n"
              then yyQ76(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"n"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ74 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"b"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"b"
              then if inp = #"a"
                  then yyQ75(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ73 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"e"
              then yyQ74(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"e"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ72 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"l"
              then yyQ73(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"l"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ71 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"o"
              then yyQ72(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction37(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction37(strm, yyNO_MATCH)
                      else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp = #"o"
              then yyQ71(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"o"
              then if inp <= #"`"
                  then yyAction37(strm, yyNO_MATCH)
                  else yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"A"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"A"
              then if inp = #"0"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                else if inp < #"0"
                  then yyAction37(strm, yyNO_MATCH)
                else if inp <= #"9"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp = #"a"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
            else if inp < #"a"
              then if inp <= #"Z"
                  then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
                  else yyAction37(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ38(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ77 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ77(strm', yyMATCH(strm, yyAction18, yyNO_MATCH))
              else yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ79 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ78 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ79(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ78(strm', yyMATCH(strm, yyAction19, yyNO_MATCH))
                  else yyAction19(strm, yyNO_MATCH)
              else yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyAction35(strm, yyNO_MATCH)
            else if inp < #"/"
              then if inp = #"."
                  then yyQ31(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ32(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ84 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ83 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ84(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ85 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ82 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ85(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ86 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ81 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ86(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ87 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ80 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"."
              then yyQ87(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #","
              then yyAction38(strm, yyNO_MATCH)
            else if inp < #","
              then if inp = #")"
                  then yyAction38(strm, yyNO_MATCH)
                else if inp < #")"
                  then if inp = #"("
                      then yyQ33(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                      else yyAction38(strm, yyNO_MATCH)
                else if inp = #"*"
                  then yyQ80(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyQ81(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp = #"/"
              then yyQ83(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
            else if inp < #"/"
              then if inp = #"-"
                  then yyQ82(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
                  else yyAction38(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ34(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ88 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"&"
              then yyQ88(strm', yyMATCH(strm, yyAction38, yyNO_MATCH))
              else yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ18(strm', lastMatch)
            else if inp < #">"
              then if inp = #")"
                  then yyQ8(strm', lastMatch)
                else if inp < #")"
                  then if inp = #"!"
                      then yyQ4(strm', lastMatch)
                    else if inp < #"!"
                      then if inp = #"\n"
                          then yyQ3(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ2(strm', lastMatch)
                              else yyQ1(strm', lastMatch)
                        else if inp = #" "
                          then yyQ2(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #"&"
                      then yyQ6(strm', lastMatch)
                    else if inp < #"&"
                      then if inp = #"%"
                          then yyQ5(strm', lastMatch)
                          else yyQ1(strm', lastMatch)
                    else if inp = #"'"
                      then yyQ1(strm', lastMatch)
                      else yyQ7(strm', lastMatch)
                else if inp = #"/"
                  then yyQ13(strm', lastMatch)
                else if inp < #"/"
                  then if inp = #","
                      then yyQ1(strm', lastMatch)
                    else if inp < #","
                      then if inp = #"*"
                          then yyQ9(strm', lastMatch)
                          else yyQ10(strm', lastMatch)
                    else if inp = #"-"
                      then yyQ11(strm', lastMatch)
                      else yyQ12(strm', lastMatch)
                else if inp = #";"
                  then yyQ15(strm', lastMatch)
                else if inp < #";"
                  then if inp = #":"
                      then yyQ1(strm', lastMatch)
                      else yyQ14(strm', lastMatch)
                else if inp = #"<"
                  then yyQ16(strm', lastMatch)
                  else yyQ17(strm', lastMatch)
            else if inp = #"p"
              then yyQ24(strm', lastMatch)
            else if inp < #"p"
              then if inp = #"c"
                  then yyQ19(strm', lastMatch)
                else if inp < #"c"
                  then if inp = #"["
                      then yyQ1(strm', lastMatch)
                    else if inp < #"["
                      then if inp <= #"@"
                          then yyQ1(strm', lastMatch)
                          else yyQ19(strm', lastMatch)
                    else if inp = #"a"
                      then yyQ19(strm', lastMatch)
                    else if inp = #"b"
                      then yyQ20(strm', lastMatch)
                      else yyQ1(strm', lastMatch)
                else if inp = #"g"
                  then yyQ19(strm', lastMatch)
                else if inp < #"g"
                  then if inp = #"e"
                      then yyQ21(strm', lastMatch)
                    else if inp = #"f"
                      then yyQ22(strm', lastMatch)
                      else yyQ19(strm', lastMatch)
                else if inp = #"i"
                  then yyQ23(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp = #"{"
              then yyQ27(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"s"
                  then yyQ19(strm', lastMatch)
                else if inp < #"s"
                  then if inp = #"q"
                      then yyQ19(strm', lastMatch)
                      else yyQ25(strm', lastMatch)
                else if inp = #"t"
                  then yyQ26(strm', lastMatch)
                  else yyQ19(strm', lastMatch)
            else if inp = #"~"
              then yyQ30(strm', lastMatch)
            else if inp < #"~"
              then if inp = #"|"
                  then yyQ28(strm', lastMatch)
                  else yyQ29(strm', lastMatch)
              else yyQ1(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
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
