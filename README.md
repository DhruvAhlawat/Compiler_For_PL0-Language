## Design Decisions
The method to run the file is at the bottom. The sources I have used are also mentioned below.

1. the reserved keywords of functions are applied to the first expression on their right, so rat 5 + 0.4(3) would meant that rat is applied on 5 to give a rational (5/1) which is then added to 0.4(3). Additionally there is support for parentheses and is recommended so function application is not ambiguous. Also, make_rat can be called like make\_rat a b or make\_rat(a,b). 
Since 'true' and 'false' are not keywords, 'tt' and 'ff' are used instead for boolean values.

2. I also output the variable name along with the ":=" sign during read() function call, so the user knows which variable they're giving the value to.

3. the language is completely **typechecked**, and will issue the *typeMismatched* exception on error prone code like using integers as rationals or booleans etc. Hence it is type-safe. Appropriate errors are also raised on giving non-digit characters during read , assignments or in expressions.

4. The language **Supports recursion**. So each call to the function forms separate declarations and variables. Also the symbol table supports deletion of variables after exiting scopes. The symbol table is implemented as a hashtable of lists, where each list is a stack of values for a particular variable name from different scopes. when getting the value of a particular variable, first the current scope is checked, and then the parent scopes and so on and we stop at the first correct scope we find. **The parent scopes are determined during parsing itself (statically).** 

 - important note -> Variables that have the same name CAN be defined but code will only use the most recent declaration of the variable name. so if we have rational a; integer a; then in this case 'a' will always refer to the integer variable 'a' and *NOT* the rational variable. Hence multiple variables with same name in the same scope can exist but there is no point since only the most recent one will be used. this is in accordance with languages like sml.

5. print(a) would print a rational in the fractional form if a is a rational expression or variable, and an integer if it is an integer variable or expression, and 'true' or 'false' if it is a boolean expression or variable. for additional functionality, print(toDecimal(a)) or print(showDecimal(a)) will print a rational in its decimal normal form. toDecimal, showDecimal, showRat can only be used inside print statements.

6. since 'var' is a keyword, I have added the functionality that a variable 'a' can be referred to as just 'a' in expressions, or as 'var(a)' or var a. This may make the language more readable.

7. the unary operator '+' is discarded as it has no use, but the unary operator '~' still remains as it is useful to negate values (either integer or rational). 

8. the language supports **nested if-else statements**. Also, the if-else statements are **typechecked**, So the condition must be a boolean expression.

9. Does *NOT* support nested comments like any other language. comments are closed as soon as any *) sequence is detected.

5. I have added all the functions and datatypes in the file datatypes.sml.


## How to run
1. Go into the folder with the files.
2. run the 'sml' command which opens sml in the terminal.
3. run

        CM.make "pi.cm";
4. then you can run the files 

        Pi.compile "factorial.rat";

 - some testfiles  "recursiontest.rat", "factorial.rat", "fibonacci.rat", "nestedIf.rat", "estimateFibonacci.rat" are also included

## Sources
like the 3rd assignment, I have taken some starter code for lex and yacc from *ML-YACC user's manual* and *ML-LEX user's manual*. Also *User's guide to ML-Lex and ML-Yacc*. This does not include anything related to the assignment, but mostly the instructions to glue the lex and yacc files as well as the compiler.sml that they have used to run the yacc file on an input. 

I have also used the *SML of New Jersey* documentation for the standard library functions.
And used the library *$/smlnj-lib.cm* for Hashtable.
