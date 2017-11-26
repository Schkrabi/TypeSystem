grammar Scheme;
options {
//    output=AST;
//    ASTLabelType=CommonTree; // type of $stat.tree ref etc...
//    backtrack=true;
//    tokenVocab=Predefined;
}


@header {package parser;
import expression.*;
import java.util.Map;
import java.util.TreeMap;
}

@parser::members {
	public static Map<String, class<? extends 
	
	
	public static String unescapeString(String s) {
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < s.length(); i++) {
			char c1 = s.charAt(i); 
			if ((c1 == '\\') && (i + 1 < s.length())) {
				char c2 = s.charAt(i + 1);
				switch (c2) {
					case 'n': result.append('\n'); i++; break;
					case 't': result.append('\t'); i++; break;
					case 'r': result.append('\r'); i++; break;
					case '\\': result.append('\\'); i++; break;
					default: result.append(c1);
				}
			} else result.append(c1);
		}
		return result.toString();
	}
	
	public static Tuple lambdaArgsTuple(Expression e) throws Exception {
		if(e instanceof Variable){
			return new Tuple(new Expression[]{e});
		}
		if(e instanceof Tuple){ //Unlikely,
			return (Tuple) e;
		}
		if(e instanceof Sequence){
			Sequence s = (Sequence) e;
			return s.asTuple();
		}
		throw new Exception("Argument name list expected, got " + e.toString());
	}
}


exprs returns [List<Expression> val]
	: { List<Expression> ll = new ArrayList<>(); }
	  ( expr { ll.add($expr.val); })*
	  { $val = ll; }
	;

expr returns [Expression val] 
	: atom 	{ $val = $atom.val; }
	| seq 	{ $val = $seq.val; }
	| quote { $val = $quote.val; }
	| typed { $val = $typed.val; }
	;

seq returns [Expression val]
	: '('	{ Expression argList, body; }
	  LAMBDA expr { argList = $expr.val; } expr { body = $expr.val; }
	  ')'	{ $val = new Lambda(lambdaArgsTuple(argList), body); }
	| '(' 	{ Expression cond, tBranch, fBranch; } 
	  IF expr { cond = $expr.val; } expr { tBranch = $expr.val; } expr { fBranch = $expr.val; } 
	  ')' 	{ $val = new IfExpression(cond, tBranch, fBranch); }
	| '(' 	{ List<Expression> ll = new ArrayList<>(); }
	  (expr	{ ll.add($expr.val); } )*
	  ')' 	{ $val = new Sequence(ll); }
	;
	
typed returns [Expression val]
	: '<' atom SYMBOL ':' SYMBOL '>'
	| '<' atom SYMBOL '>'
	;

atom returns [Expression val]
	: INT 		{ $val = new IntBinary(Integer.parseInt($INT.text)); }
	| FLOAT		{ $val = new LitDouble(Double.parseDouble($FLOAT.text)); }
	| SYMBOL 	{ $val = new Variable($SYMBOL.text); }
	| TRUE 		{ $val = LitBoolean.TRUE; }
	| FALSE 	{ $val = LitBoolean.FALSE; }
	| STRING	{ $val = new LitString(unescapeString($STRING.text.substring(1, $STRING.text.length() - 1))); }
	;

quote returns [Expression val]
	: '\'' expr	{ $val = new QuotedExpression($expr.val); }
	;


IF		: 'i' 'f';
LAMBDA  : 'l' 'a' 'm' 'b' 'd' 'a';
TRUE	: '#' 't';
FALSE	: '#' 'f';
INT 	: '-'?[0-9]+ ;
FLOAT   : '-'?[0-9]* '.' [0-9]+ ;
SYMBOL 	: SYMBOL_HEAD SYMBOL_REST*;
COMMENT : ';' ~('\r' | '\n')* -> skip;
WS 	: [ \r\t\n]+ -> skip ;
STRING: '"' ~('\n'|'"')* '"' ;

fragment SYMBOL_HEAD
	:   'a'..'z' | 'A'..'Z' | '*' | '+' | '!' | '-' | '_' | '?' | '>' | '<' | '=' | '$' | '@' | '&' | ':' | '#' | '/'
	;
    
fragment SYMBOL_REST
	: SYMBOL_HEAD
	| '0'..'9'
	| '.'
	;
