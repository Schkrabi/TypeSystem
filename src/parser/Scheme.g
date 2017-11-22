grammar Scheme;
options {
//    output=AST;
//    ASTLabelType=CommonTree; // type of $stat.tree ref etc...
//    backtrack=true;
//    tokenVocab=Predefined;
}


@header {package parser;
import expression.*;
}

@parser::members {
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
  
  public static Object parseSymbol(String text){
    switch(text){
      case "lambda":
        return new String("LAMBDA");
      case "if":
        return new String("IF");
      default:
        return new Variable(text);
    }
  }
  
  public static Expression parseList(List<Object> l){
    
  }
}


exprs returns [List<Object> val]
	: { List<Object> ll = new ArrayList<>(); }
	  ( expr { ll.add($expr.val); })*
	  { $val = ll; }
	;

expr returns [Object val] 
	: atom 	{ $val = $atom.val; }
	| seq 	{ $val = $seq.val; }
	| quote { $val = $quote.val; }
	;

seq returns [Object val]
	: '(' 	{ List<Object> ll = new ArrayList<>(); }
	  (expr	{ ll.add($expr.val); } )*
	  ')' 	{ $val = new Sequence(ll); }
	;

atom returns [Object val]
	: INT 		{ $val = new IntBinary(Integer.parseInt($INT.text)); }
	| FLOAT		{ $val = new LitDouble(Double.parseDouble($FLOAT.text)); }
	| SYMBOL 	{ $val = new Symbol($SYMBOL.text); }
	| TRUE 		{ $val = LitBoolean.TRUE; }
	| FALSE 	{ $val = LitBoolean.FALSE; }
	| STRING	{ $val = new LitString(unescapeString($STRING.text.substring(1, $STRING.text.length() - 1))); }
	;

quote returns [Object val]
	: '\'' expr	{ $val = null; }
	;


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
