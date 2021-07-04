grammar Scheme;
options {
//    output=AST;
//    ASTLabelType=CommonTree; // type of $stat.tree ref etc...
//    backtrack=true;
//    tokenVocab=Predefined;
}


@header {package parser;
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
}


exprs returns [List<SemanticNode> val]
	: { List<SemanticNode> ll = new ArrayList<>(); }
	  ( expr { ll.add($expr.val); })*
	  { $val = ll; }
	;

expr returns [SemanticNode val] 
	: atom 	{ $val = $atom.val; }
	| seq 	{ $val = $seq.val; }
	| pair	{ $val = $pair.val; }
	| arrow { $val = $arrow.val; }
	;

seq returns [SemanticNode val]
	: '(' 	{ List<SemanticNode> ll = new ArrayList<>(); }
	  (expr	{ ll.add($expr.val); } )*
	  ')' 	{ $val = SemanticNode.make(SemanticNode.NodeType.LIST, ll); }
	;

atom returns [SemanticNode val]
	: INT 		{ $val = SemanticNode.make(SemanticNode.NodeType.INT, Integer.parseInt($INT.text)); }
	| FLOAT		{ $val = SemanticNode.make(SemanticNode.NodeType.DOUBLE, Double.parseDouble($FLOAT.text)); }
	| SYMBOL 	{ $val = SemanticNode.make(SemanticNode.NodeType.SYMBOL, $SYMBOL.text); }
	| TRUE 		{ $val = SemanticNode.make(SemanticNode.NodeType.BOOL, true); }
	| FALSE 	{ $val = SemanticNode.make(SemanticNode.NodeType.BOOL, false); }
	| STRING	{ $val = SemanticNode.make(SemanticNode.NodeType.STRING, unescapeString($STRING.text.substring(1, $STRING.text.length() - 1))); }
	;
	
pair returns [SemanticNode val]
	: { String lvalue; } 
	  SYMBOL { lvalue = $SYMBOL.text; } ':' SYMBOL 
	  { $val = SemanticNode.make(SemanticNode.NodeType.PAIR, new SemanticPair(lvalue, $SYMBOL.text)); }
	;
	
arrow returns [SemanticNode val]
	: '(' 	{ SemanticNode lvalue; }
	  expr 	{ lvalue = $expr.val; } ARROW expr
	  ')' 	{ $val = SemanticNode.make(SemanticNode.NodeType.ARROW, new util.Pair<SemanticNode, SemanticNode>(lvalue, $expr.val)); }
	;

TRUE	: '#' 't';
FALSE	: '#' 'f';
INT 	: '-'?[0-9]+ ;
FLOAT   : '-'?[0-9]* '.' [0-9]+ ;
SYMBOL 	: SYMBOL_HEAD SYMBOL_REST*;
COMMENT : ';' ~('\r' | '\n')* -> skip;
WS 		: [ \r\t\n]+ -> skip ;
STRING	: '"' ~('\n'|'"')* '"' ;
ARROW	: '#' '>' ;

fragment SYMBOL_HEAD
	:   'a'..'z' | 'A'..'Z' | '*' | '+' | '!' | '-' | '_' | '?' | '>' | '<' | '=' | '$' | '@' | '/'
	;
    
fragment SYMBOL_REST
	: SYMBOL_HEAD
	| '0'..'9'
	| '.'
	;
