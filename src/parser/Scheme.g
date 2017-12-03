grammar Scheme;
options {
//    output=AST;
//    ASTLabelType=CommonTree; // type of $stat.tree ref etc...
//    backtrack=true;
//    tokenVocab=Predefined;
}


@header {package parser;
import expression.*;
import types.TypeConcrete;
import types.TypeRepresentation;
import util.ImplContainer;
import java.util.Map;
import java.util.HashMap;
}

@parser::members {
	public static Map<String, Map<String, TypeConcrete>> typeTable = new HashMap<String, Map<String, TypeConcrete>>();
	public static Map<Class<? extends Object>, TypeConcrete> untypedTable = new HashMap<Class<? extends Object>, TypeConcrete>();
	
	static{
		Map<String, TypeConcrete> map;
		
		//Int
		map = new HashMap<String, TypeConcrete>();
		map.put("", TypeConcrete.TypeInt); //Default
		map.put("Binary", TypeConcrete.TypeInt); //Alias
		map.put(TypeRepresentation.TypeIntString.name, TypeRepresentation.TypeIntString);
		map.put(TypeRepresentation.TypeIntRoman.name, TypeRepresentation.TypeIntRoman);
		
		typeTable.put(TypeConcrete.TypeInt.name, map);
		untypedTable.put(Integer.class, TypeConcrete.TypeInt);
		
		//Bool
		map = new HashMap<String, TypeConcrete>();
		map.put("", TypeConcrete.TypeBool);
		typeTable.put(TypeConcrete.TypeBool.name, map);
		untypedTable.put(Boolean.class, TypeConcrete.TypeBool);
		
		//String
		map = new HashMap<String, TypeConcrete>();
		map.put("", TypeConcrete.TypeString);
		typeTable.put(TypeConcrete.TypeString.name, map);
		untypedTable.put(String.class, TypeConcrete.TypeString);
		
		//Double
		map = new HashMap<String, TypeConcrete>();
		map.put("", TypeConcrete.TypeDouble);
		typeTable.put(TypeConcrete.TypeDouble.name, map);
		untypedTable.put(Double.class, TypeConcrete.TypeDouble);
	}
	
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
	
	public static Expression instantiateTypedLiteral(TypeConcrete type, Object value) throws Exception{
		if(value instanceof Variable){
			Variable v = (Variable) value;
			v.setType(type);
			return v;
		}	
		return type.instantiateLiteral(value);
	}
	
	public static Expression instantiateUntypedLiteral(Object value) throws Exception{
		if(value instanceof Variable){
			return (Variable)value;
		}
	
		TypeConcrete type = untypedTable.get(value.getClass());
		if(type == null){
			throw new Exception("Unrecognized untyped value " + value.toString() + " of type " + value.getClass());
		}
		
		return type.instantiateLiteral(value);
	}
	
	public static TypeConcrete getTypeByName(String typeName) throws Exception{
		return getTypeByName(typeName, "");
	}
	
	public static TypeConcrete getTypeByName(String typeName, String representationName) throws Exception{
		Map<String, TypeConcrete> reps = typeTable.get(typeName);
		
		if(reps == null){
			throw new Exception("Invalid type: " + typeName + (representationName == "" ? "" : ":" + representationName));
		}
		
		TypeConcrete type = reps.get(representationName); 
		
		if(type == null){
			throw new Exception("Invalid type: " + typeName + (representationName == "" ? "" : ":" + representationName));
		}
		
		return type;
	}
}


exprs returns [List<Expression> val]
	: { List<Expression> ll = new ArrayList<>(); }
	  ( expr { ll.add($expr.val); })*
	  { $val = ll; }
	;

expr returns [Expression val] 
	: atom 	{ $val = instantiateUntypedLiteral($atom.val); }
	| seq 	{ $val = $seq.val; }
	| quote { $val = $quote.val; }
	| typed { $val = $typed.val; }
	;

seq returns [Expression val]
	: '(' ELAMBDA expr expr '[' (impl)* ']' ')'
	| '('	{ Expression argList, body; }
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
	: '<' 	{ Object value; TypeConcrete type; }
	  atom 	{ value = $atom.val; } type { type = $type.val; }
	  '>'	{ $val = instantiateTypedLiteral(type, value); }
	;
	
type returns [TypeConcrete val]
	: SYMBOL { $val = getTypeByName($SYMBOL.text); }
	| {String typeName; }
	  SYMBOL { typeName = $SYMBOL.text; } 
	  ':' 
	  SYMBOL { $val = getTypeByName(typeName, $SYMBOL.text); } 
	;
	
impl returns [ImplContainer val]
	: '(' (type)* ')' expr
	;

atom returns [Object val]
	: INT 		{ $val = Integer.parseInt($INT.text); }
	| FLOAT		{ $val = Double.parseDouble($FLOAT.text); }
	| SYMBOL 	{ $val = new Variable($SYMBOL.text); }
	| TRUE 		{ $val = Boolean.TRUE; }
	| FALSE 	{ $val = Boolean.FALSE; }
	| STRING	{ $val = unescapeString($STRING.text.substring(1, $STRING.text.length() - 1)); }
	;

quote returns [Expression val]
	: '\'' expr	{ $val = new QuotedExpression($expr.val); }
	;


IF		: 'i' 'f';
LAMBDA  : 'l' 'a' 'm' 'b' 'd' 'a';
ELAMBDA : 'e' 'l' 'a' 'm' 'b' 'd' 'a';
TRUE	: '#' 't';
FALSE	: '#' 'f';
INT 	: '-'?[0-9]+ ;
FLOAT   : '-'?[0-9]* '.' [0-9]+ ;
SYMBOL 	: SYMBOL_HEAD SYMBOL_REST*;
COMMENT : ';' ~('\r' | '\n')* -> skip;
WS 	: [ \r\t\n]+ -> skip ;
STRING: '"' ~('\n'|'"')* '"' ;

fragment SYMBOL_HEAD
	:   'a'..'z' | 'A'..'Z' | '*' | '+' | '!' | '-' | '_' | '?' | '=' | '$' | '@' | '&' | '#' | '/'
	;
    
fragment SYMBOL_REST
	: SYMBOL_HEAD
	| '0'..'9'
	| '.'
	;
