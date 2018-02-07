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
import types.Type;
import types.TypeTuple;
import util.ImplContainer;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.Set;
}

@parser::members {
	public static Map<String, Map<String, Constructor>> typeTable = new HashMap<String, Map<String, Constructor>>();
	
	static{
		Map<String, Constructor> map;
			
			//Int
			map = new HashMap<String, Constructor>();
			map.put("", Constructor.IntPrimitiveConstructor); //Default
			map.put("Binary", Constructor.IntPrimitiveConstructor); //Alias
			map.put(TypeRepresentation.TypeIntString.name, Constructor.IntStringConstructor);
			map.put(TypeRepresentation.TypeIntRoman.name, Constructor.IntRomanConstructor);
			
			typeTable.put(TypeConcrete.TypeInt.name, map);
			
			//Bool
			map = new HashMap<String, Constructor>();
			map.put("", Constructor.BoolPrimitiveConstructor);
			typeTable.put(TypeConcrete.TypeBool.name, map);
			
			//String
			map = new HashMap<String, Constructor>();
			map.put("", Constructor.StringPrimitiveConstructor);
			typeTable.put(TypeConcrete.TypeString.name, map);
			
			//Double
			map = new HashMap<String, Constructor>();
			map.put("", Constructor.DoublePrimitiveConstructor);
			typeTable.put(TypeConcrete.TypeDouble.name, map);
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
	
	public static Expression instantiateTyped(Constructor constructor, Expression value) throws Exception{
			if(value instanceof Variable){
				Variable v = (Variable) value;
				v.setType(constructor.constructedType);
				return v;
			}	
			return new Application(constructor, value);
		}
	
	public static Expression instantiateUntypedLiteral(Object value) throws Exception{
		if(value instanceof Variable){
			return (Variable)value;
		}
		
		if(value instanceof String) {
			Literal l = new LitString((String)value);
			l.setLiteralType(TypeConcrete.TypeString);
			return l;
		}
		if(value instanceof Integer) {
			Literal l = new LitInteger((Integer)value);
			l.setLiteralType(TypeConcrete.TypeInt);
			return l;
		}
		if(value instanceof Double) {
			Literal l = new LitDouble((Double)value);
			l.setLiteralType(TypeConcrete.TypeDouble);
			return l;
		}
		if(value instanceof Boolean) {
			return (Boolean)value ? LitBoolean.TRUE : LitBoolean.FALSE; //TODO Mohlo by delat bordel, co kdyz nekdo udela typeAlias Boolu?
		}
	
		throw new Exception("Unrecognized untyped value " + value.toString() + " of type " + value.getClass());
	}
	
	public static Constructor getConstructorByTypeName(String typeName) throws Exception{
		return getConstructorByTypeName(typeName, "");
	}
	
	public static Constructor getConstructorByTypeName(String typeName, String representationName) throws Exception{
		Map<String, Constructor> reps = typeTable.get(typeName);
		
		if(reps == null){
			throw new Exception("Invalid type: " + typeName + (representationName == "" ? "" : ":" + representationName));
		}
		
		Constructor constructor = reps.get(representationName); 
		
		if(constructor == null){
			throw new Exception("Invalid type: " + typeName + (representationName == "" ? "" : ":" + representationName));
		}
		
		return constructor;
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
	: '(' { Set<ImplContainer> s = new TreeSet<ImplContainer>(); Expression argList, body; } 
	  ELAMBDA expr { argList = $expr.val; } 
	  expr { body = $expr.val; }
	  '[' (impl { s.add($impl.val); })* ']' 
	  ')' { $val = new ExtendedLambda(lambdaArgsTuple(argList), body, s); }
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
	: '<' 	{ Expression value; Constructor type; }
	  expr 	{ value = $expr.val; } type { type = $type.val; }
	  '>'	{ $val = instantiateTyped(type, value); }
	;
	
type returns [Constructor val]
	: SYMBOL { $val = getConstructorByTypeName($SYMBOL.text); }
	| {String typeName; }
	  SYMBOL { typeName = $SYMBOL.text; } 
	  ':' 
	  SYMBOL { $val = getConstructorByTypeName(typeName, $SYMBOL.text); } 
	;
	
impl returns [ImplContainer val]
	: '(' { List<Type> ll = new ArrayList<Type>(); }
	  (type { ll.add($type.val.constructedType); })* 
	  ')' expr { $val = new ImplContainer(new TypeTuple(ll), $expr.val); }
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
