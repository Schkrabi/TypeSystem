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
import java.util.HashSet;
import java.util.Optional;
}

@parser::members {
	public static Set<TypeConcrete> types = new HashSet<TypeConcrete>();
		public static Map<TypeConcrete, Constructor> constructorMap = new HashMap<TypeConcrete, Constructor>();
		
		static{
			//Int
			types.add(TypeConcrete.TypeInt);
			constructorMap.put(TypeConcrete.TypeInt, Constructor.IntPrimitiveConstructor);
			types.add(TypeRepresentation.TypeIntRoman);
			constructorMap.put(TypeRepresentation.TypeIntRoman, Constructor.IntRomanConstructor);
			types.add(TypeRepresentation.TypeIntString);
			constructorMap.put(TypeRepresentation.TypeIntString, Constructor.IntStringConstructor);
			
			//Bool
			types.add(TypeConcrete.TypeBool);
			constructorMap.put(TypeConcrete.TypeBool, Constructor.BoolPrimitiveConstructor);
			
			//String
			types.add(TypeConcrete.TypeString);
			constructorMap.put(TypeConcrete.TypeString, Constructor.StringPrimitiveConstructor);
				
			//Double
			types.add(TypeConcrete.TypeDouble);
			constructorMap.put(TypeConcrete.TypeDouble, Constructor.DoublePrimitiveConstructor);
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
			Optional<TypeConcrete> o;
			
			if(representationName == "") {					
				o = types.stream().filter(x -> (x instanceof TypeConcrete) && x.name.equals(typeName)).findAny();
			}
			else {
				o = types.stream().filter(x -> (x instanceof TypeRepresentation) && x.name.equals(representationName) && ((TypeRepresentation)x).baseType.name.equals(typeName)).findAny();
			}
			
			if(!o.isPresent()) {
				throw new Exception("Unknown type " + typeName + representationName != "" ? ":" + representationName : "");
			}
			
			TypeConcrete type = o.get();
			
			Constructor constructor = constructorMap.get(type);
			
			if(constructor == null){
				throw new Exception("No constructor exists for: " + typeName + (representationName == "" ? "" : ":" + representationName));
			}
			
			return constructor;
		}
		
		public static void defineType(String typeName, TypeTuple constructorArgsType, Lambda lConstructor) throws Exception{
				TypeConcrete newType = new TypeConcrete(typeName);
				Constructor constructor = new Constructor(newType, lConstructor.args, constructorArgsType, lConstructor.getBody());
				
				addType(newType);
				addConstructor(newType, constructor);
			}
			
		public static void defineRepresentation(String typeName, String repName, TypeTuple constructorArgsType, Lambda lConstructor) throws Exception{
			Optional<TypeConcrete> o = types.stream().filter(x -> (x instanceof TypeConcrete) && x.name.equals(typeName)).findAny(); 
			if(!o.isPresent()) {
				throw new Exception("Unknown base type: " + typeName);
			}
			
			TypeConcrete baseType = o.get();
			TypeRepresentation newType = new TypeRepresentation(repName, baseType);
			addType(newType);
			
			Constructor constructor = new Constructor(newType, lConstructor.args, constructorArgsType, lConstructor.getBody());
			addConstructor(newType, constructor);
		}
		
		public static void addType(TypeConcrete newType) throws Exception {
			if(types.contains(newType)) {
				throw new Exception("Type " + newType + " is already defined");
			}
			types.add(newType);
		}
		
		public static void addConstructor(TypeConcrete newType, Constructor constructor) throws Exception {
			
			if(constructorMap.containsKey(newType)) {
				throw new Exception("Constructor for " + newType + " is already defined");
			}
			constructorMap.put(newType, constructor);
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
	: lambda { $val = $lambda.val; }
	| elambda { $val = $elambda.val; }
	| deftype { $val = $deftype.val; }
	| defrep { $val = $defrep.val; }
	| '(' 	{ Expression cond, tBranch, fBranch; } 
	  IF expr { cond = $expr.val; } expr { tBranch = $expr.val; } expr { fBranch = $expr.val; } 
	  ')' 	{ $val = new IfExpression(cond, tBranch, fBranch); }
	| '(' 	{ List<Expression> ll = new ArrayList<>(); }
	  (expr	{ ll.add($expr.val); } )*
	  ')' 	{ $val = new Sequence(ll); }
	;
	
elambda returns [ExtendedLambda val]
	: '(' { Set<ImplContainer> s = new TreeSet<ImplContainer>(); Expression argList, body; } 
	  ELAMBDA expr { argList = $expr.val; } 
	  expr { body = $expr.val; }
	  '[' (impl { s.add($impl.val); })* ']' 
	  ')' { $val = new ExtendedLambda(lambdaArgsTuple(argList), body, s); }
	;
	
lambda returns [Lambda val]
	: '('	{ Expression argList, body; }
	  LAMBDA expr { argList = $expr.val; } expr { body = $expr.val; }
	  ')'	{ $val = new Lambda(lambdaArgsTuple(argList), body); }
	;

deftype returns [Expression val]
	: '(' { String name; TypeTuple t; Lambda lambda; } 
	  DEFTYPE SYMBOL { name = $SYMBOL.text; } typelist { t = $typelist.val; } lambda { lambda = $lambda.val; } 
	  ')' { defineType(name, t, lambda); $val = Expression.EMPTY_EXPRESSION; }
	;
	
defrep returns [Expression val]
	: '(' { String typeName, repName; TypeTuple t; Lambda lambda; }
	  DEFREP SYMBOL { repName = $SYMBOL.text; } SYMBOL { typeName = $SYMBOL.text; } typelist { t = $typelist.val; } lambda { lambda = $lambda.val; } 
	  ')' { defineRepresentation(typeName, repName, t, lambda); $val = Expression.EMPTY_EXPRESSION; }
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
	: { TypeTuple t; }
	  typelist { t = $typelist.val; } 
	  expr { $val = new ImplContainer(t, $expr.val); }
	;
	
typelist returns [TypeTuple val]
	: '(' { List<Type> ll = new ArrayList<Type>(); }
	  (type { ll.add($type.val.constructedType); })* 
	  ')' { $val = new TypeTuple(ll); }
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


DEFTYPE : 'd' 'e' 'f' 't' 'y' 'p' 'e';
DEFREP  : 'd' 'e' 'f' 'r' 'e' 'p';
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
