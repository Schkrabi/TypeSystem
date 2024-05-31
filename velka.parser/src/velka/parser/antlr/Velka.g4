grammar Velka;

@header {package velka.parser.antlr;

import velka.core.abstraction.*;
import velka.core.application.*;
import velka.core.expression.*;
import velka.core.literal.*;
import velka.types.*;
import velka.util.NameGenerator;

import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.HashSet;
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
	
	private Stack<Set<String>> typeVariableStack = new Stack<Set<String>>();
	
	/** Returns true if the name is declared type variable **/
	private boolean isDeclaredTypeVariable(String name){
		for(var s : this.typeVariableStack){
			if(s.contains(name)){
				return true;
			}
		}
		return false;
	}
}

program returns [List<Expression> val]
	: 	{ List<Expression> ll = new ArrayList<Expression>(); }
		(expr { ll.add($expr.val); })*
		{ $val = ll; }
	;
	
expr returns [Expression val]
	: special_form { $val = $special_form.val; }
	| atom         { $val = $atom.val; }
	| application  { $val = $application.val; }
	| 'nil'        { $val = Expression.EMPTY_EXPRESSION; }
	;
	
argument_list returns [List<velka.util.Pair<Type, Expression>> val]
	: '('          { var ll = new ArrayList<velka.util.Pair<Type, Expression>>(); }
	  (
	   '('
	   type   { var t = $type.val; }
	   SYMBOL { ll.add(velka.util.Pair.of(t, new Symbol($SYMBOL.text))); }
	   ')'
	  )+
	  ')'          { $val = ll; }
	| '('     { var ll = new ArrayList<velka.util.Pair<Type, Expression>>(); }
	  (SYMBOL { 
	            var t = new TypeVariable(NameGenerator.next());
				var s = new Symbol($SYMBOL.text);
				ll.add(velka.util.Pair.of(t, s)); 
			  })*
	  ')'     { $val = ll; }
	;
	
bind returns [velka.util.Pair<Symbol, Expression> val]
	: '(' 
	  SYMBOL { var s = new Symbol($SYMBOL.text); }
	  expr   { var e = $expr.val; }
	  ')'    { $val = velka.util.Pair.of(s, e); }
	;
	
bind_list returns [List<velka.util.Pair<Symbol, Expression>> val]
	: '('   { var ll = new ArrayList<velka.util.Pair<Symbol, Expression>>(); }
	  (bind { ll.add($bind.val); })*
	  ')'   { $val = ll; }
	;
	
special_form returns [Expression val]
	: '(' { var ll = new ArrayList<Expression>(); }
	  'and' 
	  (expr { ll.add($expr.val); })* 
	  ')' { $val = new AndExpression(new Tuple(ll)); }
	| '(' 
	  'can-deconstruct-as' 
	  expr { var e = $expr.val; }
	  type { var t = $type.val; }
	  ')'  { $val = new CanDeconstructAs(e, t); }
	| '(' 
	  'define' 
	  SYMBOL { var s = new Symbol($SYMBOL.text); }
	  expr   { var e = $expr.val; }
	  ')'    { $val = new DefineSymbol(s, e); }
	| '('
	  'conversion'
	  representation_atom { var f = $representation_atom.val; }
	  representation_atom { var t = $representation_atom.val; }
	  '(' 
	  SYMBOL              { var a = new Symbol($SYMBOL.text); }
	  ')' 
	  expr                { var e = $expr.val; }
	  expr                { var c = $expr.val; }
	  ')'                 { $val = new DefineConversion(f, t, new Tuple(a), e, c); }
	| '(' 
	  'conversion' 
	  representation_atom { var f = $representation_atom.val; }
	  representation_atom { var t = $representation_atom.val; }
	  '(' 
	  SYMBOL              { var a = new Symbol($SYMBOL.text); }
	  ')' 
	  expr                { var e = $expr.val; }
	  ')'                 { $val = new DefineConversion(f, t, new Tuple(a), e); }
	| '(' 
	  'constructor' 
	  representation_atom { var t = $representation_atom.val; }
	  argument_list       { var args = $argument_list.val; }
	  expr                { var e = $expr.val; }
	  ')'                 { 
							var al = new ArrayList<Expression>();
							var tl = new ArrayList<Type>();
							args.stream().forEach(p -> {
														tl.add(p.first);
														al.add(p.second);
													   });
							$val = new DefineConstructor(t, new Lambda(new Tuple(al), new TypeTuple(tl), e));
						  }
	| '(' 
	  'deconstruct' 
	  expr { var e = $expr.val; }
	  type { var t = $type.val; }
	  ')'  { $val = new Deconstruct(e, t); }
	| '(' 
	  'convert' 
	  type { var f = $type.val; }
	  type { var t = $type.val; }
	  expr { var e = $expr.val; }
	  ')'  { $val = new Convert(f, t, e); }
	| '(' 
	  'construct' 
	  representation_atom { 
	                        var t = $representation_atom.val; 
							var ll = new ArrayList<Expression>();
						  }
	  (expr               { ll.add($expr.val);} )*
	  ')'                 { $val = new Construct(t, new Tuple(ll)); }
	| '('
      'error'
	  expr { var e = $expr.val; }
	  ')'  { $val = new ExceptionExpr(e); }
	| '('
	  'extend'
	  expr { var el = $expr.val; }
	  expr { var i = $expr.val; }
	  expr { var c = $expr.val; }
	  ')'  { $val = new Extend(el, i, c); }
	| '(' 
	  'extend' 
	  expr { var el = $expr.val; }
	  expr { var i = $expr.val; }
	  ')'  { $val = new Extend(el, i); }
	| '(' 
	  'get' 
	  expr { var t = $expr.val; }
	  expr { var i = $expr.val; }
	  ')'  { $val = Get.makeGet(t, i); }
	| '(' 
	  'if' 
	  expr { var c = $expr.val; }
	  expr { var t = $expr.val; }
	  expr { var f = $expr.val; }
	  ')'  { $val = new IfExpression(c, t, f); }
	| '(' 
	  'instance-of' 
	  expr { var e = $expr.val; }
	  type { var t = $type.val; }
	  ')'  { $val = new InstanceOf(e, t); }
	| '(' 
	  'instance-of-representation' 
	  expr { var e = $expr.val; }
	  type { var t = $type.val; }
	  ')'  { $val = new InstanceOfRepresentation(e, t); }
	| '(' 
	  'loop' 
	  bind_list { var bds = $bind_list.val; }
	  expr      { var e = $expr.val; }
	  ')'       {
					var symbols = new ArrayList<Symbol>();
					var args = new ArrayList<Expression>();
					bds.stream().forEach(p -> {
												symbols.add(p.first);
												args.add(p.second);
											  });
					$val = new Loop(new Tuple(symbols), e, new Tuple(args));
		        }
	| '(' 
	  'or'  { var ll = new ArrayList<Expression>(); } 
	  (expr { ll.add($expr.val); })* 
	  ')'   { $val = new OrExpression(new Tuple(ll)); }
	| '(' 
	  'recur' { var ll = new ArrayList<Expression>(); }
	  (expr   { ll.add($expr.val); })* 
	  ')'     { $val = new Recur(new Tuple(ll)); }
	| '(' 
	  'lambda' 
	  argument_list { var ll = $argument_list.val; }
	  expr          { var e = $expr.val; }
	  ')'           {
						var args = new ArrayList<Expression>();
						var ts = new ArrayList<Type>();
						ll.stream().forEach(p -> {
													ts.add(p.first);
													args.add(p.second);
												 });
						$val = new Lambda(new Tuple(args), new TypeTuple(ts), e);
					}
	| '(' 
	  'extended-lambda' 
	  '('   { var ll = new ArrayList<Type>(); }
	  (type { ll.add($type.val); })+ 
	  ')' 
	  ')'   { $val = new ExtendedLambda(new TypeTuple(ll)); }
	| '(' 
	  'let' 
	  bind_list { var ll = $bind_list.val; }
	  expr      { var e = $expr.val; }
	  ')'       { $val = new Let(e, ll); }
	| '(' 
	  'let*' 
	  bind_list { var ll = $bind_list.val; }
	  expr      { var e = $expr.val; } 
	  ')'       { $val = new Let(e, ll); }
	| '('
  	  'let-type' 
	  '('     { var s = new HashSet<String>(); }
	  (SYMBOL { s.add($SYMBOL.text); })+ 
	  ')'     { this.typeVariableStack.push(s); }
	  expr    { var e = $expr.val; }
	  ')'     {
				this.typeVariableStack.pop();
				$val = e;
			  }
	| '('
	  'tuple' { var ll = new ArrayList<Expression>(); }
	  (expr   { ll.add($expr.val);} )*
	  ')'     { $val = new Tuple(ll); }
	| '('
	  'eapply'
	  expr { var f = $expr.val; }
	  expr { var a = $expr.val; }
	  ')'  { $val = new AbstractionApplication(f, a); }
	;

atom returns [Expression val]
	: INT	 { $val = new LitInteger(Long.parseLong($INT.text)); }
	| FLOAT  { $val = new LitDouble(Double.parseDouble($FLOAT.text)); }
	| SYMBOL { $val = new Symbol($SYMBOL.text); }
	| TRUE   { $val = LitBoolean.TRUE; }
	| FALSE  { $val = LitBoolean.FALSE; }
	| STRING { $val = new LitString(unescapeString($STRING.text.substring(1, $STRING.text.length() - 1))); }
	;

application returns [AbstractionApplication val]
	: '('   { var ll = new ArrayList<Expression>(); }
	  expr  { var f = $expr.val; }
	  (expr { ll.add($expr.val); })* 
	  ')'   { $val = new AbstractionApplication(f, new Tuple(ll)); }
	;

type returns [Type val]
    : type_arrow          { $val = $type_arrow.val; }
	| representation_atom { $val = $representation_atom.val; }
    | type_atom           { $val = $type_atom.val; }
    | type_tuple          { $val = $type_tuple.val; }
	;

//type_tail returns [Type val]
//    : type_atom           { $val = $type_atom.val; }
//    | representation_atom { $val = $representation_atom.val; }
//    | type_tuple          { $val = $type_tuple.val; }
//    ;
	
type_arrow returns [TypeArrow val]
    :
	'('
	type_tuple { var l = $type_tuple.val; } 
	ARROW
	type	   { $val = new TypeArrow(l, $type.val); }
	')'
    ;

type_atom returns [Type val]
    : SYMBOL {
				var name = $SYMBOL.text;
				if(this.isDeclaredTypeVariable(name)){
					$val = new TypeVariable(name);
				}
				else{
					$val = new TypeAtom(new TypeName($SYMBOL.text), TypeRepresentation.WILDCARD); 
				}
			 }
    ;
    
representation_atom returns [TypeAtom val]
    : SYMBOL { var t = new TypeName($SYMBOL.text); }
	  ':' 
	  SYMBOL { $val = new TypeAtom(t, new TypeRepresentation($SYMBOL.text)); }
    ;
    
type_tuple returns [TypeTuple val]
    : '('   { var ll = new ArrayList<Type>(); }
	  (type { ll.add($type.val); } )*
	  ')'   { $val = new TypeTuple(ll); }
    ;

TRUE	: '#' 't';
FALSE	: '#' 'f';
INT 	: '-'?[0-9]+ ;
FLOAT   : '-'?[0-9]* '.' [0-9]+ ;
COMMENT : ';' ~('\r' | '\n')* -> skip;
WS 		: [ \r\t\n]+ -> skip ;
STRING	: '"' ~('\n'|'"')* '"' ;
ARROW	: '#>' ;
LBRACKET: '(';
LSBRACKET: '[';
RBRACKET: ')';
RSBRACKET: ']';
COLLON	: ':';

SYMBOL 	
    : SYMBOL_HEAD SYMBOL_REST*;
  
fragment SYMBOL_HEAD
	:   'a'..'z' | 'A'..'Z' | '*' | '+' | '!' | '-' | '_' | '?' | '>' | '<' | '=' | '$' | '@' | '/'
	;
    
fragment SYMBOL_REST
	: SYMBOL_HEAD
	| '0'..'9'
	| '.'
	;