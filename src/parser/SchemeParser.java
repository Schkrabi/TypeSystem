// Generated from Scheme.g by ANTLR 4.7
package parser;
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

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;
import java.util.function.Predicate;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class SchemeParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, DEFTYPE=9, 
		DEFREP=10, IF=11, LAMBDA=12, ELAMBDA=13, TRUE=14, FALSE=15, INT=16, FLOAT=17, 
		SYMBOL=18, COMMENT=19, WS=20, STRING=21;
	public static final int
		RULE_exprs = 0, RULE_expr = 1, RULE_seq = 2, RULE_elambda = 3, RULE_lambda = 4, 
		RULE_deftype = 5, RULE_defrep = 6, RULE_typed = 7, RULE_type = 8, RULE_impl = 9, 
		RULE_typelist = 10, RULE_atom = 11, RULE_quote = 12;
	public static final String[] ruleNames = {
		"exprs", "expr", "seq", "elambda", "lambda", "deftype", "defrep", "typed", 
		"type", "impl", "typelist", "atom", "quote"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'('", "')'", "'['", "']'", "'<'", "'>'", "':'", "'''"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, "DEFTYPE", "DEFREP", 
		"IF", "LAMBDA", "ELAMBDA", "TRUE", "FALSE", "INT", "FLOAT", "SYMBOL", 
		"COMMENT", "WS", "STRING"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "Scheme.g"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }


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
				if(value instanceof Tuple){
					return new Application(constructor, (Tuple)value);
				}
				if(value instanceof Sequence){
					return new Application(constructor, ((Sequence)value).asTuple());
				}
				
				return new Application(constructor, new Tuple(new Expression[]{value}));
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
		
		public static Constructor getConstructorByTypeName(final String typeName, final String representationName) throws Exception{
			Optional<TypeConcrete> o;
			
			if(representationName == "") {	
				//o = types.stream().filter(x -> (x instanceof TypeConcrete) && x.name.equals(typeName)).findAny();
				o = types.stream().filter(new java.util.function.Predicate<TypeConcrete>(){

					@Override
					public boolean test(TypeConcrete x) {
						return (!(x instanceof TypeRepresentation)) && x.name.equals(typeName);
					}}).findAny();
			}
			else {
				//o = types.stream().filter(x -> (x instanceof TypeRepresentation) && x.name.equals(representationName) && ((TypeRepresentation)x).baseType.name.equals(typeName)).findAny();
				o = types.stream().filter(new java.util.function.Predicate<TypeConcrete>(){
					@Override
					public boolean test(TypeConcrete x) {
						return (x instanceof TypeRepresentation) && x.name.equals(representationName) && ((TypeRepresentation)x).baseType.name.equals(typeName);
					}}).findAny();
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
			
		public static void defineRepresentation(final String typeName, String repName, TypeTuple constructorArgsType, Lambda lConstructor) throws Exception{
			Optional<TypeConcrete> o = types.stream().filter(new java.util.function.Predicate<TypeConcrete>(){
				@Override
				public boolean test(TypeConcrete x) {
					return (!(x instanceof TypeRepresentation)) && x.name.equals(typeName);
				}}).findAny(); 
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

	public SchemeParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class ExprsContext extends ParserRuleContext {
		public List<Expression> val;
		public ExprContext expr;
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public ExprsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprs; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterExprs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitExprs(this);
		}
	}

	public final ExprsContext exprs() throws Exception {
		ExprsContext _localctx = new ExprsContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_exprs);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			 List<Expression> ll = new ArrayList<>(); 
			setState(32);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__4) | (1L << T__7) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
				{
				{
				setState(27);
				((ExprsContext)_localctx).expr = expr();
				 ll.add(((ExprsContext)_localctx).expr.val); 
				}
				}
				setState(34);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			 ((ExprsContext)_localctx).val =  ll; 
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprContext extends ParserRuleContext {
		public Expression val;
		public AtomContext atom;
		public SeqContext seq;
		public QuoteContext quote;
		public TypedContext typed;
		public AtomContext atom() {
			return getRuleContext(AtomContext.class,0);
		}
		public SeqContext seq() {
			return getRuleContext(SeqContext.class,0);
		}
		public QuoteContext quote() {
			return getRuleContext(QuoteContext.class,0);
		}
		public TypedContext typed() {
			return getRuleContext(TypedContext.class,0);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitExpr(this);
		}
	}

	public final ExprContext expr() throws Exception {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_expr);
		try {
			setState(49);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case TRUE:
			case FALSE:
			case INT:
			case FLOAT:
			case SYMBOL:
			case STRING:
				enterOuterAlt(_localctx, 1);
				{
				setState(37);
				((ExprContext)_localctx).atom = atom();
				 ((ExprContext)_localctx).val =  instantiateUntypedLiteral(((ExprContext)_localctx).atom.val); 
				}
				break;
			case T__0:
				enterOuterAlt(_localctx, 2);
				{
				setState(40);
				((ExprContext)_localctx).seq = seq();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).seq.val; 
				}
				break;
			case T__7:
				enterOuterAlt(_localctx, 3);
				{
				setState(43);
				((ExprContext)_localctx).quote = quote();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).quote.val; 
				}
				break;
			case T__4:
				enterOuterAlt(_localctx, 4);
				{
				setState(46);
				((ExprContext)_localctx).typed = typed();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).typed.val; 
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SeqContext extends ParserRuleContext {
		public Expression val;
		public LambdaContext lambda;
		public ElambdaContext elambda;
		public DeftypeContext deftype;
		public DefrepContext defrep;
		public ExprContext expr;
		public LambdaContext lambda() {
			return getRuleContext(LambdaContext.class,0);
		}
		public ElambdaContext elambda() {
			return getRuleContext(ElambdaContext.class,0);
		}
		public DeftypeContext deftype() {
			return getRuleContext(DeftypeContext.class,0);
		}
		public DefrepContext defrep() {
			return getRuleContext(DefrepContext.class,0);
		}
		public TerminalNode IF() { return getToken(SchemeParser.IF, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public SeqContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_seq; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterSeq(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitSeq(this);
		}
	}

	public final SeqContext seq() throws Exception {
		SeqContext _localctx = new SeqContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_seq);
		int _la;
		try {
			setState(87);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(51);
				((SeqContext)_localctx).lambda = lambda();
				 ((SeqContext)_localctx).val =  ((SeqContext)_localctx).lambda.val; 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(54);
				((SeqContext)_localctx).elambda = elambda();
				 ((SeqContext)_localctx).val =  ((SeqContext)_localctx).elambda.val; 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(57);
				((SeqContext)_localctx).deftype = deftype();
				 ((SeqContext)_localctx).val =  ((SeqContext)_localctx).deftype.val; 
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(60);
				((SeqContext)_localctx).defrep = defrep();
				 ((SeqContext)_localctx).val =  ((SeqContext)_localctx).defrep.val; 
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(63);
				match(T__0);
				 Expression cond, tBranch, fBranch; 
				setState(65);
				match(IF);
				setState(66);
				((SeqContext)_localctx).expr = expr();
				 cond = ((SeqContext)_localctx).expr.val; 
				setState(68);
				((SeqContext)_localctx).expr = expr();
				 tBranch = ((SeqContext)_localctx).expr.val; 
				setState(70);
				((SeqContext)_localctx).expr = expr();
				 fBranch = ((SeqContext)_localctx).expr.val; 
				setState(72);
				match(T__1);
				 ((SeqContext)_localctx).val =  new IfExpression(cond, tBranch, fBranch); 
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(75);
				match(T__0);
				 List<Expression> ll = new ArrayList<>(); 
				setState(82);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__4) | (1L << T__7) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
					{
					{
					setState(77);
					((SeqContext)_localctx).expr = expr();
					 ll.add(((SeqContext)_localctx).expr.val); 
					}
					}
					setState(84);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(85);
				match(T__1);
				 ((SeqContext)_localctx).val =  new Sequence(ll); 
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ElambdaContext extends ParserRuleContext {
		public ExtendedLambda val;
		public ExprContext expr;
		public ImplContext impl;
		public TerminalNode ELAMBDA() { return getToken(SchemeParser.ELAMBDA, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public List<ImplContext> impl() {
			return getRuleContexts(ImplContext.class);
		}
		public ImplContext impl(int i) {
			return getRuleContext(ImplContext.class,i);
		}
		public ElambdaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_elambda; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterElambda(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitElambda(this);
		}
	}

	public final ElambdaContext elambda() throws Exception {
		ElambdaContext _localctx = new ElambdaContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_elambda);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(89);
			match(T__0);
			 Set<ImplContainer> s = new TreeSet<ImplContainer>(); Expression argList, body; 
			setState(91);
			match(ELAMBDA);
			setState(92);
			((ElambdaContext)_localctx).expr = expr();
			 argList = ((ElambdaContext)_localctx).expr.val; 
			setState(94);
			((ElambdaContext)_localctx).expr = expr();
			 body = ((ElambdaContext)_localctx).expr.val; 
			setState(96);
			match(T__2);
			setState(102);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(97);
				((ElambdaContext)_localctx).impl = impl();
				 s.add(((ElambdaContext)_localctx).impl.val); 
				}
				}
				setState(104);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(105);
			match(T__3);
			setState(106);
			match(T__1);
			 ((ElambdaContext)_localctx).val =  new ExtendedLambda(lambdaArgsTuple(argList), body, s); 
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class LambdaContext extends ParserRuleContext {
		public Lambda val;
		public ExprContext expr;
		public TerminalNode LAMBDA() { return getToken(SchemeParser.LAMBDA, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public LambdaContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_lambda; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterLambda(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitLambda(this);
		}
	}

	public final LambdaContext lambda() throws Exception {
		LambdaContext _localctx = new LambdaContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_lambda);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(109);
			match(T__0);
			 Expression argList, body; 
			setState(111);
			match(LAMBDA);
			setState(112);
			((LambdaContext)_localctx).expr = expr();
			 argList = ((LambdaContext)_localctx).expr.val; 
			setState(114);
			((LambdaContext)_localctx).expr = expr();
			 body = ((LambdaContext)_localctx).expr.val; 
			setState(116);
			match(T__1);
			 ((LambdaContext)_localctx).val =  new Lambda(lambdaArgsTuple(argList), body); 
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DeftypeContext extends ParserRuleContext {
		public Expression val;
		public Token SYMBOL;
		public TypelistContext typelist;
		public LambdaContext lambda;
		public TerminalNode DEFTYPE() { return getToken(SchemeParser.DEFTYPE, 0); }
		public TerminalNode SYMBOL() { return getToken(SchemeParser.SYMBOL, 0); }
		public TypelistContext typelist() {
			return getRuleContext(TypelistContext.class,0);
		}
		public LambdaContext lambda() {
			return getRuleContext(LambdaContext.class,0);
		}
		public DeftypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_deftype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterDeftype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitDeftype(this);
		}
	}

	public final DeftypeContext deftype() throws Exception {
		DeftypeContext _localctx = new DeftypeContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_deftype);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(119);
			match(T__0);
			 String name; TypeTuple t; Lambda lambda; 
			setState(121);
			match(DEFTYPE);
			setState(122);
			((DeftypeContext)_localctx).SYMBOL = match(SYMBOL);
			 name = (((DeftypeContext)_localctx).SYMBOL!=null?((DeftypeContext)_localctx).SYMBOL.getText():null); 
			setState(124);
			((DeftypeContext)_localctx).typelist = typelist();
			 t = ((DeftypeContext)_localctx).typelist.val; 
			setState(126);
			((DeftypeContext)_localctx).lambda = lambda();
			 lambda = ((DeftypeContext)_localctx).lambda.val; 
			setState(128);
			match(T__1);
			 defineType(name, t, lambda); ((DeftypeContext)_localctx).val =  Expression.EMPTY_EXPRESSION; 
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class DefrepContext extends ParserRuleContext {
		public Expression val;
		public Token SYMBOL;
		public TypelistContext typelist;
		public LambdaContext lambda;
		public TerminalNode DEFREP() { return getToken(SchemeParser.DEFREP, 0); }
		public List<TerminalNode> SYMBOL() { return getTokens(SchemeParser.SYMBOL); }
		public TerminalNode SYMBOL(int i) {
			return getToken(SchemeParser.SYMBOL, i);
		}
		public TypelistContext typelist() {
			return getRuleContext(TypelistContext.class,0);
		}
		public LambdaContext lambda() {
			return getRuleContext(LambdaContext.class,0);
		}
		public DefrepContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_defrep; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterDefrep(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitDefrep(this);
		}
	}

	public final DefrepContext defrep() throws Exception {
		DefrepContext _localctx = new DefrepContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_defrep);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(131);
			match(T__0);
			 String typeName, repName; TypeTuple t; Lambda lambda; 
			setState(133);
			match(DEFREP);
			setState(134);
			((DefrepContext)_localctx).SYMBOL = match(SYMBOL);
			 repName = (((DefrepContext)_localctx).SYMBOL!=null?((DefrepContext)_localctx).SYMBOL.getText():null); 
			setState(136);
			((DefrepContext)_localctx).SYMBOL = match(SYMBOL);
			 typeName = (((DefrepContext)_localctx).SYMBOL!=null?((DefrepContext)_localctx).SYMBOL.getText():null); 
			setState(138);
			((DefrepContext)_localctx).typelist = typelist();
			 t = ((DefrepContext)_localctx).typelist.val; 
			setState(140);
			((DefrepContext)_localctx).lambda = lambda();
			 lambda = ((DefrepContext)_localctx).lambda.val; 
			setState(142);
			match(T__1);
			 defineRepresentation(typeName, repName, t, lambda); ((DefrepContext)_localctx).val =  Expression.EMPTY_EXPRESSION; 
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypedContext extends ParserRuleContext {
		public Expression val;
		public ExprContext expr;
		public TypeContext type;
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TypedContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typed; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterTyped(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitTyped(this);
		}
	}

	public final TypedContext typed() throws Exception {
		TypedContext _localctx = new TypedContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_typed);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(145);
			match(T__4);
			 Expression value; Constructor type; 
			setState(147);
			((TypedContext)_localctx).expr = expr();
			 value = ((TypedContext)_localctx).expr.val; 
			setState(149);
			((TypedContext)_localctx).type = type();
			 type = ((TypedContext)_localctx).type.val; 
			setState(151);
			match(T__5);
			 ((TypedContext)_localctx).val =  instantiateTyped(type, value); 
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypeContext extends ParserRuleContext {
		public Constructor val;
		public Token SYMBOL;
		public List<TerminalNode> SYMBOL() { return getTokens(SchemeParser.SYMBOL); }
		public TerminalNode SYMBOL(int i) {
			return getToken(SchemeParser.SYMBOL, i);
		}
		public TypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitType(this);
		}
	}

	public final TypeContext type() throws Exception {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_type);
		try {
			setState(162);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(154);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 ((TypeContext)_localctx).val =  getConstructorByTypeName((((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				String typeName; 
				setState(157);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 typeName = (((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null); 
				setState(159);
				match(T__6);
				setState(160);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 ((TypeContext)_localctx).val =  getConstructorByTypeName(typeName, (((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ImplContext extends ParserRuleContext {
		public ImplContainer val;
		public TypelistContext typelist;
		public ExprContext expr;
		public TypelistContext typelist() {
			return getRuleContext(TypelistContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public ImplContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_impl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterImpl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitImpl(this);
		}
	}

	public final ImplContext impl() throws Exception {
		ImplContext _localctx = new ImplContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_impl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			 TypeTuple t; 
			setState(165);
			((ImplContext)_localctx).typelist = typelist();
			 t = ((ImplContext)_localctx).typelist.val; 
			setState(167);
			((ImplContext)_localctx).expr = expr();
			 ((ImplContext)_localctx).val =  new ImplContainer(t, ((ImplContext)_localctx).expr.val); 
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class TypelistContext extends ParserRuleContext {
		public TypeTuple val;
		public TypeContext type;
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public TypelistContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_typelist; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterTypelist(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitTypelist(this);
		}
	}

	public final TypelistContext typelist() throws Exception {
		TypelistContext _localctx = new TypelistContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_typelist);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(170);
			match(T__0);
			 List<Type> ll = new ArrayList<Type>(); 
			setState(177);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SYMBOL) {
				{
				{
				setState(172);
				((TypelistContext)_localctx).type = type();
				 ll.add(((TypelistContext)_localctx).type.val.constructedType); 
				}
				}
				setState(179);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(180);
			match(T__1);
			 ((TypelistContext)_localctx).val =  new TypeTuple(ll); 
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AtomContext extends ParserRuleContext {
		public Object val;
		public Token INT;
		public Token FLOAT;
		public Token SYMBOL;
		public Token STRING;
		public TerminalNode INT() { return getToken(SchemeParser.INT, 0); }
		public TerminalNode FLOAT() { return getToken(SchemeParser.FLOAT, 0); }
		public TerminalNode SYMBOL() { return getToken(SchemeParser.SYMBOL, 0); }
		public TerminalNode TRUE() { return getToken(SchemeParser.TRUE, 0); }
		public TerminalNode FALSE() { return getToken(SchemeParser.FALSE, 0); }
		public TerminalNode STRING() { return getToken(SchemeParser.STRING, 0); }
		public AtomContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_atom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterAtom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitAtom(this);
		}
	}

	public final AtomContext atom() throws RecognitionException {
		AtomContext _localctx = new AtomContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_atom);
		try {
			setState(195);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(183);
				((AtomContext)_localctx).INT = match(INT);
				 ((AtomContext)_localctx).val =  Integer.parseInt((((AtomContext)_localctx).INT!=null?((AtomContext)_localctx).INT.getText():null)); 
				}
				break;
			case FLOAT:
				enterOuterAlt(_localctx, 2);
				{
				setState(185);
				((AtomContext)_localctx).FLOAT = match(FLOAT);
				 ((AtomContext)_localctx).val =  Double.parseDouble((((AtomContext)_localctx).FLOAT!=null?((AtomContext)_localctx).FLOAT.getText():null)); 
				}
				break;
			case SYMBOL:
				enterOuterAlt(_localctx, 3);
				{
				setState(187);
				((AtomContext)_localctx).SYMBOL = match(SYMBOL);
				 ((AtomContext)_localctx).val =  new Variable((((AtomContext)_localctx).SYMBOL!=null?((AtomContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case TRUE:
				enterOuterAlt(_localctx, 4);
				{
				setState(189);
				match(TRUE);
				 ((AtomContext)_localctx).val =  Boolean.TRUE; 
				}
				break;
			case FALSE:
				enterOuterAlt(_localctx, 5);
				{
				setState(191);
				match(FALSE);
				 ((AtomContext)_localctx).val =  Boolean.FALSE; 
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 6);
				{
				setState(193);
				((AtomContext)_localctx).STRING = match(STRING);
				 ((AtomContext)_localctx).val =  unescapeString((((AtomContext)_localctx).STRING!=null?((AtomContext)_localctx).STRING.getText():null).substring(1, (((AtomContext)_localctx).STRING!=null?((AtomContext)_localctx).STRING.getText():null).length() - 1)); 
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class QuoteContext extends ParserRuleContext {
		public Expression val;
		public ExprContext expr;
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public QuoteContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_quote; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterQuote(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitQuote(this);
		}
	}

	public final QuoteContext quote() throws Exception {
		QuoteContext _localctx = new QuoteContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_quote);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(197);
			match(T__7);
			setState(198);
			((QuoteContext)_localctx).expr = expr();
			 ((QuoteContext)_localctx).val =  new QuotedExpression(((QuoteContext)_localctx).expr.val); 
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\27\u00cc\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\3\2\3\2\3\2\3\2\7\2!\n\2\f\2\16\2$\13"+
		"\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3\64\n\3"+
		"\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3"+
		"\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\7\4S\n\4\f\4\16\4V\13\4"+
		"\3\4\3\4\5\4Z\n\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\7\5g\n\5"+
		"\f\5\16\5j\13\5\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3"+
		"\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\3\b"+
		"\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3"+
		"\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\5\n\u00a5\n\n\3\13\3\13\3\13\3\13\3"+
		"\13\3\13\3\f\3\f\3\f\3\f\3\f\7\f\u00b2\n\f\f\f\16\f\u00b5\13\f\3\f\3\f"+
		"\3\f\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\5\r\u00c6\n\r\3\16"+
		"\3\16\3\16\3\16\3\16\2\2\17\2\4\6\b\n\f\16\20\22\24\26\30\32\2\2\2\u00d0"+
		"\2\34\3\2\2\2\4\63\3\2\2\2\6Y\3\2\2\2\b[\3\2\2\2\no\3\2\2\2\fy\3\2\2\2"+
		"\16\u0085\3\2\2\2\20\u0093\3\2\2\2\22\u00a4\3\2\2\2\24\u00a6\3\2\2\2\26"+
		"\u00ac\3\2\2\2\30\u00c5\3\2\2\2\32\u00c7\3\2\2\2\34\"\b\2\1\2\35\36\5"+
		"\4\3\2\36\37\b\2\1\2\37!\3\2\2\2 \35\3\2\2\2!$\3\2\2\2\" \3\2\2\2\"#\3"+
		"\2\2\2#%\3\2\2\2$\"\3\2\2\2%&\b\2\1\2&\3\3\2\2\2\'(\5\30\r\2()\b\3\1\2"+
		")\64\3\2\2\2*+\5\6\4\2+,\b\3\1\2,\64\3\2\2\2-.\5\32\16\2./\b\3\1\2/\64"+
		"\3\2\2\2\60\61\5\20\t\2\61\62\b\3\1\2\62\64\3\2\2\2\63\'\3\2\2\2\63*\3"+
		"\2\2\2\63-\3\2\2\2\63\60\3\2\2\2\64\5\3\2\2\2\65\66\5\n\6\2\66\67\b\4"+
		"\1\2\67Z\3\2\2\289\5\b\5\29:\b\4\1\2:Z\3\2\2\2;<\5\f\7\2<=\b\4\1\2=Z\3"+
		"\2\2\2>?\5\16\b\2?@\b\4\1\2@Z\3\2\2\2AB\7\3\2\2BC\b\4\1\2CD\7\r\2\2DE"+
		"\5\4\3\2EF\b\4\1\2FG\5\4\3\2GH\b\4\1\2HI\5\4\3\2IJ\b\4\1\2JK\7\4\2\2K"+
		"L\b\4\1\2LZ\3\2\2\2MN\7\3\2\2NT\b\4\1\2OP\5\4\3\2PQ\b\4\1\2QS\3\2\2\2"+
		"RO\3\2\2\2SV\3\2\2\2TR\3\2\2\2TU\3\2\2\2UW\3\2\2\2VT\3\2\2\2WX\7\4\2\2"+
		"XZ\b\4\1\2Y\65\3\2\2\2Y8\3\2\2\2Y;\3\2\2\2Y>\3\2\2\2YA\3\2\2\2YM\3\2\2"+
		"\2Z\7\3\2\2\2[\\\7\3\2\2\\]\b\5\1\2]^\7\17\2\2^_\5\4\3\2_`\b\5\1\2`a\5"+
		"\4\3\2ab\b\5\1\2bh\7\5\2\2cd\5\24\13\2de\b\5\1\2eg\3\2\2\2fc\3\2\2\2g"+
		"j\3\2\2\2hf\3\2\2\2hi\3\2\2\2ik\3\2\2\2jh\3\2\2\2kl\7\6\2\2lm\7\4\2\2"+
		"mn\b\5\1\2n\t\3\2\2\2op\7\3\2\2pq\b\6\1\2qr\7\16\2\2rs\5\4\3\2st\b\6\1"+
		"\2tu\5\4\3\2uv\b\6\1\2vw\7\4\2\2wx\b\6\1\2x\13\3\2\2\2yz\7\3\2\2z{\b\7"+
		"\1\2{|\7\13\2\2|}\7\24\2\2}~\b\7\1\2~\177\5\26\f\2\177\u0080\b\7\1\2\u0080"+
		"\u0081\5\n\6\2\u0081\u0082\b\7\1\2\u0082\u0083\7\4\2\2\u0083\u0084\b\7"+
		"\1\2\u0084\r\3\2\2\2\u0085\u0086\7\3\2\2\u0086\u0087\b\b\1\2\u0087\u0088"+
		"\7\f\2\2\u0088\u0089\7\24\2\2\u0089\u008a\b\b\1\2\u008a\u008b\7\24\2\2"+
		"\u008b\u008c\b\b\1\2\u008c\u008d\5\26\f\2\u008d\u008e\b\b\1\2\u008e\u008f"+
		"\5\n\6\2\u008f\u0090\b\b\1\2\u0090\u0091\7\4\2\2\u0091\u0092\b\b\1\2\u0092"+
		"\17\3\2\2\2\u0093\u0094\7\7\2\2\u0094\u0095\b\t\1\2\u0095\u0096\5\4\3"+
		"\2\u0096\u0097\b\t\1\2\u0097\u0098\5\22\n\2\u0098\u0099\b\t\1\2\u0099"+
		"\u009a\7\b\2\2\u009a\u009b\b\t\1\2\u009b\21\3\2\2\2\u009c\u009d\7\24\2"+
		"\2\u009d\u00a5\b\n\1\2\u009e\u009f\b\n\1\2\u009f\u00a0\7\24\2\2\u00a0"+
		"\u00a1\b\n\1\2\u00a1\u00a2\7\t\2\2\u00a2\u00a3\7\24\2\2\u00a3\u00a5\b"+
		"\n\1\2\u00a4\u009c\3\2\2\2\u00a4\u009e\3\2\2\2\u00a5\23\3\2\2\2\u00a6"+
		"\u00a7\b\13\1\2\u00a7\u00a8\5\26\f\2\u00a8\u00a9\b\13\1\2\u00a9\u00aa"+
		"\5\4\3\2\u00aa\u00ab\b\13\1\2\u00ab\25\3\2\2\2\u00ac\u00ad\7\3\2\2\u00ad"+
		"\u00b3\b\f\1\2\u00ae\u00af\5\22\n\2\u00af\u00b0\b\f\1\2\u00b0\u00b2\3"+
		"\2\2\2\u00b1\u00ae\3\2\2\2\u00b2\u00b5\3\2\2\2\u00b3\u00b1\3\2\2\2\u00b3"+
		"\u00b4\3\2\2\2\u00b4\u00b6\3\2\2\2\u00b5\u00b3\3\2\2\2\u00b6\u00b7\7\4"+
		"\2\2\u00b7\u00b8\b\f\1\2\u00b8\27\3\2\2\2\u00b9\u00ba\7\22\2\2\u00ba\u00c6"+
		"\b\r\1\2\u00bb\u00bc\7\23\2\2\u00bc\u00c6\b\r\1\2\u00bd\u00be\7\24\2\2"+
		"\u00be\u00c6\b\r\1\2\u00bf\u00c0\7\20\2\2\u00c0\u00c6\b\r\1\2\u00c1\u00c2"+
		"\7\21\2\2\u00c2\u00c6\b\r\1\2\u00c3\u00c4\7\27\2\2\u00c4\u00c6\b\r\1\2"+
		"\u00c5\u00b9\3\2\2\2\u00c5\u00bb\3\2\2\2\u00c5\u00bd\3\2\2\2\u00c5\u00bf"+
		"\3\2\2\2\u00c5\u00c1\3\2\2\2\u00c5\u00c3\3\2\2\2\u00c6\31\3\2\2\2\u00c7"+
		"\u00c8\7\n\2\2\u00c8\u00c9\5\4\3\2\u00c9\u00ca\b\16\1\2\u00ca\33\3\2\2"+
		"\2\n\"\63TYh\u00a4\u00b3\u00c5";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}