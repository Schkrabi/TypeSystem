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
		RULE_atom = 10, RULE_quote = 11;
	public static final String[] ruleNames = {
		"exprs", "expr", "seq", "elambda", "lambda", "deftype", "defrep", "typed", 
		"type", "impl", "atom", "quote"
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
				types.add(TypeRepresentation.TypeString);
				constructorMap.put(TypeRepresentation.TypeString, Constructor.IntStringConstructor);
				
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
					o = types.stream().filter(x -> (x instanceof TypeConcrete) && x.name == typeName).findAny();
				}
				else {
					o = types.stream().filter(x -> (x instanceof TypeRepresentation) && x.name == representationName && ((TypeRepresentation)x).baseType.name == typeName).findAny();
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
			
			public static void defineType(String typeName, Lambda lConstructor) throws Exception{
				TypeConcrete newType = new TypeConcrete(typeName);
				Constructor constructor = new Constructor(newType, lConstructor.args, lConstructor.getBody());
				
				addType(newType);
				addConstructor(newType, constructor);
			}
			
			public static void defineRepresentation(String typeName, String repName, Lambda lConstructor) throws Exception{
				Optional<TypeConcrete> o = types.stream().filter(x -> (x instanceof TypeConcrete) && x.name == typeName).findAny(); 
				if(!o.isPresent()) {
					throw new Exception("Unknown base type: " + typeName);
				}
				
				TypeConcrete baseType = o.get();
				TypeRepresentation newType = new TypeRepresentation(repName, baseType);
				addType(newType);
				
				Constructor constructor = new Constructor(newType, lConstructor.args, lConstructor.getBody());
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
			setState(30);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__4) | (1L << T__7) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
				{
				{
				setState(25);
				((ExprsContext)_localctx).expr = expr();
				 ll.add(((ExprsContext)_localctx).expr.val); 
				}
				}
				setState(32);
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
			setState(47);
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
				setState(35);
				((ExprContext)_localctx).atom = atom();
				 ((ExprContext)_localctx).val =  instantiateUntypedLiteral(((ExprContext)_localctx).atom.val); 
				}
				break;
			case T__0:
				enterOuterAlt(_localctx, 2);
				{
				setState(38);
				((ExprContext)_localctx).seq = seq();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).seq.val; 
				}
				break;
			case T__7:
				enterOuterAlt(_localctx, 3);
				{
				setState(41);
				((ExprContext)_localctx).quote = quote();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).quote.val; 
				}
				break;
			case T__4:
				enterOuterAlt(_localctx, 4);
				{
				setState(44);
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
		public ExprContext expr;
		public DeftypeContext deftype() {
			return getRuleContext(DeftypeContext.class,0);
		}
		public DefrepContext defrep() {
			return getRuleContext(DefrepContext.class,0);
		}
		public ElambdaContext elambda() {
			return getRuleContext(ElambdaContext.class,0);
		}
		public LambdaContext lambda() {
			return getRuleContext(LambdaContext.class,0);
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
			setState(77);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(49);
				deftype();
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(50);
				defrep();
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(51);
				elambda();
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(52);
				lambda();
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(53);
				match(T__0);
				 Expression cond, tBranch, fBranch; 
				setState(55);
				match(IF);
				setState(56);
				((SeqContext)_localctx).expr = expr();
				 cond = ((SeqContext)_localctx).expr.val; 
				setState(58);
				((SeqContext)_localctx).expr = expr();
				 tBranch = ((SeqContext)_localctx).expr.val; 
				setState(60);
				((SeqContext)_localctx).expr = expr();
				 fBranch = ((SeqContext)_localctx).expr.val; 
				setState(62);
				match(T__1);
				 ((SeqContext)_localctx).val =  new IfExpression(cond, tBranch, fBranch); 
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(65);
				match(T__0);
				 List<Expression> ll = new ArrayList<>(); 
				setState(72);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__4) | (1L << T__7) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
					{
					{
					setState(67);
					((SeqContext)_localctx).expr = expr();
					 ll.add(((SeqContext)_localctx).expr.val); 
					}
					}
					setState(74);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(75);
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
			setState(79);
			match(T__0);
			 Set<ImplContainer> s = new TreeSet<ImplContainer>(); Expression argList, body; 
			setState(81);
			match(ELAMBDA);
			setState(82);
			((ElambdaContext)_localctx).expr = expr();
			 argList = ((ElambdaContext)_localctx).expr.val; 
			setState(84);
			((ElambdaContext)_localctx).expr = expr();
			 body = ((ElambdaContext)_localctx).expr.val; 
			setState(86);
			match(T__2);
			setState(92);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(87);
				((ElambdaContext)_localctx).impl = impl();
				 s.add(((ElambdaContext)_localctx).impl.val); 
				}
				}
				setState(94);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(95);
			match(T__3);
			setState(96);
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
			setState(99);
			match(T__0);
			 Expression argList, body; 
			setState(101);
			match(LAMBDA);
			setState(102);
			((LambdaContext)_localctx).expr = expr();
			 argList = ((LambdaContext)_localctx).expr.val; 
			setState(104);
			((LambdaContext)_localctx).expr = expr();
			 body = ((LambdaContext)_localctx).expr.val; 
			setState(106);
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
		public Token SYMBOL;
		public LambdaContext lambda;
		public TerminalNode DEFTYPE() { return getToken(SchemeParser.DEFTYPE, 0); }
		public TerminalNode SYMBOL() { return getToken(SchemeParser.SYMBOL, 0); }
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
			setState(109);
			match(T__0);
			 String name; Lambda lambda; 
			setState(111);
			match(DEFTYPE);
			setState(112);
			((DeftypeContext)_localctx).SYMBOL = match(SYMBOL);
			 name = (((DeftypeContext)_localctx).SYMBOL!=null?((DeftypeContext)_localctx).SYMBOL.getText():null); 
			setState(114);
			((DeftypeContext)_localctx).lambda = lambda();
			 lambda = ((DeftypeContext)_localctx).lambda.val; 
			setState(116);
			match(T__1);
			 defineType(name, lambda); 
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
		public Token SYMBOL;
		public LambdaContext lambda;
		public TerminalNode DEFREP() { return getToken(SchemeParser.DEFREP, 0); }
		public List<TerminalNode> SYMBOL() { return getTokens(SchemeParser.SYMBOL); }
		public TerminalNode SYMBOL(int i) {
			return getToken(SchemeParser.SYMBOL, i);
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
			setState(119);
			match(T__0);
			 String typeName, repName; Lambda lambda; 
			setState(121);
			match(DEFREP);
			setState(122);
			((DefrepContext)_localctx).SYMBOL = match(SYMBOL);
			 repName = (((DefrepContext)_localctx).SYMBOL!=null?((DefrepContext)_localctx).SYMBOL.getText():null); 
			setState(124);
			((DefrepContext)_localctx).SYMBOL = match(SYMBOL);
			 typeName = (((DefrepContext)_localctx).SYMBOL!=null?((DefrepContext)_localctx).SYMBOL.getText():null); 
			setState(126);
			((DefrepContext)_localctx).lambda = lambda();
			 lambda = ((DefrepContext)_localctx).lambda.val; 
			setState(128);
			match(T__1);
			 defineRepresentation(typeName, repName, lambda); 
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
			setState(131);
			match(T__4);
			 Expression value; Constructor type; 
			setState(133);
			((TypedContext)_localctx).expr = expr();
			 value = ((TypedContext)_localctx).expr.val; 
			setState(135);
			((TypedContext)_localctx).type = type();
			 type = ((TypedContext)_localctx).type.val; 
			setState(137);
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
			setState(148);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(140);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 ((TypeContext)_localctx).val =  getConstructorByTypeName((((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				String typeName; 
				setState(143);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 typeName = (((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null); 
				setState(145);
				match(T__6);
				setState(146);
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
		public TypeContext type;
		public ExprContext expr;
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
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
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(150);
			match(T__0);
			 List<Type> ll = new ArrayList<Type>(); 
			setState(157);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SYMBOL) {
				{
				{
				setState(152);
				((ImplContext)_localctx).type = type();
				 ll.add(((ImplContext)_localctx).type.val.constructedType); 
				}
				}
				setState(159);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(160);
			match(T__1);
			setState(161);
			((ImplContext)_localctx).expr = expr();
			 ((ImplContext)_localctx).val =  new ImplContainer(new TypeTuple(ll), ((ImplContext)_localctx).expr.val); 
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
		enterRule(_localctx, 20, RULE_atom);
		try {
			setState(176);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(164);
				((AtomContext)_localctx).INT = match(INT);
				 ((AtomContext)_localctx).val =  Integer.parseInt((((AtomContext)_localctx).INT!=null?((AtomContext)_localctx).INT.getText():null)); 
				}
				break;
			case FLOAT:
				enterOuterAlt(_localctx, 2);
				{
				setState(166);
				((AtomContext)_localctx).FLOAT = match(FLOAT);
				 ((AtomContext)_localctx).val =  Double.parseDouble((((AtomContext)_localctx).FLOAT!=null?((AtomContext)_localctx).FLOAT.getText():null)); 
				}
				break;
			case SYMBOL:
				enterOuterAlt(_localctx, 3);
				{
				setState(168);
				((AtomContext)_localctx).SYMBOL = match(SYMBOL);
				 ((AtomContext)_localctx).val =  new Variable((((AtomContext)_localctx).SYMBOL!=null?((AtomContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case TRUE:
				enterOuterAlt(_localctx, 4);
				{
				setState(170);
				match(TRUE);
				 ((AtomContext)_localctx).val =  Boolean.TRUE; 
				}
				break;
			case FALSE:
				enterOuterAlt(_localctx, 5);
				{
				setState(172);
				match(FALSE);
				 ((AtomContext)_localctx).val =  Boolean.FALSE; 
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 6);
				{
				setState(174);
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
		enterRule(_localctx, 22, RULE_quote);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(178);
			match(T__7);
			setState(179);
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\27\u00b9\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\3\2\3\2\3\2\3\2\7\2\37\n\2\f\2\16\2\"\13\2\3\2\3"+
		"\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3\62\n\3\3\4\3\4"+
		"\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3"+
		"\4\3\4\7\4I\n\4\f\4\16\4L\13\4\3\4\3\4\5\4P\n\4\3\5\3\5\3\5\3\5\3\5\3"+
		"\5\3\5\3\5\3\5\3\5\3\5\7\5]\n\5\f\5\16\5`\13\5\3\5\3\5\3\5\3\5\3\6\3\6"+
		"\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3"+
		"\7\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t"+
		"\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\5\n\u0097\n\n\3\13\3"+
		"\13\3\13\3\13\3\13\7\13\u009e\n\13\f\13\16\13\u00a1\13\13\3\13\3\13\3"+
		"\13\3\13\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\5\f\u00b3\n\f"+
		"\3\r\3\r\3\r\3\r\3\r\2\2\16\2\4\6\b\n\f\16\20\22\24\26\30\2\2\2\u00be"+
		"\2\32\3\2\2\2\4\61\3\2\2\2\6O\3\2\2\2\bQ\3\2\2\2\ne\3\2\2\2\fo\3\2\2\2"+
		"\16y\3\2\2\2\20\u0085\3\2\2\2\22\u0096\3\2\2\2\24\u0098\3\2\2\2\26\u00b2"+
		"\3\2\2\2\30\u00b4\3\2\2\2\32 \b\2\1\2\33\34\5\4\3\2\34\35\b\2\1\2\35\37"+
		"\3\2\2\2\36\33\3\2\2\2\37\"\3\2\2\2 \36\3\2\2\2 !\3\2\2\2!#\3\2\2\2\""+
		" \3\2\2\2#$\b\2\1\2$\3\3\2\2\2%&\5\26\f\2&\'\b\3\1\2\'\62\3\2\2\2()\5"+
		"\6\4\2)*\b\3\1\2*\62\3\2\2\2+,\5\30\r\2,-\b\3\1\2-\62\3\2\2\2./\5\20\t"+
		"\2/\60\b\3\1\2\60\62\3\2\2\2\61%\3\2\2\2\61(\3\2\2\2\61+\3\2\2\2\61.\3"+
		"\2\2\2\62\5\3\2\2\2\63P\5\f\7\2\64P\5\16\b\2\65P\5\b\5\2\66P\5\n\6\2\67"+
		"8\7\3\2\289\b\4\1\29:\7\r\2\2:;\5\4\3\2;<\b\4\1\2<=\5\4\3\2=>\b\4\1\2"+
		">?\5\4\3\2?@\b\4\1\2@A\7\4\2\2AB\b\4\1\2BP\3\2\2\2CD\7\3\2\2DJ\b\4\1\2"+
		"EF\5\4\3\2FG\b\4\1\2GI\3\2\2\2HE\3\2\2\2IL\3\2\2\2JH\3\2\2\2JK\3\2\2\2"+
		"KM\3\2\2\2LJ\3\2\2\2MN\7\4\2\2NP\b\4\1\2O\63\3\2\2\2O\64\3\2\2\2O\65\3"+
		"\2\2\2O\66\3\2\2\2O\67\3\2\2\2OC\3\2\2\2P\7\3\2\2\2QR\7\3\2\2RS\b\5\1"+
		"\2ST\7\17\2\2TU\5\4\3\2UV\b\5\1\2VW\5\4\3\2WX\b\5\1\2X^\7\5\2\2YZ\5\24"+
		"\13\2Z[\b\5\1\2[]\3\2\2\2\\Y\3\2\2\2]`\3\2\2\2^\\\3\2\2\2^_\3\2\2\2_a"+
		"\3\2\2\2`^\3\2\2\2ab\7\6\2\2bc\7\4\2\2cd\b\5\1\2d\t\3\2\2\2ef\7\3\2\2"+
		"fg\b\6\1\2gh\7\16\2\2hi\5\4\3\2ij\b\6\1\2jk\5\4\3\2kl\b\6\1\2lm\7\4\2"+
		"\2mn\b\6\1\2n\13\3\2\2\2op\7\3\2\2pq\b\7\1\2qr\7\13\2\2rs\7\24\2\2st\b"+
		"\7\1\2tu\5\n\6\2uv\b\7\1\2vw\7\4\2\2wx\b\7\1\2x\r\3\2\2\2yz\7\3\2\2z{"+
		"\b\b\1\2{|\7\f\2\2|}\7\24\2\2}~\b\b\1\2~\177\7\24\2\2\177\u0080\b\b\1"+
		"\2\u0080\u0081\5\n\6\2\u0081\u0082\b\b\1\2\u0082\u0083\7\4\2\2\u0083\u0084"+
		"\b\b\1\2\u0084\17\3\2\2\2\u0085\u0086\7\7\2\2\u0086\u0087\b\t\1\2\u0087"+
		"\u0088\5\4\3\2\u0088\u0089\b\t\1\2\u0089\u008a\5\22\n\2\u008a\u008b\b"+
		"\t\1\2\u008b\u008c\7\b\2\2\u008c\u008d\b\t\1\2\u008d\21\3\2\2\2\u008e"+
		"\u008f\7\24\2\2\u008f\u0097\b\n\1\2\u0090\u0091\b\n\1\2\u0091\u0092\7"+
		"\24\2\2\u0092\u0093\b\n\1\2\u0093\u0094\7\t\2\2\u0094\u0095\7\24\2\2\u0095"+
		"\u0097\b\n\1\2\u0096\u008e\3\2\2\2\u0096\u0090\3\2\2\2\u0097\23\3\2\2"+
		"\2\u0098\u0099\7\3\2\2\u0099\u009f\b\13\1\2\u009a\u009b\5\22\n\2\u009b"+
		"\u009c\b\13\1\2\u009c\u009e\3\2\2\2\u009d\u009a\3\2\2\2\u009e\u00a1\3"+
		"\2\2\2\u009f\u009d\3\2\2\2\u009f\u00a0\3\2\2\2\u00a0\u00a2\3\2\2\2\u00a1"+
		"\u009f\3\2\2\2\u00a2\u00a3\7\4\2\2\u00a3\u00a4\5\4\3\2\u00a4\u00a5\b\13"+
		"\1\2\u00a5\25\3\2\2\2\u00a6\u00a7\7\22\2\2\u00a7\u00b3\b\f\1\2\u00a8\u00a9"+
		"\7\23\2\2\u00a9\u00b3\b\f\1\2\u00aa\u00ab\7\24\2\2\u00ab\u00b3\b\f\1\2"+
		"\u00ac\u00ad\7\20\2\2\u00ad\u00b3\b\f\1\2\u00ae\u00af\7\21\2\2\u00af\u00b3"+
		"\b\f\1\2\u00b0\u00b1\7\27\2\2\u00b1\u00b3\b\f\1\2\u00b2\u00a6\3\2\2\2"+
		"\u00b2\u00a8\3\2\2\2\u00b2\u00aa\3\2\2\2\u00b2\u00ac\3\2\2\2\u00b2\u00ae"+
		"\3\2\2\2\u00b2\u00b0\3\2\2\2\u00b3\27\3\2\2\2\u00b4\u00b5\7\n\2\2\u00b5"+
		"\u00b6\5\4\3\2\u00b6\u00b7\b\r\1\2\u00b7\31\3\2\2\2\n \61JO^\u0096\u009f"+
		"\u00b2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}