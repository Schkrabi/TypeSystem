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
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, IF=9, 
		LAMBDA=10, ELAMBDA=11, TRUE=12, FALSE=13, INT=14, FLOAT=15, SYMBOL=16, 
		COMMENT=17, WS=18, STRING=19;
	public static final int
		RULE_exprs = 0, RULE_expr = 1, RULE_seq = 2, RULE_typed = 3, RULE_type = 4, 
		RULE_impl = 5, RULE_atom = 6, RULE_quote = 7;
	public static final String[] ruleNames = {
		"exprs", "expr", "seq", "typed", "type", "impl", "atom", "quote"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'('", "'['", "']'", "')'", "'<'", "'>'", "':'", "'''"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, "IF", "LAMBDA", 
		"ELAMBDA", "TRUE", "FALSE", "INT", "FLOAT", "SYMBOL", "COMMENT", "WS", 
		"STRING"
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
			setState(22);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__4) | (1L << T__7) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
				{
				{
				setState(17);
				((ExprsContext)_localctx).expr = expr();
				 ll.add(((ExprsContext)_localctx).expr.val); 
				}
				}
				setState(24);
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
			setState(39);
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
				setState(27);
				((ExprContext)_localctx).atom = atom();
				 ((ExprContext)_localctx).val =  instantiateUntypedLiteral(((ExprContext)_localctx).atom.val); 
				}
				break;
			case T__0:
				enterOuterAlt(_localctx, 2);
				{
				setState(30);
				((ExprContext)_localctx).seq = seq();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).seq.val; 
				}
				break;
			case T__7:
				enterOuterAlt(_localctx, 3);
				{
				setState(33);
				((ExprContext)_localctx).quote = quote();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).quote.val; 
				}
				break;
			case T__4:
				enterOuterAlt(_localctx, 4);
				{
				setState(36);
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
		public TerminalNode LAMBDA() { return getToken(SchemeParser.LAMBDA, 0); }
		public TerminalNode IF() { return getToken(SchemeParser.IF, 0); }
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
			setState(95);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(41);
				match(T__0);
				 Set<ImplContainer> s = new TreeSet<ImplContainer>(); Expression argList, body; 
				setState(43);
				match(ELAMBDA);
				setState(44);
				((SeqContext)_localctx).expr = expr();
				 argList = ((SeqContext)_localctx).expr.val; 
				setState(46);
				((SeqContext)_localctx).expr = expr();
				 body = ((SeqContext)_localctx).expr.val; 
				setState(48);
				match(T__1);
				setState(54);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__0) {
					{
					{
					setState(49);
					((SeqContext)_localctx).impl = impl();
					 s.add(((SeqContext)_localctx).impl.val); 
					}
					}
					setState(56);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(57);
				match(T__2);
				setState(58);
				match(T__3);
				 ((SeqContext)_localctx).val =  new ExtendedLambda(lambdaArgsTuple(argList), body, s); 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(61);
				match(T__0);
				 Expression argList, body; 
				setState(63);
				match(LAMBDA);
				setState(64);
				((SeqContext)_localctx).expr = expr();
				 argList = ((SeqContext)_localctx).expr.val; 
				setState(66);
				((SeqContext)_localctx).expr = expr();
				 body = ((SeqContext)_localctx).expr.val; 
				setState(68);
				match(T__3);
				 ((SeqContext)_localctx).val =  new Lambda(lambdaArgsTuple(argList), body); 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(71);
				match(T__0);
				 Expression cond, tBranch, fBranch; 
				setState(73);
				match(IF);
				setState(74);
				((SeqContext)_localctx).expr = expr();
				 cond = ((SeqContext)_localctx).expr.val; 
				setState(76);
				((SeqContext)_localctx).expr = expr();
				 tBranch = ((SeqContext)_localctx).expr.val; 
				setState(78);
				((SeqContext)_localctx).expr = expr();
				 fBranch = ((SeqContext)_localctx).expr.val; 
				setState(80);
				match(T__3);
				 ((SeqContext)_localctx).val =  new IfExpression(cond, tBranch, fBranch); 
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(83);
				match(T__0);
				 List<Expression> ll = new ArrayList<>(); 
				setState(90);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__4) | (1L << T__7) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
					{
					{
					setState(85);
					((SeqContext)_localctx).expr = expr();
					 ll.add(((SeqContext)_localctx).expr.val); 
					}
					}
					setState(92);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(93);
				match(T__3);
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
		enterRule(_localctx, 6, RULE_typed);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(97);
			match(T__4);
			 Expression value; Constructor type; 
			setState(99);
			((TypedContext)_localctx).expr = expr();
			 value = ((TypedContext)_localctx).expr.val; 
			setState(101);
			((TypedContext)_localctx).type = type();
			 type = ((TypedContext)_localctx).type.val; 
			setState(103);
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
		enterRule(_localctx, 8, RULE_type);
		try {
			setState(114);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,5,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(106);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 ((TypeContext)_localctx).val =  getConstructorByTypeName((((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				String typeName; 
				setState(109);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 typeName = (((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null); 
				setState(111);
				match(T__6);
				setState(112);
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
		enterRule(_localctx, 10, RULE_impl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(116);
			match(T__0);
			 List<Type> ll = new ArrayList<Type>(); 
			setState(123);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SYMBOL) {
				{
				{
				setState(118);
				((ImplContext)_localctx).type = type();
				 ll.add(((ImplContext)_localctx).type.val.constructedType); 
				}
				}
				setState(125);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(126);
			match(T__3);
			setState(127);
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
		enterRule(_localctx, 12, RULE_atom);
		try {
			setState(142);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(130);
				((AtomContext)_localctx).INT = match(INT);
				 ((AtomContext)_localctx).val =  Integer.parseInt((((AtomContext)_localctx).INT!=null?((AtomContext)_localctx).INT.getText():null)); 
				}
				break;
			case FLOAT:
				enterOuterAlt(_localctx, 2);
				{
				setState(132);
				((AtomContext)_localctx).FLOAT = match(FLOAT);
				 ((AtomContext)_localctx).val =  Double.parseDouble((((AtomContext)_localctx).FLOAT!=null?((AtomContext)_localctx).FLOAT.getText():null)); 
				}
				break;
			case SYMBOL:
				enterOuterAlt(_localctx, 3);
				{
				setState(134);
				((AtomContext)_localctx).SYMBOL = match(SYMBOL);
				 ((AtomContext)_localctx).val =  new Variable((((AtomContext)_localctx).SYMBOL!=null?((AtomContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case TRUE:
				enterOuterAlt(_localctx, 4);
				{
				setState(136);
				match(TRUE);
				 ((AtomContext)_localctx).val =  Boolean.TRUE; 
				}
				break;
			case FALSE:
				enterOuterAlt(_localctx, 5);
				{
				setState(138);
				match(FALSE);
				 ((AtomContext)_localctx).val =  Boolean.FALSE; 
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 6);
				{
				setState(140);
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
		enterRule(_localctx, 14, RULE_quote);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(144);
			match(T__7);
			setState(145);
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\25\u0097\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\3\2\3\2\3\2\3"+
		"\2\7\2\27\n\2\f\2\16\2\32\13\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\3\3\3\3\3\3\5\3*\n\3\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4"+
		"\7\4\67\n\4\f\4\16\4:\13\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4"+
		"\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3"+
		"\4\3\4\3\4\7\4[\n\4\f\4\16\4^\13\4\3\4\3\4\5\4b\n\4\3\5\3\5\3\5\3\5\3"+
		"\5\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\5\6u\n\6\3\7\3\7\3"+
		"\7\3\7\3\7\7\7|\n\7\f\7\16\7\177\13\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b"+
		"\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\5\b\u0091\n\b\3\t\3\t\3\t\3\t\3\t\2\2"+
		"\n\2\4\6\b\n\f\16\20\2\2\2\u009e\2\22\3\2\2\2\4)\3\2\2\2\6a\3\2\2\2\b"+
		"c\3\2\2\2\nt\3\2\2\2\fv\3\2\2\2\16\u0090\3\2\2\2\20\u0092\3\2\2\2\22\30"+
		"\b\2\1\2\23\24\5\4\3\2\24\25\b\2\1\2\25\27\3\2\2\2\26\23\3\2\2\2\27\32"+
		"\3\2\2\2\30\26\3\2\2\2\30\31\3\2\2\2\31\33\3\2\2\2\32\30\3\2\2\2\33\34"+
		"\b\2\1\2\34\3\3\2\2\2\35\36\5\16\b\2\36\37\b\3\1\2\37*\3\2\2\2 !\5\6\4"+
		"\2!\"\b\3\1\2\"*\3\2\2\2#$\5\20\t\2$%\b\3\1\2%*\3\2\2\2&\'\5\b\5\2\'("+
		"\b\3\1\2(*\3\2\2\2)\35\3\2\2\2) \3\2\2\2)#\3\2\2\2)&\3\2\2\2*\5\3\2\2"+
		"\2+,\7\3\2\2,-\b\4\1\2-.\7\r\2\2./\5\4\3\2/\60\b\4\1\2\60\61\5\4\3\2\61"+
		"\62\b\4\1\2\628\7\4\2\2\63\64\5\f\7\2\64\65\b\4\1\2\65\67\3\2\2\2\66\63"+
		"\3\2\2\2\67:\3\2\2\28\66\3\2\2\289\3\2\2\29;\3\2\2\2:8\3\2\2\2;<\7\5\2"+
		"\2<=\7\6\2\2=>\b\4\1\2>b\3\2\2\2?@\7\3\2\2@A\b\4\1\2AB\7\f\2\2BC\5\4\3"+
		"\2CD\b\4\1\2DE\5\4\3\2EF\b\4\1\2FG\7\6\2\2GH\b\4\1\2Hb\3\2\2\2IJ\7\3\2"+
		"\2JK\b\4\1\2KL\7\13\2\2LM\5\4\3\2MN\b\4\1\2NO\5\4\3\2OP\b\4\1\2PQ\5\4"+
		"\3\2QR\b\4\1\2RS\7\6\2\2ST\b\4\1\2Tb\3\2\2\2UV\7\3\2\2V\\\b\4\1\2WX\5"+
		"\4\3\2XY\b\4\1\2Y[\3\2\2\2ZW\3\2\2\2[^\3\2\2\2\\Z\3\2\2\2\\]\3\2\2\2]"+
		"_\3\2\2\2^\\\3\2\2\2_`\7\6\2\2`b\b\4\1\2a+\3\2\2\2a?\3\2\2\2aI\3\2\2\2"+
		"aU\3\2\2\2b\7\3\2\2\2cd\7\7\2\2de\b\5\1\2ef\5\4\3\2fg\b\5\1\2gh\5\n\6"+
		"\2hi\b\5\1\2ij\7\b\2\2jk\b\5\1\2k\t\3\2\2\2lm\7\22\2\2mu\b\6\1\2no\b\6"+
		"\1\2op\7\22\2\2pq\b\6\1\2qr\7\t\2\2rs\7\22\2\2su\b\6\1\2tl\3\2\2\2tn\3"+
		"\2\2\2u\13\3\2\2\2vw\7\3\2\2w}\b\7\1\2xy\5\n\6\2yz\b\7\1\2z|\3\2\2\2{"+
		"x\3\2\2\2|\177\3\2\2\2}{\3\2\2\2}~\3\2\2\2~\u0080\3\2\2\2\177}\3\2\2\2"+
		"\u0080\u0081\7\6\2\2\u0081\u0082\5\4\3\2\u0082\u0083\b\7\1\2\u0083\r\3"+
		"\2\2\2\u0084\u0085\7\20\2\2\u0085\u0091\b\b\1\2\u0086\u0087\7\21\2\2\u0087"+
		"\u0091\b\b\1\2\u0088\u0089\7\22\2\2\u0089\u0091\b\b\1\2\u008a\u008b\7"+
		"\16\2\2\u008b\u0091\b\b\1\2\u008c\u008d\7\17\2\2\u008d\u0091\b\b\1\2\u008e"+
		"\u008f\7\25\2\2\u008f\u0091\b\b\1\2\u0090\u0084\3\2\2\2\u0090\u0086\3"+
		"\2\2\2\u0090\u0088\3\2\2\2\u0090\u008a\3\2\2\2\u0090\u008c\3\2\2\2\u0090"+
		"\u008e\3\2\2\2\u0091\17\3\2\2\2\u0092\u0093\7\n\2\2\u0093\u0094\5\4\3"+
		"\2\u0094\u0095\b\t\1\2\u0095\21\3\2\2\2\n\30)8\\at}\u0090";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}