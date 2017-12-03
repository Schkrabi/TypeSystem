// Generated from Scheme.g by ANTLR 4.7
package parser;
import expression.*;
import types.TypeConcrete;
import types.TypeRepresentation;
import java.util.Map;
import java.util.HashMap;

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
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, IF=7, LAMBDA=8, TRUE=9, 
		FALSE=10, INT=11, FLOAT=12, SYMBOL=13, COMMENT=14, WS=15, STRING=16;
	public static final int
		RULE_exprs = 0, RULE_expr = 1, RULE_seq = 2, RULE_typed = 3, RULE_type = 4, 
		RULE_atom = 5, RULE_quote = 6;
	public static final String[] ruleNames = {
		"exprs", "expr", "seq", "typed", "type", "atom", "quote"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'('", "')'", "'<'", "'>'", "':'", "'''"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, "IF", "LAMBDA", "TRUE", "FALSE", 
		"INT", "FLOAT", "SYMBOL", "COMMENT", "WS", "STRING"
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
			setState(20);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__2) | (1L << T__5) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
				{
				{
				setState(15);
				((ExprsContext)_localctx).expr = expr();
				 ll.add(((ExprsContext)_localctx).expr.val); 
				}
				}
				setState(22);
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
			setState(37);
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
				setState(25);
				((ExprContext)_localctx).atom = atom();
				 ((ExprContext)_localctx).val =  instantiateUntypedLiteral(((ExprContext)_localctx).atom.val); 
				}
				break;
			case T__0:
				enterOuterAlt(_localctx, 2);
				{
				setState(28);
				((ExprContext)_localctx).seq = seq();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).seq.val; 
				}
				break;
			case T__5:
				enterOuterAlt(_localctx, 3);
				{
				setState(31);
				((ExprContext)_localctx).quote = quote();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).quote.val; 
				}
				break;
			case T__2:
				enterOuterAlt(_localctx, 4);
				{
				setState(34);
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
		public TerminalNode LAMBDA() { return getToken(SchemeParser.LAMBDA, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
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
			setState(73);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(39);
				match(T__0);
				 Expression argList, body; 
				setState(41);
				match(LAMBDA);
				setState(42);
				((SeqContext)_localctx).expr = expr();
				 argList = ((SeqContext)_localctx).expr.val; 
				setState(44);
				((SeqContext)_localctx).expr = expr();
				 body = ((SeqContext)_localctx).expr.val; 
				setState(46);
				match(T__1);
				 ((SeqContext)_localctx).val =  new Lambda(lambdaArgsTuple(argList), body); 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(49);
				match(T__0);
				 Expression cond, tBranch, fBranch; 
				setState(51);
				match(IF);
				setState(52);
				((SeqContext)_localctx).expr = expr();
				 cond = ((SeqContext)_localctx).expr.val; 
				setState(54);
				((SeqContext)_localctx).expr = expr();
				 tBranch = ((SeqContext)_localctx).expr.val; 
				setState(56);
				((SeqContext)_localctx).expr = expr();
				 fBranch = ((SeqContext)_localctx).expr.val; 
				setState(58);
				match(T__1);
				 ((SeqContext)_localctx).val =  new IfExpression(cond, tBranch, fBranch); 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(61);
				match(T__0);
				 List<Expression> ll = new ArrayList<>(); 
				setState(68);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__2) | (1L << T__5) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
					{
					{
					setState(63);
					((SeqContext)_localctx).expr = expr();
					 ll.add(((SeqContext)_localctx).expr.val); 
					}
					}
					setState(70);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(71);
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

	public static class TypedContext extends ParserRuleContext {
		public Expression val;
		public AtomContext atom;
		public TypeContext type;
		public AtomContext atom() {
			return getRuleContext(AtomContext.class,0);
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
			setState(75);
			match(T__2);
			 Object value; TypeConcrete type; 
			setState(77);
			((TypedContext)_localctx).atom = atom();
			 value = ((TypedContext)_localctx).atom.val; 
			setState(79);
			((TypedContext)_localctx).type = type();
			 type = ((TypedContext)_localctx).type.val; 
			setState(81);
			match(T__3);
			 ((TypedContext)_localctx).val =  instantiateTypedLiteral(type, value); 
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
		public TypeConcrete val;
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
			setState(92);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(84);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 ((TypeContext)_localctx).val =  getTypeByName((((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				String typeName; 
				setState(87);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 typeName = (((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null); 
				setState(89);
				match(T__4);
				setState(90);
				((TypeContext)_localctx).SYMBOL = match(SYMBOL);
				 ((TypeContext)_localctx).val =  getTypeByName(typeName, (((TypeContext)_localctx).SYMBOL!=null?((TypeContext)_localctx).SYMBOL.getText():null)); 
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
		enterRule(_localctx, 10, RULE_atom);
		try {
			setState(106);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(94);
				((AtomContext)_localctx).INT = match(INT);
				 ((AtomContext)_localctx).val =  Integer.parseInt((((AtomContext)_localctx).INT!=null?((AtomContext)_localctx).INT.getText():null)); 
				}
				break;
			case FLOAT:
				enterOuterAlt(_localctx, 2);
				{
				setState(96);
				((AtomContext)_localctx).FLOAT = match(FLOAT);
				 ((AtomContext)_localctx).val =  Double.parseDouble((((AtomContext)_localctx).FLOAT!=null?((AtomContext)_localctx).FLOAT.getText():null)); 
				}
				break;
			case SYMBOL:
				enterOuterAlt(_localctx, 3);
				{
				setState(98);
				((AtomContext)_localctx).SYMBOL = match(SYMBOL);
				 ((AtomContext)_localctx).val =  new Variable((((AtomContext)_localctx).SYMBOL!=null?((AtomContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case TRUE:
				enterOuterAlt(_localctx, 4);
				{
				setState(100);
				match(TRUE);
				 ((AtomContext)_localctx).val =  Boolean.TRUE; 
				}
				break;
			case FALSE:
				enterOuterAlt(_localctx, 5);
				{
				setState(102);
				match(FALSE);
				 ((AtomContext)_localctx).val =  Boolean.FALSE; 
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 6);
				{
				setState(104);
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
		enterRule(_localctx, 12, RULE_quote);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(108);
			match(T__5);
			setState(109);
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\22s\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\3\2\3\2\3\2\3\2\7\2\25\n\2"+
		"\f\2\16\2\30\13\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\5\3(\n\3\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4"+
		"\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\7\4E\n\4\f\4\16\4"+
		"H\13\4\3\4\3\4\5\4L\n\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\6\3\6\3"+
		"\6\3\6\3\6\3\6\3\6\3\6\5\6_\n\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3"+
		"\7\3\7\3\7\5\7m\n\7\3\b\3\b\3\b\3\b\3\b\2\2\t\2\4\6\b\n\f\16\2\2\2x\2"+
		"\20\3\2\2\2\4\'\3\2\2\2\6K\3\2\2\2\bM\3\2\2\2\n^\3\2\2\2\fl\3\2\2\2\16"+
		"n\3\2\2\2\20\26\b\2\1\2\21\22\5\4\3\2\22\23\b\2\1\2\23\25\3\2\2\2\24\21"+
		"\3\2\2\2\25\30\3\2\2\2\26\24\3\2\2\2\26\27\3\2\2\2\27\31\3\2\2\2\30\26"+
		"\3\2\2\2\31\32\b\2\1\2\32\3\3\2\2\2\33\34\5\f\7\2\34\35\b\3\1\2\35(\3"+
		"\2\2\2\36\37\5\6\4\2\37 \b\3\1\2 (\3\2\2\2!\"\5\16\b\2\"#\b\3\1\2#(\3"+
		"\2\2\2$%\5\b\5\2%&\b\3\1\2&(\3\2\2\2\'\33\3\2\2\2\'\36\3\2\2\2\'!\3\2"+
		"\2\2\'$\3\2\2\2(\5\3\2\2\2)*\7\3\2\2*+\b\4\1\2+,\7\n\2\2,-\5\4\3\2-.\b"+
		"\4\1\2./\5\4\3\2/\60\b\4\1\2\60\61\7\4\2\2\61\62\b\4\1\2\62L\3\2\2\2\63"+
		"\64\7\3\2\2\64\65\b\4\1\2\65\66\7\t\2\2\66\67\5\4\3\2\678\b\4\1\289\5"+
		"\4\3\29:\b\4\1\2:;\5\4\3\2;<\b\4\1\2<=\7\4\2\2=>\b\4\1\2>L\3\2\2\2?@\7"+
		"\3\2\2@F\b\4\1\2AB\5\4\3\2BC\b\4\1\2CE\3\2\2\2DA\3\2\2\2EH\3\2\2\2FD\3"+
		"\2\2\2FG\3\2\2\2GI\3\2\2\2HF\3\2\2\2IJ\7\4\2\2JL\b\4\1\2K)\3\2\2\2K\63"+
		"\3\2\2\2K?\3\2\2\2L\7\3\2\2\2MN\7\5\2\2NO\b\5\1\2OP\5\f\7\2PQ\b\5\1\2"+
		"QR\5\n\6\2RS\b\5\1\2ST\7\6\2\2TU\b\5\1\2U\t\3\2\2\2VW\7\17\2\2W_\b\6\1"+
		"\2XY\b\6\1\2YZ\7\17\2\2Z[\b\6\1\2[\\\7\7\2\2\\]\7\17\2\2]_\b\6\1\2^V\3"+
		"\2\2\2^X\3\2\2\2_\13\3\2\2\2`a\7\r\2\2am\b\7\1\2bc\7\16\2\2cm\b\7\1\2"+
		"de\7\17\2\2em\b\7\1\2fg\7\13\2\2gm\b\7\1\2hi\7\f\2\2im\b\7\1\2jk\7\22"+
		"\2\2km\b\7\1\2l`\3\2\2\2lb\3\2\2\2ld\3\2\2\2lf\3\2\2\2lh\3\2\2\2lj\3\2"+
		"\2\2m\r\3\2\2\2no\7\b\2\2op\5\4\3\2pq\b\b\1\2q\17\3\2\2\2\b\26\'FK^l";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}