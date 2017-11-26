// Generated from Scheme.g by ANTLR 4.7
package parser;
import expression.*;

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
		T__0=1, T__1=2, T__2=3, IF=4, LAMBDA=5, TRUE=6, FALSE=7, INT=8, FLOAT=9, 
		SYMBOL=10, COMMENT=11, WS=12, STRING=13;
	public static final int
		RULE_exprs = 0, RULE_expr = 1, RULE_seq = 2, RULE_atom = 3, RULE_quote = 4;
	public static final String[] ruleNames = {
		"exprs", "expr", "seq", "atom", "quote"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'('", "')'", "'''"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, "IF", "LAMBDA", "TRUE", "FALSE", "INT", "FLOAT", 
		"SYMBOL", "COMMENT", "WS", "STRING"
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
			setState(16);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__2) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
				{
				{
				setState(11);
				((ExprsContext)_localctx).expr = expr();
				 ll.add(((ExprsContext)_localctx).expr.val); 
				}
				}
				setState(18);
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
		public AtomContext atom() {
			return getRuleContext(AtomContext.class,0);
		}
		public SeqContext seq() {
			return getRuleContext(SeqContext.class,0);
		}
		public QuoteContext quote() {
			return getRuleContext(QuoteContext.class,0);
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
			setState(30);
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
				setState(21);
				((ExprContext)_localctx).atom = atom();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).atom.val; 
				}
				break;
			case T__0:
				enterOuterAlt(_localctx, 2);
				{
				setState(24);
				((ExprContext)_localctx).seq = seq();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).seq.val; 
				}
				break;
			case T__2:
				enterOuterAlt(_localctx, 3);
				{
				setState(27);
				((ExprContext)_localctx).quote = quote();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).quote.val; 
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
			setState(66);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(32);
				match(T__0);
				 Expression argList, body; 
				setState(34);
				match(LAMBDA);
				setState(35);
				((SeqContext)_localctx).expr = expr();
				 argList = ((SeqContext)_localctx).expr.val; 
				setState(37);
				((SeqContext)_localctx).expr = expr();
				 body = ((SeqContext)_localctx).expr.val; 
				setState(39);
				match(T__1);
				 ((SeqContext)_localctx).val =  new Lambda(lambdaArgsTuple(argList), body); 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(42);
				match(T__0);
				 Expression cond, tBranch, fBranch; 
				setState(44);
				match(IF);
				setState(45);
				((SeqContext)_localctx).expr = expr();
				 cond = ((SeqContext)_localctx).expr.val; 
				setState(47);
				((SeqContext)_localctx).expr = expr();
				 tBranch = ((SeqContext)_localctx).expr.val; 
				setState(49);
				((SeqContext)_localctx).expr = expr();
				 fBranch = ((SeqContext)_localctx).expr.val; 
				setState(51);
				match(T__1);
				 ((SeqContext)_localctx).val =  new IfExpression(cond, tBranch, fBranch); 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(54);
				match(T__0);
				 List<Expression> ll = new ArrayList<>(); 
				setState(61);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << T__2) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
					{
					{
					setState(56);
					((SeqContext)_localctx).expr = expr();
					 ll.add(((SeqContext)_localctx).expr.val); 
					}
					}
					setState(63);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(64);
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

	public static class AtomContext extends ParserRuleContext {
		public Expression val;
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
		enterRule(_localctx, 6, RULE_atom);
		try {
			setState(80);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(68);
				((AtomContext)_localctx).INT = match(INT);
				 ((AtomContext)_localctx).val =  new IntBinary(Integer.parseInt((((AtomContext)_localctx).INT!=null?((AtomContext)_localctx).INT.getText():null))); 
				}
				break;
			case FLOAT:
				enterOuterAlt(_localctx, 2);
				{
				setState(70);
				((AtomContext)_localctx).FLOAT = match(FLOAT);
				 ((AtomContext)_localctx).val =  new LitDouble(Double.parseDouble((((AtomContext)_localctx).FLOAT!=null?((AtomContext)_localctx).FLOAT.getText():null))); 
				}
				break;
			case SYMBOL:
				enterOuterAlt(_localctx, 3);
				{
				setState(72);
				((AtomContext)_localctx).SYMBOL = match(SYMBOL);
				 ((AtomContext)_localctx).val =  new Variable((((AtomContext)_localctx).SYMBOL!=null?((AtomContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case TRUE:
				enterOuterAlt(_localctx, 4);
				{
				setState(74);
				match(TRUE);
				 ((AtomContext)_localctx).val =  LitBoolean.TRUE; 
				}
				break;
			case FALSE:
				enterOuterAlt(_localctx, 5);
				{
				setState(76);
				match(FALSE);
				 ((AtomContext)_localctx).val =  LitBoolean.FALSE; 
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 6);
				{
				setState(78);
				((AtomContext)_localctx).STRING = match(STRING);
				 ((AtomContext)_localctx).val =  new LitString(unescapeString((((AtomContext)_localctx).STRING!=null?((AtomContext)_localctx).STRING.getText():null).substring(1, (((AtomContext)_localctx).STRING!=null?((AtomContext)_localctx).STRING.getText():null).length() - 1))); 
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
		enterRule(_localctx, 8, RULE_quote);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(82);
			match(T__2);
			setState(83);
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\17Y\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\3\2\3\2\3\2\3\2\7\2\21\n\2\f\2\16\2\24\13\2"+
		"\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3!\n\3\3\4\3\4\3\4\3\4"+
		"\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3"+
		"\4\3\4\3\4\3\4\3\4\3\4\7\4>\n\4\f\4\16\4A\13\4\3\4\3\4\5\4E\n\4\3\5\3"+
		"\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\5\5S\n\5\3\6\3\6\3\6\3\6\3"+
		"\6\2\2\7\2\4\6\b\n\2\2\2^\2\f\3\2\2\2\4 \3\2\2\2\6D\3\2\2\2\bR\3\2\2\2"+
		"\nT\3\2\2\2\f\22\b\2\1\2\r\16\5\4\3\2\16\17\b\2\1\2\17\21\3\2\2\2\20\r"+
		"\3\2\2\2\21\24\3\2\2\2\22\20\3\2\2\2\22\23\3\2\2\2\23\25\3\2\2\2\24\22"+
		"\3\2\2\2\25\26\b\2\1\2\26\3\3\2\2\2\27\30\5\b\5\2\30\31\b\3\1\2\31!\3"+
		"\2\2\2\32\33\5\6\4\2\33\34\b\3\1\2\34!\3\2\2\2\35\36\5\n\6\2\36\37\b\3"+
		"\1\2\37!\3\2\2\2 \27\3\2\2\2 \32\3\2\2\2 \35\3\2\2\2!\5\3\2\2\2\"#\7\3"+
		"\2\2#$\b\4\1\2$%\7\7\2\2%&\5\4\3\2&\'\b\4\1\2\'(\5\4\3\2()\b\4\1\2)*\7"+
		"\4\2\2*+\b\4\1\2+E\3\2\2\2,-\7\3\2\2-.\b\4\1\2./\7\6\2\2/\60\5\4\3\2\60"+
		"\61\b\4\1\2\61\62\5\4\3\2\62\63\b\4\1\2\63\64\5\4\3\2\64\65\b\4\1\2\65"+
		"\66\7\4\2\2\66\67\b\4\1\2\67E\3\2\2\289\7\3\2\29?\b\4\1\2:;\5\4\3\2;<"+
		"\b\4\1\2<>\3\2\2\2=:\3\2\2\2>A\3\2\2\2?=\3\2\2\2?@\3\2\2\2@B\3\2\2\2A"+
		"?\3\2\2\2BC\7\4\2\2CE\b\4\1\2D\"\3\2\2\2D,\3\2\2\2D8\3\2\2\2E\7\3\2\2"+
		"\2FG\7\n\2\2GS\b\5\1\2HI\7\13\2\2IS\b\5\1\2JK\7\f\2\2KS\b\5\1\2LM\7\b"+
		"\2\2MS\b\5\1\2NO\7\t\2\2OS\b\5\1\2PQ\7\17\2\2QS\b\5\1\2RF\3\2\2\2RH\3"+
		"\2\2\2RJ\3\2\2\2RL\3\2\2\2RN\3\2\2\2RP\3\2\2\2S\t\3\2\2\2TU\7\5\2\2UV"+
		"\5\4\3\2VW\b\6\1\2W\13\3\2\2\2\7\22 ?DR";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}