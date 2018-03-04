// Generated from Scheme.g by ANTLR 4.7
package parser;

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
		T__0=1, T__1=2, T__2=3, TRUE=4, FALSE=5, INT=6, FLOAT=7, SYMBOL=8, COMMENT=9, 
		WS=10, STRING=11;
	public static final int
		RULE_exprs = 0, RULE_expr = 1, RULE_seq = 2, RULE_atom = 3, RULE_pair = 4;
	public static final String[] ruleNames = {
		"exprs", "expr", "seq", "atom", "pair"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'('", "')'", "':'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, "TRUE", "FALSE", "INT", "FLOAT", "SYMBOL", "COMMENT", 
		"WS", "STRING"
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

	public SchemeParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class ExprsContext extends ParserRuleContext {
		public List<SemanticNode> val;
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

	public final ExprsContext exprs() throws RecognitionException {
		ExprsContext _localctx = new ExprsContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_exprs);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			 List<SemanticNode> ll = new ArrayList<>(); 
			setState(16);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
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
		public SemanticNode val;
		public AtomContext atom;
		public SeqContext seq;
		public PairContext pair;
		public AtomContext atom() {
			return getRuleContext(AtomContext.class,0);
		}
		public SeqContext seq() {
			return getRuleContext(SeqContext.class,0);
		}
		public PairContext pair() {
			return getRuleContext(PairContext.class,0);
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

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_expr);
		try {
			setState(30);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(21);
				((ExprContext)_localctx).atom = atom();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).atom.val; 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(24);
				((ExprContext)_localctx).seq = seq();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).seq.val; 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(27);
				((ExprContext)_localctx).pair = pair();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).pair.val; 
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

	public static class SeqContext extends ParserRuleContext {
		public SemanticNode val;
		public ExprContext expr;
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

	public final SeqContext seq() throws RecognitionException {
		SeqContext _localctx = new SeqContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_seq);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(32);
			match(T__0);
			 List<SemanticNode> ll = new ArrayList<>(); 
			setState(39);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
				{
				{
				setState(34);
				((SeqContext)_localctx).expr = expr();
				 ll.add(((SeqContext)_localctx).expr.val); 
				}
				}
				setState(41);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(42);
			match(T__1);
			 ((SeqContext)_localctx).val =  new SemanticNode(SemanticNode.NodeType.LIST, ll); 
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
		public SemanticNode val;
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
			setState(57);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(45);
				((AtomContext)_localctx).INT = match(INT);
				 ((AtomContext)_localctx).val =  new SemanticNode(SemanticNode.NodeType.INT, Integer.parseInt((((AtomContext)_localctx).INT!=null?((AtomContext)_localctx).INT.getText():null))); 
				}
				break;
			case FLOAT:
				enterOuterAlt(_localctx, 2);
				{
				setState(47);
				((AtomContext)_localctx).FLOAT = match(FLOAT);
				 ((AtomContext)_localctx).val =  new SemanticNode(SemanticNode.NodeType.DOUBLE, Double.parseDouble((((AtomContext)_localctx).FLOAT!=null?((AtomContext)_localctx).FLOAT.getText():null))); 
				}
				break;
			case SYMBOL:
				enterOuterAlt(_localctx, 3);
				{
				setState(49);
				((AtomContext)_localctx).SYMBOL = match(SYMBOL);
				 ((AtomContext)_localctx).val =  new SemanticNode(SemanticNode.NodeType.SYMBOL, (((AtomContext)_localctx).SYMBOL!=null?((AtomContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case TRUE:
				enterOuterAlt(_localctx, 4);
				{
				setState(51);
				match(TRUE);
				 ((AtomContext)_localctx).val =  new SemanticNode(SemanticNode.NodeType.BOOL, true); 
				}
				break;
			case FALSE:
				enterOuterAlt(_localctx, 5);
				{
				setState(53);
				match(FALSE);
				 ((AtomContext)_localctx).val =  new SemanticNode(SemanticNode.NodeType.BOOL, false); 
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 6);
				{
				setState(55);
				((AtomContext)_localctx).STRING = match(STRING);
				 ((AtomContext)_localctx).val =  new SemanticNode(SemanticNode.NodeType.STRING, unescapeString((((AtomContext)_localctx).STRING!=null?((AtomContext)_localctx).STRING.getText():null).substring(1, (((AtomContext)_localctx).STRING!=null?((AtomContext)_localctx).STRING.getText():null).length() - 1))); 
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

	public static class PairContext extends ParserRuleContext {
		public SemanticNode val;
		public Token SYMBOL;
		public List<TerminalNode> SYMBOL() { return getTokens(SchemeParser.SYMBOL); }
		public TerminalNode SYMBOL(int i) {
			return getToken(SchemeParser.SYMBOL, i);
		}
		public PairContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pair; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).enterPair(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof SchemeListener ) ((SchemeListener)listener).exitPair(this);
		}
	}

	public final PairContext pair() throws RecognitionException {
		PairContext _localctx = new PairContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_pair);
		try {
			enterOuterAlt(_localctx, 1);
			{
			 String lvalue; 
			setState(60);
			((PairContext)_localctx).SYMBOL = match(SYMBOL);
			 lvalue = (((PairContext)_localctx).SYMBOL!=null?((PairContext)_localctx).SYMBOL.getText():null); 
			setState(62);
			match(T__2);
			setState(63);
			((PairContext)_localctx).SYMBOL = match(SYMBOL);
			 ((PairContext)_localctx).val =  new SemanticNode(SemanticNode.NodeType.PAIR, new SemanticNode.Pair(lvalue, (((PairContext)_localctx).SYMBOL!=null?((PairContext)_localctx).SYMBOL.getText():null))); 
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\rE\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\3\2\3\2\3\2\3\2\7\2\21\n\2\f\2\16\2\24\13\2"+
		"\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3!\n\3\3\4\3\4\3\4\3\4"+
		"\3\4\7\4(\n\4\f\4\16\4+\13\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3"+
		"\5\3\5\3\5\3\5\3\5\5\5<\n\5\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\2\2\7\2\4"+
		"\6\b\n\2\2\2H\2\f\3\2\2\2\4 \3\2\2\2\6\"\3\2\2\2\b;\3\2\2\2\n=\3\2\2\2"+
		"\f\22\b\2\1\2\r\16\5\4\3\2\16\17\b\2\1\2\17\21\3\2\2\2\20\r\3\2\2\2\21"+
		"\24\3\2\2\2\22\20\3\2\2\2\22\23\3\2\2\2\23\25\3\2\2\2\24\22\3\2\2\2\25"+
		"\26\b\2\1\2\26\3\3\2\2\2\27\30\5\b\5\2\30\31\b\3\1\2\31!\3\2\2\2\32\33"+
		"\5\6\4\2\33\34\b\3\1\2\34!\3\2\2\2\35\36\5\n\6\2\36\37\b\3\1\2\37!\3\2"+
		"\2\2 \27\3\2\2\2 \32\3\2\2\2 \35\3\2\2\2!\5\3\2\2\2\"#\7\3\2\2#)\b\4\1"+
		"\2$%\5\4\3\2%&\b\4\1\2&(\3\2\2\2\'$\3\2\2\2(+\3\2\2\2)\'\3\2\2\2)*\3\2"+
		"\2\2*,\3\2\2\2+)\3\2\2\2,-\7\4\2\2-.\b\4\1\2.\7\3\2\2\2/\60\7\b\2\2\60"+
		"<\b\5\1\2\61\62\7\t\2\2\62<\b\5\1\2\63\64\7\n\2\2\64<\b\5\1\2\65\66\7"+
		"\6\2\2\66<\b\5\1\2\678\7\7\2\28<\b\5\1\29:\7\r\2\2:<\b\5\1\2;/\3\2\2\2"+
		";\61\3\2\2\2;\63\3\2\2\2;\65\3\2\2\2;\67\3\2\2\2;9\3\2\2\2<\t\3\2\2\2"+
		"=>\b\6\1\2>?\7\n\2\2?@\b\6\1\2@A\7\5\2\2AB\7\n\2\2BC\b\6\1\2C\13\3\2\2"+
		"\2\6\22 );";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}