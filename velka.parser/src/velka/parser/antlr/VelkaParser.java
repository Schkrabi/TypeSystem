// Generated from Scheme.g by ANTLR 4.7.2
package velka.parser.antlr;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;

import velka.util.AppendableException;

import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class VelkaParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.7.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, TRUE=4, FALSE=5, INT=6, FLOAT=7, SYMBOL=8, COMMENT=9, 
		WS=10, STRING=11, ARROW=12;
	public static final int
		RULE_exprs = 0, RULE_expr = 1, RULE_seq = 2, RULE_atom = 3, RULE_pair = 4, 
		RULE_arrow = 5;
	private static String[] makeRuleNames() {
		return new String[] {
			"exprs", "expr", "seq", "atom", "pair", "arrow"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'('", "')'", "':'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, "TRUE", "FALSE", "INT", "FLOAT", "SYMBOL", "COMMENT", 
			"WS", "STRING", "ARROW"
		};
	}
	private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
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

	public VelkaParser(TokenStream input) {
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
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterExprs(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitExprs(this);
		}
	}

	public final ExprsContext exprs() throws RecognitionException, AppendableException {
		ExprsContext _localctx = new ExprsContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_exprs);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			 List<SemanticNode> ll = new ArrayList<>(); 
			setState(18);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
				{
				{
				setState(13);
				((ExprsContext)_localctx).expr = expr();
				 ll.add(((ExprsContext)_localctx).expr.val); 
				}
				}
				setState(20);
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
		public ArrowContext arrow;
		public AtomContext atom() {
			return getRuleContext(AtomContext.class,0);
		}
		public SeqContext seq() {
			return getRuleContext(SeqContext.class,0);
		}
		public PairContext pair() {
			return getRuleContext(PairContext.class,0);
		}
		public ArrowContext arrow() {
			return getRuleContext(ArrowContext.class,0);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitExpr(this);
		}
	}

	public final ExprContext expr() throws RecognitionException, AppendableException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_expr);
		try {
			setState(35);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(23);
				((ExprContext)_localctx).atom = atom();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).atom.val; 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(26);
				((ExprContext)_localctx).seq = seq();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).seq.val; 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(29);
				((ExprContext)_localctx).pair = pair();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).pair.val; 
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(32);
				((ExprContext)_localctx).arrow = arrow();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).arrow.val; 
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
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterSeq(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitSeq(this);
		}
	}

	public final SeqContext seq() throws RecognitionException, AppendableException {
		SeqContext _localctx = new SeqContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_seq);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(37);
			match(T__0);
			 List<SemanticNode> ll = new ArrayList<>(); 
			setState(44);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__0) | (1L << TRUE) | (1L << FALSE) | (1L << INT) | (1L << FLOAT) | (1L << SYMBOL) | (1L << STRING))) != 0)) {
				{
				{
				setState(39);
				((SeqContext)_localctx).expr = expr();
				 ll.add(((SeqContext)_localctx).expr.val); 
				}
				}
				setState(46);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(47);
			match(T__1);
			 ((SeqContext)_localctx).val =  SemanticNode.make(SemanticNode.NodeType.LIST, ll); 
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
		public TerminalNode INT() { return getToken(VelkaParser.INT, 0); }
		public TerminalNode FLOAT() { return getToken(VelkaParser.FLOAT, 0); }
		public TerminalNode SYMBOL() { return getToken(VelkaParser.SYMBOL, 0); }
		public TerminalNode TRUE() { return getToken(VelkaParser.TRUE, 0); }
		public TerminalNode FALSE() { return getToken(VelkaParser.FALSE, 0); }
		public TerminalNode STRING() { return getToken(VelkaParser.STRING, 0); }
		public AtomContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_atom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterAtom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitAtom(this);
		}
	}

	public final AtomContext atom() throws RecognitionException, AppendableException {
		AtomContext _localctx = new AtomContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_atom);
		try {
			setState(62);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(50);
				((AtomContext)_localctx).INT = match(INT);
				 ((AtomContext)_localctx).val =  SemanticNode.make(SemanticNode.NodeType.INT, Integer.parseInt((((AtomContext)_localctx).INT!=null?((AtomContext)_localctx).INT.getText():null))); 
				}
				break;
			case FLOAT:
				enterOuterAlt(_localctx, 2);
				{
				setState(52);
				((AtomContext)_localctx).FLOAT = match(FLOAT);
				 ((AtomContext)_localctx).val =  SemanticNode.make(SemanticNode.NodeType.DOUBLE, Double.parseDouble((((AtomContext)_localctx).FLOAT!=null?((AtomContext)_localctx).FLOAT.getText():null))); 
				}
				break;
			case SYMBOL:
				enterOuterAlt(_localctx, 3);
				{
				setState(54);
				((AtomContext)_localctx).SYMBOL = match(SYMBOL);
				 ((AtomContext)_localctx).val =  SemanticNode.make(SemanticNode.NodeType.SYMBOL, (((AtomContext)_localctx).SYMBOL!=null?((AtomContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case TRUE:
				enterOuterAlt(_localctx, 4);
				{
				setState(56);
				match(TRUE);
				 ((AtomContext)_localctx).val =  SemanticNode.make(SemanticNode.NodeType.BOOL, true); 
				}
				break;
			case FALSE:
				enterOuterAlt(_localctx, 5);
				{
				setState(58);
				match(FALSE);
				 ((AtomContext)_localctx).val =  SemanticNode.make(SemanticNode.NodeType.BOOL, false); 
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 6);
				{
				setState(60);
				((AtomContext)_localctx).STRING = match(STRING);
				 ((AtomContext)_localctx).val =  SemanticNode.make(SemanticNode.NodeType.STRING, unescapeString((((AtomContext)_localctx).STRING!=null?((AtomContext)_localctx).STRING.getText():null).substring(1, (((AtomContext)_localctx).STRING!=null?((AtomContext)_localctx).STRING.getText():null).length() - 1))); 
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
		public List<TerminalNode> SYMBOL() { return getTokens(VelkaParser.SYMBOL); }
		public TerminalNode SYMBOL(int i) {
			return getToken(VelkaParser.SYMBOL, i);
		}
		public PairContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_pair; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterPair(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitPair(this);
		}
	}

	public final PairContext pair() throws RecognitionException, AppendableException {
		PairContext _localctx = new PairContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_pair);
		try {
			enterOuterAlt(_localctx, 1);
			{
			 String lvalue; 
			setState(65);
			((PairContext)_localctx).SYMBOL = match(SYMBOL);
			 lvalue = (((PairContext)_localctx).SYMBOL!=null?((PairContext)_localctx).SYMBOL.getText():null); 
			setState(67);
			match(T__2);
			setState(68);
			((PairContext)_localctx).SYMBOL = match(SYMBOL);
			 ((PairContext)_localctx).val =  SemanticNode.make(SemanticNode.NodeType.PAIR, new SemanticPair(lvalue, (((PairContext)_localctx).SYMBOL!=null?((PairContext)_localctx).SYMBOL.getText():null))); 
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

	public static class ArrowContext extends ParserRuleContext {
		public SemanticNode val;
		public ExprContext expr;
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode ARROW() { return getToken(VelkaParser.ARROW, 0); }
		public ArrowContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arrow; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterArrow(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitArrow(this);
		}
	}

	public final ArrowContext arrow() throws RecognitionException, AppendableException {
		ArrowContext _localctx = new ArrowContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_arrow);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(71);
			match(T__0);
			 SemanticNode lvalue; 
			setState(73);
			((ArrowContext)_localctx).expr = expr();
			 lvalue = ((ArrowContext)_localctx).expr.val; 
			setState(75);
			match(ARROW);
			setState(76);
			((ArrowContext)_localctx).expr = expr();
			setState(77);
			match(T__1);
			 ((ArrowContext)_localctx).val =  SemanticNode.make(SemanticNode.NodeType.ARROW, new velka.util.Pair<SemanticNode, SemanticNode>(lvalue, ((ArrowContext)_localctx).expr.val)); 
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\3\16S\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\3\2\3\2\3\2\3\2\7\2\23\n\2\f\2\16\2"+
		"\26\13\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3&"+
		"\n\3\3\4\3\4\3\4\3\4\3\4\7\4-\n\4\f\4\16\4\60\13\4\3\4\3\4\3\4\3\5\3\5"+
		"\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\5\5A\n\5\3\6\3\6\3\6\3\6\3\6"+
		"\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\2\2\b\2\4\6\b\n\f\2\2"+
		"\2V\2\16\3\2\2\2\4%\3\2\2\2\6\'\3\2\2\2\b@\3\2\2\2\nB\3\2\2\2\fI\3\2\2"+
		"\2\16\24\b\2\1\2\17\20\5\4\3\2\20\21\b\2\1\2\21\23\3\2\2\2\22\17\3\2\2"+
		"\2\23\26\3\2\2\2\24\22\3\2\2\2\24\25\3\2\2\2\25\27\3\2\2\2\26\24\3\2\2"+
		"\2\27\30\b\2\1\2\30\3\3\2\2\2\31\32\5\b\5\2\32\33\b\3\1\2\33&\3\2\2\2"+
		"\34\35\5\6\4\2\35\36\b\3\1\2\36&\3\2\2\2\37 \5\n\6\2 !\b\3\1\2!&\3\2\2"+
		"\2\"#\5\f\7\2#$\b\3\1\2$&\3\2\2\2%\31\3\2\2\2%\34\3\2\2\2%\37\3\2\2\2"+
		"%\"\3\2\2\2&\5\3\2\2\2\'(\7\3\2\2(.\b\4\1\2)*\5\4\3\2*+\b\4\1\2+-\3\2"+
		"\2\2,)\3\2\2\2-\60\3\2\2\2.,\3\2\2\2./\3\2\2\2/\61\3\2\2\2\60.\3\2\2\2"+
		"\61\62\7\4\2\2\62\63\b\4\1\2\63\7\3\2\2\2\64\65\7\b\2\2\65A\b\5\1\2\66"+
		"\67\7\t\2\2\67A\b\5\1\289\7\n\2\29A\b\5\1\2:;\7\6\2\2;A\b\5\1\2<=\7\7"+
		"\2\2=A\b\5\1\2>?\7\r\2\2?A\b\5\1\2@\64\3\2\2\2@\66\3\2\2\2@8\3\2\2\2@"+
		":\3\2\2\2@<\3\2\2\2@>\3\2\2\2A\t\3\2\2\2BC\b\6\1\2CD\7\n\2\2DE\b\6\1\2"+
		"EF\7\5\2\2FG\7\n\2\2GH\b\6\1\2H\13\3\2\2\2IJ\7\3\2\2JK\b\7\1\2KL\5\4\3"+
		"\2LM\b\7\1\2MN\7\16\2\2NO\5\4\3\2OP\7\4\2\2PQ\b\7\1\2Q\r\3\2\2\2\6\24"+
		"%.@";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}