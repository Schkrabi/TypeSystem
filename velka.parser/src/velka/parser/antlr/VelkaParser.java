// Generated from ./Velka.g4 by ANTLR 4.13.1
package velka.parser.antlr;

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

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast", "CheckReturnValue"})
public class VelkaParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.13.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, T__23=24, 
		T__24=25, TRUE=26, FALSE=27, INT=28, FLOAT=29, COMMENT=30, WS=31, STRING=32, 
		ARROW=33, LBRACKET=34, LSBRACKET=35, RBRACKET=36, RSBRACKET=37, COLLON=38, 
		SYMBOL=39;
	public static final int
		RULE_program = 0, RULE_expr = 1, RULE_argument_list = 2, RULE_bind = 3, 
		RULE_bind_list = 4, RULE_special_form = 5, RULE_atom = 6, RULE_application = 7, 
		RULE_type = 8, RULE_type_arrow = 9, RULE_type_atom = 10, RULE_representation_atom = 11, 
		RULE_type_tuple = 12;
	private static String[] makeRuleNames() {
		return new String[] {
			"program", "expr", "argument_list", "bind", "bind_list", "special_form", 
			"atom", "application", "type", "type_arrow", "type_atom", "representation_atom", 
			"type_tuple"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'nil'", "'and'", "'can-deconstruct-as'", "'define'", "'conversion'", 
			"'constructor'", "'deconstruct'", "'convert'", "'construct'", "'error'", 
			"'extend'", "'get'", "'if'", "'instance-of'", "'instance-of-representation'", 
			"'loop'", "'or'", "'recur'", "'lambda'", "'extended-lambda'", "'let'", 
			"'let*'", "'let-type'", "'tuple'", "'eapply'", null, null, null, null, 
			null, null, null, "'#>'", "'('", "'['", "')'", "']'", "':'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, null, null, null, null, null, null, null, null, null, null, 
			null, null, "TRUE", "FALSE", "INT", "FLOAT", "COMMENT", "WS", "STRING", 
			"ARROW", "LBRACKET", "LSBRACKET", "RBRACKET", "RSBRACKET", "COLLON", 
			"SYMBOL"
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
	public String getGrammarFileName() { return "Velka.g4"; }

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

	public VelkaParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@SuppressWarnings("CheckReturnValue")
	public static class ProgramContext extends ParserRuleContext {
		public List<Expression> val;
		public ExprContext expr;
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public ProgramContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_program; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterProgram(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitProgram(this);
		}
	}

	public final ProgramContext program() throws RecognitionException {
		ProgramContext _localctx = new ProgramContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_program);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			 List<Expression> ll = new ArrayList<Expression>(); 
			setState(32);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 572237283330L) != 0)) {
				{
				{
				setState(27);
				((ProgramContext)_localctx).expr = expr();
				 ll.add(((ProgramContext)_localctx).expr.val); 
				}
				}
				setState(34);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			 ((ProgramContext)_localctx).val =  ll; 
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

	@SuppressWarnings("CheckReturnValue")
	public static class ExprContext extends ParserRuleContext {
		public Expression val;
		public Special_formContext special_form;
		public AtomContext atom;
		public ApplicationContext application;
		public Special_formContext special_form() {
			return getRuleContext(Special_formContext.class,0);
		}
		public AtomContext atom() {
			return getRuleContext(AtomContext.class,0);
		}
		public ApplicationContext application() {
			return getRuleContext(ApplicationContext.class,0);
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

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_expr);
		try {
			setState(48);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,1,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(37);
				((ExprContext)_localctx).special_form = special_form();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).special_form.val; 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(40);
				((ExprContext)_localctx).atom = atom();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).atom.val; 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(43);
				((ExprContext)_localctx).application = application();
				 ((ExprContext)_localctx).val =  ((ExprContext)_localctx).application.val; 
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(46);
				match(T__0);
				 ((ExprContext)_localctx).val =  Expression.EMPTY_EXPRESSION; 
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

	@SuppressWarnings("CheckReturnValue")
	public static class Argument_listContext extends ParserRuleContext {
		public List<velka.util.Pair<Type, Expression>> val;
		public TypeContext type;
		public Token SYMBOL;
		public List<TerminalNode> LBRACKET() { return getTokens(VelkaParser.LBRACKET); }
		public TerminalNode LBRACKET(int i) {
			return getToken(VelkaParser.LBRACKET, i);
		}
		public List<TerminalNode> RBRACKET() { return getTokens(VelkaParser.RBRACKET); }
		public TerminalNode RBRACKET(int i) {
			return getToken(VelkaParser.RBRACKET, i);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<TerminalNode> SYMBOL() { return getTokens(VelkaParser.SYMBOL); }
		public TerminalNode SYMBOL(int i) {
			return getToken(VelkaParser.SYMBOL, i);
		}
		public Argument_listContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_argument_list; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterArgument_list(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitArgument_list(this);
		}
	}

	public final Argument_listContext argument_list() throws RecognitionException {
		Argument_listContext _localctx = new Argument_listContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_argument_list);
		int _la;
		try {
			setState(77);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,4,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(50);
				match(LBRACKET);
				 var ll = new ArrayList<velka.util.Pair<Type, Expression>>(); 
				setState(59); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(52);
					match(LBRACKET);
					setState(53);
					((Argument_listContext)_localctx).type = type();
					 var t = ((Argument_listContext)_localctx).type.val; 
					setState(55);
					((Argument_listContext)_localctx).SYMBOL = match(SYMBOL);
					 ll.add(velka.util.Pair.of(t, new Symbol((((Argument_listContext)_localctx).SYMBOL!=null?((Argument_listContext)_localctx).SYMBOL.getText():null)))); 
					setState(57);
					match(RBRACKET);
					}
					}
					setState(61); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==LBRACKET );
				setState(63);
				match(RBRACKET);
				 ((Argument_listContext)_localctx).val =  ll; 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(66);
				match(LBRACKET);
				 var ll = new ArrayList<velka.util.Pair<Type, Expression>>(); 
				setState(72);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SYMBOL) {
					{
					{
					setState(68);
					((Argument_listContext)_localctx).SYMBOL = match(SYMBOL);
					 
						            var t = new TypeVariable(NameGenerator.next());
									var s = new Symbol((((Argument_listContext)_localctx).SYMBOL!=null?((Argument_listContext)_localctx).SYMBOL.getText():null));
									ll.add(velka.util.Pair.of(t, s)); 
								  
					}
					}
					setState(74);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(75);
				match(RBRACKET);
				 ((Argument_listContext)_localctx).val =  ll; 
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

	@SuppressWarnings("CheckReturnValue")
	public static class BindContext extends ParserRuleContext {
		public velka.util.Pair<Symbol, Expression> val;
		public Token SYMBOL;
		public ExprContext expr;
		public TerminalNode LBRACKET() { return getToken(VelkaParser.LBRACKET, 0); }
		public TerminalNode SYMBOL() { return getToken(VelkaParser.SYMBOL, 0); }
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public TerminalNode RBRACKET() { return getToken(VelkaParser.RBRACKET, 0); }
		public BindContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_bind; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterBind(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitBind(this);
		}
	}

	public final BindContext bind() throws RecognitionException {
		BindContext _localctx = new BindContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_bind);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(79);
			match(LBRACKET);
			setState(80);
			((BindContext)_localctx).SYMBOL = match(SYMBOL);
			 var s = new Symbol((((BindContext)_localctx).SYMBOL!=null?((BindContext)_localctx).SYMBOL.getText():null)); 
			setState(82);
			((BindContext)_localctx).expr = expr();
			 var e = ((BindContext)_localctx).expr.val; 
			setState(84);
			match(RBRACKET);
			 ((BindContext)_localctx).val =  velka.util.Pair.of(s, e); 
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

	@SuppressWarnings("CheckReturnValue")
	public static class Bind_listContext extends ParserRuleContext {
		public List<velka.util.Pair<Symbol, Expression>> val;
		public BindContext bind;
		public TerminalNode LBRACKET() { return getToken(VelkaParser.LBRACKET, 0); }
		public TerminalNode RBRACKET() { return getToken(VelkaParser.RBRACKET, 0); }
		public List<BindContext> bind() {
			return getRuleContexts(BindContext.class);
		}
		public BindContext bind(int i) {
			return getRuleContext(BindContext.class,i);
		}
		public Bind_listContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_bind_list; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterBind_list(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitBind_list(this);
		}
	}

	public final Bind_listContext bind_list() throws RecognitionException {
		Bind_listContext _localctx = new Bind_listContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_bind_list);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(87);
			match(LBRACKET);
			 var ll = new ArrayList<velka.util.Pair<Symbol, Expression>>(); 
			setState(94);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==LBRACKET) {
				{
				{
				setState(89);
				((Bind_listContext)_localctx).bind = bind();
				 ll.add(((Bind_listContext)_localctx).bind.val); 
				}
				}
				setState(96);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(97);
			match(RBRACKET);
			 ((Bind_listContext)_localctx).val =  ll; 
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

	@SuppressWarnings("CheckReturnValue")
	public static class Special_formContext extends ParserRuleContext {
		public Expression val;
		public ExprContext expr;
		public TypeContext type;
		public Token SYMBOL;
		public Representation_atomContext representation_atom;
		public Argument_listContext argument_list;
		public Bind_listContext bind_list;
		public List<TerminalNode> LBRACKET() { return getTokens(VelkaParser.LBRACKET); }
		public TerminalNode LBRACKET(int i) {
			return getToken(VelkaParser.LBRACKET, i);
		}
		public List<TerminalNode> RBRACKET() { return getTokens(VelkaParser.RBRACKET); }
		public TerminalNode RBRACKET(int i) {
			return getToken(VelkaParser.RBRACKET, i);
		}
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public List<TerminalNode> SYMBOL() { return getTokens(VelkaParser.SYMBOL); }
		public TerminalNode SYMBOL(int i) {
			return getToken(VelkaParser.SYMBOL, i);
		}
		public List<Representation_atomContext> representation_atom() {
			return getRuleContexts(Representation_atomContext.class);
		}
		public Representation_atomContext representation_atom(int i) {
			return getRuleContext(Representation_atomContext.class,i);
		}
		public Argument_listContext argument_list() {
			return getRuleContext(Argument_listContext.class,0);
		}
		public Bind_listContext bind_list() {
			return getRuleContext(Bind_listContext.class,0);
		}
		public Special_formContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_special_form; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterSpecial_form(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitSpecial_form(this);
		}
	}

	public final Special_formContext special_form() throws RecognitionException {
		Special_formContext _localctx = new Special_formContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_special_form);
		int _la;
		try {
			setState(390);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,13,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(100);
				match(LBRACKET);
				 var ll = new ArrayList<Expression>(); 
				setState(102);
				match(T__1);
				setState(108);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 572237283330L) != 0)) {
					{
					{
					setState(103);
					((Special_formContext)_localctx).expr = expr();
					 ll.add(((Special_formContext)_localctx).expr.val); 
					}
					}
					setState(110);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(111);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new AndExpression(new Tuple(ll)); 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(113);
				match(LBRACKET);
				setState(114);
				match(T__2);
				setState(115);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(117);
				((Special_formContext)_localctx).type = type();
				 var t = ((Special_formContext)_localctx).type.val; 
				setState(119);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new CanDeconstructAs(e, t); 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(122);
				match(LBRACKET);
				setState(123);
				match(T__3);
				setState(124);
				((Special_formContext)_localctx).SYMBOL = match(SYMBOL);
				 var s = new Symbol((((Special_formContext)_localctx).SYMBOL!=null?((Special_formContext)_localctx).SYMBOL.getText():null)); 
				setState(126);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(128);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new DefineSymbol(s, e); 
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(131);
				match(LBRACKET);
				setState(132);
				match(T__4);
				setState(133);
				((Special_formContext)_localctx).representation_atom = representation_atom();
				 var f = ((Special_formContext)_localctx).representation_atom.val; 
				setState(135);
				((Special_formContext)_localctx).representation_atom = representation_atom();
				 var t = ((Special_formContext)_localctx).representation_atom.val; 
				setState(137);
				match(LBRACKET);
				setState(138);
				((Special_formContext)_localctx).SYMBOL = match(SYMBOL);
				 var a = new Symbol((((Special_formContext)_localctx).SYMBOL!=null?((Special_formContext)_localctx).SYMBOL.getText():null)); 
				setState(140);
				match(RBRACKET);
				setState(141);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(143);
				((Special_formContext)_localctx).expr = expr();
				 var c = ((Special_formContext)_localctx).expr.val; 
				setState(145);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new DefineConversion(f, t, new Tuple(a), e); 
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(148);
				match(LBRACKET);
				setState(149);
				match(T__4);
				setState(150);
				((Special_formContext)_localctx).representation_atom = representation_atom();
				 var f = ((Special_formContext)_localctx).representation_atom.val; 
				setState(152);
				((Special_formContext)_localctx).representation_atom = representation_atom();
				 var t = ((Special_formContext)_localctx).representation_atom.val; 
				setState(154);
				match(LBRACKET);
				setState(155);
				((Special_formContext)_localctx).SYMBOL = match(SYMBOL);
				 var a = new Symbol((((Special_formContext)_localctx).SYMBOL!=null?((Special_formContext)_localctx).SYMBOL.getText():null)); 
				setState(157);
				match(RBRACKET);
				setState(158);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(160);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new DefineConversion(f, t, new Tuple(a), e); 
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(163);
				match(LBRACKET);
				setState(164);
				match(T__5);
				setState(165);
				((Special_formContext)_localctx).representation_atom = representation_atom();
				 var t = ((Special_formContext)_localctx).representation_atom.val; 
				setState(167);
				((Special_formContext)_localctx).argument_list = argument_list();
				 var args = ((Special_formContext)_localctx).argument_list.val; 
				setState(169);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(171);
				match(RBRACKET);
				 
											var al = new ArrayList<Expression>();
											var tl = new ArrayList<Type>();
											args.stream().forEach(p -> {
																		tl.add(p.first);
																		al.add(p.second);
																	   });
											((Special_formContext)_localctx).val =  new DefineConstructor(t, new Lambda(new Tuple(al), new TypeTuple(tl), e));
										  
				}
				break;
			case 7:
				enterOuterAlt(_localctx, 7);
				{
				setState(174);
				match(LBRACKET);
				setState(175);
				match(T__6);
				setState(176);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(178);
				((Special_formContext)_localctx).type = type();
				 var t = ((Special_formContext)_localctx).type.val; 
				setState(180);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new Deconstruct(e, t); 
				}
				break;
			case 8:
				enterOuterAlt(_localctx, 8);
				{
				setState(183);
				match(LBRACKET);
				setState(184);
				match(T__7);
				setState(185);
				((Special_formContext)_localctx).type = type();
				 var f = ((Special_formContext)_localctx).type.val; 
				setState(187);
				((Special_formContext)_localctx).type = type();
				 var t = ((Special_formContext)_localctx).type.val; 
				setState(189);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(191);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new Convert(f, t, e); 
				}
				break;
			case 9:
				enterOuterAlt(_localctx, 9);
				{
				setState(194);
				match(LBRACKET);
				setState(195);
				match(T__8);
				setState(196);
				((Special_formContext)_localctx).representation_atom = representation_atom();
				 
					                        var t = ((Special_formContext)_localctx).representation_atom.val; 
											var ll = new ArrayList<Expression>();
										  
				setState(203);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 572237283330L) != 0)) {
					{
					{
					setState(198);
					((Special_formContext)_localctx).expr = expr();
					 ll.add(((Special_formContext)_localctx).expr.val);
					}
					}
					setState(205);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(206);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new Construct(t, new Tuple(ll)); 
				}
				break;
			case 10:
				enterOuterAlt(_localctx, 10);
				{
				setState(209);
				match(LBRACKET);
				setState(210);
				match(T__9);
				setState(211);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(213);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new ExceptionExpr(e); 
				}
				break;
			case 11:
				enterOuterAlt(_localctx, 11);
				{
				setState(216);
				match(LBRACKET);
				setState(217);
				match(T__10);
				setState(218);
				((Special_formContext)_localctx).expr = expr();
				 var el = ((Special_formContext)_localctx).expr.val; 
				setState(220);
				((Special_formContext)_localctx).expr = expr();
				 var i = ((Special_formContext)_localctx).expr.val; 
				setState(222);
				((Special_formContext)_localctx).expr = expr();
				 var c = ((Special_formContext)_localctx).expr.val; 
				setState(224);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new Extend(el, i, c); 
				}
				break;
			case 12:
				enterOuterAlt(_localctx, 12);
				{
				setState(227);
				match(LBRACKET);
				setState(228);
				match(T__10);
				setState(229);
				((Special_formContext)_localctx).expr = expr();
				 var el = ((Special_formContext)_localctx).expr.val; 
				setState(231);
				((Special_formContext)_localctx).expr = expr();
				 var i = ((Special_formContext)_localctx).expr.val; 
				setState(233);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new Extend(el, i); 
				}
				break;
			case 13:
				enterOuterAlt(_localctx, 13);
				{
				setState(236);
				match(LBRACKET);
				setState(237);
				match(T__11);
				setState(238);
				((Special_formContext)_localctx).expr = expr();
				 var t = ((Special_formContext)_localctx).expr.val; 
				setState(240);
				((Special_formContext)_localctx).expr = expr();
				 var i = ((Special_formContext)_localctx).expr.val; 
				setState(242);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  Get.makeGet(t, i); 
				}
				break;
			case 14:
				enterOuterAlt(_localctx, 14);
				{
				setState(245);
				match(LBRACKET);
				setState(246);
				match(T__12);
				setState(247);
				((Special_formContext)_localctx).expr = expr();
				 var c = ((Special_formContext)_localctx).expr.val; 
				setState(249);
				((Special_formContext)_localctx).expr = expr();
				 var t = ((Special_formContext)_localctx).expr.val; 
				setState(251);
				((Special_formContext)_localctx).expr = expr();
				 var f = ((Special_formContext)_localctx).expr.val; 
				setState(253);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new IfExpression(c, t, f); 
				}
				break;
			case 15:
				enterOuterAlt(_localctx, 15);
				{
				setState(256);
				match(LBRACKET);
				setState(257);
				match(T__13);
				setState(258);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(260);
				((Special_formContext)_localctx).type = type();
				 var t = ((Special_formContext)_localctx).type.val; 
				setState(262);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new InstanceOf(e, t); 
				}
				break;
			case 16:
				enterOuterAlt(_localctx, 16);
				{
				setState(265);
				match(LBRACKET);
				setState(266);
				match(T__14);
				setState(267);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(269);
				((Special_formContext)_localctx).type = type();
				 var t = ((Special_formContext)_localctx).type.val; 
				setState(271);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new InstanceOfRepresentation(e, t); 
				}
				break;
			case 17:
				enterOuterAlt(_localctx, 17);
				{
				setState(274);
				match(LBRACKET);
				setState(275);
				match(T__15);
				setState(276);
				((Special_formContext)_localctx).bind_list = bind_list();
				 var bds = ((Special_formContext)_localctx).bind_list.val; 
				setState(278);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(280);
				match(RBRACKET);

									var symbols = new ArrayList<Symbol>();
									var args = new ArrayList<Expression>();
									bds.stream().forEach(p -> {
																symbols.add(p.first);
																args.add(p.second);
															  });
									((Special_formContext)_localctx).val =  new Loop(new Tuple(symbols), e, new Tuple(args));
						        
				}
				break;
			case 18:
				enterOuterAlt(_localctx, 18);
				{
				setState(283);
				match(LBRACKET);
				setState(284);
				match(T__16);
				 var ll = new ArrayList<Expression>(); 
				setState(291);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 572237283330L) != 0)) {
					{
					{
					setState(286);
					((Special_formContext)_localctx).expr = expr();
					 ll.add(((Special_formContext)_localctx).expr.val); 
					}
					}
					setState(293);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(294);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new OrExpression(new Tuple(ll)); 
				}
				break;
			case 19:
				enterOuterAlt(_localctx, 19);
				{
				setState(296);
				match(LBRACKET);
				setState(297);
				match(T__17);
				 var ll = new ArrayList<Expression>(); 
				setState(304);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 572237283330L) != 0)) {
					{
					{
					setState(299);
					((Special_formContext)_localctx).expr = expr();
					 ll.add(((Special_formContext)_localctx).expr.val); 
					}
					}
					setState(306);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(307);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new Recur(new Tuple(ll)); 
				}
				break;
			case 20:
				enterOuterAlt(_localctx, 20);
				{
				setState(309);
				match(LBRACKET);
				setState(310);
				match(T__18);
				setState(311);
				((Special_formContext)_localctx).argument_list = argument_list();
				 var ll = ((Special_formContext)_localctx).argument_list.val; 
				setState(313);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(315);
				match(RBRACKET);

										var args = new ArrayList<Expression>();
										var ts = new ArrayList<Type>();
										ll.stream().forEach(p -> {
																	ts.add(p.first);
																	args.add(p.second);
																 });
										((Special_formContext)_localctx).val =  new Lambda(new Tuple(args), new TypeTuple(ts), e);
									
				}
				break;
			case 21:
				enterOuterAlt(_localctx, 21);
				{
				setState(318);
				match(LBRACKET);
				setState(319);
				match(T__19);
				setState(320);
				match(LBRACKET);
				 var ll = new ArrayList<Type>(); 
				setState(325); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(322);
					((Special_formContext)_localctx).type = type();
					 ll.add(((Special_formContext)_localctx).type.val); 
					}
					}
					setState(327); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==LBRACKET || _la==SYMBOL );
				setState(329);
				match(RBRACKET);
				setState(330);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new ExtendedLambda(new TypeTuple(ll)); 
				}
				break;
			case 22:
				enterOuterAlt(_localctx, 22);
				{
				setState(333);
				match(LBRACKET);
				setState(334);
				match(T__20);
				setState(335);
				((Special_formContext)_localctx).bind_list = bind_list();
				 var ll = ((Special_formContext)_localctx).bind_list.val; 
				setState(337);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(339);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new Let(e, ll); 
				}
				break;
			case 23:
				enterOuterAlt(_localctx, 23);
				{
				setState(342);
				match(LBRACKET);
				setState(343);
				match(T__21);
				setState(344);
				((Special_formContext)_localctx).bind_list = bind_list();
				 var ll = ((Special_formContext)_localctx).bind_list.val; 
				setState(346);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(348);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new Let(e, ll); 
				}
				break;
			case 24:
				enterOuterAlt(_localctx, 24);
				{
				setState(351);
				match(LBRACKET);
				setState(352);
				match(T__22);
				setState(353);
				match(LBRACKET);
				 var s = new HashSet<String>(); 
				setState(357); 
				_errHandler.sync(this);
				_la = _input.LA(1);
				do {
					{
					{
					setState(355);
					((Special_formContext)_localctx).SYMBOL = match(SYMBOL);
					 s.add((((Special_formContext)_localctx).SYMBOL!=null?((Special_formContext)_localctx).SYMBOL.getText():null)); 
					}
					}
					setState(359); 
					_errHandler.sync(this);
					_la = _input.LA(1);
				} while ( _la==SYMBOL );
				setState(361);
				match(RBRACKET);
				 this.typeVariableStack.push(s); 
				setState(363);
				((Special_formContext)_localctx).expr = expr();
				 var e = ((Special_formContext)_localctx).expr.val; 
				setState(365);
				match(RBRACKET);

								this.typeVariableStack.pop();
								((Special_formContext)_localctx).val =  e;
							  
				}
				break;
			case 25:
				enterOuterAlt(_localctx, 25);
				{
				setState(368);
				match(LBRACKET);
				setState(369);
				match(T__23);
				 var ll = new ArrayList<Expression>(); 
				setState(376);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 572237283330L) != 0)) {
					{
					{
					setState(371);
					((Special_formContext)_localctx).expr = expr();
					 ll.add(((Special_formContext)_localctx).expr.val);
					}
					}
					setState(378);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(379);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new Tuple(ll); 
				}
				break;
			case 26:
				enterOuterAlt(_localctx, 26);
				{
				setState(381);
				match(LBRACKET);
				setState(382);
				match(T__24);
				setState(383);
				((Special_formContext)_localctx).expr = expr();
				 var f = ((Special_formContext)_localctx).expr.val; 
				setState(385);
				((Special_formContext)_localctx).expr = expr();
				 var a = ((Special_formContext)_localctx).expr.val; 
				setState(387);
				match(RBRACKET);
				 ((Special_formContext)_localctx).val =  new AbstractionApplication(f, a); 
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

	@SuppressWarnings("CheckReturnValue")
	public static class AtomContext extends ParserRuleContext {
		public Expression val;
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

	public final AtomContext atom() throws RecognitionException {
		AtomContext _localctx = new AtomContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_atom);
		try {
			setState(404);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(392);
				((AtomContext)_localctx).INT = match(INT);
				 ((AtomContext)_localctx).val =  new LitInteger(Long.parseLong((((AtomContext)_localctx).INT!=null?((AtomContext)_localctx).INT.getText():null))); 
				}
				break;
			case FLOAT:
				enterOuterAlt(_localctx, 2);
				{
				setState(394);
				((AtomContext)_localctx).FLOAT = match(FLOAT);
				 ((AtomContext)_localctx).val =  new LitDouble(Double.parseDouble((((AtomContext)_localctx).FLOAT!=null?((AtomContext)_localctx).FLOAT.getText():null))); 
				}
				break;
			case SYMBOL:
				enterOuterAlt(_localctx, 3);
				{
				setState(396);
				((AtomContext)_localctx).SYMBOL = match(SYMBOL);
				 ((AtomContext)_localctx).val =  new Symbol((((AtomContext)_localctx).SYMBOL!=null?((AtomContext)_localctx).SYMBOL.getText():null)); 
				}
				break;
			case TRUE:
				enterOuterAlt(_localctx, 4);
				{
				setState(398);
				match(TRUE);
				 ((AtomContext)_localctx).val =  LitBoolean.TRUE; 
				}
				break;
			case FALSE:
				enterOuterAlt(_localctx, 5);
				{
				setState(400);
				match(FALSE);
				 ((AtomContext)_localctx).val =  LitBoolean.FALSE; 
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 6);
				{
				setState(402);
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

	@SuppressWarnings("CheckReturnValue")
	public static class ApplicationContext extends ParserRuleContext {
		public AbstractionApplication val;
		public ExprContext expr;
		public TerminalNode LBRACKET() { return getToken(VelkaParser.LBRACKET, 0); }
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public TerminalNode RBRACKET() { return getToken(VelkaParser.RBRACKET, 0); }
		public ApplicationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_application; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterApplication(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitApplication(this);
		}
	}

	public final ApplicationContext application() throws RecognitionException {
		ApplicationContext _localctx = new ApplicationContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_application);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(406);
			match(LBRACKET);
			 var ll = new ArrayList<Expression>(); 
			setState(408);
			((ApplicationContext)_localctx).expr = expr();
			 var f = ((ApplicationContext)_localctx).expr.val; 
			setState(415);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while ((((_la) & ~0x3f) == 0 && ((1L << _la) & 572237283330L) != 0)) {
				{
				{
				setState(410);
				((ApplicationContext)_localctx).expr = expr();
				 ll.add(((ApplicationContext)_localctx).expr.val); 
				}
				}
				setState(417);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(418);
			match(RBRACKET);
			 ((ApplicationContext)_localctx).val =  new AbstractionApplication(f, new Tuple(ll)); 
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

	@SuppressWarnings("CheckReturnValue")
	public static class TypeContext extends ParserRuleContext {
		public Type val;
		public Type_arrowContext type_arrow;
		public Representation_atomContext representation_atom;
		public Type_atomContext type_atom;
		public Type_tupleContext type_tuple;
		public Type_arrowContext type_arrow() {
			return getRuleContext(Type_arrowContext.class,0);
		}
		public Representation_atomContext representation_atom() {
			return getRuleContext(Representation_atomContext.class,0);
		}
		public Type_atomContext type_atom() {
			return getRuleContext(Type_atomContext.class,0);
		}
		public Type_tupleContext type_tuple() {
			return getRuleContext(Type_tupleContext.class,0);
		}
		public TypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitType(this);
		}
	}

	public final TypeContext type() throws RecognitionException {
		TypeContext _localctx = new TypeContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_type);
		try {
			setState(433);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,16,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(421);
				((TypeContext)_localctx).type_arrow = type_arrow();
				 ((TypeContext)_localctx).val =  ((TypeContext)_localctx).type_arrow.val; 
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(424);
				((TypeContext)_localctx).representation_atom = representation_atom();
				 ((TypeContext)_localctx).val =  ((TypeContext)_localctx).representation_atom.val; 
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(427);
				((TypeContext)_localctx).type_atom = type_atom();
				 ((TypeContext)_localctx).val =  ((TypeContext)_localctx).type_atom.val; 
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(430);
				((TypeContext)_localctx).type_tuple = type_tuple();
				 ((TypeContext)_localctx).val =  ((TypeContext)_localctx).type_tuple.val; 
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

	@SuppressWarnings("CheckReturnValue")
	public static class Type_arrowContext extends ParserRuleContext {
		public TypeArrow val;
		public Type_tupleContext type_tuple;
		public TypeContext type;
		public TerminalNode LBRACKET() { return getToken(VelkaParser.LBRACKET, 0); }
		public Type_tupleContext type_tuple() {
			return getRuleContext(Type_tupleContext.class,0);
		}
		public TerminalNode ARROW() { return getToken(VelkaParser.ARROW, 0); }
		public TypeContext type() {
			return getRuleContext(TypeContext.class,0);
		}
		public TerminalNode RBRACKET() { return getToken(VelkaParser.RBRACKET, 0); }
		public Type_arrowContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type_arrow; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterType_arrow(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitType_arrow(this);
		}
	}

	public final Type_arrowContext type_arrow() throws RecognitionException {
		Type_arrowContext _localctx = new Type_arrowContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_type_arrow);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(435);
			match(LBRACKET);
			setState(436);
			((Type_arrowContext)_localctx).type_tuple = type_tuple();
			 var l = ((Type_arrowContext)_localctx).type_tuple.val; 
			setState(438);
			match(ARROW);
			setState(439);
			((Type_arrowContext)_localctx).type = type();
			 ((Type_arrowContext)_localctx).val =  new TypeArrow(l, ((Type_arrowContext)_localctx).type.val); 
			setState(441);
			match(RBRACKET);
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

	@SuppressWarnings("CheckReturnValue")
	public static class Type_atomContext extends ParserRuleContext {
		public Type val;
		public Token SYMBOL;
		public TerminalNode SYMBOL() { return getToken(VelkaParser.SYMBOL, 0); }
		public Type_atomContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type_atom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterType_atom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitType_atom(this);
		}
	}

	public final Type_atomContext type_atom() throws RecognitionException {
		Type_atomContext _localctx = new Type_atomContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_type_atom);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(443);
			((Type_atomContext)_localctx).SYMBOL = match(SYMBOL);

							var name = (((Type_atomContext)_localctx).SYMBOL!=null?((Type_atomContext)_localctx).SYMBOL.getText():null);
							if(this.isDeclaredTypeVariable(name)){
								((Type_atomContext)_localctx).val =  new TypeVariable(name);
							}
							else{
								((Type_atomContext)_localctx).val =  new TypeAtom(new TypeName((((Type_atomContext)_localctx).SYMBOL!=null?((Type_atomContext)_localctx).SYMBOL.getText():null)), TypeRepresentation.WILDCARD); 
							}
						 
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

	@SuppressWarnings("CheckReturnValue")
	public static class Representation_atomContext extends ParserRuleContext {
		public TypeAtom val;
		public Token SYMBOL;
		public List<TerminalNode> SYMBOL() { return getTokens(VelkaParser.SYMBOL); }
		public TerminalNode SYMBOL(int i) {
			return getToken(VelkaParser.SYMBOL, i);
		}
		public TerminalNode COLLON() { return getToken(VelkaParser.COLLON, 0); }
		public Representation_atomContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_representation_atom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterRepresentation_atom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitRepresentation_atom(this);
		}
	}

	public final Representation_atomContext representation_atom() throws RecognitionException {
		Representation_atomContext _localctx = new Representation_atomContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_representation_atom);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(446);
			((Representation_atomContext)_localctx).SYMBOL = match(SYMBOL);
			 var t = new TypeName((((Representation_atomContext)_localctx).SYMBOL!=null?((Representation_atomContext)_localctx).SYMBOL.getText():null)); 
			setState(448);
			match(COLLON);
			setState(449);
			((Representation_atomContext)_localctx).SYMBOL = match(SYMBOL);
			 ((Representation_atomContext)_localctx).val =  new TypeAtom(t, new TypeRepresentation((((Representation_atomContext)_localctx).SYMBOL!=null?((Representation_atomContext)_localctx).SYMBOL.getText():null))); 
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

	@SuppressWarnings("CheckReturnValue")
	public static class Type_tupleContext extends ParserRuleContext {
		public TypeTuple val;
		public TypeContext type;
		public TerminalNode LBRACKET() { return getToken(VelkaParser.LBRACKET, 0); }
		public TerminalNode RBRACKET() { return getToken(VelkaParser.RBRACKET, 0); }
		public List<TypeContext> type() {
			return getRuleContexts(TypeContext.class);
		}
		public TypeContext type(int i) {
			return getRuleContext(TypeContext.class,i);
		}
		public Type_tupleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_type_tuple; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).enterType_tuple(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof VelkaListener ) ((VelkaListener)listener).exitType_tuple(this);
		}
	}

	public final Type_tupleContext type_tuple() throws RecognitionException {
		Type_tupleContext _localctx = new Type_tupleContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_type_tuple);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(452);
			match(LBRACKET);
			 var ll = new ArrayList<Type>(); 
			setState(459);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==LBRACKET || _la==SYMBOL) {
				{
				{
				setState(454);
				((Type_tupleContext)_localctx).type = type();
				 ll.add(((Type_tupleContext)_localctx).type.val); 
				}
				}
				setState(461);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(462);
			match(RBRACKET);
			 ((Type_tupleContext)_localctx).val =  new TypeTuple(ll); 
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
		"\u0004\u0001\'\u01d2\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002"+
		"\u0002\u0007\u0002\u0002\u0003\u0007\u0003\u0002\u0004\u0007\u0004\u0002"+
		"\u0005\u0007\u0005\u0002\u0006\u0007\u0006\u0002\u0007\u0007\u0007\u0002"+
		"\b\u0007\b\u0002\t\u0007\t\u0002\n\u0007\n\u0002\u000b\u0007\u000b\u0002"+
		"\f\u0007\f\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0000\u0005\u0000"+
		"\u001f\b\u0000\n\u0000\f\u0000\"\t\u0000\u0001\u0000\u0001\u0000\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"+
		"\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0003\u00011\b"+
		"\u0001\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0004\u0002<\b\u0002\u000b"+
		"\u0002\f\u0002=\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"+
		"\u0002\u0001\u0002\u0001\u0002\u0005\u0002G\b\u0002\n\u0002\f\u0002J\t"+
		"\u0002\u0001\u0002\u0001\u0002\u0003\u0002N\b\u0002\u0001\u0003\u0001"+
		"\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001\u0003\u0001"+
		"\u0003\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0001\u0004\u0005"+
		"\u0004]\b\u0004\n\u0004\f\u0004`\t\u0004\u0001\u0004\u0001\u0004\u0001"+
		"\u0004\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0005\u0005k\b\u0005\n\u0005\f\u0005n\t\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0005\u0005\u00ca"+
		"\b\u0005\n\u0005\f\u0005\u00cd\t\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0005\u0005\u0122\b\u0005\n\u0005\f\u0005\u0125"+
		"\t\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0005\u0005\u012f\b\u0005\n\u0005\f\u0005"+
		"\u0132\t\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0004\u0005\u0146\b\u0005\u000b\u0005\f\u0005\u0147\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0004\u0005\u0166\b\u0005\u000b"+
		"\u0005\f\u0005\u0167\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001"+
		"\u0005\u0001\u0005\u0001\u0005\u0005\u0005\u0177\b\u0005\n\u0005\f\u0005"+
		"\u017a\t\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005\u0001\u0005"+
		"\u0003\u0005\u0187\b\u0005\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006\u0001\u0006"+
		"\u0001\u0006\u0001\u0006\u0003\u0006\u0195\b\u0006\u0001\u0007\u0001\u0007"+
		"\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0001\u0007\u0005\u0007"+
		"\u019e\b\u0007\n\u0007\f\u0007\u01a1\t\u0007\u0001\u0007\u0001\u0007\u0001"+
		"\u0007\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b\u0001\b"+
		"\u0001\b\u0001\b\u0001\b\u0001\b\u0003\b\u01b2\b\b\u0001\t\u0001\t\u0001"+
		"\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\t\u0001\n\u0001\n\u0001\n\u0001"+
		"\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001\u000b\u0001"+
		"\f\u0001\f\u0001\f\u0001\f\u0001\f\u0005\f\u01ca\b\f\n\f\f\f\u01cd\t\f"+
		"\u0001\f\u0001\f\u0001\f\u0001\f\u0000\u0000\r\u0000\u0002\u0004\u0006"+
		"\b\n\f\u000e\u0010\u0012\u0014\u0016\u0018\u0000\u0000\u01f6\u0000\u001a"+
		"\u0001\u0000\u0000\u0000\u00020\u0001\u0000\u0000\u0000\u0004M\u0001\u0000"+
		"\u0000\u0000\u0006O\u0001\u0000\u0000\u0000\bW\u0001\u0000\u0000\u0000"+
		"\n\u0186\u0001\u0000\u0000\u0000\f\u0194\u0001\u0000\u0000\u0000\u000e"+
		"\u0196\u0001\u0000\u0000\u0000\u0010\u01b1\u0001\u0000\u0000\u0000\u0012"+
		"\u01b3\u0001\u0000\u0000\u0000\u0014\u01bb\u0001\u0000\u0000\u0000\u0016"+
		"\u01be\u0001\u0000\u0000\u0000\u0018\u01c4\u0001\u0000\u0000\u0000\u001a"+
		" \u0006\u0000\uffff\uffff\u0000\u001b\u001c\u0003\u0002\u0001\u0000\u001c"+
		"\u001d\u0006\u0000\uffff\uffff\u0000\u001d\u001f\u0001\u0000\u0000\u0000"+
		"\u001e\u001b\u0001\u0000\u0000\u0000\u001f\"\u0001\u0000\u0000\u0000 "+
		"\u001e\u0001\u0000\u0000\u0000 !\u0001\u0000\u0000\u0000!#\u0001\u0000"+
		"\u0000\u0000\" \u0001\u0000\u0000\u0000#$\u0006\u0000\uffff\uffff\u0000"+
		"$\u0001\u0001\u0000\u0000\u0000%&\u0003\n\u0005\u0000&\'\u0006\u0001\uffff"+
		"\uffff\u0000\'1\u0001\u0000\u0000\u0000()\u0003\f\u0006\u0000)*\u0006"+
		"\u0001\uffff\uffff\u0000*1\u0001\u0000\u0000\u0000+,\u0003\u000e\u0007"+
		"\u0000,-\u0006\u0001\uffff\uffff\u0000-1\u0001\u0000\u0000\u0000./\u0005"+
		"\u0001\u0000\u0000/1\u0006\u0001\uffff\uffff\u00000%\u0001\u0000\u0000"+
		"\u00000(\u0001\u0000\u0000\u00000+\u0001\u0000\u0000\u00000.\u0001\u0000"+
		"\u0000\u00001\u0003\u0001\u0000\u0000\u000023\u0005\"\u0000\u00003;\u0006"+
		"\u0002\uffff\uffff\u000045\u0005\"\u0000\u000056\u0003\u0010\b\u00006"+
		"7\u0006\u0002\uffff\uffff\u000078\u0005\'\u0000\u000089\u0006\u0002\uffff"+
		"\uffff\u00009:\u0005$\u0000\u0000:<\u0001\u0000\u0000\u0000;4\u0001\u0000"+
		"\u0000\u0000<=\u0001\u0000\u0000\u0000=;\u0001\u0000\u0000\u0000=>\u0001"+
		"\u0000\u0000\u0000>?\u0001\u0000\u0000\u0000?@\u0005$\u0000\u0000@A\u0006"+
		"\u0002\uffff\uffff\u0000AN\u0001\u0000\u0000\u0000BC\u0005\"\u0000\u0000"+
		"CH\u0006\u0002\uffff\uffff\u0000DE\u0005\'\u0000\u0000EG\u0006\u0002\uffff"+
		"\uffff\u0000FD\u0001\u0000\u0000\u0000GJ\u0001\u0000\u0000\u0000HF\u0001"+
		"\u0000\u0000\u0000HI\u0001\u0000\u0000\u0000IK\u0001\u0000\u0000\u0000"+
		"JH\u0001\u0000\u0000\u0000KL\u0005$\u0000\u0000LN\u0006\u0002\uffff\uffff"+
		"\u0000M2\u0001\u0000\u0000\u0000MB\u0001\u0000\u0000\u0000N\u0005\u0001"+
		"\u0000\u0000\u0000OP\u0005\"\u0000\u0000PQ\u0005\'\u0000\u0000QR\u0006"+
		"\u0003\uffff\uffff\u0000RS\u0003\u0002\u0001\u0000ST\u0006\u0003\uffff"+
		"\uffff\u0000TU\u0005$\u0000\u0000UV\u0006\u0003\uffff\uffff\u0000V\u0007"+
		"\u0001\u0000\u0000\u0000WX\u0005\"\u0000\u0000X^\u0006\u0004\uffff\uffff"+
		"\u0000YZ\u0003\u0006\u0003\u0000Z[\u0006\u0004\uffff\uffff\u0000[]\u0001"+
		"\u0000\u0000\u0000\\Y\u0001\u0000\u0000\u0000]`\u0001\u0000\u0000\u0000"+
		"^\\\u0001\u0000\u0000\u0000^_\u0001\u0000\u0000\u0000_a\u0001\u0000\u0000"+
		"\u0000`^\u0001\u0000\u0000\u0000ab\u0005$\u0000\u0000bc\u0006\u0004\uffff"+
		"\uffff\u0000c\t\u0001\u0000\u0000\u0000de\u0005\"\u0000\u0000ef\u0006"+
		"\u0005\uffff\uffff\u0000fl\u0005\u0002\u0000\u0000gh\u0003\u0002\u0001"+
		"\u0000hi\u0006\u0005\uffff\uffff\u0000ik\u0001\u0000\u0000\u0000jg\u0001"+
		"\u0000\u0000\u0000kn\u0001\u0000\u0000\u0000lj\u0001\u0000\u0000\u0000"+
		"lm\u0001\u0000\u0000\u0000mo\u0001\u0000\u0000\u0000nl\u0001\u0000\u0000"+
		"\u0000op\u0005$\u0000\u0000p\u0187\u0006\u0005\uffff\uffff\u0000qr\u0005"+
		"\"\u0000\u0000rs\u0005\u0003\u0000\u0000st\u0003\u0002\u0001\u0000tu\u0006"+
		"\u0005\uffff\uffff\u0000uv\u0003\u0010\b\u0000vw\u0006\u0005\uffff\uffff"+
		"\u0000wx\u0005$\u0000\u0000xy\u0006\u0005\uffff\uffff\u0000y\u0187\u0001"+
		"\u0000\u0000\u0000z{\u0005\"\u0000\u0000{|\u0005\u0004\u0000\u0000|}\u0005"+
		"\'\u0000\u0000}~\u0006\u0005\uffff\uffff\u0000~\u007f\u0003\u0002\u0001"+
		"\u0000\u007f\u0080\u0006\u0005\uffff\uffff\u0000\u0080\u0081\u0005$\u0000"+
		"\u0000\u0081\u0082\u0006\u0005\uffff\uffff\u0000\u0082\u0187\u0001\u0000"+
		"\u0000\u0000\u0083\u0084\u0005\"\u0000\u0000\u0084\u0085\u0005\u0005\u0000"+
		"\u0000\u0085\u0086\u0003\u0016\u000b\u0000\u0086\u0087\u0006\u0005\uffff"+
		"\uffff\u0000\u0087\u0088\u0003\u0016\u000b\u0000\u0088\u0089\u0006\u0005"+
		"\uffff\uffff\u0000\u0089\u008a\u0005\"\u0000\u0000\u008a\u008b\u0005\'"+
		"\u0000\u0000\u008b\u008c\u0006\u0005\uffff\uffff\u0000\u008c\u008d\u0005"+
		"$\u0000\u0000\u008d\u008e\u0003\u0002\u0001\u0000\u008e\u008f\u0006\u0005"+
		"\uffff\uffff\u0000\u008f\u0090\u0003\u0002\u0001\u0000\u0090\u0091\u0006"+
		"\u0005\uffff\uffff\u0000\u0091\u0092\u0005$\u0000\u0000\u0092\u0093\u0006"+
		"\u0005\uffff\uffff\u0000\u0093\u0187\u0001\u0000\u0000\u0000\u0094\u0095"+
		"\u0005\"\u0000\u0000\u0095\u0096\u0005\u0005\u0000\u0000\u0096\u0097\u0003"+
		"\u0016\u000b\u0000\u0097\u0098\u0006\u0005\uffff\uffff\u0000\u0098\u0099"+
		"\u0003\u0016\u000b\u0000\u0099\u009a\u0006\u0005\uffff\uffff\u0000\u009a"+
		"\u009b\u0005\"\u0000\u0000\u009b\u009c\u0005\'\u0000\u0000\u009c\u009d"+
		"\u0006\u0005\uffff\uffff\u0000\u009d\u009e\u0005$\u0000\u0000\u009e\u009f"+
		"\u0003\u0002\u0001\u0000\u009f\u00a0\u0006\u0005\uffff\uffff\u0000\u00a0"+
		"\u00a1\u0005$\u0000\u0000\u00a1\u00a2\u0006\u0005\uffff\uffff\u0000\u00a2"+
		"\u0187\u0001\u0000\u0000\u0000\u00a3\u00a4\u0005\"\u0000\u0000\u00a4\u00a5"+
		"\u0005\u0006\u0000\u0000\u00a5\u00a6\u0003\u0016\u000b\u0000\u00a6\u00a7"+
		"\u0006\u0005\uffff\uffff\u0000\u00a7\u00a8\u0003\u0004\u0002\u0000\u00a8"+
		"\u00a9\u0006\u0005\uffff\uffff\u0000\u00a9\u00aa\u0003\u0002\u0001\u0000"+
		"\u00aa\u00ab\u0006\u0005\uffff\uffff\u0000\u00ab\u00ac\u0005$\u0000\u0000"+
		"\u00ac\u00ad\u0006\u0005\uffff\uffff\u0000\u00ad\u0187\u0001\u0000\u0000"+
		"\u0000\u00ae\u00af\u0005\"\u0000\u0000\u00af\u00b0\u0005\u0007\u0000\u0000"+
		"\u00b0\u00b1\u0003\u0002\u0001\u0000\u00b1\u00b2\u0006\u0005\uffff\uffff"+
		"\u0000\u00b2\u00b3\u0003\u0010\b\u0000\u00b3\u00b4\u0006\u0005\uffff\uffff"+
		"\u0000\u00b4\u00b5\u0005$\u0000\u0000\u00b5\u00b6\u0006\u0005\uffff\uffff"+
		"\u0000\u00b6\u0187\u0001\u0000\u0000\u0000\u00b7\u00b8\u0005\"\u0000\u0000"+
		"\u00b8\u00b9\u0005\b\u0000\u0000\u00b9\u00ba\u0003\u0010\b\u0000\u00ba"+
		"\u00bb\u0006\u0005\uffff\uffff\u0000\u00bb\u00bc\u0003\u0010\b\u0000\u00bc"+
		"\u00bd\u0006\u0005\uffff\uffff\u0000\u00bd\u00be\u0003\u0002\u0001\u0000"+
		"\u00be\u00bf\u0006\u0005\uffff\uffff\u0000\u00bf\u00c0\u0005$\u0000\u0000"+
		"\u00c0\u00c1\u0006\u0005\uffff\uffff\u0000\u00c1\u0187\u0001\u0000\u0000"+
		"\u0000\u00c2\u00c3\u0005\"\u0000\u0000\u00c3\u00c4\u0005\t\u0000\u0000"+
		"\u00c4\u00c5\u0003\u0016\u000b\u0000\u00c5\u00cb\u0006\u0005\uffff\uffff"+
		"\u0000\u00c6\u00c7\u0003\u0002\u0001\u0000\u00c7\u00c8\u0006\u0005\uffff"+
		"\uffff\u0000\u00c8\u00ca\u0001\u0000\u0000\u0000\u00c9\u00c6\u0001\u0000"+
		"\u0000\u0000\u00ca\u00cd\u0001\u0000\u0000\u0000\u00cb\u00c9\u0001\u0000"+
		"\u0000\u0000\u00cb\u00cc\u0001\u0000\u0000\u0000\u00cc\u00ce\u0001\u0000"+
		"\u0000\u0000\u00cd\u00cb\u0001\u0000\u0000\u0000\u00ce\u00cf\u0005$\u0000"+
		"\u0000\u00cf\u00d0\u0006\u0005\uffff\uffff\u0000\u00d0\u0187\u0001\u0000"+
		"\u0000\u0000\u00d1\u00d2\u0005\"\u0000\u0000\u00d2\u00d3\u0005\n\u0000"+
		"\u0000\u00d3\u00d4\u0003\u0002\u0001\u0000\u00d4\u00d5\u0006\u0005\uffff"+
		"\uffff\u0000\u00d5\u00d6\u0005$\u0000\u0000\u00d6\u00d7\u0006\u0005\uffff"+
		"\uffff\u0000\u00d7\u0187\u0001\u0000\u0000\u0000\u00d8\u00d9\u0005\"\u0000"+
		"\u0000\u00d9\u00da\u0005\u000b\u0000\u0000\u00da\u00db\u0003\u0002\u0001"+
		"\u0000\u00db\u00dc\u0006\u0005\uffff\uffff\u0000\u00dc\u00dd\u0003\u0002"+
		"\u0001\u0000\u00dd\u00de\u0006\u0005\uffff\uffff\u0000\u00de\u00df\u0003"+
		"\u0002\u0001\u0000\u00df\u00e0\u0006\u0005\uffff\uffff\u0000\u00e0\u00e1"+
		"\u0005$\u0000\u0000\u00e1\u00e2\u0006\u0005\uffff\uffff\u0000\u00e2\u0187"+
		"\u0001\u0000\u0000\u0000\u00e3\u00e4\u0005\"\u0000\u0000\u00e4\u00e5\u0005"+
		"\u000b\u0000\u0000\u00e5\u00e6\u0003\u0002\u0001\u0000\u00e6\u00e7\u0006"+
		"\u0005\uffff\uffff\u0000\u00e7\u00e8\u0003\u0002\u0001\u0000\u00e8\u00e9"+
		"\u0006\u0005\uffff\uffff\u0000\u00e9\u00ea\u0005$\u0000\u0000\u00ea\u00eb"+
		"\u0006\u0005\uffff\uffff\u0000\u00eb\u0187\u0001\u0000\u0000\u0000\u00ec"+
		"\u00ed\u0005\"\u0000\u0000\u00ed\u00ee\u0005\f\u0000\u0000\u00ee\u00ef"+
		"\u0003\u0002\u0001\u0000\u00ef\u00f0\u0006\u0005\uffff\uffff\u0000\u00f0"+
		"\u00f1\u0003\u0002\u0001\u0000\u00f1\u00f2\u0006\u0005\uffff\uffff\u0000"+
		"\u00f2\u00f3\u0005$\u0000\u0000\u00f3\u00f4\u0006\u0005\uffff\uffff\u0000"+
		"\u00f4\u0187\u0001\u0000\u0000\u0000\u00f5\u00f6\u0005\"\u0000\u0000\u00f6"+
		"\u00f7\u0005\r\u0000\u0000\u00f7\u00f8\u0003\u0002\u0001\u0000\u00f8\u00f9"+
		"\u0006\u0005\uffff\uffff\u0000\u00f9\u00fa\u0003\u0002\u0001\u0000\u00fa"+
		"\u00fb\u0006\u0005\uffff\uffff\u0000\u00fb\u00fc\u0003\u0002\u0001\u0000"+
		"\u00fc\u00fd\u0006\u0005\uffff\uffff\u0000\u00fd\u00fe\u0005$\u0000\u0000"+
		"\u00fe\u00ff\u0006\u0005\uffff\uffff\u0000\u00ff\u0187\u0001\u0000\u0000"+
		"\u0000\u0100\u0101\u0005\"\u0000\u0000\u0101\u0102\u0005\u000e\u0000\u0000"+
		"\u0102\u0103\u0003\u0002\u0001\u0000\u0103\u0104\u0006\u0005\uffff\uffff"+
		"\u0000\u0104\u0105\u0003\u0010\b\u0000\u0105\u0106\u0006\u0005\uffff\uffff"+
		"\u0000\u0106\u0107\u0005$\u0000\u0000\u0107\u0108\u0006\u0005\uffff\uffff"+
		"\u0000\u0108\u0187\u0001\u0000\u0000\u0000\u0109\u010a\u0005\"\u0000\u0000"+
		"\u010a\u010b\u0005\u000f\u0000\u0000\u010b\u010c\u0003\u0002\u0001\u0000"+
		"\u010c\u010d\u0006\u0005\uffff\uffff\u0000\u010d\u010e\u0003\u0010\b\u0000"+
		"\u010e\u010f\u0006\u0005\uffff\uffff\u0000\u010f\u0110\u0005$\u0000\u0000"+
		"\u0110\u0111\u0006\u0005\uffff\uffff\u0000\u0111\u0187\u0001\u0000\u0000"+
		"\u0000\u0112\u0113\u0005\"\u0000\u0000\u0113\u0114\u0005\u0010\u0000\u0000"+
		"\u0114\u0115\u0003\b\u0004\u0000\u0115\u0116\u0006\u0005\uffff\uffff\u0000"+
		"\u0116\u0117\u0003\u0002\u0001\u0000\u0117\u0118\u0006\u0005\uffff\uffff"+
		"\u0000\u0118\u0119\u0005$\u0000\u0000\u0119\u011a\u0006\u0005\uffff\uffff"+
		"\u0000\u011a\u0187\u0001\u0000\u0000\u0000\u011b\u011c\u0005\"\u0000\u0000"+
		"\u011c\u011d\u0005\u0011\u0000\u0000\u011d\u0123\u0006\u0005\uffff\uffff"+
		"\u0000\u011e\u011f\u0003\u0002\u0001\u0000\u011f\u0120\u0006\u0005\uffff"+
		"\uffff\u0000\u0120\u0122\u0001\u0000\u0000\u0000\u0121\u011e\u0001\u0000"+
		"\u0000\u0000\u0122\u0125\u0001\u0000\u0000\u0000\u0123\u0121\u0001\u0000"+
		"\u0000\u0000\u0123\u0124\u0001\u0000\u0000\u0000\u0124\u0126\u0001\u0000"+
		"\u0000\u0000\u0125\u0123\u0001\u0000\u0000\u0000\u0126\u0127\u0005$\u0000"+
		"\u0000\u0127\u0187\u0006\u0005\uffff\uffff\u0000\u0128\u0129\u0005\"\u0000"+
		"\u0000\u0129\u012a\u0005\u0012\u0000\u0000\u012a\u0130\u0006\u0005\uffff"+
		"\uffff\u0000\u012b\u012c\u0003\u0002\u0001\u0000\u012c\u012d\u0006\u0005"+
		"\uffff\uffff\u0000\u012d\u012f\u0001\u0000\u0000\u0000\u012e\u012b\u0001"+
		"\u0000\u0000\u0000\u012f\u0132\u0001\u0000\u0000\u0000\u0130\u012e\u0001"+
		"\u0000\u0000\u0000\u0130\u0131\u0001\u0000\u0000\u0000\u0131\u0133\u0001"+
		"\u0000\u0000\u0000\u0132\u0130\u0001\u0000\u0000\u0000\u0133\u0134\u0005"+
		"$\u0000\u0000\u0134\u0187\u0006\u0005\uffff\uffff\u0000\u0135\u0136\u0005"+
		"\"\u0000\u0000\u0136\u0137\u0005\u0013\u0000\u0000\u0137\u0138\u0003\u0004"+
		"\u0002\u0000\u0138\u0139\u0006\u0005\uffff\uffff\u0000\u0139\u013a\u0003"+
		"\u0002\u0001\u0000\u013a\u013b\u0006\u0005\uffff\uffff\u0000\u013b\u013c"+
		"\u0005$\u0000\u0000\u013c\u013d\u0006\u0005\uffff\uffff\u0000\u013d\u0187"+
		"\u0001\u0000\u0000\u0000\u013e\u013f\u0005\"\u0000\u0000\u013f\u0140\u0005"+
		"\u0014\u0000\u0000\u0140\u0141\u0005\"\u0000\u0000\u0141\u0145\u0006\u0005"+
		"\uffff\uffff\u0000\u0142\u0143\u0003\u0010\b\u0000\u0143\u0144\u0006\u0005"+
		"\uffff\uffff\u0000\u0144\u0146\u0001\u0000\u0000\u0000\u0145\u0142\u0001"+
		"\u0000\u0000\u0000\u0146\u0147\u0001\u0000\u0000\u0000\u0147\u0145\u0001"+
		"\u0000\u0000\u0000\u0147\u0148\u0001\u0000\u0000\u0000\u0148\u0149\u0001"+
		"\u0000\u0000\u0000\u0149\u014a\u0005$\u0000\u0000\u014a\u014b\u0005$\u0000"+
		"\u0000\u014b\u014c\u0006\u0005\uffff\uffff\u0000\u014c\u0187\u0001\u0000"+
		"\u0000\u0000\u014d\u014e\u0005\"\u0000\u0000\u014e\u014f\u0005\u0015\u0000"+
		"\u0000\u014f\u0150\u0003\b\u0004\u0000\u0150\u0151\u0006\u0005\uffff\uffff"+
		"\u0000\u0151\u0152\u0003\u0002\u0001\u0000\u0152\u0153\u0006\u0005\uffff"+
		"\uffff\u0000\u0153\u0154\u0005$\u0000\u0000\u0154\u0155\u0006\u0005\uffff"+
		"\uffff\u0000\u0155\u0187\u0001\u0000\u0000\u0000\u0156\u0157\u0005\"\u0000"+
		"\u0000\u0157\u0158\u0005\u0016\u0000\u0000\u0158\u0159\u0003\b\u0004\u0000"+
		"\u0159\u015a\u0006\u0005\uffff\uffff\u0000\u015a\u015b\u0003\u0002\u0001"+
		"\u0000\u015b\u015c\u0006\u0005\uffff\uffff\u0000\u015c\u015d\u0005$\u0000"+
		"\u0000\u015d\u015e\u0006\u0005\uffff\uffff\u0000\u015e\u0187\u0001\u0000"+
		"\u0000\u0000\u015f\u0160\u0005\"\u0000\u0000\u0160\u0161\u0005\u0017\u0000"+
		"\u0000\u0161\u0162\u0005\"\u0000\u0000\u0162\u0165\u0006\u0005\uffff\uffff"+
		"\u0000\u0163\u0164\u0005\'\u0000\u0000\u0164\u0166\u0006\u0005\uffff\uffff"+
		"\u0000\u0165\u0163\u0001\u0000\u0000\u0000\u0166\u0167\u0001\u0000\u0000"+
		"\u0000\u0167\u0165\u0001\u0000\u0000\u0000\u0167\u0168\u0001\u0000\u0000"+
		"\u0000\u0168\u0169\u0001\u0000\u0000\u0000\u0169\u016a\u0005$\u0000\u0000"+
		"\u016a\u016b\u0006\u0005\uffff\uffff\u0000\u016b\u016c\u0003\u0002\u0001"+
		"\u0000\u016c\u016d\u0006\u0005\uffff\uffff\u0000\u016d\u016e\u0005$\u0000"+
		"\u0000\u016e\u016f\u0006\u0005\uffff\uffff\u0000\u016f\u0187\u0001\u0000"+
		"\u0000\u0000\u0170\u0171\u0005\"\u0000\u0000\u0171\u0172\u0005\u0018\u0000"+
		"\u0000\u0172\u0178\u0006\u0005\uffff\uffff\u0000\u0173\u0174\u0003\u0002"+
		"\u0001\u0000\u0174\u0175\u0006\u0005\uffff\uffff\u0000\u0175\u0177\u0001"+
		"\u0000\u0000\u0000\u0176\u0173\u0001\u0000\u0000\u0000\u0177\u017a\u0001"+
		"\u0000\u0000\u0000\u0178\u0176\u0001\u0000\u0000\u0000\u0178\u0179\u0001"+
		"\u0000\u0000\u0000\u0179\u017b\u0001\u0000\u0000\u0000\u017a\u0178\u0001"+
		"\u0000\u0000\u0000\u017b\u017c\u0005$\u0000\u0000\u017c\u0187\u0006\u0005"+
		"\uffff\uffff\u0000\u017d\u017e\u0005\"\u0000\u0000\u017e\u017f\u0005\u0019"+
		"\u0000\u0000\u017f\u0180\u0003\u0002\u0001\u0000\u0180\u0181\u0006\u0005"+
		"\uffff\uffff\u0000\u0181\u0182\u0003\u0002\u0001\u0000\u0182\u0183\u0006"+
		"\u0005\uffff\uffff\u0000\u0183\u0184\u0005$\u0000\u0000\u0184\u0185\u0006"+
		"\u0005\uffff\uffff\u0000\u0185\u0187\u0001\u0000\u0000\u0000\u0186d\u0001"+
		"\u0000\u0000\u0000\u0186q\u0001\u0000\u0000\u0000\u0186z\u0001\u0000\u0000"+
		"\u0000\u0186\u0083\u0001\u0000\u0000\u0000\u0186\u0094\u0001\u0000\u0000"+
		"\u0000\u0186\u00a3\u0001\u0000\u0000\u0000\u0186\u00ae\u0001\u0000\u0000"+
		"\u0000\u0186\u00b7\u0001\u0000\u0000\u0000\u0186\u00c2\u0001\u0000\u0000"+
		"\u0000\u0186\u00d1\u0001\u0000\u0000\u0000\u0186\u00d8\u0001\u0000\u0000"+
		"\u0000\u0186\u00e3\u0001\u0000\u0000\u0000\u0186\u00ec\u0001\u0000\u0000"+
		"\u0000\u0186\u00f5\u0001\u0000\u0000\u0000\u0186\u0100\u0001\u0000\u0000"+
		"\u0000\u0186\u0109\u0001\u0000\u0000\u0000\u0186\u0112\u0001\u0000\u0000"+
		"\u0000\u0186\u011b\u0001\u0000\u0000\u0000\u0186\u0128\u0001\u0000\u0000"+
		"\u0000\u0186\u0135\u0001\u0000\u0000\u0000\u0186\u013e\u0001\u0000\u0000"+
		"\u0000\u0186\u014d\u0001\u0000\u0000\u0000\u0186\u0156\u0001\u0000\u0000"+
		"\u0000\u0186\u015f\u0001\u0000\u0000\u0000\u0186\u0170\u0001\u0000\u0000"+
		"\u0000\u0186\u017d\u0001\u0000\u0000\u0000\u0187\u000b\u0001\u0000\u0000"+
		"\u0000\u0188\u0189\u0005\u001c\u0000\u0000\u0189\u0195\u0006\u0006\uffff"+
		"\uffff\u0000\u018a\u018b\u0005\u001d\u0000\u0000\u018b\u0195\u0006\u0006"+
		"\uffff\uffff\u0000\u018c\u018d\u0005\'\u0000\u0000\u018d\u0195\u0006\u0006"+
		"\uffff\uffff\u0000\u018e\u018f\u0005\u001a\u0000\u0000\u018f\u0195\u0006"+
		"\u0006\uffff\uffff\u0000\u0190\u0191\u0005\u001b\u0000\u0000\u0191\u0195"+
		"\u0006\u0006\uffff\uffff\u0000\u0192\u0193\u0005 \u0000\u0000\u0193\u0195"+
		"\u0006\u0006\uffff\uffff\u0000\u0194\u0188\u0001\u0000\u0000\u0000\u0194"+
		"\u018a\u0001\u0000\u0000\u0000\u0194\u018c\u0001\u0000\u0000\u0000\u0194"+
		"\u018e\u0001\u0000\u0000\u0000\u0194\u0190\u0001\u0000\u0000\u0000\u0194"+
		"\u0192\u0001\u0000\u0000\u0000\u0195\r\u0001\u0000\u0000\u0000\u0196\u0197"+
		"\u0005\"\u0000\u0000\u0197\u0198\u0006\u0007\uffff\uffff\u0000\u0198\u0199"+
		"\u0003\u0002\u0001\u0000\u0199\u019f\u0006\u0007\uffff\uffff\u0000\u019a"+
		"\u019b\u0003\u0002\u0001\u0000\u019b\u019c\u0006\u0007\uffff\uffff\u0000"+
		"\u019c\u019e\u0001\u0000\u0000\u0000\u019d\u019a\u0001\u0000\u0000\u0000"+
		"\u019e\u01a1\u0001\u0000\u0000\u0000\u019f\u019d\u0001\u0000\u0000\u0000"+
		"\u019f\u01a0\u0001\u0000\u0000\u0000\u01a0\u01a2\u0001\u0000\u0000\u0000"+
		"\u01a1\u019f\u0001\u0000\u0000\u0000\u01a2\u01a3\u0005$\u0000\u0000\u01a3"+
		"\u01a4\u0006\u0007\uffff\uffff\u0000\u01a4\u000f\u0001\u0000\u0000\u0000"+
		"\u01a5\u01a6\u0003\u0012\t\u0000\u01a6\u01a7\u0006\b\uffff\uffff\u0000"+
		"\u01a7\u01b2\u0001\u0000\u0000\u0000\u01a8\u01a9\u0003\u0016\u000b\u0000"+
		"\u01a9\u01aa\u0006\b\uffff\uffff\u0000\u01aa\u01b2\u0001\u0000\u0000\u0000"+
		"\u01ab\u01ac\u0003\u0014\n\u0000\u01ac\u01ad\u0006\b\uffff\uffff\u0000"+
		"\u01ad\u01b2\u0001\u0000\u0000\u0000\u01ae\u01af\u0003\u0018\f\u0000\u01af"+
		"\u01b0\u0006\b\uffff\uffff\u0000\u01b0\u01b2\u0001\u0000\u0000\u0000\u01b1"+
		"\u01a5\u0001\u0000\u0000\u0000\u01b1\u01a8\u0001\u0000\u0000\u0000\u01b1"+
		"\u01ab\u0001\u0000\u0000\u0000\u01b1\u01ae\u0001\u0000\u0000\u0000\u01b2"+
		"\u0011\u0001\u0000\u0000\u0000\u01b3\u01b4\u0005\"\u0000\u0000\u01b4\u01b5"+
		"\u0003\u0018\f\u0000\u01b5\u01b6\u0006\t\uffff\uffff\u0000\u01b6\u01b7"+
		"\u0005!\u0000\u0000\u01b7\u01b8\u0003\u0010\b\u0000\u01b8\u01b9\u0006"+
		"\t\uffff\uffff\u0000\u01b9\u01ba\u0005$\u0000\u0000\u01ba\u0013\u0001"+
		"\u0000\u0000\u0000\u01bb\u01bc\u0005\'\u0000\u0000\u01bc\u01bd\u0006\n"+
		"\uffff\uffff\u0000\u01bd\u0015\u0001\u0000\u0000\u0000\u01be\u01bf\u0005"+
		"\'\u0000\u0000\u01bf\u01c0\u0006\u000b\uffff\uffff\u0000\u01c0\u01c1\u0005"+
		"&\u0000\u0000\u01c1\u01c2\u0005\'\u0000\u0000\u01c2\u01c3\u0006\u000b"+
		"\uffff\uffff\u0000\u01c3\u0017\u0001\u0000\u0000\u0000\u01c4\u01c5\u0005"+
		"\"\u0000\u0000\u01c5\u01cb\u0006\f\uffff\uffff\u0000\u01c6\u01c7\u0003"+
		"\u0010\b\u0000\u01c7\u01c8\u0006\f\uffff\uffff\u0000\u01c8\u01ca\u0001"+
		"\u0000\u0000\u0000\u01c9\u01c6\u0001\u0000\u0000\u0000\u01ca\u01cd\u0001"+
		"\u0000\u0000\u0000\u01cb\u01c9\u0001\u0000\u0000\u0000\u01cb\u01cc\u0001"+
		"\u0000\u0000\u0000\u01cc\u01ce\u0001\u0000\u0000\u0000\u01cd\u01cb\u0001"+
		"\u0000\u0000\u0000\u01ce\u01cf\u0005$\u0000\u0000\u01cf\u01d0\u0006\f"+
		"\uffff\uffff\u0000\u01d0\u0019\u0001\u0000\u0000\u0000\u0012 0=HM^l\u00cb"+
		"\u0123\u0130\u0147\u0167\u0178\u0186\u0194\u019f\u01b1\u01cb";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}