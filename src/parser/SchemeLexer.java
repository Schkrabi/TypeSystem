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

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class SchemeLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.7", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, DEFTYPE=9, 
		DEFREP=10, IF=11, LAMBDA=12, ELAMBDA=13, TRUE=14, FALSE=15, INT=16, FLOAT=17, 
		SYMBOL=18, COMMENT=19, WS=20, STRING=21;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "DEFTYPE", 
		"DEFREP", "IF", "LAMBDA", "ELAMBDA", "TRUE", "FALSE", "INT", "FLOAT", 
		"SYMBOL", "COMMENT", "WS", "STRING", "SYMBOL_HEAD", "SYMBOL_REST"
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


	public SchemeLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Scheme.g"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getChannelNames() { return channelNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public static final String _serializedATN =
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\27\u00a5\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\3\2"+
		"\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n\3"+
		"\n\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\f\3\f\3\f"+
		"\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3\16\3"+
		"\17\3\17\3\17\3\20\3\20\3\20\3\21\5\21j\n\21\3\21\6\21m\n\21\r\21\16\21"+
		"n\3\22\5\22r\n\22\3\22\7\22u\n\22\f\22\16\22x\13\22\3\22\3\22\6\22|\n"+
		"\22\r\22\16\22}\3\23\3\23\7\23\u0082\n\23\f\23\16\23\u0085\13\23\3\24"+
		"\3\24\7\24\u0089\n\24\f\24\16\24\u008c\13\24\3\24\3\24\3\25\6\25\u0091"+
		"\n\25\r\25\16\25\u0092\3\25\3\25\3\26\3\26\7\26\u0099\n\26\f\26\16\26"+
		"\u009c\13\26\3\26\3\26\3\27\3\27\3\30\3\30\5\30\u00a4\n\30\2\2\31\3\3"+
		"\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21"+
		"!\22#\23%\24\'\25)\26+\27-\2/\2\3\2\b\3\2\62;\4\2\f\f\17\17\5\2\13\f\17"+
		"\17\"\"\4\2\f\f$$\f\2##%&((,-//\61\61??A\\aac|\4\2\60\60\62;\2\u00ac\2"+
		"\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2"+
		"\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2"+
		"\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2"+
		"\2\2%\3\2\2\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\3\61\3\2\2\2\5\63\3\2"+
		"\2\2\7\65\3\2\2\2\t\67\3\2\2\2\139\3\2\2\2\r;\3\2\2\2\17=\3\2\2\2\21?"+
		"\3\2\2\2\23A\3\2\2\2\25I\3\2\2\2\27P\3\2\2\2\31S\3\2\2\2\33Z\3\2\2\2\35"+
		"b\3\2\2\2\37e\3\2\2\2!i\3\2\2\2#q\3\2\2\2%\177\3\2\2\2\'\u0086\3\2\2\2"+
		")\u0090\3\2\2\2+\u0096\3\2\2\2-\u009f\3\2\2\2/\u00a3\3\2\2\2\61\62\7*"+
		"\2\2\62\4\3\2\2\2\63\64\7+\2\2\64\6\3\2\2\2\65\66\7]\2\2\66\b\3\2\2\2"+
		"\678\7_\2\28\n\3\2\2\29:\7>\2\2:\f\3\2\2\2;<\7@\2\2<\16\3\2\2\2=>\7<\2"+
		"\2>\20\3\2\2\2?@\7)\2\2@\22\3\2\2\2AB\7f\2\2BC\7g\2\2CD\7h\2\2DE\7v\2"+
		"\2EF\7{\2\2FG\7r\2\2GH\7g\2\2H\24\3\2\2\2IJ\7f\2\2JK\7g\2\2KL\7h\2\2L"+
		"M\7t\2\2MN\7g\2\2NO\7r\2\2O\26\3\2\2\2PQ\7k\2\2QR\7h\2\2R\30\3\2\2\2S"+
		"T\7n\2\2TU\7c\2\2UV\7o\2\2VW\7d\2\2WX\7f\2\2XY\7c\2\2Y\32\3\2\2\2Z[\7"+
		"g\2\2[\\\7n\2\2\\]\7c\2\2]^\7o\2\2^_\7d\2\2_`\7f\2\2`a\7c\2\2a\34\3\2"+
		"\2\2bc\7%\2\2cd\7v\2\2d\36\3\2\2\2ef\7%\2\2fg\7h\2\2g \3\2\2\2hj\7/\2"+
		"\2ih\3\2\2\2ij\3\2\2\2jl\3\2\2\2km\t\2\2\2lk\3\2\2\2mn\3\2\2\2nl\3\2\2"+
		"\2no\3\2\2\2o\"\3\2\2\2pr\7/\2\2qp\3\2\2\2qr\3\2\2\2rv\3\2\2\2su\t\2\2"+
		"\2ts\3\2\2\2ux\3\2\2\2vt\3\2\2\2vw\3\2\2\2wy\3\2\2\2xv\3\2\2\2y{\7\60"+
		"\2\2z|\t\2\2\2{z\3\2\2\2|}\3\2\2\2}{\3\2\2\2}~\3\2\2\2~$\3\2\2\2\177\u0083"+
		"\5-\27\2\u0080\u0082\5/\30\2\u0081\u0080\3\2\2\2\u0082\u0085\3\2\2\2\u0083"+
		"\u0081\3\2\2\2\u0083\u0084\3\2\2\2\u0084&\3\2\2\2\u0085\u0083\3\2\2\2"+
		"\u0086\u008a\7=\2\2\u0087\u0089\n\3\2\2\u0088\u0087\3\2\2\2\u0089\u008c"+
		"\3\2\2\2\u008a\u0088\3\2\2\2\u008a\u008b\3\2\2\2\u008b\u008d\3\2\2\2\u008c"+
		"\u008a\3\2\2\2\u008d\u008e\b\24\2\2\u008e(\3\2\2\2\u008f\u0091\t\4\2\2"+
		"\u0090\u008f\3\2\2\2\u0091\u0092\3\2\2\2\u0092\u0090\3\2\2\2\u0092\u0093"+
		"\3\2\2\2\u0093\u0094\3\2\2\2\u0094\u0095\b\25\2\2\u0095*\3\2\2\2\u0096"+
		"\u009a\7$\2\2\u0097\u0099\n\5\2\2\u0098\u0097\3\2\2\2\u0099\u009c\3\2"+
		"\2\2\u009a\u0098\3\2\2\2\u009a\u009b\3\2\2\2\u009b\u009d\3\2\2\2\u009c"+
		"\u009a\3\2\2\2\u009d\u009e\7$\2\2\u009e,\3\2\2\2\u009f\u00a0\t\6\2\2\u00a0"+
		".\3\2\2\2\u00a1\u00a4\5-\27\2\u00a2\u00a4\t\7\2\2\u00a3\u00a1\3\2\2\2"+
		"\u00a3\u00a2\3\2\2\2\u00a4\60\3\2\2\2\r\2inqv}\u0083\u008a\u0092\u009a"+
		"\u00a3\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}