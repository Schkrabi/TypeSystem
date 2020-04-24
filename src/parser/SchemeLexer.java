// Generated from Scheme.g by ANTLR 4.7.2
package parser;

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
	static { RuntimeMetaData.checkVersion("4.7.2", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, TRUE=6, FALSE=7, INT=8, FLOAT=9, 
		SYMBOL=10, COMMENT=11, WS=12, STRING=13;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "T__3", "T__4", "TRUE", "FALSE", "INT", "FLOAT", 
			"SYMBOL", "COMMENT", "WS", "STRING", "SYMBOL_HEAD", "SYMBOL_REST"
		};
	}
	public static final String[] ruleNames = makeRuleNames();

	private static String[] makeLiteralNames() {
		return new String[] {
			null, "'('", "')'", "':'", "'-'", "'>'"
		};
	}
	private static final String[] _LITERAL_NAMES = makeLiteralNames();
	private static String[] makeSymbolicNames() {
		return new String[] {
			null, null, null, null, null, null, "TRUE", "FALSE", "INT", "FLOAT", 
			"SYMBOL", "COMMENT", "WS", "STRING"
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\17n\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\3\2\3\2\3\3\3\3\3\4"+
		"\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3\7\3\b\3\b\3\b\3\t\5\t\63\n\t\3\t\6\t\66"+
		"\n\t\r\t\16\t\67\3\n\5\n;\n\n\3\n\7\n>\n\n\f\n\16\nA\13\n\3\n\3\n\6\n"+
		"E\n\n\r\n\16\nF\3\13\3\13\7\13K\n\13\f\13\16\13N\13\13\3\f\3\f\7\fR\n"+
		"\f\f\f\16\fU\13\f\3\f\3\f\3\r\6\rZ\n\r\r\r\16\r[\3\r\3\r\3\16\3\16\7\16"+
		"b\n\16\f\16\16\16e\13\16\3\16\3\16\3\17\3\17\3\20\3\20\5\20m\n\20\2\2"+
		"\21\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\17\35"+
		"\2\37\2\3\2\b\3\2\62;\4\2\f\f\17\17\5\2\13\f\17\17\"\"\4\2\f\f$$\13\2"+
		"##%&((,-//\61\61>\\aac|\4\2\60\60\62;\2u\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3"+
		"\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2"+
		"\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\3!\3"+
		"\2\2\2\5#\3\2\2\2\7%\3\2\2\2\t\'\3\2\2\2\13)\3\2\2\2\r+\3\2\2\2\17.\3"+
		"\2\2\2\21\62\3\2\2\2\23:\3\2\2\2\25H\3\2\2\2\27O\3\2\2\2\31Y\3\2\2\2\33"+
		"_\3\2\2\2\35h\3\2\2\2\37l\3\2\2\2!\"\7*\2\2\"\4\3\2\2\2#$\7+\2\2$\6\3"+
		"\2\2\2%&\7<\2\2&\b\3\2\2\2\'(\7/\2\2(\n\3\2\2\2)*\7@\2\2*\f\3\2\2\2+,"+
		"\7%\2\2,-\7v\2\2-\16\3\2\2\2./\7%\2\2/\60\7h\2\2\60\20\3\2\2\2\61\63\7"+
		"/\2\2\62\61\3\2\2\2\62\63\3\2\2\2\63\65\3\2\2\2\64\66\t\2\2\2\65\64\3"+
		"\2\2\2\66\67\3\2\2\2\67\65\3\2\2\2\678\3\2\2\28\22\3\2\2\29;\7/\2\2:9"+
		"\3\2\2\2:;\3\2\2\2;?\3\2\2\2<>\t\2\2\2=<\3\2\2\2>A\3\2\2\2?=\3\2\2\2?"+
		"@\3\2\2\2@B\3\2\2\2A?\3\2\2\2BD\7\60\2\2CE\t\2\2\2DC\3\2\2\2EF\3\2\2\2"+
		"FD\3\2\2\2FG\3\2\2\2G\24\3\2\2\2HL\5\35\17\2IK\5\37\20\2JI\3\2\2\2KN\3"+
		"\2\2\2LJ\3\2\2\2LM\3\2\2\2M\26\3\2\2\2NL\3\2\2\2OS\7=\2\2PR\n\3\2\2QP"+
		"\3\2\2\2RU\3\2\2\2SQ\3\2\2\2ST\3\2\2\2TV\3\2\2\2US\3\2\2\2VW\b\f\2\2W"+
		"\30\3\2\2\2XZ\t\4\2\2YX\3\2\2\2Z[\3\2\2\2[Y\3\2\2\2[\\\3\2\2\2\\]\3\2"+
		"\2\2]^\b\r\2\2^\32\3\2\2\2_c\7$\2\2`b\n\5\2\2a`\3\2\2\2be\3\2\2\2ca\3"+
		"\2\2\2cd\3\2\2\2df\3\2\2\2ec\3\2\2\2fg\7$\2\2g\34\3\2\2\2hi\t\6\2\2i\36"+
		"\3\2\2\2jm\5\35\17\2km\t\7\2\2lj\3\2\2\2lk\3\2\2\2m \3\2\2\2\r\2\62\67"+
		":?FLS[cl\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}