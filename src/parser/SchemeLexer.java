// Generated from Scheme.g by ANTLR 4.7
package parser;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;

@SuppressWarnings({ "all", "warnings", "unchecked", "unused", "cast" })
public class SchemeLexer extends Lexer {
	static {
		RuntimeMetaData.checkVersion("4.7", RuntimeMetaData.VERSION);
	}

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache = new PredictionContextCache();
	public static final int T__0 = 1, T__1 = 2, T__2 = 3, TRUE = 4, FALSE = 5, INT = 6, FLOAT = 7, SYMBOL = 8,
			COMMENT = 9, WS = 10, STRING = 11;
	public static String[] channelNames = { "DEFAULT_TOKEN_CHANNEL", "HIDDEN" };

	public static String[] modeNames = { "DEFAULT_MODE" };

	public static final String[] ruleNames = { "T__0", "T__1", "T__2", "TRUE", "FALSE", "INT", "FLOAT", "SYMBOL",
			"COMMENT", "WS", "STRING", "SYMBOL_HEAD", "SYMBOL_REST" };

	private static final String[] _LITERAL_NAMES = { null, "'('", "')'", "':'" };
	private static final String[] _SYMBOLIC_NAMES = { null, null, null, null, "TRUE", "FALSE", "INT", "FLOAT", "SYMBOL",
			"COMMENT", "WS", "STRING" };
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
		_interp = new LexerATNSimulator(this, _ATN, _decisionToDFA, _sharedContextCache);
	}

	@Override
	public String getGrammarFileName() {
		return "Scheme.g";
	}

	@Override
	public String[] getRuleNames() {
		return ruleNames;
	}

	@Override
	public String getSerializedATN() {
		return _serializedATN;
	}

	@Override
	public String[] getChannelNames() {
		return channelNames;
	}

	@Override
	public String[] getModeNames() {
		return modeNames;
	}

	@Override
	public ATN getATN() {
		return _ATN;
	}

	public static final String _serializedATN = "\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\rf\b\1\4\2\t\2\4"
			+ "\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"
			+ "\13\4\f\t\f\4\r\t\r\4\16\t\16\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\5\3\6"
			+ "\3\6\3\6\3\7\5\7+\n\7\3\7\6\7.\n\7\r\7\16\7/\3\b\5\b\63\n\b\3\b\7\b\66"
			+ "\n\b\f\b\16\b9\13\b\3\b\3\b\6\b=\n\b\r\b\16\b>\3\t\3\t\7\tC\n\t\f\t\16"
			+ "\tF\13\t\3\n\3\n\7\nJ\n\n\f\n\16\nM\13\n\3\n\3\n\3\13\6\13R\n\13\r\13"
			+ "\16\13S\3\13\3\13\3\f\3\f\7\fZ\n\f\f\f\16\f]\13\f\3\f\3\f\3\r\3\r\3\16"
			+ "\3\16\5\16e\n\16\2\2\17\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23\13\25\f"
			+ "\27\r\31\2\33\2\3\2\b\3\2\62;\4\2\f\f\17\17\5\2\13\f\17\17\"\"\4\2\f\f"
			+ "$$\13\2##%&((,-//\61\61>\\aac|\4\2\60\60\62;\2m\2\3\3\2\2\2\2\5\3\2\2"
			+ "\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21"
			+ "\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\3\35\3\2\2\2\5\37\3\2"
			+ "\2\2\7!\3\2\2\2\t#\3\2\2\2\13&\3\2\2\2\r*\3\2\2\2\17\62\3\2\2\2\21@\3"
			+ "\2\2\2\23G\3\2\2\2\25Q\3\2\2\2\27W\3\2\2\2\31`\3\2\2\2\33d\3\2\2\2\35"
			+ "\36\7*\2\2\36\4\3\2\2\2\37 \7+\2\2 \6\3\2\2\2!\"\7<\2\2\"\b\3\2\2\2#$"
			+ "\7%\2\2$%\7v\2\2%\n\3\2\2\2&\'\7%\2\2\'(\7h\2\2(\f\3\2\2\2)+\7/\2\2*)"
			+ "\3\2\2\2*+\3\2\2\2+-\3\2\2\2,.\t\2\2\2-,\3\2\2\2./\3\2\2\2/-\3\2\2\2/"
			+ "\60\3\2\2\2\60\16\3\2\2\2\61\63\7/\2\2\62\61\3\2\2\2\62\63\3\2\2\2\63"
			+ "\67\3\2\2\2\64\66\t\2\2\2\65\64\3\2\2\2\669\3\2\2\2\67\65\3\2\2\2\678"
			+ "\3\2\2\28:\3\2\2\29\67\3\2\2\2:<\7\60\2\2;=\t\2\2\2<;\3\2\2\2=>\3\2\2"
			+ "\2><\3\2\2\2>?\3\2\2\2?\20\3\2\2\2@D\5\31\r\2AC\5\33\16\2BA\3\2\2\2CF"
			+ "\3\2\2\2DB\3\2\2\2DE\3\2\2\2E\22\3\2\2\2FD\3\2\2\2GK\7=\2\2HJ\n\3\2\2"
			+ "IH\3\2\2\2JM\3\2\2\2KI\3\2\2\2KL\3\2\2\2LN\3\2\2\2MK\3\2\2\2NO\b\n\2\2"
			+ "O\24\3\2\2\2PR\t\4\2\2QP\3\2\2\2RS\3\2\2\2SQ\3\2\2\2ST\3\2\2\2TU\3\2\2"
			+ "\2UV\b\13\2\2V\26\3\2\2\2W[\7$\2\2XZ\n\5\2\2YX\3\2\2\2Z]\3\2\2\2[Y\3\2"
			+ "\2\2[\\\3\2\2\2\\^\3\2\2\2][\3\2\2\2^_\7$\2\2_\30\3\2\2\2`a\t\6\2\2a\32"
			+ "\3\2\2\2be\5\31\r\2ce\t\7\2\2db\3\2\2\2dc\3\2\2\2e\34\3\2\2\2\r\2*/\62" + "\67>DKS[d\3\b\2\2";
	public static final ATN _ATN = new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}