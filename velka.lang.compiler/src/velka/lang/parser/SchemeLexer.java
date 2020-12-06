// Generated from Scheme.g by ANTLR 4.7.2
package velka.lang.parser;

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
		T__0=1, T__1=2, T__2=3, TRUE=4, FALSE=5, INT=6, FLOAT=7, SYMBOL=8, COMMENT=9, 
		WS=10, STRING=11, ARROW=12;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	private static String[] makeRuleNames() {
		return new String[] {
			"T__0", "T__1", "T__2", "TRUE", "FALSE", "INT", "FLOAT", "SYMBOL", "COMMENT", 
			"WS", "STRING", "ARROW", "SYMBOL_HEAD", "SYMBOL_REST"
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\16k\b\1\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3"+
		"\5\3\5\3\6\3\6\3\6\3\7\5\7-\n\7\3\7\6\7\60\n\7\r\7\16\7\61\3\b\5\b\65"+
		"\n\b\3\b\7\b8\n\b\f\b\16\b;\13\b\3\b\3\b\6\b?\n\b\r\b\16\b@\3\t\3\t\7"+
		"\tE\n\t\f\t\16\tH\13\t\3\n\3\n\7\nL\n\n\f\n\16\nO\13\n\3\n\3\n\3\13\6"+
		"\13T\n\13\r\13\16\13U\3\13\3\13\3\f\3\f\7\f\\\n\f\f\f\16\f_\13\f\3\f\3"+
		"\f\3\r\3\r\3\r\3\16\3\16\3\17\3\17\5\17j\n\17\2\2\20\3\3\5\4\7\5\t\6\13"+
		"\7\r\b\17\t\21\n\23\13\25\f\27\r\31\16\33\2\35\2\3\2\b\3\2\62;\4\2\f\f"+
		"\17\17\5\2\13\f\17\17\"\"\4\2\f\f$$\n\2##&&,-//\61\61>\\aac|\4\2\60\60"+
		"\62;\2r\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2"+
		"\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3"+
		"\2\2\2\2\31\3\2\2\2\3\37\3\2\2\2\5!\3\2\2\2\7#\3\2\2\2\t%\3\2\2\2\13("+
		"\3\2\2\2\r,\3\2\2\2\17\64\3\2\2\2\21B\3\2\2\2\23I\3\2\2\2\25S\3\2\2\2"+
		"\27Y\3\2\2\2\31b\3\2\2\2\33e\3\2\2\2\35i\3\2\2\2\37 \7*\2\2 \4\3\2\2\2"+
		"!\"\7+\2\2\"\6\3\2\2\2#$\7<\2\2$\b\3\2\2\2%&\7%\2\2&\'\7v\2\2\'\n\3\2"+
		"\2\2()\7%\2\2)*\7h\2\2*\f\3\2\2\2+-\7/\2\2,+\3\2\2\2,-\3\2\2\2-/\3\2\2"+
		"\2.\60\t\2\2\2/.\3\2\2\2\60\61\3\2\2\2\61/\3\2\2\2\61\62\3\2\2\2\62\16"+
		"\3\2\2\2\63\65\7/\2\2\64\63\3\2\2\2\64\65\3\2\2\2\659\3\2\2\2\668\t\2"+
		"\2\2\67\66\3\2\2\28;\3\2\2\29\67\3\2\2\29:\3\2\2\2:<\3\2\2\2;9\3\2\2\2"+
		"<>\7\60\2\2=?\t\2\2\2>=\3\2\2\2?@\3\2\2\2@>\3\2\2\2@A\3\2\2\2A\20\3\2"+
		"\2\2BF\5\33\16\2CE\5\35\17\2DC\3\2\2\2EH\3\2\2\2FD\3\2\2\2FG\3\2\2\2G"+
		"\22\3\2\2\2HF\3\2\2\2IM\7=\2\2JL\n\3\2\2KJ\3\2\2\2LO\3\2\2\2MK\3\2\2\2"+
		"MN\3\2\2\2NP\3\2\2\2OM\3\2\2\2PQ\b\n\2\2Q\24\3\2\2\2RT\t\4\2\2SR\3\2\2"+
		"\2TU\3\2\2\2US\3\2\2\2UV\3\2\2\2VW\3\2\2\2WX\b\13\2\2X\26\3\2\2\2Y]\7"+
		"$\2\2Z\\\n\5\2\2[Z\3\2\2\2\\_\3\2\2\2][\3\2\2\2]^\3\2\2\2^`\3\2\2\2_]"+
		"\3\2\2\2`a\7$\2\2a\30\3\2\2\2bc\7%\2\2cd\7@\2\2d\32\3\2\2\2ef\t\6\2\2"+
		"f\34\3\2\2\2gj\5\33\16\2hj\t\7\2\2ig\3\2\2\2ih\3\2\2\2j\36\3\2\2\2\r\2"+
		",\61\649@FMU]i\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}