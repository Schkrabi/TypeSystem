// Generated from Scheme.g by ANTLR 4.7
package parser;
import expression.*;
import types.TypeConcrete;
import types.TypeRepresentation;
import java.util.Map;
import java.util.HashMap;

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
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, IF=7, LAMBDA=8, TRUE=9, 
		FALSE=10, INT=11, FLOAT=12, SYMBOL=13, COMMENT=14, WS=15, STRING=16;
	public static String[] channelNames = {
		"DEFAULT_TOKEN_CHANNEL", "HIDDEN"
	};

	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "IF", "LAMBDA", "TRUE", 
		"FALSE", "INT", "FLOAT", "SYMBOL", "COMMENT", "WS", "STRING", "SYMBOL_HEAD", 
		"SYMBOL_REST"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'('", "')'", "'<'", "':'", "'>'", "'''"
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
		"\3\u608b\ua72a\u8133\ub9ed\u417c\u3be7\u7786\u5964\2\22\u0080\b\1\4\2"+
		"\t\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4"+
		"\13\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22"+
		"\t\22\4\23\t\23\3\2\3\2\3\3\3\3\3\4\3\4\3\5\3\5\3\6\3\6\3\7\3\7\3\b\3"+
		"\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\13\3\13\3\13\3\f\5\f"+
		"E\n\f\3\f\6\fH\n\f\r\f\16\fI\3\r\5\rM\n\r\3\r\7\rP\n\r\f\r\16\rS\13\r"+
		"\3\r\3\r\6\rW\n\r\r\r\16\rX\3\16\3\16\7\16]\n\16\f\16\16\16`\13\16\3\17"+
		"\3\17\7\17d\n\17\f\17\16\17g\13\17\3\17\3\17\3\20\6\20l\n\20\r\20\16\20"+
		"m\3\20\3\20\3\21\3\21\7\21t\n\21\f\21\16\21w\13\21\3\21\3\21\3\22\3\22"+
		"\3\23\3\23\5\23\177\n\23\2\2\24\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21\n\23"+
		"\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\2%\2\3\2\b\3\2\62;\4\2\f\f"+
		"\17\17\5\2\13\f\17\17\"\"\4\2\f\f$$\f\2##%&((,-//\61\61??A\\aac|\4\2\60"+
		"\60\62;\2\u0087\2\3\3\2\2\2\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3"+
		"\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2"+
		"\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3"+
		"\2\2\2\3\'\3\2\2\2\5)\3\2\2\2\7+\3\2\2\2\t-\3\2\2\2\13/\3\2\2\2\r\61\3"+
		"\2\2\2\17\63\3\2\2\2\21\66\3\2\2\2\23=\3\2\2\2\25@\3\2\2\2\27D\3\2\2\2"+
		"\31L\3\2\2\2\33Z\3\2\2\2\35a\3\2\2\2\37k\3\2\2\2!q\3\2\2\2#z\3\2\2\2%"+
		"~\3\2\2\2\'(\7*\2\2(\4\3\2\2\2)*\7+\2\2*\6\3\2\2\2+,\7>\2\2,\b\3\2\2\2"+
		"-.\7<\2\2.\n\3\2\2\2/\60\7@\2\2\60\f\3\2\2\2\61\62\7)\2\2\62\16\3\2\2"+
		"\2\63\64\7k\2\2\64\65\7h\2\2\65\20\3\2\2\2\66\67\7n\2\2\678\7c\2\289\7"+
		"o\2\29:\7d\2\2:;\7f\2\2;<\7c\2\2<\22\3\2\2\2=>\7%\2\2>?\7v\2\2?\24\3\2"+
		"\2\2@A\7%\2\2AB\7h\2\2B\26\3\2\2\2CE\7/\2\2DC\3\2\2\2DE\3\2\2\2EG\3\2"+
		"\2\2FH\t\2\2\2GF\3\2\2\2HI\3\2\2\2IG\3\2\2\2IJ\3\2\2\2J\30\3\2\2\2KM\7"+
		"/\2\2LK\3\2\2\2LM\3\2\2\2MQ\3\2\2\2NP\t\2\2\2ON\3\2\2\2PS\3\2\2\2QO\3"+
		"\2\2\2QR\3\2\2\2RT\3\2\2\2SQ\3\2\2\2TV\7\60\2\2UW\t\2\2\2VU\3\2\2\2WX"+
		"\3\2\2\2XV\3\2\2\2XY\3\2\2\2Y\32\3\2\2\2Z^\5#\22\2[]\5%\23\2\\[\3\2\2"+
		"\2]`\3\2\2\2^\\\3\2\2\2^_\3\2\2\2_\34\3\2\2\2`^\3\2\2\2ae\7=\2\2bd\n\3"+
		"\2\2cb\3\2\2\2dg\3\2\2\2ec\3\2\2\2ef\3\2\2\2fh\3\2\2\2ge\3\2\2\2hi\b\17"+
		"\2\2i\36\3\2\2\2jl\t\4\2\2kj\3\2\2\2lm\3\2\2\2mk\3\2\2\2mn\3\2\2\2no\3"+
		"\2\2\2op\b\20\2\2p \3\2\2\2qu\7$\2\2rt\n\5\2\2sr\3\2\2\2tw\3\2\2\2us\3"+
		"\2\2\2uv\3\2\2\2vx\3\2\2\2wu\3\2\2\2xy\7$\2\2y\"\3\2\2\2z{\t\6\2\2{$\3"+
		"\2\2\2|\177\5#\22\2}\177\t\7\2\2~|\3\2\2\2~}\3\2\2\2\177&\3\2\2\2\r\2"+
		"DILQX^emu~\3\b\2\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}