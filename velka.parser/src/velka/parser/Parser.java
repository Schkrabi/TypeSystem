package velka.parser;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.List;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.TokenStream;

import velka.core.expression.Expression;
import velka.parser.antlr.VelkaLexer;
import velka.parser.antlr.VelkaParser;
import velka.util.AppendableException;

/**
 * Main entrypoint for velka.parser module
 * @author Mgr. Radomir Skrabal
 *
 */
public class Parser {

	/** Entrypoint for reader */
	public static List<Expression> read(Reader in) throws IOException, AppendableException {
		var exprsContext = parseCharStream(CharStreams.fromReader(in));
		return exprsContext.val;
	}
	
	/**
	 * Reads and parses input stream into List of expressions
	 * 
	 * @param in input stream
	 * @return list of parsed expressions
	 * @throws IOException
	 * @throws RecognitionException
	 * @throws AppendableException
	 */
	public static List<Expression> read(InputStream in) throws IOException, AppendableException {
		var exprsContext = parseCharStream(CharStreams.fromStream(in));		
		return exprsContext.val;
	}
	
	/**
	 * Reads and parses string into List of expressions
	 * 
	 * @param in
	 * @return list of parsed expressions
	 * @throws RecognitionException
	 * @throws AppendableException
	 */
	public static List<Expression> read(String in) throws RecognitionException, AppendableException {
		var exprsContext = parseCharStream(CharStreams.fromString(in));
		return exprsContext.val;
	}
	
	/**
	 * First parsing phase, parses CharStream into stream of tokens
	 * 
	 * @param charStream
	 * @return Stream of tokens
	 * @throws RecognitionException
	 * @throws AppendableException
	 */
	private static velka.parser.antlr.VelkaParser.ProgramContext parseCharStream(CharStream charStream) throws RecognitionException, AppendableException {
		TokenStream tokens = new CommonTokenStream(new VelkaLexer(charStream));
		VelkaParser parser = new VelkaParser(tokens);
		return parser.program();
	}
}
