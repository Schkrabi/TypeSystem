package velka.parser;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.TokenStream;

import velka.core.expression.Expression;
import velka.parser.antlr.VelkaLexer;
import velka.parser.antlr.VelkaParser;
import velka.parser.antlr.SemanticNode;
import velka.parser.antlr.VelkaParser.ExprsContext;
import velka.parser.semantic.SemanticParser;
import velka.util.AppendableException;

/**
 * Main entrypoint for velka.parser module
 * @author Mgr. Radomir Skrabal
 *
 */
public class Parser {

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
		ExprsContext exprsContext = parseCharStream(CharStreams.fromStream(in));		
		return tokenParse(exprsContext);
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
		ExprsContext exprsContext = parseCharStream(CharStreams.fromString(in));
		return tokenParse(exprsContext);
	}
	
	/**
	 * First parsing phase, parses CharStream into stream of tokens
	 * 
	 * @param charStream
	 * @return Stream of tokens
	 * @throws RecognitionException
	 * @throws AppendableException
	 */
	private static ExprsContext parseCharStream(CharStream charStream) throws RecognitionException, AppendableException {
		TokenStream tokens = new CommonTokenStream(new VelkaLexer(charStream));
		VelkaParser parser = new VelkaParser(tokens);
		return parser.exprs();
	}
	
	/**
	 * Second parsing phase parses stream of tokens into list of expressions
	 * 
	 * @param exprsContext
	 * @return
	 * @throws AppendableException
	 */
	private static List<Expression> tokenParse(ExprsContext exprsContext) throws AppendableException{
		List<Expression> exprs = new ArrayList<Expression>();
		
		for (SemanticNode s : exprsContext.val) {
			exprs.add(SemanticParser.parseNode(s));
		}
	
		return exprs;
	}
}
