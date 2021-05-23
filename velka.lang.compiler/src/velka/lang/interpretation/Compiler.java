package velka.lang.interpretation;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.Reader;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.TokenStream;

import velka.lang.expression.Expression;
import velka.lang.langbase.ListNative;
import velka.lang.parser.SchemeLexer;
import velka.lang.parser.SchemeParser;
import velka.lang.parser.SchemeParser.ExprsContext;
import velka.lang.parser.SemanticNode;
import velka.lang.semantic.SemanticParser;
import velka.lang.util.AppendableException;
import velka.lang.util.ThrowingFunction;

public class Compiler {

	public static void initTopLevelEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		ListNative.initializeInEnvironment(env, typeEnv);
	}

	public static void initTypesConversions(Environment env) throws Exception {
		TypeEnvironment.initBasicTypes(env);
	}

	public static void init(Environment env, TypeEnvironment typeEnv) throws Exception {
		initTopLevelEnvironment(env, typeEnv);
		initTypesConversions(env);
	}

	/**
	 * Read function for the REPL
	 * 
	 * @param in input stream
	 * @return list of parsed expressions
	 * @throws IOException
	 * @throws RecognitionException
	 * @throws AppendableException
	 */
	public static List<Expression> read(InputStream in) throws IOException, RecognitionException, AppendableException {
		CharStream charStream = CharStreams.fromStream(in);
		TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
		SchemeParser parser = new SchemeParser(tokens);
	
		ExprsContext exprsContext = parser.exprs();
		List<Expression> exprs = new ArrayList<Expression>();
	
		for (SemanticNode s : exprsContext.val) {
			exprs.add(SemanticParser.parseNode(s));
		}
	
		return exprs;
	}

	/**
	 * Eval function for the REPL
	 * 
	 * @param in      input parsed expressions
	 * @param env     environment for evaluation
	 * @param typeEnv type environment for evaluation
	 * @return list of evaluated expressions
	 */
	public static List<Expression> eval(List<Expression> in, Environment env, TypeEnvironment typeEnv) {
		return in.stream().map(ThrowingFunction.wrapper(e -> {
			e.infer(env, typeEnv);
			return e.interpret(env, typeEnv);
		})).collect(Collectors.toList());
	}

	/**
	 * Char to be printed to output for repl
	 */
	public static final String PROMPT_LEADING_CHAR = ">";

	/**
	 * Compile function for clojure compilation. Compiles list of expression into
	 * clojure code
	 * 
	 * @param in      input list of expression
	 * @param env     used environment
	 * @param typeEnv type environment
	 * @return string with compiled clojure codes
	 * @throws Exception
	 */
	public static String compile(List<Expression> in, Environment env, TypeEnvironment typeEnv) throws Exception {
		return ClojureCodeGenerator.toClojureCode(in, env, typeEnv);
	}

	/**
	 * Print function for the repl
	 * 
	 * @param in list of expression to print to string
	 * @return string with printed expression
	 */
	public static String print(List<Expression> in) {
		StringBuilder sb = new StringBuilder();
		Iterator<Expression> i = in.iterator();
	
		while (i.hasNext()) {
			Expression e = i.next();
			sb.append(e.toString());
			if (i.hasNext()) {
				sb.append('\n');
			}
		}
	
		return sb.toString();
	}

	/**
	 * Repl function
	 * @param in input stream for repl, usually System.in (stdin)
	 * @param out output stream for repl, usually System.out (stdout)
	 * @param topLevel environment for repl
	 * @param typeEnv type environment for repl
	 * @param showPromptLeadingChar whether to print Main.PROMPT_LEADING_CHAR to output stream before reading line
	 * @throws Exception
	 */
	public static void repl(InputStream in, PrintStream out, Environment topLevel, TypeEnvironment typeEnv,
			boolean showPromptLeadingChar) throws Exception {
		Scanner input = new Scanner(in);
	
		try {
			while (true) {
				if (showPromptLeadingChar) {
					out.print(Compiler.PROMPT_LEADING_CHAR);
				}
	
				out.println(Compiler.print(eval(read(new ByteArrayInputStream(input.nextLine().getBytes())),
						topLevel, typeEnv)));
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			input.close();
		}
	}

	/**
	 * Compiles contents of in stream into a clojure code and prints it to out stream
	 * @param in input stream
	 * @param out output stream
	 * @param topLevel top level environment for compilation
	 * @param typeEnv type environment for compilation
	 * @throws Exception
	 */
	public static void clojure(InputStream in, PrintStream out, Environment topLevel, TypeEnvironment typeEnv)
			throws Exception {
		Reader input = null;
		Writer output = null;
	
		try {
			out.print(ClojureCodeGenerator.writeHeaders(topLevel, typeEnv));
			out.print(Compiler.compile(read(in), topLevel, typeEnv));
			out.print(ClojureCodeGenerator.writeFooters());
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (input != null) {
				input.close();
			}
			if (output != null) {
				output.close();
			}
		}
	}

	/**
	 * Interprets content of in stream and prints result into out stream
	 * @param in input stream with code
	 * @param out output stream
	 * @param topLevel environment for interpretation
	 * @param typeEnv type environment for interpretation
	 * @throws Exception 
	 */
	public static void interpret(InputStream in, Environment topLevel, TypeEnvironment typeEnv) throws Exception {
		Compiler.print(eval(read(in), topLevel, typeEnv));
	}

}
