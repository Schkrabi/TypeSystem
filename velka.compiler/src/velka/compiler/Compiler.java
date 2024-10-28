package velka.compiler;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.Reader;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import velka.clojure.ClojureCodeGenerator;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.parser.Parser;
import velka.util.AppendableException;

public class Compiler {

	/**
	 * Eval function for the REPL
	 * 
	 * @param in      input parsed expressions
	 * @param env     environment for evaluation
	 * @param typeEnv type environment for evaluation
	 * @return list of evaluated expressions
	 * @throws AppendableException 
	 */
	public static List<Expression> eval(List<Expression> in, Environment env) throws AppendableException {
		List<Expression> out = new LinkedList<Expression>();
		for(Expression e : in) {
			Expression intp = e.interpret(env);
			out.add(intp);
		}
		return out;
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
	public static String compile(List<Expression> in, Environment env) throws Exception {
		StringBuilder sb = new StringBuilder();
		sb.append(ClojureCodeGenerator.ExpressionListToClojureCode(in, env));
		sb.append(ClojureCodeGenerator.writeMain());
		return sb.toString();
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
	public static void repl(InputStream in, PrintStream out, Environment topLevel, boolean showPromptLeadingChar) {
		Scanner input = new Scanner(in);
	
		try {
			while (true) {
				if (showPromptLeadingChar) {
					out.print(Compiler.PROMPT_LEADING_CHAR);
				}
	
				out.println(Compiler.print(eval(Parser.read(new ByteArrayInputStream(input.nextLine().getBytes())),
						topLevel)));
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			input.close();
		}
	}
	
	public static void clojureCompile(Path in, Path out, Environment env) {
		try {
			Compiler.clojure(Files.newInputStream(in), new PrintStream(Files.newOutputStream(out)), env);
		} catch (AppendableException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (Exception e) {
			e.printStackTrace();
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
	public static void clojure(InputStream in, PrintStream out, Environment topLevel)
			throws Exception {
		Reader input = null;
		Writer output = null;
	
		try {
			out.print(Compiler.compile(Parser.read(in), topLevel));
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
	 * @throws IOException 
	 * @throws AppendableException 
	 * @throws Exception 
	 */
	public static void interpret(InputStream in, Environment topLevel) throws AppendableException, IOException {
		Compiler.print(eval(Parser.read(in), topLevel));
	}

}
