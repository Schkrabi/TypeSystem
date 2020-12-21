package velka.lang.compiler;

import velka.lang.interpretation.Environment;

import java.io.Reader;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;

import velka.lang.parser.SchemeLexer;
import velka.lang.parser.SchemeParser;
import velka.lang.parser.SchemeParser.ExprsContext;
import velka.lang.parser.SemanticNode;
import velka.lang.semantic.SemanticParser;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.langbase.ListNative;
import velka.lang.util.AppendableException;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.expression.Expression;

/**
 * Main entry point for testing
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Main {

	/**
	 * Main entrypoint
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		try {
			Environment topLevel = Environment.initTopLevelEnvitonment();
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(topLevel);
			
			Main.init(topLevel, typeEnv);
			// Very basic options, didn't wanted to add more external libraries etc...
			if (args.length == 0) {
				// Interactive parser mode
				Main.interpretLoop(topLevel, typeEnv);
			} else if (args.length == 1) {
				// Load code and interpret further
				Main.interpretFile(Paths.get(args[0]), topLevel, typeEnv);
			} else if (args.length == 2) {
				// Compiler mode
				Main.compile(Paths.get(args[0]), Paths.get(args[1]), topLevel, typeEnv);
			} else {
				System.out.println("Wrong number of arguments specified.");
				return;
			}
		} catch (Exception e) {
			e.printStackTrace();
			return;
		}
	}

	private static void initTopLevelEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		ListNative.initializeInEnvironment(env, typeEnv);
	}

	private static void initTypesConversions(Environment env) throws Exception {
		TypeEnvironment.initBasicTypes(env);
	}

	private static void init(Environment env, TypeEnvironment typeEnv) throws Exception {
		Main.initTopLevelEnvironment(env, typeEnv);
		Main.initTypesConversions(env);
	}

	private static void interpretLoop(Environment topLevel, TypeEnvironment typeEnv) throws Exception {
		Scanner input = new Scanner(System.in);

		try {
			while (true) {
				System.out.print(">");
				CharStream charStream = CharStreams.fromString(input.nextLine());
				TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
				SchemeParser parser = new SchemeParser(tokens);

				ExprsContext exprsContext = parser.exprs();
				List<Expression> exprs = new ArrayList<Expression>();

				for (SemanticNode s : exprsContext.val) {
					exprs.add(SemanticParser.parseNode(s));
				}

				for (Expression e : exprs) {
					e.infer(topLevel, typeEnv);
					System.out.println(e.interpret(topLevel, typeEnv));
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			input.close();
		}
	}

	private static void compile(Path inputPath, Path outputPath, Environment topLevel, TypeEnvironment typeEnv) throws Exception {
		Reader input = null;
		Writer output = null;

		try {
			input = Files.newBufferedReader(inputPath);

			CharStream charStream = CharStreams.fromReader(input);
			TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
			SchemeParser parser = new SchemeParser(tokens);
			ExprsContext exprsContext = parser.exprs();

			List<Expression> l = new LinkedList<Expression>();
			List<Expression> exprs = new ArrayList<Expression>();

			for (SemanticNode s : exprsContext.val) {
				exprs.add(SemanticParser.parseNode(s));
			}

			for (Expression e : exprs) {
				e.infer(topLevel, typeEnv);
				l.add(e);
			}

			output = Files.newBufferedWriter(outputPath, Charset.defaultCharset());
			ClojureCodeGenerator.toClojureCode(l, output, topLevel, typeEnv);
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

	private static void interpretFile(Path inputPath, Environment topLevel, TypeEnvironment typeEnv) throws Exception {
		Reader input = null;
		Scanner inputI = null;

		try {
			input = Files.newBufferedReader(inputPath);

			CharStream charStream = CharStreams.fromReader(input);
			TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
			SchemeParser parser = new SchemeParser(tokens);
			ExprsContext exprsContext = parser.exprs();

			List<Expression> exprs = new ArrayList<Expression>();

			for (SemanticNode s : exprsContext.val) {
				exprs.add(SemanticParser.parseNode(s));
			}

			for (Expression e : exprs) {
				e.infer(topLevel, typeEnv);
				e.interpret(topLevel, typeEnv);
			}

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (input != null) {
				input.close();
			}
			if (inputI != null) {
				inputI.close();
			}
		}
	}
}
