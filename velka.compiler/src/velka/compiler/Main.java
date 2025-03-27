package velka.compiler;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import velka.clojure.ClojureCodeGenerator;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TopLevelEnvironment;
import velka.core.langbase.OperatorBank;
import velka.core.literal.LitString;
import velka.java.CodeModelInstance;
import velka.java.generate.ClassGenerator;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;

/**
 * Main entry point for testing
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Main {
	
	public static final String COMPILE_CLJ = "compile_clj";
	public static final String BUILD_CLJ = "build_clj";
	public static final String BUILD_JAVA = "build_java";
	public static final String INTERPRET = "interpret";
	public static final String REPL = "repl";
	public static final String PREPARE = "prepare";
	public static final String HELP = "help";
	public static final String DOCUMENTATION = "documentation";
	
	public static final String USAGE = 
			"Usage:\n" 
			+ "    java -jar velka.clojure.compiler.jar OPTION args\n"
			+ "    Options:\n"
			+ "        " + COMPILE_CLJ + " <file> - compiles file into clojure code\n"
			+ "        " + PREPARE + " - prepares current folder for clojure project\n"
			+ "        " + BUILD_CLJ + " <file> - prepares current folder for clojure project and compiles code to clojure\n"
			+ "        " + BUILD_JAVA + " <file> <output dir> - builds a java project in given working directory"
			+ "        " + INTERPRET + " <file> - interprets file\n"
			+ "        " + REPL + " - runs repl\n"
			+ "        " + HELP + " - prints this help\n";
	
	private static void printHelp() {
		System.out.println(USAGE);
	}
	
	/**
	 * Main entrypoint
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		if(args.length < 1) {
			Main.printHelp();
			return;
		}
		
		try {
			switch(args[0].toLowerCase()) {
			
				case COMPILE_CLJ:{
						var fileArg = Path.of(args[1]);
						var topLevel = TopLevelEnvironment.instantiate();
						var fld = Path.of(System.getProperty("user.dir")).resolve("user.clj");
						Compiler.clojureCompile(fileArg, fld, topLevel);
					}
					break;
				case PREPARE:{
						var fld = Path.of(System.getProperty("user.dir"));
						ClojureCodeGenerator.generateClojureProject(fld);
					}
					break;
				case BUILD_CLJ:{
						var topLevel = TopLevelEnvironment.instantiate();
						var fld = Path.of(System.getProperty("user.dir"));
						var fileArg = Path.of(args[1]);
						ClojureCodeGenerator.generateClojureProject(fld);
						Compiler.clojureCompile(fileArg, fld.resolve(ClojureCodeGenerator.DEFAULT_FILE_PROJECT_PATH), topLevel);
					}
					break;
				case BUILD_JAVA:
					var file = Files.newInputStream(Path.of(args[1]));
					var workingDir = new File(args[2]);
					var generator = new ClassGenerator(CodeModelInstance.instance());
					generator.buildProject(file, workingDir);
					break;
				case INTERPRET:{
						var inStream = Files.newInputStream(Path.of(args[1]));
						var topLevel = TopLevelEnvironment.instantiate();
//						var l = new ArrayList<Expression>(args.length - 2);
//						for(int i = 2; i < args.length; i++) {
//							l.add(new LitString(args[i]));
//						}
//						topLevel.put(new Symbol(ClojureCoreSymbols.CONSOLE_ARGS_SYMBOL), new Tuple(l));
						
						Compiler.interpret(inStream, topLevel, Arrays.copyOfRange(args, 2, args.length));
					}
					break;
				case REPL:{
						var topLevel = TopLevelEnvironment.instantiate();
						var l = new ArrayList<Expression>(args.length - 1);
						for(int i = 1; i < args.length; i++) {
							l.add(new LitString(args[i]));
						}
						topLevel.put(new Symbol(ClojureCoreSymbols.CONSOLE_ARGS_SYMBOL), new Tuple(l));
						Compiler.repl(System.in, System.out, topLevel, true);
					}
					break;
				case DOCUMENTATION:{
						LangbaseDocumentationGenerator.spitDocStatic(
								OperatorBank.operatorBanks.stream().map(o -> o.getClass()).collect(Collectors.toList()),
								Path.of(args[0]));
					}
					break;
				case HELP:
				default:
					printHelp();
					break;
			}
		}
		catch(AppendableException | IOException e) {
			throw new RuntimeException(e);
		}
		
		return;
	}
}
