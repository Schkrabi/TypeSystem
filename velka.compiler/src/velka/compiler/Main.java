package velka.compiler;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
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
import velka.util.ClojureCoreSymbols;

/**
 * Main entry point for testing
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Main {
	
	public static final String COMPILE = "compile";
	public static final String BUILD = "build";
	public static final String INTERPRET = "interpret";
	public static final String REPL = "repl";
	public static final String PREPARE = "prepare";
	public static final String HELP = "help";
	public static final String DOCUMENTATION = "documentation";
	
	public static final String USAGE = 
			"Usage:\n" 
			+ "    java -jar velka.clojure.compiler.jar OPTION args\n"
			+ "    Options:\n"
			+ "        " + COMPILE + " <file> - compiles file into clojure code\n"
			+ "        " + PREPARE + " - prepares current folder for clojure project\n"
			+ "        " + BUILD + " <file> - prepares current folder for clojure project and compiles code to clojure\n"
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
			
				case COMPILE:{
						var fileArg = Path.of(args[1]);
						var topLevel = TopLevelEnvironment.instantiate();
						var fld = Path.of(System.getProperty("user.dir"));
						Compiler.clojureCompile(fileArg, fld, topLevel);
					}
					break;
				case PREPARE:{
						var fld = Path.of(System.getProperty("user.dir"));
						ClojureCodeGenerator.generateClojureProject(fld);
					}
					break;
				case BUILD:{
						var topLevel = TopLevelEnvironment.instantiate();
						var fld = Path.of(System.getProperty("user.dir"));
						var fileArg = Path.of(args[1]);
						ClojureCodeGenerator.generateClojureProject(fld);
						Compiler.clojureCompile(fileArg, fld.resolve(ClojureCodeGenerator.DEFAULT_FILE_PROJECT_PATH), topLevel);
					}
					break;
				case INTERPRET:{
						var inStream = Files.newInputStream(Path.of(args[0]));
						var topLevel = TopLevelEnvironment.instantiate();
						var l = new ArrayList<Expression>(args.length - 1);
						for(int i = 1; i < args.length; i++) {
							l.add(new LitString(args[i]));
						}
						topLevel.put(new Symbol(ClojureCoreSymbols.CONSOLE_ARGS_SYMBOL), new Tuple(l));
						
						Compiler.interpret(inStream, topLevel);
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
		catch(Exception e) {
			throw new RuntimeException(e);
		}
		
		return;
	}
}
