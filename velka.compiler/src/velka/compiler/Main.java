package velka.compiler;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.stream.Collectors;

import velka.clojure.ClojureCodeGenerator;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.ConstructorOperators;
import velka.core.langbase.ConversionOperators;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaBitSet;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.core.langbase.OperatorBank;
import velka.core.langbase.Operators;
import velka.core.langbase.Scanner;

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

	private static void runMode(String mode, Path currentFolder) throws Exception {
		runMode(mode, currentFolder, "");
	}
	
	private static void runMode(String mode, Path currentFolder, String fileArg) throws Exception {
		Environment topLevel = Environment.initTopLevelEnvironment();
		TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(topLevel);
		Compiler.init(topLevel, typeEnv);
		switch(mode) {
		
		case COMPILE:
			Compiler.clojureCompile(Paths.get(fileArg), currentFolder.resolve(ClojureCodeGenerator.DEFAULT_FILENAME), topLevel, typeEnv);
			break;
		case PREPARE:
			ClojureCodeGenerator.generateClojureProject(currentFolder);
			break;
		case BUILD:
			ClojureCodeGenerator.generateClojureProject(currentFolder);
			Compiler.clojureCompile(Paths.get(fileArg), currentFolder.resolve(ClojureCodeGenerator.DEFAULT_FILE_PROJECT_PATH), topLevel, typeEnv);
			break;
		case INTERPRET:
			InputStream inStream = Files.newInputStream(Paths.get(fileArg));
			Compiler.interpret(inStream, topLevel, typeEnv);
			break;
		case REPL:
			Compiler.repl(System.in, System.out, topLevel, typeEnv, true);
			break;
		case DOCUMENTATION:
			LangbaseDocumentationGenerator.spitDocStatic(
					OperatorBank.operatorBanks.stream().map(o -> o.getClass()).collect(Collectors.toList()),
					Paths.get(fileArg));
			break;
		case HELP:
		default:
			printHelp();
			break;
		}
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
		if(args.length < 2) {
			try {
				Main.runMode(args[0], Paths.get(System.getProperty("user.dir")));
				return;
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		try {
			Main.runMode(args[0], Paths.get(System.getProperty("user.dir")), args[1]);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return;
	}
}
