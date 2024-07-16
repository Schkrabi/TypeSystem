package velka.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Iterator;
import java.util.List;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.langbase.OperatorBank;
import velka.types.Type;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;

/**
 * Class generating files and project structure for runnable and compilable
 * clojure code
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ClojureCodeGenerator {

	/**
	 * Name of default namespace if no namespace was declared by user
	 */
	public static final String DEFAULT_NAMESPACE = "velka.clojure.user";
	public static final Path DEFAULT_FILENAME = Paths.get("user.clj");
	public static final Path DEFAULT_FILE_PROJECT_PATH = Paths.get("velka", "clojure").resolve(DEFAULT_FILENAME);
	public static final Path CLASSES_PATH = Paths.get("classes");

	/**
	 * Generates code for clj file from list of expressions with default namespace
	 * 
	 * @param exprs     compiled expressions
	 * @param env       environment
	 * @param typeEnv   typeEnvironment
	 * @return clojure code
	 * @throws Exception
	 */
	public static String ExpressionListToClojureCode(List<Expression> exprs, Environment env)
			throws Exception {
		return ExpressionListToClojureCode(DEFAULT_NAMESPACE, exprs, env);
	}

	/**
	 * Generates code for clj file from list of expressions
	 * 
	 * @param namespace used namespace
	 * @param exprs     compiled expressions
	 * @param env       environment
	 * @param typeEnv   typeEnvironment
	 * @return clojure code
	 * @throws Exception
	 */
	public static String ExpressionListToClojureCode(String namespace, List<Expression> exprs, Environment env) throws Exception {
		StringBuilder sb = new StringBuilder();

		sb.append(ClojureHelper.declareNamespace(namespace));
		sb.append(ClojureHelper.requireNamespace(ClojureCoreSymbols.NAMESPACE));
		
		for(OperatorBank ob : OperatorBank.operatorBanks) {
			sb.append(ClojureHelper.requireNamespace(ob.getNamespace()));
		}
		
		sb.append(VelkaClojureCore.cmdArgs);

		Iterator<Expression> i = exprs.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			sb.append(e.toClojureCode(env));
			if (i.hasNext()) {
				sb.append('\n');
			}
		}
		return sb.toString();
	}

	/**
	 * Generates code to clojure code file
	 * 
	 * @param dest      path to file where code will be written
	 * @param namespace used namespace
	 * @param exprs     compiled expressions
	 * @param env       environment
	 * @param typeEnv   typeEnvironment
	 * @return dest
	 * @throws Exception
	 */
	public static Path ExpressionListToCljFile(Path dest, String namespace, List<Expression> exprs, Environment env) throws Exception {
		return Files.writeString(dest,
				ClojureCodeGenerator.ExpressionListToClojureCode(namespace, exprs, env));
	}

	/**
	 * Generates code to clojure code file with default namespace
	 * 
	 * @param dest      path to file where code will be written
	 * @param exprs     compiled expressions
	 * @param env       environment
	 * @param typeEnv   typeEnvironment
	 * @return dest
	 * @throws Exception
	 */
	public static Path ExpressionListToCljFile(Path dir, List<Expression> exprs, Environment env) throws Exception {
		return ExpressionListToCljFile(dir.resolve(DEFAULT_FILENAME), DEFAULT_NAMESPACE, exprs, env);
	}

	/**
	 * Generates .clj code file for given operator bank and directory
	 * @param operatorBank operator bank instance
	 * @param directory directory to place the code into
	 * @throws IOException if write goes awry
	 */
	private static void generateOperatorBank(OperatorBank operatorBank, Path directory) throws IOException {
		Files.createDirectories(directory.resolve(operatorBank.getPath()));
		operatorBank.generateFile(directory);
	}
	
	public static Path generateClojureProject(Path directory) throws IOException {
		ClojureCodeGenerator.createDepsEdn(directory);
		
		Files.createDirectories(directory.resolve(VelkaClojureDev.VELKA_CLOJURE_DEV_PATH));
		Path velkaClojureDev = directory.resolve(VelkaClojureDev.VELKA_CLOJURE_DEV_PATH).resolve(VelkaClojureDev.VELKA_CLOJURE_DEV_NAME);
		VelkaClojureDev.generateFile(velkaClojureDev);
		
		Files.createDirectories(directory.resolve(VelkaClojureCore.VELKA_CLOJURE_CORE_PATH));
		Path velkaClojureCore = directory.resolve(VelkaClojureCore.VELKA_CLOJURE_CORE_PATH).resolve(VelkaClojureCore.VELKA_CLOJURE_CORE_NAME);
		VelkaClojureCore.generateFile(velkaClojureCore);			
		
		for(OperatorBank bank : OperatorBank.operatorBanks) {
			generateOperatorBank(bank, directory);
		}		
		
		Files.createDirectories(directory.resolve(ClojureCodeGenerator.CLASSES_PATH));
		
		return directory;
	}

	public static String writeHeaders(Environment env) {
		StringBuilder sb = new StringBuilder();
		sb.append("(require '[clojure.string])\n");
		sb.append("(ns " + ClojureCoreSymbols.NAMESPACE + " (:gen-class))\n");
		sb.append(VelkaClojureCore.writeDefinitions());
		sb.append("\n\n");
		return sb.toString();
	}

	/**
	 * Writes footers
	 * 
	 * @remark creates main entry point for clojure applictation calling function
	 *         main with no arguments
	 * @return string with code
	 * @throws AppendableException 
	 */
	public static String writeMain() throws AppendableException {
		final String args = "_args";
		return ClojureHelper.clojureDefnHelper("-main", List.of(ClojureHelper.varargs(args)), 
				ClojureHelper.applyVelkaFunction("main", 
								ClojureHelper.tupleHelper_str( //Converts the clj &args into a velka tuple
										ClojureHelper.applyClojureFunction("vec", args))));				
	}
	
	public static Path createDepsEdn(Path directory) throws IOException {
		StringBuilder sb = new StringBuilder();
		sb.append("{\n");
		
		sb.append(":aliases {\n")
		.append(":run {\n")
		.append(":main-opts [\"-m\" \"velka.clojure.user\"]\n")
		.append("}\n")
		.append("}\n");
		
		sb.append(":paths\n");
		sb.append("[\n");
		
		//Default clojure dependencies
		sb.append("\".\"\n");
		sb.append("\"./" + VelkaClojureCore.RELATIVE_PATH.toString().replace('\\', '/') + "\"\n");
		
		for(OperatorBank ob : OperatorBank.operatorBanks) {
			sb.append("\"./" + ob.getRelative().toString().replace('\\', '/') + "\"\n");
		}
		
		//Main source file
		sb.append("\"./" + ClojureCodeGenerator.DEFAULT_FILE_PROJECT_PATH.toString().replace('\\', '/') + "\"\n");
		//Velka libs
		sb.append("\"./velka.util.jar\"\n");
		sb.append("\"./velka.types.jar\"\n");
		//Path for ahead of time clojure compilation
		sb.append("\"classes\"");
		
		sb.append("]\n");
		sb.append("}\n");
		
		Path depsEdn = Files.createFile(directory.resolve("deps.edn"));
		Files.writeString(depsEdn, sb.toString(), StandardOpenOption.APPEND);
		return depsEdn;
	}
}
