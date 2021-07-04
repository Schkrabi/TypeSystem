package velka.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Iterator;
import java.util.List;

import velka.clojure.langbase.VelkaClojureArrayList;
import velka.clojure.langbase.VelkaClojureConstructors;
import velka.clojure.langbase.VelkaClojureConversions;
import velka.clojure.langbase.VelkaClojureLinkedList;
import velka.clojure.langbase.VelkaClojureList;
import velka.clojure.langbase.VelkaClojureOperators;
import velka.core.abstraction.ConstructorOperators;
import velka.core.abstraction.Operators;
import velka.core.conversions.Conversions;
import velka.core.expression.Expression;
import velka.core.interpretation.ClojureCoreSymbols;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;

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
	public static String ExpressionListToClojureCode(List<Expression> exprs, Environment env, TypeEnvironment typeEnv)
			throws Exception {
		return ExpressionListToClojureCode(DEFAULT_NAMESPACE, exprs, env, typeEnv);
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
	public static String ExpressionListToClojureCode(String namespace, List<Expression> exprs, Environment env,
			TypeEnvironment typeEnv) throws Exception {
		StringBuilder sb = new StringBuilder();

		sb.append(ClojureHelper.declareNamespace(namespace));
		sb.append(ClojureHelper.requireNamespace(ClojureCoreSymbols.NAMESPACE));
		sb.append(ClojureHelper.requireNamespace(Operators.NAMESPACE));
		sb.append(ClojureHelper.requireNamespace(ListNative.NAMESPACE));
		sb.append(ClojureHelper.requireNamespace(ConstructorOperators.NAMESPACE));
		sb.append(ClojureHelper.requireNamespace(Conversions.NAMESPACE));
		sb.append(ClojureHelper.requireNamespace(JavaArrayList.NAMESPACE));
		sb.append(ClojureHelper.requireNamespace(JavaLinkedList.NAMESPACE));

		Iterator<Expression> i = exprs.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			sb.append(e.toClojureCode(env, typeEnv));
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
	public static Path ExpressionListToCljFile(Path dest, String namespace, List<Expression> exprs, Environment env,
			TypeEnvironment typeEnv) throws Exception {
		return Files.writeString(dest,
				ClojureCodeGenerator.ExpressionListToClojureCode(namespace, exprs, env, typeEnv));
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
	public static Path ExpressionListToCljFile(Path dir, List<Expression> exprs, Environment env,
			TypeEnvironment typeEnv) throws Exception {
		return ExpressionListToCljFile(dir.resolve(DEFAULT_FILENAME), DEFAULT_NAMESPACE, exprs, env, typeEnv);
	}

	
	public static Path generateClojureProject(Path directory) throws IOException {
		ClojureCodeGenerator.createDepsEdn(directory);
		
		Files.createDirectories(directory.resolve(VelkaClojureCore.VELKA_CLOJURE_CORE_PATH));
		Path velkaClojureCore = directory.resolve(VelkaClojureCore.VELKA_CLOJURE_CORE_PATH).resolve(VelkaClojureCore.VELKA_CLOJURE_CORE_NAME);
		VelkaClojureCore.generateFile(velkaClojureCore);
		
		Files.createDirectories(directory.resolve(VelkaClojureOperators.VELKA_CLOJURE_OPERATORS_PATH));
		Path velkaClojureOperators = directory.resolve(VelkaClojureOperators.VELKA_CLOJURE_OPERATORS_PATH).resolve(VelkaClojureOperators.VELKA_CLOJURE_OPERAOTRS_NAME);
		VelkaClojureOperators.generateFile(velkaClojureOperators);		
		
		Files.createDirectories(directory.resolve(VelkaClojureList.VELKA_CLOJURE_LIST_PATH));
		Path velkaClojureList = directory.resolve(VelkaClojureList.VELKA_CLOJURE_LIST_PATH).resolve(VelkaClojureList.VELKA_CLOJURE_LIST_NAME);
		VelkaClojureList.generateFile(velkaClojureList);
		
		Files.createDirectories(directory.resolve(VelkaClojureConstructors.VELKA_CLOJURE_CONSTRUCTORS_PATH));
		Path velkaClojureConstructors = directory.resolve(VelkaClojureConstructors.RELATIVE_PATH);
		VelkaClojureConstructors.generateFile(velkaClojureConstructors);
		
		Files.createDirectories(directory.resolve(VelkaClojureConversions.VELKA_CLOJURE_CONVERSIONS_PATH));
		Path velkaClojureConversions = directory.resolve(VelkaClojureConversions.RELATIVE_PATH);
		VelkaClojureConversions.generateFile(velkaClojureConversions);
		
		Files.createDirectories(directory.resolve(VelkaClojureArrayList.VELKA_CLOJURE_ARRAYLIST_PATH));
		Path velkaClojurArrayList = directory.resolve(VelkaClojureArrayList.RELATIVE_PATH);
		VelkaClojureArrayList.generateFile(velkaClojurArrayList);
		
		Files.createDirectories(directory.resolve(VelkaClojureLinkedList.VELKA_CLOJURE_LINKEDLIST_PATH));
		Path velkaClojureLinkedList = directory.resolve(VelkaClojureLinkedList.RELATIVE_PATH);
		VelkaClojureLinkedList.generateFile(velkaClojureLinkedList);
		
		Files.createDirectories(directory.resolve(ClojureCodeGenerator.CLASSES_PATH));
		
		return directory;
	}

	public static String writeHeaders(Environment env, TypeEnvironment typeEnv) {
		StringBuilder sb = new StringBuilder();
		sb.append("(require '[clojure.string])\n");
		sb.append("(ns " + ClojureCoreSymbols.NAMESPACE + " (:gen-class))\n");
		sb.append(VelkaClojureCore.writeDefinitions());
		sb.append("\n");
		//sb.append(ListNative.makeClojureCode(env, typeEnv));
		sb.append("\n\n");
		return sb.toString();
	}

	/**
	 * Writes footers
	 * 
	 * @remark creates main entry point for clojure applictation calling function
	 *         main with no arguments
	 * @return string with code
	 */
	public static String writeMain() {
		// TODO add support of command line arguments!
		return "(defn -main\n" + "  []\n"
				+ "(" + ClojureCoreSymbols.eapplyClojureSymbol_full + " main (with-meta [] {:lang-type (velka.lang.types.TypeTuple/EMPTY_TUPLE)})))";
	}
	
	public static Path createDepsEdn(Path directory) throws IOException {
		StringBuilder sb = new StringBuilder();
		sb.append("{\n");
		sb.append(":paths\n");
		sb.append("[\n");
		
		//Default clojure dependencies
		sb.append("\".\"\n");
		sb.append("\"./" + VelkaClojureCore.RELATIVE_PATH.toString() + "\"\n");
		sb.append("\"./" + VelkaClojureOperators.RELATIVE_PATH.toString() + "\"\n");
		sb.append("\"./" + VelkaClojureList.RELATIVE_PATH.toString() + "\"\n");
		sb.append("\"./" + VelkaClojureConversions.RELATIVE_PATH.toString() + "\"\n");
		sb.append("\"./" + VelkaClojureArrayList.RELATIVE_PATH.toString() + "\"\n");
		sb.append("\"./" + VelkaClojureLinkedList.RELATIVE_PATH.toString() + "\"\n");
		//Main source file
		sb.append("\"./" + ClojureCodeGenerator.DEFAULT_FILE_PROJECT_PATH.toString() + "\"\n");
		//Velka libs
		sb.append("\"./velka.lang.util.jar\"\n");
		sb.append("\"./velka.lang.types.jar\"\n");
		//Path for ahead of time clojure compilation
		sb.append("\"classes\"");
		
		sb.append("]\n");
		sb.append("}\n");
		
		Path depsEdn = Files.createFile(directory.resolve("deps.edn"));
		Files.writeString(depsEdn, sb.toString(), StandardOpenOption.APPEND);
		return depsEdn;
	}
}
