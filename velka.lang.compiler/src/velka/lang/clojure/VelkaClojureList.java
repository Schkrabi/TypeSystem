package velka.lang.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.lang.interpretation.ClojureHelper;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.langbase.ListNative;
import velka.lang.util.AppendableException;

/**
 * This class is generating velka.clojure.list namespace file
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class VelkaClojureList {

	/**
	 * Relative path to velka.clojure.list file
	 */
	public static final Path VELKA_CLOJURE_LIST_PATH = Paths.get("velka", "clojure");

	/**
	 * Name of the velka.clojure.list file
	 */
	public static final Path VELKA_CLOJURE_LIST_NAME = Paths.get("list.clj");
	
	/**
	 * Relative file path
	 */
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_LIST_PATH.resolve(VELKA_CLOJURE_LIST_NAME);

	/**
	 * Generates clojure code for definitions of velka.clojure.list namespace
	 * 
	 * @return string with code
	 */
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();

		// Require namespace
		sb.append(ClojureHelper.requireNamespace("clojure.string"));
		//sb.append(ClojureHelper.requireNamespace(VelkaClojureLinkedList.NAMESPACE));
		//sb.append(ClojureHelper.requireNamespace(VelkaClojureArrayList.NAMESPACE));

		// Declare namespace
		sb.append(ClojureHelper.declareNamespace(ListNative.NAMESPACE));

		// Declarations
		sb.append(ClojureHelper.makeDeclaration(ListNative.addToEndOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.foldlListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.foldrListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.headListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.isEmpty.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.map2ListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.mapListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.tailListNativeOperator.getClojureSymbol().name));
		
		sb.append(ClojureHelper.makeDeclaration(ListNative.constructorSymbol.name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.constructorEmptySymbol.name));
		
		sb.append(ClojureHelper.makeDeclaration(ListNative.ListNativeToLinkedListOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.ListNativeToArrayListOperator.getClojureSymbol().name));

		Environment env = Environment.initTopLevelEnvitonment();
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

			sb.append(ClojureHelper.makeOperatorDef(ListNative.addToEndOperator, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ListNative.foldlListNativeOperator, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ListNative.foldrListNativeOperator, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ListNative.headListNativeOperator, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ListNative.isEmpty, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ListNative.map2ListNativeOperator, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ListNative.mapListNativeOperator, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ListNative.tailListNativeOperator, env, typeEnv));
			
			//sb.append(ClojureHelper.makeOperatorDef(ListNative.ListNativeToLinkedListOperator, env, typeEnv));
			//sb.append(ClojureHelper.makeOperatorDef(ListNative.ListNativeToArrayListOperator, env, typeEnv));
			
			sb.append(ClojureHelper.makeLambdaDef(ListNative.constructorSymbol.name, ListNative.constructor, env, typeEnv));
			sb.append(ClojureHelper.makeLambdaDef(ListNative.constructorEmptySymbol.name, ListNative.constructorEmpty, env, typeEnv));
		} catch (AppendableException e) {
			System.err.println("Error generating velka.clojure.list file: " + e.getMessage());
			return "";
		}

		return sb.toString();
	}

	/**
	 * Writes file content into given destination
	 * 
	 * @param dest destination file path
	 * @return dest
	 * @throws IOException
	 */
	public static Path generateFile(Path dest) throws IOException {
		return Files.writeString(dest, VelkaClojureList.writeDefinitions());
	}
}
