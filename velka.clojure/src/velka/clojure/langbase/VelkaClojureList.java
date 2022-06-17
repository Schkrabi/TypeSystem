package velka.clojure.langbase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.core.abstraction.Operator;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaLinkedList;
import velka.core.langbase.ListNative;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureHelper;

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

		// Declare namespace
		sb.append(ClojureHelper.declareNamespace(ListNative.NAMESPACE));

		// Declarations
		sb.append(ClojureHelper.makeDeclaration(ListNative.addToEndOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.foldlListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.headListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.isEmpty.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.map2ListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.mapListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.tailListNativeOperator.getClojureSymbol().name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.contains.getClojureSymbol().name));
		sb.append(Operator.makeOperatorDeclaration(ListNative.filter));
		sb.append(Operator.makeOperatorDeclaration(ListNative.get));
		sb.append(Operator.makeOperatorDeclaration(ListNative.buildList));
		sb.append(Operator.makeOperatorDeclaration(ListNative.remove));
		sb.append(Operator.makeOperatorDeclaration(ListNative.size));
		sb.append(Operator.makeOperatorDeclaration(ListNative.append));
		sb.append(Operator.makeOperatorDeclaration(ListNative.reverse));
		sb.append(Operator.makeOperatorDeclaration(ListNative.everyp));
		
		sb.append(ClojureHelper.makeDeclaration(ListNative.constructorSymbol.name));
		sb.append(ClojureHelper.makeDeclaration(ListNative.constructorEmptySymbol.name));
		
		sb.append(Operator.makeOperatorDeclaration(ListNative.ListNativeToLinkedListOperator));
		sb.append(Operator.makeOperatorDeclaration(ListNative.ListNativeToArrayListOperator));

		Environment env = Environment.initTopLevelEnvironment();
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

			sb.append(Operator.makeOperatorDef(ListNative.addToEndOperator, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.foldlListNativeOperator, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.headListNativeOperator, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.isEmpty, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.map2ListNativeOperator, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.mapListNativeOperator, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.tailListNativeOperator, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.contains, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.filter, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.get, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.buildList, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.remove, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.size, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.append, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.reverse, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.everyp, env, typeEnv));
			
			sb.append(Operator.makeOperatorDef(ListNative.ListNativeToLinkedListOperator, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ListNative.ListNativeToArrayListOperator, env, typeEnv));

			sb.append(TypeAtom.addConversionToGlobalTable(TypeAtom.TypeListNative, JavaArrayList.TypeListJavaArray, ListNative.listNativeToArrayListSymbol.toClojureCode(env, typeEnv)));
			sb.append(TypeAtom.addConversionToGlobalTable(TypeAtom.TypeListNative, JavaLinkedList.TypeListJavaLinked, ListNative.ListNativeToLinkedListSymbol.toClojureCode(env, typeEnv)));
			
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
