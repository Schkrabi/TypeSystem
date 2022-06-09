package velka.clojure.langbase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.langbase.JavaArrayList;
import velka.core.langbase.JavaLinkedList;
import velka.types.TypeAtom;
import velka.util.AppendableException;

public class VelkaClojureArrayList {
	public static final Path VELKA_CLOJURE_ARRAYLIST_PATH = Paths.get("velka", "clojure");
	public static final Path VELKA_CLOJURE_ARRAYLIST_NAME = Paths.get("arrayList.clj");
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_ARRAYLIST_PATH.resolve(VELKA_CLOJURE_ARRAYLIST_NAME);
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();
		
		// Require namespace
		sb.append(ClojureHelper.requireNamespace("clojure.string"));

		// Declare namespace
		sb.append(ClojureHelper.declareNamespace(JavaArrayList.NAMESPACE));
		
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.addAll));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.addToEnd));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.addToIndex));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.ArrayListToLinkedList));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.ArrayListToNativeList));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.constructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.contains));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.containsAll));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.foldl));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.foldr));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.get));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.indexOf));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.isEmpty));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.lastIndexOf));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.map));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.map2));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.remove));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.removeAll));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.retainAll));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.set));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.size));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.sublist));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaArrayList.everyp));
		
		Environment env = Environment.initTopLevelEnvironment();
		
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.addAll, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.addToEnd, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.addToIndex, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.ArrayListToLinkedList, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.ArrayListToNativeList, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.constructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.contains, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.containsAll, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.foldl, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.foldr, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.get, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.indexOf, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.isEmpty, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.lastIndexOf, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.map, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.map2, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.remove, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.removeAll, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.retainAll, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.set, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.size, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.sublist, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaArrayList.everyp, env, typeEnv));
			
			sb.append(ClojureHelper.addConversionToGlobalTable(JavaArrayList.TypeListJavaArray, TypeAtom.TypeListNative, JavaArrayList.ArrayListToNativeListSymbol.toClojureCode(env, typeEnv)));
			sb.append(ClojureHelper.addConversionToGlobalTable(JavaArrayList.TypeListJavaArray, JavaLinkedList.TypeListJavaLinked, JavaArrayList.ArrayListToLinkedListSymbol.toClojureCode(env, typeEnv)));
		} catch (AppendableException e) {
			System.err.println("Error generating " + RELATIVE_PATH.toString() + " :" + e.getMessage());
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
		return Files.writeString(dest, VelkaClojureArrayList.writeDefinitions());
	}
}
