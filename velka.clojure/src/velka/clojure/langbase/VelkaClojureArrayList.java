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
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureHelper;

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
		
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.addAll));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.addToEnd));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.addToIndex));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.ArrayListToLinkedList));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.ArrayListToNativeList));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.constructor));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.contains));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.containsAll));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.foldl));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.foldr));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.get));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.indexOf));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.isEmpty));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.lastIndexOf));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.map));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.map2));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.remove));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.removeAll));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.retainAll));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.set));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.size));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.sublist));
		sb.append(Operator.makeOperatorDeclaration(JavaArrayList.everyp));
		
		Environment env = Environment.initTopLevelEnvironment();
		
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			sb.append(Operator.makeOperatorDef(JavaArrayList.addAll, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.addToEnd, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.addToIndex, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.ArrayListToLinkedList, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.ArrayListToNativeList, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.constructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.contains, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.containsAll, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.foldl, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.foldr, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.get, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.indexOf, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.isEmpty, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.lastIndexOf, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.map, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.map2, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.remove, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.removeAll, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.retainAll, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.set, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.size, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.sublist, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaArrayList.everyp, env, typeEnv));
			
			sb.append(TypeAtom.addConversionToGlobalTable(JavaArrayList.TypeListJavaArray, TypeAtom.TypeListNative, JavaArrayList.ArrayListToNativeListSymbol.toClojureCode(env, typeEnv)));
			sb.append(TypeAtom.addConversionToGlobalTable(JavaArrayList.TypeListJavaArray, JavaLinkedList.TypeListJavaLinked, JavaArrayList.ArrayListToLinkedListSymbol.toClojureCode(env, typeEnv)));
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
