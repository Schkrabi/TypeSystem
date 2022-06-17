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

public class VelkaClojureLinkedList {
	public static final Path VELKA_CLOJURE_LINKEDLIST_PATH = Paths.get("velka", "clojure");
	public static final Path VELKA_CLOJURE_LINKEDLIST_NAME = Paths.get("linkedList.clj");
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_LINKEDLIST_PATH.resolve(VELKA_CLOJURE_LINKEDLIST_NAME);
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();
		
		// Require namespace
		sb.append(ClojureHelper.requireNamespace("clojure.string"));

		// Declare namespace
		sb.append(ClojureHelper.declareNamespace(JavaLinkedList.NAMESPACE));
		
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.addAll));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.addToEnd));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.addToIndex));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.LinkedListToArrayList));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.LinkedListToNativeList));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.constructor));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.contains));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.containsAll));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.foldl));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.foldr));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.get));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.indexOf));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.isEmpty));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.lastIndexOf));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.map));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.map2));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.remove));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.removeAll));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.retainAll));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.set));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.size));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.sublist));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.sublist));
		sb.append(Operator.makeOperatorDeclaration(JavaLinkedList.everyp));
		
		Environment env = Environment.initTopLevelEnvironment();
		
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			sb.append(Operator.makeOperatorDef(JavaLinkedList.addAll, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.addToEnd, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.addToIndex, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.LinkedListToArrayList, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.LinkedListToNativeList, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.constructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.contains, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.containsAll, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.foldl, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.foldr, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.get, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.indexOf, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.isEmpty, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.lastIndexOf, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.map, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.map2, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.remove, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.removeAll, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.retainAll, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.set, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.size, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.sublist, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.sublist, env, typeEnv));
			sb.append(Operator.makeOperatorDef(JavaLinkedList.everyp, env, typeEnv));
			
			sb.append(TypeAtom.addConversionToGlobalTable(JavaLinkedList.TypeListJavaLinked, TypeAtom.TypeListNative, JavaLinkedList.LinkedListToNativeListSymbol.toClojureCode(env, typeEnv)));
			sb.append(TypeAtom.addConversionToGlobalTable(JavaLinkedList.TypeListJavaLinked, JavaArrayList.TypeListJavaArray, JavaLinkedList.LinkedListToArrayListSymbol.toClojureCode(env, typeEnv)));
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
		return Files.writeString(dest, VelkaClojureLinkedList.writeDefinitions());
	}
}
