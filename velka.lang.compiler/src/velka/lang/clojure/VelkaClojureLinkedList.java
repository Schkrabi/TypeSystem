package velka.lang.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.lang.interpretation.ClojureHelper;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.langbase.JavaLinkedList;
import velka.lang.util.AppendableException;

public class VelkaClojureLinkedList {
	public static final Path VELKA_CLOJURE_LINKEDLIST_PATH = Paths.get("velka", "clojure");
	public static final Path VELKA_CLOJURE_LINKEDLIST_NAME = Paths.get("linkedList.clj");
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_LINKEDLIST_PATH.resolve(VELKA_CLOJURE_LINKEDLIST_NAME);
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();
		
		// Require namespace
		sb.append(ClojureHelper.requireNamespace("clojure.string"));
		//sb.append(ClojureHelper.requireNamespace(VelkaClojureList.NAMESPACE));
		//sb.append(ClojureHelper.requireNamespace(VelkaClojureArrayList.NAMESPACE));

		// Declare namespace
		sb.append(ClojureHelper.declareNamespace(JavaLinkedList.NAMESPACE));
		
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.addAll));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.addToEnd));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.addToIndex));
		//sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.LinkedListToArrayList));
		//sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.LinkedListToNativeList));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.constructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.contains));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.containsAll));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.foldl));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.foldr));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.get));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.indexOf));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.isEmpty));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.lastIndexOf));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.map));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.map2));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.remove));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.removeAll));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.retainAll));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.set));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.size));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.sublist));
		sb.append(ClojureHelper.makeOperatorDeclaration(JavaLinkedList.sublist));
		
		Environment env = Environment.initTopLevelEnvitonment();
		
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.addAll, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.addToEnd, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.addToIndex, env, typeEnv));
			//sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.LinkedListToArrayList, env, typeEnv));
			//sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.LinkedListToNativeList, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.constructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.contains, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.containsAll, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.foldl, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.foldr, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.get, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.indexOf, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.isEmpty, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.lastIndexOf, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.map, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.map2, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.remove, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.removeAll, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.retainAll, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.set, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.size, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.sublist, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(JavaLinkedList.sublist, env, typeEnv));
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
