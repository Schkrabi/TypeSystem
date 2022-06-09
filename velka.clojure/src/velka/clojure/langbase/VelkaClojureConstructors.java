package velka.clojure.langbase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.core.abstraction.ConstructorOperators;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.util.AppendableException;

public class VelkaClojureConstructors {
	public static final Path VELKA_CLOJURE_CONSTRUCTORS_PATH = Paths.get("velka", "clojure");
	public static final Path VELKA_CLOJURE_CONSTRUCTORS_NAME = Paths.get("constructors.clj");
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_CONSTRUCTORS_PATH.resolve(VELKA_CLOJURE_CONSTRUCTORS_NAME);
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();
		
		// Require namespace
		sb.append(ClojureHelper.requireNamespace("clojure.string"));

		// Declare namespace
		sb.append(ClojureHelper.declareNamespace(ConstructorOperators.NAMESPACE));
		
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.IntConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.IntNativeConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.IntRomanConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.IntStringConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.StringConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.StringNativeConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.DoubleConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.DoubleNativeConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.BoolConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConstructorOperators.BoolNativeConstructor));
		
		Environment env = Environment.initTopLevelEnvironment();
		
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.IntConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.IntNativeConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.IntRomanConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.IntStringConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.StringConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.StringNativeConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.DoubleConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.DoubleNativeConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.BoolConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConstructorOperators.BoolNativeConstructor, env, typeEnv));
			
		}catch(AppendableException e) {
			System.err.println("Error generating " + RELATIVE_PATH.toString() + " :" + e.getMessage());
			return "";
		}
		
		return sb.toString();
	}
	
	public static Path generateFile(Path dest) throws IOException {
		return Files.writeString(dest, VelkaClojureConstructors.writeDefinitions());
	}
}
