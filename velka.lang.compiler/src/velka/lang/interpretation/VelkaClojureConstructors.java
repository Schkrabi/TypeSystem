package velka.lang.interpretation;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.lang.abstraction.Operators;
import velka.lang.util.AppendableException;

public class VelkaClojureConstructors {
	public static final Path VELKA_CLOJURE_CONSTRUCTORS_PATH = Paths.get("velka", "clojure");
	public static final Path VELKA_CLOJURE_CONSTRUCTORS_NAME = Paths.get("constructors.clj");
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_CONSTRUCTORS_PATH.resolve(VELKA_CLOJURE_CONSTRUCTORS_NAME);
	public static final String NAMESPACE = "velka.clojure.constructors";
	
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();
		
		// Require namespace
		sb.append(ClojureHelper.requireNamespace("clojure.string"));

		// Declare namespace
		sb.append(ClojureHelper.declareNamespace(NAMESPACE));
		
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.IntConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.IntNativeConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.IntRomanConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.IntStringConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.StringConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.StringNativeConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.DoubleConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.DoubleNativeConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.BoolConstructor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.BoolNativeConstructor));
		
		Environment env = Environment.initTopLevelEnvitonment();
		
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			sb.append(ClojureHelper.makeOperatorDef(Operators.IntConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.IntNativeConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.IntRomanConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.IntStringConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.StringConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.StringNativeConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.DoubleConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.DoubleNativeConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.BoolConstructor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.BoolNativeConstructor, env, typeEnv));
			
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
