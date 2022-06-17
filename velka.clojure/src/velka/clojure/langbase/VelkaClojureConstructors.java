package velka.clojure.langbase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.core.abstraction.ConstructorOperators;
import velka.core.abstraction.Operator;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.util.AppendableException;
import velka.util.ClojureHelper;

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
		
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.IntConstructor));
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.IntNativeConstructor));
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.IntRomanConstructor));
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.IntStringConstructor));
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.StringConstructor));
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.StringNativeConstructor));
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.DoubleConstructor));
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.DoubleNativeConstructor));
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.BoolConstructor));
		sb.append(Operator.makeOperatorDeclaration(ConstructorOperators.BoolNativeConstructor));
		
		Environment env = Environment.initTopLevelEnvironment();
		
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			sb.append(Operator.makeOperatorDef(ConstructorOperators.IntConstructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ConstructorOperators.IntNativeConstructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ConstructorOperators.IntRomanConstructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ConstructorOperators.IntStringConstructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ConstructorOperators.StringConstructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ConstructorOperators.StringNativeConstructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ConstructorOperators.DoubleConstructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ConstructorOperators.DoubleNativeConstructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ConstructorOperators.BoolConstructor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ConstructorOperators.BoolNativeConstructor, env, typeEnv));
			
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
