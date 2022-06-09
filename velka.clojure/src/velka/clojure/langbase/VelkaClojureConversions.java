package velka.clojure.langbase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.core.abstraction.ConversionOperators;
import velka.core.conversions.Conversions;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.util.AppendableException;

public class VelkaClojureConversions {

	public static final Path VELKA_CLOJURE_CONVERSIONS_PATH = Paths.get("velka", "clojure");
	public static final Path VELKA_CLOJURE_CONVERSIONS_NAME = Paths.get("conversions.clj");
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_CONVERSIONS_PATH.resolve(VELKA_CLOJURE_CONVERSIONS_NAME);
	
	
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();

		// Require namespace
		sb.append(ClojureHelper.requireNamespace("clojure.string"));

		// Declare namespace
		sb.append(ClojureHelper.declareNamespace(Conversions.NAMESPACE));
		
		sb.append(ClojureHelper.makeOperatorDeclaration(ConversionOperators.IntNativeToIntRoman));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConversionOperators.IntNativeToIntString));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConversionOperators.IntRomanToIntNative));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConversionOperators.IntRomanToIntString));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConversionOperators.IntStringToIntNative));
		sb.append(ClojureHelper.makeOperatorDeclaration(ConversionOperators.IntStringToIntRoman));
		
		Environment env = Environment.initTopLevelEnvironment();
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);
			
			sb.append(ClojureHelper.makeOperatorDef(ConversionOperators.IntNativeToIntRoman, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConversionOperators.IntNativeToIntString, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConversionOperators.IntRomanToIntNative, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConversionOperators.IntRomanToIntString, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConversionOperators.IntStringToIntNative, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(ConversionOperators.IntStringToIntRoman, env, typeEnv));
		}catch(AppendableException e) {
			System.err.println("Error generating " + RELATIVE_PATH.toString() + " :" + e.getMessage());
			return "";
		}
		
		return sb.toString();
	}
	
	public static Path generateFile(Path dest) throws IOException {
		return Files.writeString(dest, VelkaClojureConversions.writeDefinitions());
	}
}
