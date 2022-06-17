package velka.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import velka.types.TypeAtom;
import velka.util.ClojureHelper;

/**
 * Class containing dev utilities for debugging generated clojure code
 * @author Mgr. Radomir Skrabal
 *
 */
public class VelkaClojureDev {
	/**
	 * Relative path to velka.clojure.core file
	 */
	public static final Path VELKA_CLOJURE_DEV_PATH = Paths.get("velka", "clojure");
	
	/**
	 * Name of the velka.clojure.core file
	 */
	public static final Path VELKA_CLOJURE_DEV_NAME = Paths.get("dev.clj");

	/**
	 * Relative path to file
	 */
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_DEV_PATH.resolve(VELKA_CLOJURE_DEV_NAME);
	
	/**
	 * Namespace
	 */
	public static final String NAMESPACE = "velka.clojure.dev";
	
	private static final String velkaLiteralSymbol = "velka-literal";
	private static final String velkaLiteral_val = "_val";
	private static final String velkaLiteral_type = "_type";
	private static final String velkaLiteral = ClojureHelper.clojureDefnHelper(
			velkaLiteralSymbol, 
			Arrays.asList(velkaLiteral_val, velkaLiteral_type), 
			ClojureHelper.litCompositeHelper_str(velkaLiteral_type, velkaLiteral_val));
	
	private static final String velkaIntSymbol = "velka-int";
	private static final String velkaInt_val = "_val";
	private static final String velkaInt = ClojureHelper.clojureDefnHelper(
			velkaIntSymbol, 
			Arrays.asList(velkaInt_val), 
			ClojureHelper.applyClojureFunction(velkaLiteralSymbol, 
					velkaInt_val, TypeAtom.TypeIntNative.clojureTypeRepresentation()));
	
	private static final String velkaStrSymbol = "velka-str";
	private static final String velkaStr_val = "_val";
	private static final String velkaStr = ClojureHelper.clojureDefnHelper(
			velkaStrSymbol, 
			Arrays.asList(velkaStr_val), 
			ClojureHelper.applyClojureFunction(velkaLiteralSymbol, 
					velkaStr_val, TypeAtom.TypeStringNative.clojureTypeRepresentation()));
	
	private static final String velkaDoubleSymbol = "velka-double";
	private static final String velkaDouble_val = "_val";
	private static final String velkaDouble = ClojureHelper.clojureDefnHelper(
			velkaDoubleSymbol, 
			Arrays.asList(velkaDouble_val), 
			ClojureHelper.applyClojureFunction(velkaLiteralSymbol, 
					velkaDouble_val, TypeAtom.TypeDoubleNative.clojureTypeRepresentation()));
	
	private static final String velkaBoolSymbol = "velka-bool";
	private static final String velkaBool_val = "_val";
	private static final String velkaBool = ClojureHelper.clojureDefnHelper(
			velkaBoolSymbol, 
			Arrays.asList(velkaBool_val), 
			ClojureHelper.applyClojureFunction(velkaLiteralSymbol, 
					velkaBool_val, TypeAtom.TypeBoolNative.clojureTypeRepresentation()));
	
	private static final String velkaTupleSymbol = "velka-tuple";
	private static final String velkaTuple_args = "_args";
	private static final String velkaTuple = ClojureHelper.clojureDefnHelper(
			velkaTupleSymbol, 
			Arrays.asList("& " +  velkaTuple_args), 
			ClojureHelper.tupleHelper_str(velkaTuple_args));
	
	private static final String velkaApplySymbol = "velka-apply";
	private static final String velkaApply_fun = "_fun";
	private static final String velkaApply_args = "_args";
	private static final String velkaApply = ClojureHelper.clojureDefnHelper(velkaApplySymbol,
			Arrays.asList(velkaApply_fun, "& " + velkaApply_args),
			ClojureHelper.applyVelkaFunction_argsTuple(velkaApply_fun, ClojureHelper.tupleHelper_str(velkaApply_args)));
	
	/**
	 * Generates clojure code for definitions of velka.clojure.dev namespace
	 * 
	 * @return string with code
	 */
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();
		
		sb.append(ClojureHelper.declareNamespace(NAMESPACE));
		
		sb.append(ClojureHelper.makeDeclaration(velkaApplySymbol));
		sb.append(ClojureHelper.makeDeclaration(velkaBoolSymbol));
		sb.append(ClojureHelper.makeDeclaration(velkaDoubleSymbol));
		sb.append(ClojureHelper.makeDeclaration(velkaIntSymbol));
		sb.append(ClojureHelper.makeDeclaration(velkaStrSymbol));
		sb.append(ClojureHelper.makeDeclaration(velkaLiteralSymbol));
		sb.append(ClojureHelper.makeDeclaration(velkaTupleSymbol));
		
		sb.append(velkaApply);
		sb.append("\n");
		sb.append(velkaBool);
		sb.append("\n");
		sb.append(velkaDouble);
		sb.append("\n");
		sb.append(velkaInt);
		sb.append("\n");
		sb.append(velkaStr);
		sb.append("\n");
		sb.append(velkaLiteral);
		sb.append("\n");
		sb.append(velkaTuple);
		
		return sb.toString();
	}
	
	/**
	 * Writes file contents into given destination
	 * @param dest destination file path
	 * @return dest path
	 * @throws IOException 
	 */
	public static Path generateFile(Path dest) throws IOException {
		return Files.writeString(dest, VelkaClojureDev.writeDefinitions());
	}
}
