package velka.lang.clojure;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import velka.lang.abstraction.Operators;
import velka.lang.application.AbstractionApplication;
import velka.lang.interpretation.ClojureHelper;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.util.AppendableException;

/**
 * This class is generating velka.clojure.operators namespace file
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class VelkaClojureOperators {
	/**
	 * Relative path to velka.clojure.operators file
	 */
	public static final Path VELKA_CLOJURE_OPERATORS_PATH = Paths.get("velka", "clojure");

	/**
	 * Name of the velka.clojure.operators file
	 */
	public static final Path VELKA_CLOJURE_OPERAOTRS_NAME = Paths.get("operators.clj");
	
	/**
	 * Relative file path
	 */
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_OPERATORS_PATH.resolve(VELKA_CLOJURE_OPERAOTRS_NAME);

	/**
	 * Namespace for velka.clojure.operators
	 */
	public static final String NAMESPACE = "velka.clojure.operators";

	/**
	 * Generates clojure code for definitions of velka.clojure.operators namespace
	 * 
	 * @return clojure code
	 */
	public static String writeDefinitions() {
		StringBuilder sb = new StringBuilder();

		// Require namespace
		sb.append(ClojureHelper.requireNamespace("clojure.string"));

		// Declare namespace
		sb.append(ClojureHelper.declareNamespace(NAMESPACE));

		// Declarations
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Addition));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.BitAnd));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.BitNot));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.BitOr));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.BitShiftLeft));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.BitShiftRight));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.BitXor));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.CanUnifyRepresentations));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.CanUnifyTypes));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Car));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Cdr));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Concantenation));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Division));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Equals));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.InitLogger));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.IsSameRepresentation));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.IsSameType));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.LesserThan));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Log));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Multiplication));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Not));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.NumericEqual));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.parseInt));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.PrintlnOperator));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.ReadFile));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.StrSplit));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Subtraction));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.Timestamp));
		sb.append(ClojureHelper.makeOperatorDeclaration(Operators.ToStr));
		sb.append(ClojureHelper.makeOperatorDeclaration(AbstractionApplication.defaultRanking));

		Environment env = Environment.initTopLevelEnvitonment();
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

			// Definitions
			sb.append(ClojureHelper.makeOperatorDef(Operators.Addition, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.BitAnd, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.BitNot, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.BitOr, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.BitShiftLeft, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.BitShiftRight, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.BitShiftRight, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.BitXor, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.CanUnifyRepresentations, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.CanUnifyTypes, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Car, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Cdr, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Concantenation, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Division, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Equals, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.InitLogger, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.IsSameRepresentation, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.IsSameType, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.LesserThan, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Log, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Multiplication, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Not, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.NumericEqual, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.parseInt, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.PrintlnOperator, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.ReadFile, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.StrSplit, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Subtraction, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.Timestamp, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(Operators.ToStr, env, typeEnv));
			sb.append(ClojureHelper.makeOperatorDef(AbstractionApplication.defaultRanking, env, typeEnv));
		} catch (AppendableException e) {
			System.err.println("Error generating velka.clojure.operaotrs: " + e.getMessage());
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
		return Files.writeString(dest, VelkaClojureOperators.writeDefinitions());
	}
}
