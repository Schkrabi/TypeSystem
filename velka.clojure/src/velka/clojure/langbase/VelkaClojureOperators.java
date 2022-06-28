package velka.clojure.langbase;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import velka.core.abstraction.ExtendedLambda;
import velka.core.abstraction.Operator;
import velka.core.abstraction.Operators;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;

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
	
	private static final String defaultCostFunctionDef_fn = "_fn";
	private static final String defaultCostFunctionDef_args = "_args";
	public static final String defaultCostFunctionDef =
			ClojureHelper.clojureDefnHelper(
					Operators.defaultCostFunction,
					Arrays.asList(defaultCostFunctionDef_fn),
						Type.addTypeMetaInfo(
							ClojureHelper.fnHelper(
									Arrays.asList("& " + defaultCostFunctionDef_args),
									ClojureHelper.applyVelkaFunction(
											Operators.conversionCostSym_full,
											defaultCostFunctionDef_fn,
											ClojureHelper.tupleHelper_str(defaultCostFunctionDef_args))),
						new TypeArrow(new TypeVariable(NameGenerator.next()), TypeAtom.TypeIntNative)));

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
		sb.append(ClojureHelper.declareNamespace(Operators.NAMESPACE));

		// Declarations
		sb.append(Operator.makeOperatorDeclaration(Operators.Addition));
		sb.append(Operator.makeOperatorDeclaration(Operators.BitAnd));
		sb.append(Operator.makeOperatorDeclaration(Operators.BitNot));
		sb.append(Operator.makeOperatorDeclaration(Operators.BitOr));
		sb.append(Operator.makeOperatorDeclaration(Operators.BitShiftLeft));
		sb.append(Operator.makeOperatorDeclaration(Operators.BitShiftRight));
		sb.append(Operator.makeOperatorDeclaration(Operators.UnsignedBitShiftRight));
		sb.append(Operator.makeOperatorDeclaration(Operators.BitXor));
		sb.append(Operator.makeOperatorDeclaration(Operators.CanUnifyRepresentations));
		sb.append(Operator.makeOperatorDeclaration(Operators.CanUnifyTypes));
		sb.append(Operator.makeOperatorDeclaration(Operators.Car));
		sb.append(Operator.makeOperatorDeclaration(Operators.Cdr));
		sb.append(Operator.makeOperatorDeclaration(Operators.Concantenation));
		sb.append(Operator.makeOperatorDeclaration(Operators.Division));
		sb.append(Operator.makeOperatorDeclaration(Operators.Equals));
		sb.append(Operator.makeOperatorDeclaration(Operators.InitLogger));
		sb.append(Operator.makeOperatorDeclaration(Operators.IsSameRepresentation));
		sb.append(Operator.makeOperatorDeclaration(Operators.IsSameType));
		sb.append(Operator.makeOperatorDeclaration(Operators.LesserThan));
		sb.append(Operator.makeOperatorDeclaration(Operators.Log));
		sb.append(Operator.makeOperatorDeclaration(Operators.Multiplication));
		sb.append(Operator.makeOperatorDeclaration(Operators.Not));
		sb.append(Operator.makeOperatorDeclaration(Operators.NumericEqual));
		sb.append(Operator.makeOperatorDeclaration(Operators.parseInt));
		sb.append(Operator.makeOperatorDeclaration(Operators.PrintlnOperator));
		sb.append(Operator.makeOperatorDeclaration(Operators.ReadFile));
		sb.append(Operator.makeOperatorDeclaration(Operators.StrSplit));
		sb.append(Operator.makeOperatorDeclaration(Operators.Subtraction));
		sb.append(Operator.makeOperatorDeclaration(Operators.Timestamp));
		sb.append(Operator.makeOperatorDeclaration(Operators.ToStr));
		sb.append(Operator.makeOperatorDeclaration(ExtendedLambda.defaultSelectionFunction));
		sb.append(Operator.makeOperatorDeclaration(Operators.DivisionFloatingPoint));
		sb.append(Operator.makeOperatorDeclaration(Operators.intToDouble));
		sb.append(Operator.makeOperatorDeclaration(Operators.floor));
		sb.append(Operator.makeOperatorDeclaration(Operators.dadd));
		sb.append(Operator.makeOperatorDeclaration(Operators.doubleLesserThan));
		sb.append(Operator.makeOperatorDeclaration(Operators.modulo));
		sb.append(Operator.makeOperatorDeclaration(Operators.conversionCost));
		sb.append(ClojureHelper.makeDeclaration(Operators.defaultCostFunction));

		Environment env = Environment.initTopLevelEnvironment();
		try {
			TypeEnvironment typeEnv = TypeEnvironment.initBasicTypes(env);

			// Definitions
			sb.append(Operator.makeOperatorDef(Operators.Addition, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.BitAnd, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.BitNot, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.BitOr, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.BitShiftLeft, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.BitShiftRight, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.BitShiftRight, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.UnsignedBitShiftRight, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.BitXor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.CanUnifyRepresentations, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.CanUnifyTypes, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Car, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Cdr, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Concantenation, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Division, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Equals, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.InitLogger, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.IsSameRepresentation, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.IsSameType, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.LesserThan, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Log, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Multiplication, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Not, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.NumericEqual, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.parseInt, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.PrintlnOperator, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.ReadFile, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.StrSplit, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Subtraction, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.Timestamp, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.ToStr, env, typeEnv));
			sb.append(Operator.makeOperatorDef(ExtendedLambda.defaultSelectionFunction, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.DivisionFloatingPoint, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.intToDouble, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.floor, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.dadd, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.doubleLesserThan, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.modulo, env, typeEnv));
			sb.append(Operator.makeOperatorDef(Operators.conversionCost, env, typeEnv));
			sb.append("\n" + defaultCostFunctionDef);
			
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
