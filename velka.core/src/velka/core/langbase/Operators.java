package velka.core.langbase;

import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.XMLFormatter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import velka.core.abstraction.Operator;
import velka.core.exceptions.DuplicateTypeDefinitionException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Header;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaOperator;
import velka.util.annotations.VelkaOperatorBank;

/**
 * This class contains Velka's operators
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
@VelkaOperatorBank
@Header("General")
@Description("General operators for primitive types and utility.")
public final class Operators extends OperatorBank {
	
	/**
	 * Namespace for velka.clojure.operators
	 */
	public static final String NAMESPACE = "velka.clojure.operators";

	/**
	 * Addition (+) operator
	 */
	@VelkaOperator
	@Description("Adds two integers.") 
	@Example("(+ 21 21) ; = 42")
	@Syntax("(+ arg1 arg2)")
	public static final Operator Addition = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger x = (LitInteger) args.get(0);
			LitInteger y = (LitInteger) args.get(1);

			return new LitInteger(x.value + y.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "+";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String x = "_x";
//			String y = "_y";
//			String code = ClojureHelper.fnHelper(
//					Arrays.asList(x, y),
//					LitInteger.clojureLit(
//							ClojureHelper.applyClojureFunction("unchecked-add",
//									ClojureHelper.getLiteralInnerValue(x),
//									ClojureHelper.getLiteralInnerValue(y))));
			
			return "unchecked-add";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-addition", NAMESPACE);
		}

	};
	
	/**
	 * Bit and (&) operator
	 */
	@VelkaOperator
	@Description("Performs bit-wise and of two integers.") 
	@Example("(bit-and 5 1) ; = 1") 
	@Syntax("(bit-and <arg1> <arg2>)")
	public static final Operator BitAnd = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value & arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-and";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			//return "(fn [_x _y] " + LitInteger.clojureLit("(bit-and (get _x 0) (get _y 0))") + ")";
			return "bit-and";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-and", NAMESPACE);
		}
	};
	
	/**
	 * Bit Not operator
	 */
	@VelkaOperator
	@Description("Negates all bits in binary representation of the argument.") 
	@Example(">(bit-not 42) ;;= -43") 
	@Syntax("(bit-not <arg>)")
	public static final Operator BitNot = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String code = "(fn [_val] " + LitInteger.clojureLit("(bit-not (first _val))") + ")";
			return "bit-not";
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger l = (LitInteger) args.get(0);

			long ret = ~l.value;
			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-not";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-not", NAMESPACE);
		}

	};
	
	/**
	 * Bit or (|) operator
	 */
	@VelkaOperator
	@Description("Performs bit-wise or of two integers.") 
	@Example("(bit-or 5 1) ; = 5") 
	@Syntax("(bit-or <arg1> <arg2>)")
	public static final Operator BitOr = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value | arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-or";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			//return "(fn [_x _y] " + LitInteger.clojureLit("(bit-or (get _x 0) (get _y 0))") + ")";
			return "bit-or";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-or", NAMESPACE);
		}
	};
	
	/**
	 * Bit shift left (shl) operator
	 */
	@VelkaOperator
	@Description("Shifts bits of binary representation_bits_ left by _n_ positions.") 
	@Example(">(shl 1 4) ;;=16") 
	@Syntax("(shl <bits> <n>)")
	public static final Operator BitShiftLeft = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String code = "(fn [_num _n] "
//					+ LitInteger.clojureLit("(bit-shift-left (first _num) (first _n))") + ")";
//			return code;
			return "bit-shift-left";
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger num = (LitInteger) args.get(0);

			LitInteger n;

			if (!(args.get(1) instanceof LitInteger)) {
				n = new LitInteger(1);
			} else {
				n = (LitInteger) args.get(1);
			}

			long res = num.value << n.value;

			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "shl";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-shr", NAMESPACE);
		}

	};

	/**
	 * Bit shift right (shr) operator
	 */
	@VelkaOperator
	@Description("Shifts bits of binary representation_bits_ right by _n_ positions.") 
	@Example(">(shr 16 4) ;; = 1") 
	@Syntax("(shr <bits> <n>)")
	public static final Operator BitShiftRight = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String code = "(fn [_num _n] "
//					+ LitInteger.clojureLit("(bit-shift-right (first _num) (first _n))") + ")";
//			return code;
			return "bit-shift-right";
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger num = (LitInteger) args.get(0);
			LitInteger n = (LitInteger) args.get(1);

			long res = num.value >> n.value;

			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "shr";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-shr", NAMESPACE);
		}

	};

	/**
	 * Bit XOR operator
	 */
	@VelkaOperator
	@Description("Computes xor of binary representations of integer arguments.") 
	@Example(">(bit-xor 6 3) ;; = 5") 
	@Syntax("(bit-xor <arg1> <arg2>)")
	public static final Operator BitXor = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String code = "(fn [_val1 _val2] "
//					+ LitInteger.clojureLit("(bit-xor (first _val1) (first _val2))") + ")";
//			return code;
			return "bit-xor";
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger val1 = (LitInteger) args.get(0);
			LitInteger val2 = (LitInteger) args.get(1);

			long ret = val1.value ^ val2.value;

			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-xor";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-bit-xor", NAMESPACE);
		}
	};
	
//	/**
//	 * can-unify-representations operator
//	 */
//	@VelkaOperator
//	@Description("Returns _true_ if representations carried by _arg1_ and _arg2_ can be unified. "
//			+ "Otherwise returns _false_. " 
//			+ "This operator works on level of representations, meaning for example " 
//			+ "_Int:Native_ and _Int:String_ cannot unify and type symbols carriing them will yield _false_ on application of the operator.\n\n"
//			+ "For type level variant see [can-unify-types](#canUnifyTypes).") 
//	@Example("(can-unify-representations 42 42) ;; = #t\n"
//			+ "(can-unify-representations 42 (construct Int String \"42\"))") 
//	@Syntax("(can-unify-representations <arg1> <arg2>)")
//	public static final Operator CanUnifyRepresentations = new Operator() {
//		@Override
//		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			TypeSymbol t1 = (TypeSymbol) args.get(0);
//			TypeSymbol t2 = (TypeSymbol) args.get(1);
//
//			if(Type.unifyRepresentation(t1.type, t2.type).isPresent()) {
//				return LitBoolean.TRUE;
//			}
//			return LitBoolean.FALSE;
//		}
//
//		@Override
//		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//
//			String t1 = "_t1";
//			String t2 = "_t2";
//			String opt = "_opt";
//			String fn = ClojureHelper.fnHelper(Arrays.asList(t1, t2), ClojureHelper.letHelper(
//					LitBoolean.clojureLit(ClojureHelper.applyClojureFunction(".isPresent", opt)),
//					new Pair<String, String>(opt, ClojureHelper.applyClojureFunction(
//							"velka.types.Type/unifyRepresentation",
//							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, t1),
//							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, t2)))));
//
//			return fn;
//		}
//
//		@Override
//		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			return new Pair<Type, Substitution>(new TypeArrow(new TypeTuple(
//					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
//					TypeAtom.TypeBoolNative), Substitution.EMPTY);
//		}
//
//		@Override
//		public String toString() {
//			return "can-unify-representations";
//		}
//
//		@Override
//		public Symbol getClojureSymbol() {
//			return new Symbol("velka-can-unify-representations", NAMESPACE);
//		}
//	};
//
//	/**
//	 * can-unify-types operator
//	 */
//	@VelkaOperator
//	@Description("Returns _true_ if types carried by _arg1_ and _arg2_ can be unified. " 
//					+ "Otherwise returns _false_. This operator works on level of types, not taking type " 
//					+ "representations into account. Meaning for example _Int:Native_ and _Int:String_ can unify " 
//					+ "and type symbols carriing them will yield _true_ on application of the operator. \n\n"
//					+ "For representation level variant see [can-unify-representations](#canUnifyRepresentations).") 
//	@Example("(can-unify-types 42 (construct Int String \"42\")) ;; = #t)\n"
//					+ "(can-unify-types 42 \"42\") ;; = #f") 
//	@Syntax("(can-unify-types <arg1> <arg2>)")
//	public static final Operator CanUnifyTypes = new Operator() {
//
//		@Override
//		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			TypeSymbol t1 = (TypeSymbol) args.get(0);
//			TypeSymbol t2 = (TypeSymbol) args.get(1);
//
//			if(Type.unifyTypes(t1.type, t2.type).isPresent()) {
//				return LitBoolean.TRUE;
//			}
//			return LitBoolean.FALSE;
//		}
//
//		@Override
//		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//
//			String t1 = "_t1";
//			String t2 = "_t2";
//			String opt = "_opt";
//			String fn = ClojureHelper.fnHelper(Arrays.asList(t1, t2), ClojureHelper.letHelper(
//					LitBoolean.clojureLit(ClojureHelper.applyClojureFunction(".isPresent", opt)),
//					new Pair<String, String>(opt, ClojureHelper.applyClojureFunction(
//							"velka.types.Type/unifyTypes",
//							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, t1),
//							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, t2)))));
//
//			return fn;
//		}
//
//		@Override
//		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			return new Pair<Type, Substitution>(new TypeArrow(new TypeTuple(
//					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
//					TypeAtom.TypeBoolNative), Substitution.EMPTY);
//		}
//
//		@Override
//		public String toString() {
//			return "can-unify-types";
//		}
//
//		@Override
//		public Symbol getClojureSymbol() {
//			return new Symbol("velka-can-unify-types", NAMESPACE);
//		}
//	};
	
	/**
	 * car operator
	 */
	@VelkaOperator
	@Description("Extracts first value from a pair.") 
	@Example("(car (cons 42 \"42\")) ; = 42") 
	@Syntax("(car <arg>)")
	public static final Operator Car = new Operator() {
		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			TypeVariable left = new TypeVariable(NameGenerator.next());
			TypeVariable right = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeTuple(left, right)), left);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Tuple arg = (Tuple) args.get(0);

			return arg.get(0);
		}

		@Override
		public String toString() {
			return "car";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			//return "(fn [_x] (first _x))";
			return "first";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-car", NAMESPACE);
		}
	};
	
	/**
	 * cdr operator
	 */
	@VelkaOperator
	@Description("Extracts second value from a pair.") 
	@Example("(cdr (cons 42 \"42\")) ; = \"42\"") 
	@Syntax("(cdr <arg>)")
	public static final Operator Cdr = new Operator() {

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			TypeVariable left = new TypeVariable(NameGenerator.next());
			TypeVariable right = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeTuple(left, right)), right);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Tuple arg = (Tuple) args.get(0);

			return arg.get(1);
		}

		@Override
		public String toString() {
			return "cdr";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			//return "(fn [_x] (second _x))";
			return "second";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-cdr", NAMESPACE);
		}
	};
	
	/**
	 * Concatenation operator
	 */
	@VelkaOperator
	@Description("Concatenates two strings.") 
	@Example("(concat \"foo\" \"bar\") ; = \"foobar\"") 
	@Syntax("(concat <arg1> <arg2>)")
	public static final Operator Concantenation = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitString arg0 = (LitString) args.get(0);
			LitString arg1 = (LitString) args.get(1);

			return new LitString(arg0.value + arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(
					new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative)),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "concat";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			//return "(fn [_x _y] " + LitString.clojureLit("(str (get _x 0) (get _y 0))") + ")";
			return "str";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-concat", NAMESPACE);
		}

	};
	
	public static final String conversionCostSym = "converison-cost";
	public static final String conversionCostSym_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, conversionCostSym);

	/**
	 * Operator for computing conversion cost
	 */
	@VelkaOperator
	@Description("Computes cost of representation conversion for applying _fun_ with _arg_.") 
	@Example("(conversion-cost <fun> <arg>)") 
	@Syntax("(conversion-cost\r\n"
		+ "	(lambda ((Int:Native x) (Int:Native y)) (+ x y))\r\n"
		+ "	(tuple\r\n"
		+ "		(construct Int String \"42\")\r\n"
		+ "		(construct Int Roman \"XLII\"))) ; = 2")
	public static final Operator ConversionCost = new Operator() {
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String fun = "_fun";
			String arg = "_arg";
			String funArgType = "_funArgType";
			String argType = "_argType";
			String x = "_x";
			String y = "_y";
			
			String code = ClojureHelper.fnHelper(Arrays.asList(fun, arg),
					ClojureHelper.letHelper(
							LitInteger.clojureLit(
									ClojureHelper.applyClojureFunction(
											"reduce",
											"+",
											"0",
											ClojureHelper.applyClojureFunction(
													"map",
													ClojureHelper.fnHelper(
															Arrays.asList(x, y),
															ClojureHelper.applyClojureFunction(
																	"if",
																	ClojureHelper.applyClojureFunction(
																			".isEmpty",
																			ClojureHelper.applyClojureFunction(
																					"velka.types.Type/unifyRepresentation",
																					x,
																					y)),
																	"1",
																	"0")),
													funArgType,
													argType))),
							new Pair<String, String>(
									funArgType, 
									ClojureHelper.applyClojureFunction(
											".ltype",
											ClojureHelper.applyClojureFunction(
													ClojureCoreSymbols.getTypeClojureSymbol_full,
													fun))),
							new Pair<String, String>(
									argType,
									ClojureHelper.applyClojureFunction(
											ClojureCoreSymbols.getTypeClojureSymbol_full,
											arg))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol(conversionCostSym, NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Expression fun = args.get(0);
			Pair<Type, Substitution> funInfered = fun.infer(env, typeEnv);
			
			if(!(funInfered.first instanceof TypeArrow))
			{
				throw new AppendableException("First argument of "
						+ this.toString()
						+ " must be a function, got: "
						+ fun.toString()
						+ " infering to: "
						+ funInfered.first.toString()
						+ " in "
						+ "(" + this.toString() + " " + fun.toString() + " " + args.toString() + ")");
			}
			TypeTuple funArgsTypeTuple = (TypeTuple)((TypeArrow)funInfered.first).ltype;
			
			Expression applArgs = args.get(1);
			Pair<Type, Substitution> applArgsInfered = applArgs.infer(env, typeEnv);
			
			if(!(applArgsInfered.first instanceof TypeTuple)) {
				throw new AppendableException("Second argument of "
						+ this.toString()
						+ " must be an argument tuple, got: "
						+ applArgs.toString()
						+ " infering to: "
						+ applArgsInfered.first.toString()
						+ " in "
						+ "(" + this.toString() + " " + fun.toString() + " " + args.toString() + ")");
			}
			TypeTuple applArgsTypeTuple = (TypeTuple)(applArgsInfered.first);
			
			Iterator<Type> itFun = funArgsTypeTuple.iterator();
			Iterator<Type> itApplArgs = applArgsTypeTuple.iterator();
			
			int cost = 0;
			while(itFun.hasNext()) {
				Type funCurrent = itFun.next();
				Type applArgCurrent = itApplArgs.next();
				
				Optional<Substitution> o = Type.unifyRepresentation(funCurrent, applArgCurrent);
				if(o.isEmpty()) {
					cost++;
				}
			}
			
			return new LitInteger(cost);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeVariable B = new TypeVariable(NameGenerator.next());
			
			Type type = new TypeArrow(new TypeTuple(new TypeArrow(A, B), A), TypeAtom.TypeIntNative);
			
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "conversion-cost";
		}
	};
	
	/**
	 * Division (/) operator
	 */
	@VelkaOperator
	@Description("Divides _arg1_ by _arg2_. if _arg2_ evaluates to zero, causes exception.") 
	@Example("(/ 84 2) ; = 42") 
	@Syntax("(/ <arg1> <arg2)")
	public static final Operator Division = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value / arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "/";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String x = "_x";
//			String y = "_y";
//			String code = ClojureHelper.fnHelper(
//							Arrays.asList(x, y),
//							LitInteger.clojureLit(
//								ClojureHelper.applyClojureFunction("int",
//										ClojureHelper.applyClojureFunction("/",
//												ClojureHelper.getLiteralInnerValue(x),
//												ClojureHelper.getLiteralInnerValue(y)))));
//			
//			return code;
			return "/";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-division", NAMESPACE);
		}

	};
	
	/**
	 * Operator for addition of double values
	 */
	@VelkaOperator
	@Description("Adds two double numbers.") 
	@Example("(dadd 21.5 22.5) += 42.0") 
	@Syntax("(dadd <arg1> <arg2>)")
	public static final Operator DoubleAddition = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String d1 = "_d1";
//			String d2 = "_d2";
//			String code = ClojureHelper.fnHelper(
//					Arrays.asList(d1, d2), 
//					LitDouble.clojureLit("(+ (first " + d1 + ") (first " + d2 + "))"));
//			return code;
			return "+";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-double-add", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitDouble d1 = (LitDouble)args.get(0);
			LitDouble d2 = (LitDouble)args.get(1);
			
			double sum = d1.value + d2.value;
			
			return new LitDouble(sum);
		}
		
		@Override
		public String toString() {
			return "dadd";
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};
	
	/**
	 * Floating point division operator
	 */
	@VelkaOperator
	@Description("Divides _arg1_ by _arg2_. If _arg2_ evaluates to zero, causes exception.") 
	@Example("(ddiv 8.4 0.2) ;= 42.0") 
	@Syntax("(ddiv <arg1> <arg2>)")
	public static final Operator DoubleDivision = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String x = "_x";
//			String y = "_y";
//			String code = ClojureHelper.fnHelper(
//					Arrays.asList(x, y), 
//					LitDouble.clojureLit("(/ (first " + x + ") (first " + y + "))"));
//			return code;
			return "/";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-double-div", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitDouble d1 = (LitDouble)args.get(0);
			LitDouble d2 = (LitDouble)args.get(1);
			
			double rslt = d1.value / d2.value;
			
			return new LitDouble(rslt);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "ddiv";
		}
		
	};
	
	/**
	 * Operator for Double lesser than
	 */
	@VelkaOperator
	@Description("Comapres two doubles. Returns _true_  if _arg1_ is smaller or equal than _arg2_, otherwise returns _false_.") 
	@Example("(dlt 42.1 54.3) ; = #t\n"
					+ "(dlt 42.1 21.3) + = #f") 
	@Syntax("(dlt <arg1> <arg2>)")
	public static final Operator DoubleLesserThan = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String d1 = "_d1";
//			String d2 = "_d2";
//			String code = ClojureHelper.fnHelper(
//					Arrays.asList(d1, d2), 
//					LitBoolean.clojureLit("(< (first " + d1 + ") (first " + d2 + "))"));
//			
//			return code;
			return "<";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("double-lesser-than", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitDouble d1 = (LitDouble)args.get(0);
			LitDouble d2 = (LitDouble)args.get(1);
			
			if(d1.value < d2.value) {
				return LitBoolean.TRUE;
			}
			
			return LitBoolean.FALSE;
		}
		
		@Override
		public String toString() {
			return "dlt";
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};
	
	/**
	 * Equality operator
	 */
	@VelkaOperator
	@Description("Returns true if arguments are equal, otherwise returns false.") 
	@Example("(equals? 42 \"42\") ; = #f\n"
			+ "(equals? (cons 42 42) (cons 42 42)) ; = #t") 
	@Syntax("(equals? <arg1> <arg2>)")
	public static final Operator Equals = new Operator() {

		private final TypeArrow type = new TypeArrow(
				new TypeTuple(
						Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
				TypeAtom.TypeBoolNative);

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Expression arg0 = args.get(0);
			Expression arg1 = args.get(1);

			return arg0.equals(arg1) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "equals?";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			//return "(fn [_x _y] " + LitBoolean.clojureLit("(= (get _x 0) (get _y 0))") + ")";
			return  "=";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-equals", NAMESPACE);
		}

	};
	
	/**
	 * Operator for logging initialization
	 */
	@VelkaOperator
	@Description("Initializes logger, which will write to file specified by _name_. For logging messages see [log](#log).") 
	@Example(">(init-logger \"test-log.log\")\n"
					+ "[]")
	@Syntax("(init-logger <name>)")
	public static Operator InitLogger = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String name = "_name", logger = "_looger", rootLogger = "_rootLogger", consoleHandler = "_consolehandler", file = "_file", formatter = "_formatter";
			String code = 
					ClojureHelper.fnHelper(List.of(name),
							ClojureHelper.letHelper(
									ClojureHelper.applyClojureFunction("first", 
											ClojureHelper.applyClojureFunction("doall", 
													ClojureHelper.clojureVectorHelper(
															ClojureHelper.clojureIfHelper(
																	ClojureHelper.applyClojureFunction("instance?", "java.util.logging.ConsoleHandler", consoleHandler), 
																	ClojureHelper.applyClojureFunction(".removeHandler", rootLogger, consoleHandler), 
																	"nil"),
															ClojureHelper.applyClojureFunction(".setLevel", logger, "java.util.logging.Level/INFO"),
															ClojureHelper.letHelper(
																	ClojureHelper.applyClojureFunction("doall", 
																			ClojureHelper.clojureVectorHelper(
																					ClojureHelper.applyClojureFunction(".setFormatter", file, formatter),
																					ClojureHelper.applyClojureFunction(".addHandler", logger, file))), 
																	Pair.of(file, ClojureHelper.applyClojureFunction("java.util.logging.FileHandler.", name)),
																	Pair.of(formatter, ClojureHelper.applyClojureFunction("java.util.logging.XMLFormatter.")))))), 
									Pair.of(logger, ClojureHelper.applyClojureFunction("java.util.logging.Logger/getLogger", "java.util.logging.Logger/GLOBAL_LOGGER_NAME")),
									Pair.of(rootLogger, ClojureHelper.applyClojureFunction("java.util.logging.Logger/getLogger", ClojureHelper.stringHelper(""))),
									Pair.of(consoleHandler, ClojureHelper.applyClojureFunction("first", 
											ClojureHelper.applyClojureFunction(".getHandlers", rootLogger)))));

			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

			// suppress the logging output to the console
			Logger rootLogger = Logger.getLogger("");
			Handler[] handlers = rootLogger.getHandlers();
			if (handlers.length > 0 && handlers[0] instanceof ConsoleHandler) {
				rootLogger.removeHandler(handlers[0]);
			}

			logger.setLevel(Level.INFO);

			LitString name = (LitString) args.get(0);
			FileHandler file = null;

			try {
				file = new FileHandler(name.value);
			} catch (Exception e) {
				AppendableException ae = new AppendableException("Error initalizing logger in file " + name.value);
				ae.initCause(e);
				throw ae;
			}

			Formatter formatter = new XMLFormatter();
			file.setFormatter(formatter);
			logger.addHandler(file);

			return Expression.EMPTY_EXPRESSION;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeTuple.EMPTY_TUPLE);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "init-logger";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-init-logger", NAMESPACE);
		}

	};
	
	/**
	 * Operator for coercing int to double
	 */
	@VelkaOperator
	@Description("Coerces _arg_ to _Double:Native_ type.") 
	@Example("(int-to-double 42) ; = 42.0") 
	@Syntax("(int-to-double <arg>)")
	public static final Operator IntToDouble = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String i = "_i";
//			String code = ClojureHelper.fnHelper(
//					Arrays.asList(i), 
//					LitDouble.clojureLit("(double (first " + i + "))"));
//			return code;
			return "double";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("int-to-double-clj", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger i = (LitInteger)args.get(0);
			return new LitDouble((double)i.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "int-to-double";
		}
		
	};
	
	
	
	/**
	 * Floor operator
	 */
	@VelkaOperator
	@Description("Coerces _arg_ to _Double:Native_ type.") 
	@Example("(int-to-double 42) ; = 42.0") 
	@Syntax("(int-to-double <arg>)")
	public static final Operator Floor = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String d = "_d";
//			String code = ClojureHelper.fnHelper(
//					Arrays.asList(d),					
//					LitInteger.clojureLit("(int (Math/floor (first " + d + ")))"));
//			return code;
			return "int";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-floor", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitDouble d = (LitDouble)args.get(0);
			
			double floored = Math.floor(d.value);
			
			return new LitInteger((long)floored);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "floor";
		}
		
	};
	
	/**
	 * is-same-representation operator
	 */
	@VelkaOperator
	@Description("Returns _true_ if representations _A_ and _B_ can be unified. Otherwise returns _false_. This operator works on level of representations.\n"
			+ "For type level variant see [is-same-type](#isSameType).")
	@Example("(is-same-type 42 84) ; = true\n" + "(is-same-type 42 (construct Int String \"84\")) ; = false\n"
			+ "(is-same-type 42 \"84\") ; = false")
	@Syntax("(is-same-representation <arg1> <arg2>)")
	public static final Operator IsSameRepresentation = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Expression e1 = args.get(0);
			Expression e2 = args.get(1);

			Pair<Type, Substitution> p1 = e1.infer(env, typeEnv);
			Pair<Type, Substitution> p2 = e2.infer(env, typeEnv);

			if(Type.unifyRepresentation(p1.first, p2.first).isPresent()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String e1 = "e1", e2 = "e2", opt = "opt";
			String fn = ClojureHelper.fnHelper(List.of(e1, e2),
						ClojureHelper.letHelper(ClojureHelper.applyClojureFunction(".isPresent", opt), 
								Pair.of(opt, ClojureHelper.applyClojureFunction("velka.types.Type/unifyRepresentation", 
										ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, e1),
										ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, e2))))); 

			return fn;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow t = new TypeArrow(new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
					TypeAtom.TypeBoolNative);

			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "is-same-representation";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-is-same-representation", NAMESPACE);
		}

	};
	
	/**
	 * is-same-type operator
	 */
	@VelkaOperator
	@Description("Returns _true_ if types _A_ and _B_ can be unified. Otherwise returns _false_. This operator works on level of types, not taking type representations into account.\n"
			+ "For representation level variant see [is-same-representation](#isSameRepresentation).")
	@Example("(is-same-type 42 84) ; = true\n" + "(is-same-type 42 (construct Int String \"84\")) ; = true\n"
			+ "(is-same-type 42 \"84\") ; = false")
	@Syntax("(is-same-type <arg1> <arg2>)")
	public static final Operator IsSameType = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Expression e1 = args.get(0);
			Expression e2 = args.get(1);

			Pair<Type, Substitution> p1 = e1.infer(env, typeEnv);
			Pair<Type, Substitution> p2 = e2.infer(env, typeEnv);

			if(Type.unifyTypes(p1.first, p2.first).isPresent()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String e1 = "e1", e2 = "e2", opt = "opt";
			String fn = ClojureHelper.fnHelper(List.of(e1, e2),
					ClojureHelper.letHelper(ClojureHelper.applyClojureFunction(".isPresent", opt), 
							Pair.of(opt, ClojureHelper.applyClojureFunction("velka.types.Type/unifyTypes", 
									ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, e1),
									ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, e2)))));  

			return fn;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow t = new TypeArrow(new TypeTuple(
					Arrays.asList(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()))),
					TypeAtom.TypeBoolNative);

			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "is-same-type";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-is-same-type", NAMESPACE);
		}

	};
	
	/**
	 * Lesser than (<) operator
	 */
	@VelkaOperator
	@Description("Returns _true_ if first argument is lesser than second argument. Returns _false_ otherwise.") 
	@Example("(< 42 1) ; = #f") 
	@Syntax("(< <arg1> <arg2>)")
	public static final Operator LesserThan = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value < arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "<";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return "<";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-lesser-than", NAMESPACE);
		}
	};
	
	/**
	 * Log operator
	 */
	@VelkaOperator
	@Description("Logs a message. [init-logger](#initLogger) must be called before use of _log_.")
	@Example(">(init-logger \"test-log.log\")\n" + "[]\n" + ">(log \"test message\")\n" + "[]")
	@Syntax("(log <message>)")
	public static final Operator Log = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String msg = "_msg";
			String code = ClojureHelper.fnHelper(List.of(msg),
					ClojureHelper.applyClojureFunction("first",
							ClojureHelper.applyClojureFunction("doall", 
									ClojureHelper.clojureVectorHelper(
											Expression.EMPTY_EXPRESSION.toClojureCode(env, typeEnv),
											ClojureHelper.applyClojureFunction(".info", 
													ClojureHelper.applyClojureFunction("java.util.logging.Logger/getLogger", "java.util.logging.Logger/GLOBAL_LOGGER_NAME"),
													msg)))));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitString s = (LitString) args.get(0);
			Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
			logger.info(s.value);
			return Expression.EMPTY_EXPRESSION;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeTuple.EMPTY_TUPLE);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "log";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-log", NAMESPACE);
		}

	};
	
	@VelkaOperator
	@Description("Returns a remainder after an integer division.") 
	@Example("(mod 43 3) ; = 1") 
	@Syntax("(mod <arg1> <arg2>)")
	public static final Operator Modulo = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String i = "_i";
//			String j = "_j";
//			String code = ClojureHelper.fnHelper(
//					Arrays.asList(i, j), 
//					LitInteger.clojureLit(ClojureHelper.applyClojureFunction("mod", 
//							ClojureHelper.getLiteralInnerValue(i),
//							ClojureHelper.getLiteralInnerValue(j))));
//			return code;
			return "mod";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("integer-modulo", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger i = (LitInteger)args.get(0);
			LitInteger j = (LitInteger)args.get(1);
			
			long res = i.value % j.value;
						
			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "mod";
		}
	};
	
	/**
	 * Multiplication (*) operator
	 */
	@VelkaOperator
	@Description("Multiplies two integers.") 
	@Example("(* 6 7) ; = 42") 
	@Syntax("(* <arg1> <arg2>)")
	public static final Operator Multiplication = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value * arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "*";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String x = "_x";
//			String y = "_y";
//			String code = ClojureHelper.fnHelper(
//					Arrays.asList(x, y),
//					LitInteger.clojureLit(
//							ClojureHelper.applyClojureFunction("unchecked-multiply",
//									ClojureHelper.getLiteralInnerValue(x),
//									ClojureHelper.getLiteralInnerValue(y))));
//			
//			return code;
			return "unchecked-multiply";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-multiplication", NAMESPACE);
		}
	};
	/**
	 * Not operator
	 */
	@VelkaOperator
	@Description("Logical not of argument.")
	@Example("(not #t) ; = #f\n" + "(not (equals? 42 \"42\")) ; = #t")
	@Syntax("(not <arg>)")
	public static final Operator Not = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitBoolean arg = (LitBoolean) args.get(0);

			return arg.value ? LitBoolean.FALSE : LitBoolean.TRUE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		public String toString() {
			return "not";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			//return "(fn [_x] " + LitBoolean.clojureLit("(not (get _x 0))") + ")";
			return "not";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-not", NAMESPACE);
		}
	};
	
	/**
	 * Numeric equal (=) operator
	 */
	@VelkaOperator
	@Description("Compares two integers for equality.") 
	@Example("(= 42 42) ; = #t") 
	@Syntax("(= <arg1> <arg2>)")
	public static final Operator NumericEqual = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value == arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "=";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			//return "(fn [_x _y] " + LitBoolean.clojureLit("(= (get _x 0) (get _y 0))") + ")";
			return "=";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-numeric-equals", NAMESPACE);
		}

	};
	
	/**
	 * parse-int operator
	 */
	@VelkaOperator
	@Description("Tries to parse string into integer. Throws error if string cannot be parsed.") 
	@Example(">(parse-int \"42\") ;; = 42") 
	@Syntax("(parse-int <string>)")
	public static final Operator ParseInt = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String code = "(fn [_str] " + LitInteger.clojureLit("(Integer/parseInt (first _str))")
//					+ ")";
//			return code;
			String arg = "_arg";
			return ClojureHelper.fnHelper(List.of(arg), ClojureHelper.applyClojureFunction("Integer/parseInt", arg));
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitString arg = (LitString) args.get(0);

			int i;
			try {
				i = Integer.parseInt(arg.value);
			}
			catch(java.lang.NumberFormatException e) {
				throw e;
			}

			return new LitInteger(i);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "parse-int";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-parse-int", NAMESPACE);
		}

	};
	
	/**
	 * Println operator
	 */
	@VelkaOperator
	@Description("Prints its argument to standard output with endline and returns number of printed characters.") 
	@Example("(println \"foo\") ; prints \"foo\" and returns 5") 
	@Syntax("(println <arg>)")
	public static final Operator PrintlnOperator = new Operator() {

		private final TypeArrow type = new TypeArrow(
				new TypeTuple(Arrays.asList(new TypeVariable(NameGenerator.next()))), TypeAtom.TypeIntNative);

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Expression arg = (Expression) args.get(0);

			String s = arg.toString();
			if(arg instanceof LitInteropObject
					&& ((LitInteropObject)arg).type.equals(TypeAtom.TypeListNative)) {
				s = s.replace('[', '(').replace(']', ')').replace(",", "");
			}
			
			System.out.println(s);

			return new LitInteger(s.length());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "println";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String expr = "_expr";
			String str = "_str";
			return ClojureHelper.fnHelper(Arrays.asList(expr), 
					ClojureHelper.letHelper(
							LitInteger.clojureLit(
								ClojureHelper.applyClojureFunction("second",
										ClojureHelper.applyClojureFunction("doall",
												ClojureHelper.clojureVectorHelper(
														ClojureHelper.applyClojureFunction("clojure.core/println", str),
														ClojureHelper.applyClojureFunction("count", str))))), 
							new Pair<String, String>(str, ClojureHelper.applyClojureFunction("pr-str", expr)))
					);
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-println", NAMESPACE);
		}
	};
	
	/**
	 * Operator read-file
	 */
	@VelkaOperator
	@Description("Reads contents of file specified by _filename_ and returns it as string.")
	@Example(">(read-file \"foo.txt\")\n" + "\"foo bar baz\"")
	@Syntax("(read-file <filename>)")
	public static final Operator ReadFile = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String code = "(fn [_path] " + LitString.clojureLit("(slurp (first _path))") + ")";
//			return code;
			return "slurp";
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitString arg = (LitString) args.get(0);

			String content = "";
			try {
				content = Files.readString(Path.of(arg.value));
			} catch (IOException ioe) {
				AppendableException e = new AppendableException(ioe.getMessage());
				e.initCause(ioe);
				throw e;
			}

			return new LitString(content);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "read-file";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-read-file", NAMESPACE);
		}

	};
	
	/**
	 * str-split operator
	 */
	@VelkaOperator
	@Description("Splits _string_ by _by_ into a List:Native.")
	@Example(">(str-split \"foo;bar;baz\" \";\")\n" + "[\"foo\" [\"bar\" [\"baz\" []]]]")
	@Syntax("(str-split <string> <by>)")
	public static final Operator StrSplit = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String str = "_str", by = "_by";
			return ClojureHelper.fnHelper(List.of(str, by),
					ListNative.listNativeClojure(
						ClojureHelper.applyClojureFunction("clojure.string/split", 
								str, 
								ClojureHelper.applyClojureFunction("re-pattern", by))));
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitString lsStr = (LitString) args.get(0);
			LitString lsBy = (LitString) args.get(1);

			String[] splitted = lsStr.value.split(lsBy.value);
			LinkedList<Expression> l = new LinkedList<Expression>();
			for(String s : splitted) {
				LitString ls = new LitString(s);
				l.add(ls);
			}

			return new LitInteropObject(l, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "str-split";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-str-split", NAMESPACE);
		}

	};
	
	/**
	 * Subtraction (-) operator
	 */
	@VelkaOperator
	@Description("Subtracts _arg2_ from _arg1_.") 
	@Example("(- 43 1) ; = 42") 
	@Syntax("(- <arg1> <arg2>)")
	public static final Operator Subtraction = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value - arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "-";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String x = "_x";
//			String y = "_y";
//			String code = ClojureHelper.fnHelper(
//					Arrays.asList(x, y),
//					LitInteger.clojureLit(
//							ClojureHelper.applyClojureFunction("unchecked-subtract",
//									ClojureHelper.getLiteralInnerValue(x),
//									ClojureHelper.getLiteralInnerValue(y))));
//			
//			return code;
			return "unchecked-subtract";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-subtraction", NAMESPACE);
		}

	};
	
	
	
	/**
	 * Operator for current timestamp
	 */
	@VelkaOperator
	@Description("Returns current System/currentTimeMillis wrapped from java.") 
	@Example("(timestamp) ; = 1658062149471") 
	@Syntax("(timestamp)")
	public static Operator Timestamp = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String code = ClojureHelper.fnHelper(Arrays.asList(),
//					LitInteger.clojureLit("(System/currentTimeMillis)"));
//			return code;
			return ClojureHelper.fnHelper(List.of(), ClojureHelper.applyClojureFunction("System/currentTimeMillis"));
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return new LitInteger(System.currentTimeMillis());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "timestamp";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-timestamp", NAMESPACE);
		}

	};

	/**
	 * to-str operator
	 */
	@VelkaOperator
	@Description("Returns readable representation of its argument.")
	@Example(">(to-str 42)\n" + "\"42\"\n" + ">(to-str (construct List:Native))\n" + "\"[]\"")
	@Syntax("(to-str <expr>)")
	public static final Operator ToStr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String code = "(fn [_arg] " + LitString.clojureLit("(str _arg)") + ")";
//			return code;
			return "str";
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Expression e = args.get(0);
			String s;
			
//			if(e instanceof LitInteger) {
//				s = Long.toString(((LitInteger)e).value);
//			}
//			else if(e instanceof LitDouble) {
//				s = Double.toString(((LitDouble)e).value);
//			}
//			else if(e instanceof LitBoolean) {
//				s = Boolean.toString(((LitBoolean)e).value);
//			}
//			else if(e instanceof LitString) {
//				s = ((LitString)e).value;
//			}
//			else {
				s = e.toString();
//			}
			
			return new LitString(s);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeVariable(NameGenerator.next())),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "to-str";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-to-str", NAMESPACE);
		}

	};
	
	/**
	 * Unsigned bit shift right (ushr) operator
	 */
	@VelkaOperator
	@Description(" Bitwise shift right, without sign-extension. ") 
	@Example("(ushr -1 3) ;; = 2305843009213693951") 
	@Syntax("(ushr <bits> <n>)")
	public static final Operator UnsignedBitShiftRight = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
//			String code = "(fn [_num _n] "
//					+ LitInteger.clojureLit("(unsigned-bit-shift-right (first _num) (first _n))") + ")";
//			return code;
			return "unsigned-bit-shift-right";
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger num = (LitInteger) args.get(0);
			LitInteger n = (LitInteger) args.get(1);

			long res = num.value >>> n.value;

			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "ushr";
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-unsigned-bit-shr", NAMESPACE);
		}

	};
	
	@VelkaOperator
	@Description("Returns string with type of its argument. This is NOT a special form and the argument will be evaluated.") 
	@Example("(type-str 1) ;; = \"Int:*\"") 
	@Syntax("(type-str <arg>)")
	public static final Operator typeStr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String arg = "_arg";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(arg),
					LitString.clojureLit(
						ClojureHelper.applyClojureFunction(
								".toString",
								ClojureHelper.applyClojureFunction(
										".removeRepresentationInformation",
										ClojureHelper.applyClojureFunction(
												ClojureCoreSymbols.getTypeClojureSymbol_full,
												arg)))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-type-str", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			Expression arg = args.get(0);
			
			Pair<Type, Substitution> p = arg.infer(env, typeEnv);
			
			return new LitString(p.first.removeRepresentationInformation().toString());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable tv = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(tv), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "type-str";
		}
		
	};
	
	@VelkaOperator
	@Description("Returns string with representation of its argument. This is NOT a special form and the argument will be evaluated.") 
	@Example("(representation-str 1) ;; = \"Int:Native\"") 
	@Syntax("(representation-str <arg>)")
	public static final Operator representationStr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String arg = "_arg";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(arg),
					LitString.clojureLit(
						ClojureHelper.applyClojureFunction(
								".toString",
								ClojureHelper.applyClojureFunction(
										ClojureCoreSymbols.getTypeClojureSymbol_full,
										arg))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-representation-str", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
Expression arg = args.get(0);
			
			Pair<Type, Substitution> p = arg.infer(env, typeEnv);
			
			return new LitString(p.first.toString());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable tv = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(tv), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "representation-str";
		}
		
	};
	
	@VelkaOperator
	@Description("Returns a string that is a substring of this string.") 
	@Example("(substr \"hamburger\" 4 8) ;; = \"urge\"") 
	@Syntax("(substr <str> <start-index> <end-index>)")
	public static final Operator substr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String str = "_str";
			String bgnIndex = "_bgn-index";
			String endIndex = "_endIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(str, bgnIndex, endIndex),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
									".substring",
									str,
									bgnIndex,
									endIndex)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-substr", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitString litStr = (LitString)args.get(0);
			LitInteger bgnIndex = (LitInteger)args.get(1);
			LitInteger endIndex = (LitInteger)args.get(2);
			
			String substr = litStr.value.substring((int)bgnIndex.value, (int)endIndex.value);
			
			return new LitString(substr);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeStringNative, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "substr";
		}
	};
	
	@VelkaOperator
	@Description("Returns the length of this string. The length is equal to the number of Unicode code units in the string.") 
	@Example("(strlen \"hamburger\") ;; = 9") 
	@Syntax("(strlen <str>)")
	public static final Operator strlen = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String str = "_str";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(str),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".length",
									str)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-strlen", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitString litStr = (LitString)args.get(0);
			return new LitInteger(litStr.value.length());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "strlen";
		}
	};
	
	public static final String defaultCostFunction = "default-cost-function";
	public static final String defaultCostFunction_full = ClojureHelper.fullyQualifySymbol(NAMESPACE, defaultCostFunction);

	/**
	 * Relative path to velka.clojure.operators file
	 */
	public static final Path VELKA_CLOJURE_OPERATORS_PATH = Paths.get("velka", "clojure");

	/**
	 * Name of the velka.clojure.operators file
	 */
	public static final Path VELKA_CLOJURE_OPERAOTRS_NAME = Paths.get("operators.clj");

	public static final String defaultCostFunctionDef_fn = "_fn";

	private static final String defaultCostFunctionDef_args = "_args";

	public static final String defaultCostFunctionDef =
	ClojureHelper.clojureDefnHelper(
			defaultCostFunction,
			Arrays.asList(defaultCostFunctionDef_fn),
				Type.addTypeMetaInfo(
					ClojureHelper.fnHelper(
							Arrays.asList("& " + defaultCostFunctionDef_args),
							ClojureHelper.applyVelkaFunction(
									conversionCostSym_full,
									defaultCostFunctionDef_fn,
									ClojureHelper.tupleHelper_str(defaultCostFunctionDef_args))),
				new TypeArrow(new TypeVariable(NameGenerator.next()), TypeAtom.TypeIntNative)));
	@Override
	public String getNamespace() {
		return NAMESPACE;
	}
	@Override
	public Path getPath() {
		return VELKA_CLOJURE_OPERATORS_PATH;
	}
	@Override
	public Path getFileName() {
		return VELKA_CLOJURE_OPERAOTRS_NAME;
	}
	@Override
	public void initTypes(TypeEnvironment typeEnv) throws DuplicateTypeDefinitionException {
		//No types to initialize
	}
	
	@Override
	protected String writeDefinitions(Class<?> clazz, String Namespace) {
		StringBuilder sb = new StringBuilder(super.writeDefinitions(clazz, Namespace));
		
		sb.append("\n" + Operators.defaultCostFunctionDef);
		
		return sb.toString();
	}
	
	private Operators() {}
	private static Operators instance = null;
	public static Operators singleton() {
		if(instance == null) {
			instance = new Operators();
		}
		return instance;
	}
}
