package velka.core.langbase;

import java.util.logging.ConsoleHandler;
import java.util.logging.FileHandler;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.logging.XMLFormatter;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JMod;
import com.sun.codemodel.JVar;

import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import velka.core.abstraction.Abstraction;
import velka.core.abstraction.Operator;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.JavaTypeSystem;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Functions;
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
	 * Addition (+) operator
	 */
	@VelkaOperator
	@Description("Adds two integers.") 
	@Example("(+ 21 21) ; = 42")
	@Syntax("(+ arg1 arg2)")
	public static final Operator Addition = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger x = (LitInteger) args.get(0);
			LitInteger y = (LitInteger) args.get(1);

			return new LitInteger(x.value + y.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "+";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {			
			var x = "_x";
			var y = "_y";
			
			return ClojureHelper.fnHelper(
					List.of(x, y),
					ClojureHelper.applyClojureFunction("int",
							ClojureHelper.applyClojureFunction("unchecked-add",
									x, y)));
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_addition", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).plus(mappedArgs.get(new Symbol("_1"))));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value & arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-and";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("bit-and");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_bit_and", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).band(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn("bit-not");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger l = (LitInteger) args.get(0);

			int ret = ~l.value;
			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-not";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_bit_not", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).complement());
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value | arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type t = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-or";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("bit-or");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_bit_or", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).bor(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("bit-shift-left");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger num = (LitInteger) args.get(0);

			LitInteger n;

			if (!(args.get(1) instanceof LitInteger)) {
				n = new LitInteger(1);
			} else {
				n = (LitInteger) args.get(1);
			}

			var res = num.value << n.value;

			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "shl";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_shr", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).shl(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("bit-shift-right");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger num = (LitInteger) args.get(0);
			LitInteger n = (LitInteger) args.get(1);

			var res = num.value >> n.value;

			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "shr";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_bit_shr", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).shr(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("bit-xor");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger val1 = (LitInteger) args.get(0);
			LitInteger val2 = (LitInteger) args.get(1);

			var ret = val1.value ^ val2.value;

			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "bit-xor";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_bit_xor", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).xor(mappedArgs.get(new Symbol("_1"))));
		}
	};
	
	/**
	 * car operator
	 */
	@VelkaOperator
	@Description("Extracts first value from a pair.") 
	@Example("(car (cons 42 \"42\")) ; = 42") 
	@Syntax("(car <arg>)")
	public static final Operator Car = new Operator() {
		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			TypeVariable left = new TypeVariable(NameGenerator.next());
			TypeVariable right = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeTuple(left, right)), left);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Tuple arg = (Tuple) args.get(0);

			return arg.get(0);
		}

		@Override
		public String toString() {
			return "car";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn("first");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_car", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).invoke("get").arg(JExpr.lit(0)));
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
		public Pair<Type, Substitution> infer(Environment env) {
			TypeVariable left = new TypeVariable(NameGenerator.next());
			TypeVariable right = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeTuple(left, right)), right);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Tuple arg = (Tuple) args.get(0);

			return arg.get(1);
		}

		@Override
		public String toString() {
			return "cdr";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn("second");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_cdr", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).invoke("get").arg(JExpr.lit(1)));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString arg0 = (LitString) args.get(0);
			LitString arg1 = (LitString) args.get(1);

			return new LitString(arg0.value + arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("str");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_concat", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).plus(mappedArgs.get(new Symbol("_1"))));
		}
	};
	
	public static final String conversionCostSym = "_converison_cost";
	public static final String conversionCostSym_full = ClojureHelper.fullyQualifySymbol(Operators.singleton().getNamespace(), conversionCostSym);

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
		+ "		(construct Int Roman \"XLII\"))) ")
	public static final Operator ConversionCost = new Operator() {
		
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			//TODO!
			String fun = "_fun";
			String arg = "_arg";
			String funArgType = "_funArgType";
			String argType = "_argType";
//			String x = "_x";
//			String y = "_y";
			
			String code = ClojureHelper.fnHelper(Arrays.asList(fun, arg),
					ClojureHelper.letHelper(
							ClojureHelper.applyClojureFunction(ClojureCoreSymbols.conversionCost_full, argType, funArgType, arg),
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
		public Symbol getInternalSymbol() {
			return new Symbol(conversionCostSym, Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression fun = args.get(0);
			Pair<Type, Substitution> funInfered = fun.infer(env);
						
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
			Pair<Type, Substitution> applArgsInfered = applArgs.infer(env);
			
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
			
			var cost = env.getTypeSystem().conversionCost(applArgsTypeTuple, funArgsTypeTuple, applArgs, env);
			
			return new LitDouble(cost);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeVariable B = new TypeVariable(NameGenerator.next());
			
			Type type = new TypeArrow(new TypeTuple(new TypeArrow(A, B), A), TypeAtom.TypeIntNative);
			
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "conversion-cost";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var typeSystem = JavaTypeSystem.codeInstance();
			var ftype = method.body().decl(TypeUtil.instance().typeArrowJType(), "ftype",
					JExpr.cast(TypeUtil.instance().typeArrowJType(), typeSystem.invoke("getType").arg(mappedArgs.get(new Symbol("_0")))));
			var fatype = method.body().decl(TypeUtil.instance().typeTupleJType(), "fatype",
					JExpr.cast(TypeUtil.instance().typeTupleJType(), ftype.ref("ltype")));
			var atype = method.body().decl(TypeUtil.instance().typeTupleJType(), "atype",
					JExpr.cast(TypeUtil.instance().typeTupleJType(), typeSystem.invoke("getType").arg(mappedArgs.get(new Symbol("_1")))));
			
			method.body()._return(
					typeSystem.invoke("conversionCost")
						.arg(atype)
						.arg(fatype)
						.arg(mappedArgs.get(new Symbol("_1")))
						.arg(JExpr._null()));			
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value / arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "/";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var x = "_x";
			var y = "_y";
			
			return ClojureHelper.fnHelper(
					List.of(x, y),
					ClojureHelper.applyClojureFunction("int",
							ClojureHelper.applyClojureFunction("/",
									x, y)));
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_division", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).div(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("+");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_double_add", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
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
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).plus(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("/");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_double_div", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitDouble d1 = (LitDouble)args.get(0);
			LitDouble d2 = (LitDouble)args.get(1);
			
			double rslt = d1.value / d2.value;
			
			return new LitDouble(rslt);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "ddiv";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).div(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("<");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_double_lesser_than", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
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
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).lt(mappedArgs.get(new Symbol("_1"))));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression arg0 = args.get(0);
			Expression arg1 = args.get(1);

			return arg0.equals(arg1) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "equalp";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return  ClojureHelper.binaryOperatorToFn("=");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_equals", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			Method mthd;
			try {
				mthd = Object.class.getMethod("equals", Object.class);
			} catch (NoSuchMethodException | SecurityException e) {
				throw new RuntimeException(e);
			}
			this.wrapNaryMethod(method, mthd, mappedArgs, 1);
		}
	};
	
	/** > operator */
	@VelkaOperator
	@Description("Returns _true_ if first argument is greater than to second argument. Returns _false_ otherwise.") 
	@Example("(> 42 1) ; = #t") 
	@Syntax("(> <arg1> <arg2>)")
	public static final Operator GreaaterThan = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value > arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return ">";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn(">");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_greater_than", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).gt(mappedArgs.get(new Symbol("_1"))));
		}
	};
	
	/** >= operator */
	@VelkaOperator
	@Description("Returns _true_ if first argument is greater than or equals to second argument. Returns _false_ otherwise.") 
	@Example("(>= 42 1) ; = #t") 
	@Syntax("(>= <arg1> <arg2>)")
	public static final Operator GreaaterThanOrEquals = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value >= arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return ">=";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn(">=");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_greater_than_or_equals", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).gte(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
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
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeTuple.EMPTY_TUPLE);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "init-logger";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_init_logger", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var loggerCl = CodeModelInstance.instance().ref(Logger.class);
			
			var logger = method.body().decl(loggerCl, "logger", loggerCl.staticInvoke("getLogger").arg(loggerCl.staticRef("GLOBAL_LOGGER_NAME")));
			var rootLogger = method.body().decl(loggerCl, "rootLogger", loggerCl.staticInvoke("getLogger").arg(JExpr.lit("")));
			
			var handlers = method.body().decl(CodeModelInstance.instance().ref(Handler.class).array(), "handlers",
					rootLogger.invoke("getHandlers"));
			
			method.body()._if(handlers.ref("length").gt(JExpr.lit(0)).cand(handlers.component(JExpr.lit(0))._instanceof(CodeModelInstance.instance()._ref(ConsoleHandler.class))))
				._then().add(rootLogger.invoke("removeHandler").arg(handlers.component(JExpr.lit(0))));
			
			method.body().add(logger.invoke("setLevel").arg(CodeModelInstance.instance().ref(Level.class).staticRef("INFO")));
			
			var name = method.body().decl(CodeModelInstance.instance().ref(String.class), "name", mappedArgs.get(new Symbol("_0")));
			
			var fileHandlerCl = CodeModelInstance.instance().ref(FileHandler.class);
			var file = method.body().decl(fileHandlerCl, "file", JExpr._null());
			
			var _try = method.body()._try();
			_try.body().assign(file, JExpr._new(fileHandlerCl).arg(name));
			var _catch = _try._catch(CodeModelInstance.instance().ref(Exception.class));
			var e = _catch.param("e");
			var runtimeExceptionCl = CodeModelInstance.instance()._ref(RuntimeException.class);
			var re = _catch.body().decl(runtimeExceptionCl, "re", JExpr._new(runtimeExceptionCl).arg(JExpr.lit("Error initalizing logger in file ").plus(name)));
			_catch.body().add(re.invoke("initCause").arg(e));
			
			var formatter = method.body().decl(CodeModelInstance.instance().ref(Formatter.class), "formatter", JExpr._new(CodeModelInstance.instance().ref(XMLFormatter.class)));
			method.body().add(file.invoke("setFormatter").arg(formatter));
			method.body().add(logger.invoke("addHandler").arg(file));				
			
			method.body()._return(CodeModelInstance.emptyExpression());
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn("double");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("int_to_double_clj", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger i = (LitInteger)args.get(0);
			return new LitDouble((double)i.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeDoubleNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "int-to-double";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()
				._return(mappedArgs.get(new Symbol("_0")).invoke("doubleValue"));
		}
	};
	
	
	
	/**
	 * Floor operator
	 */
	@VelkaOperator
	@Description("Floor operator.") 
	@Example("(floor 42.4) ; = 42.0") 
	@Syntax("(floor <arg>)")
	public static final Operator Floor = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn("int");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_floor", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitDouble d = (LitDouble)args.get(0);
			
			double floored = Math.floor(d.value);
			
			return new LitInteger(Double.valueOf(floored).intValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "floor";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()
					._return(CodeModelInstance.instance().ref(Integer.class).staticInvoke("valueOf")
							.arg(JExpr.cast(CodeModelInstance.instance().INT, CodeModelInstance.instance()
									.ref(Math.class).staticInvoke("floor").arg(mappedArgs.get(new Symbol("_0"))))));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression e1 = args.get(0);
			Expression e2 = args.get(1);

			Pair<Type, Substitution> p1 = e1.infer(env);
			Pair<Type, Substitution> p2 = e2.infer(env);

			if(Type.unifyRepresentation(p1.first, p2.first).isPresent()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String e1 = "e1", e2 = "e2", opt = "opt";
			String fn = ClojureHelper.fnHelper(List.of(e1, e2),
						ClojureHelper.letHelper(ClojureHelper.applyClojureFunction(".isPresent", opt), 
								Pair.of(opt, ClojureHelper.applyClojureFunction("velka.types.Type/unifyRepresentation", 
										ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, e1),
										ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, e2))))); 

			return fn;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_is_same_representation", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var t1 = method.body().decl(TypeUtil.instance().typeJType(), "t1",
					JavaTypeSystem.codeInstance().invoke("getType").arg(mappedArgs.get(new Symbol("_0"))));
			var t2 = method.body().decl(TypeUtil.instance().typeJType(), "t2",
					JavaTypeSystem.codeInstance().invoke("getType").arg(mappedArgs.get(new Symbol("_1"))));
			
			method.body()._return(
					TypeUtil.instance().typeJClass().staticInvoke("unifyRepresentation")
						.arg(t1).arg(t2)
						.invoke("isPresent"));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression e1 = args.get(0);
			Expression e2 = args.get(1);

			Pair<Type, Substitution> p1 = e1.infer(env);
			Pair<Type, Substitution> p2 = e2.infer(env);

			if(Type.unifyTypes(p1.first, p2.first).isPresent()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String e1 = "e1", e2 = "e2", opt = "opt";
			String fn = ClojureHelper.fnHelper(List.of(e1, e2),
					ClojureHelper.letHelper(ClojureHelper.applyClojureFunction(".isPresent", opt), 
							Pair.of(opt, ClojureHelper.applyClojureFunction("velka.types.Type/unifyTypes", 
									ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, e1),
									ClojureHelper.applyClojureFunction(ClojureCoreSymbols.getTypeClojureSymbol_full, e2)))));  

			return fn;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_is_same_type", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var t1 = method.body().decl(TypeUtil.instance().typeJType(), "t1",
					JavaTypeSystem.codeInstance().invoke("getType").arg(mappedArgs.get(new Symbol("_0"))));
			var t2 = method.body().decl(TypeUtil.instance().typeJType(), "t2",
					JavaTypeSystem.codeInstance().invoke("getType").arg(mappedArgs.get(new Symbol("_1"))));
			
			method.body()._return(
					TypeUtil.instance().typeJClass().staticInvoke("unifyTypes")
						.arg(t1).arg(t2)
						.invoke("isPresent"));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value < arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "<";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("<");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_lesser_than", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).lt(mappedArgs.get(new Symbol("_1"))));
		}
	};
	
	/** <= operator */
	@VelkaOperator
	@Description("Returns _true_ if first argument is lesser than or equal to second argument. Returns _false_ otherwise.") 
	@Example("(<= 42 1) ; = #f") 
	@Syntax("(<= <arg1> <arg2>)")
	public static final Operator LesserThanOrEquals = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value <= arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "<=";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("<=");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_lesser_than_or_equals", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).lte(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			String msg = "_msg";
			String code = ClojureHelper.fnHelper(List.of(msg),
					ClojureHelper.applyClojureFunction("first",
							ClojureHelper.applyClojureFunction("doall", 
									ClojureHelper.clojureVectorHelper(
											Expression.EMPTY_EXPRESSION.toClojureCode(env),
											ClojureHelper.applyClojureFunction(".info", 
													ClojureHelper.applyClojureFunction("java.util.logging.Logger/getLogger", "java.util.logging.Logger/GLOBAL_LOGGER_NAME"),
													msg)))));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString s = (LitString) args.get(0);
			Logger logger = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);
			logger.info(s.value);
			return Expression.EMPTY_EXPRESSION;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeTuple.EMPTY_TUPLE);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "log";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_log", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var loggerCl = CodeModelInstance.instance().ref(Logger.class);
			var logger = method.body().decl(loggerCl, "logger", loggerCl.staticInvoke("getLogger").arg(loggerCl.staticRef("GLOBAL_LOGGER_NAME")));
			method.body().add(logger.invoke("info").arg(mappedArgs.get(new Symbol("_0"))));
			
			method.body()._return(CodeModelInstance.emptyExpression());
		}
	};
	
	@VelkaOperator
	@Description("Returns a maximum of two integers.") 
	@Example("(max 43 3) ; = 43") 
	@Syntax("(max <arg1> <arg2>)")
	public static final Operator Max = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("max");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_integer_max", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger i = (LitInteger)args.get(0);
			LitInteger j = (LitInteger)args.get(1);
			
			return new LitInteger(Math.max(i.value, j.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "max";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(
					CodeModelInstance.instance().ref(Math.class).staticInvoke("max")
						.arg(mappedArgs.get(new Symbol("_0")))
						.arg(mappedArgs.get(new Symbol("_1"))));
		}
	};
	
	@VelkaOperator
	@Description("Returns a minimum of two integers.") 
	@Example("(min 43 3) ; = 3") 
	@Syntax("(min <arg1> <arg2>)")
	public static final Operator Min = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("min");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_integer_min", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger i = (LitInteger)args.get(0);
			LitInteger j = (LitInteger)args.get(1);
			
			return new LitInteger(Math.min(i.value, j.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "min";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {			
			method.body()._return(
					CodeModelInstance.instance().ref(Math.class).staticInvoke("min")
						.arg(mappedArgs.get(new Symbol("_0")))
						.arg(mappedArgs.get(new Symbol("_1"))));
		}
	};
	
	@VelkaOperator
	@Description("Returns a remainder after an integer division.") 
	@Example("(mod 43 3) ; = 1") 
	@Syntax("(mod <arg1> <arg2>)")
	public static final Operator Modulo = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var x = "_x";
			var y = "_y";
			
			return ClojureHelper.fnHelper(
					List.of(x, y),
					ClojureHelper.applyClojureFunction("int",
							ClojureHelper.applyClojureFunction("mod",
									x, y)));
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_integer_modulo", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger i = (LitInteger)args.get(0);
			LitInteger j = (LitInteger)args.get(1);
			
			var res = i.value % j.value;
						
			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "mod";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).mod(mappedArgs.get(new Symbol("_1"))));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value * arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "*";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var x = "_x";
			var y = "_y";
			
			return ClojureHelper.fnHelper(
					List.of(x, y),
					ClojureHelper.applyClojureFunction("int",
							ClojureHelper.applyClojureFunction("unchecked-multiply",
									x, y)));
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_multiplication", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).mul(mappedArgs.get(new Symbol("_1"))));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitBoolean arg = (LitBoolean) args.get(0);

			return arg.value ? LitBoolean.FALSE : LitBoolean.TRUE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative)), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		public String toString() {
			return "not";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn("not");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_not", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).not());
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return arg0.value == arg1.value ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "=";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("=");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_numeric_equals", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")).eq(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn("Integer/parseInt");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
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
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "parse-int";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_parse_int", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(CodeModelInstance.instance().ref(Integer.class).staticInvoke("parseInt").arg(mappedArgs.get(new Symbol("_0"))));
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
				new TypeTuple(Arrays.asList(TypeAtom.TypeStringNative)), TypeTuple.EMPTY_TUPLE);

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var arg = (LitString) args.get(0);
			
			System.out.println(arg.value);

			return Expression.EMPTY_EXPRESSION;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "println";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var str = "_str";
			return ClojureHelper.wrapVoidClojureOperatorToFn(1,
					ClojureHelper.fnHelper(List.of(str), 
							ClojureHelper.applyClojureFunction(".println",
									"System/out",
									str)));
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_println", Operators.singleton().getNamespace());
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body().add(
					CodeModelInstance.instance().ref(System.class).staticRef("out").invoke("println")
					.arg(mappedArgs.get(new Symbol("_0"))));
							
			method.body()._return(CodeModelInstance.emptyExpression());
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn("slurp");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
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
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "read-file";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_read_file", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var content = method.body().decl(CodeModelInstance.instance()._ref(String.class), "content", JExpr.lit(""));
			var _try = method.body()._try();
			_try.body().assign(content, CodeModelInstance.instance().ref(Files.class).staticInvoke("readString")
					.arg(CodeModelInstance.instance().ref(Path.class).staticInvoke("of").arg(mappedArgs.get(new Symbol("_0")))));
			var _catch = _try._catch(CodeModelInstance.instance().ref(IOException.class));
			var ioe = _catch.param("ioe");
			var reCl = CodeModelInstance.instance().ref(RuntimeException.class);
			var e = _catch.body().decl(reCl, "e", JExpr._new(reCl).arg(ioe.invoke("getMessage")));
			_catch.body().add(e.invoke("initCause").arg(ioe));
			_catch.body()._throw(e);
			
			method.body()._return(content);
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			String str = "_str", by = "_by";
			return ClojureHelper.fnHelper(List.of(str, by),
					ListNative.listNativeClojure(
						ClojureHelper.applyClojureFunction("clojure.string/split", 
								str, 
								ClojureHelper.applyClojureFunction("re-pattern", by))));
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitString lsStr = (LitString) args.get(0);
			LitString lsBy = (LitString) args.get(1);

			String[] splitted = lsStr.value.split(lsBy.value);
			var l = new ArrayList<Object>(splitted.length);
			for(String s : splitted) {
				l.add(s);
			}

			return new LitInteropObject(l, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative, TypeAtom.TypeStringNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "str-split";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_str_split", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			Method mthd;
			try {
				mthd = String.class.getMethod("split", String.class);
			} catch (NoSuchMethodException | SecurityException e) {
				throw new RuntimeException(e);
			}
			this.wrapNaryMethod(method, mthd, mappedArgs, 1);
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger arg0 = (LitInteger) args.get(0);
			LitInteger arg1 = (LitInteger) args.get(1);

			return new LitInteger(arg0.value - arg1.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) {
			Type type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative)),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "-";
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var x = "_x";
			var y = "_y";
			
			return ClojureHelper.fnHelper(
					List.of(x, y),
					ClojureHelper.applyClojureFunction("int",
							ClojureHelper.applyClojureFunction("unchecked-subtract",
									x, y)));
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_subtraction", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()
				._return(mappedArgs.get(new Symbol("_0")).minus(mappedArgs.get(new Symbol("_1"))));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.wrapClojureOperatorToFn(0, "System/currentTimeMillis");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			return new LitInteger(Long.valueOf(System.currentTimeMillis()).intValue());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "timestamp";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_timestamp", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()
				._return(
						CodeModelInstance.instance().ref(Long.class).staticInvoke("valueOf")
							.arg(CodeModelInstance.instance().ref(System.class).staticInvoke("currentTimeMillis"))
							.invoke("intValue"));
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn(".toString");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression e = args.get(0);
			String s;
			
			s = e.toString();
			
			return new LitString(s);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeVariable(NameGenerator.next())),
					TypeAtom.TypeStringNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "to-str";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_to_str", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			Method mthd;
			try {
				mthd = Object.class.getMethod("toString");
			} catch (NoSuchMethodException | SecurityException e) {
				throw new RuntimeException(e);
			}
			this.wrapNaryMethod(method, mthd, mappedArgs, 0);
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
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.binaryOperatorToFn("velka.util.BitwiseOperations/ushr");
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger num = (LitInteger) args.get(0);
			LitInteger n = (LitInteger) args.get(1);

			var res = num.value >>> n.value;

			return new LitInteger(res);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "ushr";
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_unsigned_bit_shr", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()
				._return(mappedArgs.get(new Symbol("_0")).shrz(mappedArgs.get(new Symbol("_1"))));
		}
	};
	
	@VelkaOperator
	@Description("Returns string with type of its argument. This is NOT a special form and the argument will be evaluated.") 
	@Example("(type-str 1) ;; = \"Int:*\"") 
	@Syntax("(type-str <arg>)")
	public static final Operator typeStr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_type_str", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			Expression arg = args.get(0);
			
			Pair<Type, Substitution> p = arg.infer(env);
			
			return new LitString(p.first.removeRepresentationInformation().toString());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable tv = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(tv), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "type-str";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var type = method.body().decl(TypeUtil.instance().typeJType(), "type", JExpr._null());
			
			var _try = method.body()._try();
			_try.body().assign(type, JavaTypeSystem.codeInstance()
					.invoke("getType").arg(mappedArgs.get(new Symbol("_0")))
					.invoke("removeRepresentationInformation"));
			
			var rteCl = CodeModelInstance.instance().ref(RuntimeException.class);
			var _catch = _try._catch(CodeModelInstance.instance().ref(AppendableException.class));
			var e = _catch.param("e");
			_catch.body()._throw(JExpr._new(rteCl).arg(e));
			
			method.body()
				._return(type.invoke("toString"));
		}
	};
	
	@VelkaOperator
	@Description("Returns string with representation of its argument. This is NOT a special form and the argument will be evaluated.") 
	@Example("(representation-str 1) ;; = \"Int:Native\"") 
	@Syntax("(representation-str <arg>)")
	public static final Operator representationStr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_representation_str", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			Expression arg = args.get(0);
			
			Pair<Type, Substitution> p = arg.infer(env);
			
			return new LitString(p.first.toString());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable tv = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(tv), TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "representation-str";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {			
			method.body()
				._return(JavaTypeSystem.codeInstance()
							.invoke("getType").arg(mappedArgs.get(new Symbol("_0")))
							.invoke("toString"));
		}
	};
	
	@VelkaOperator
	@Description("Returns a string that is a substring of this string.") 
	@Example("(substr \"hamburger\" 4 8) ;; = \"urge\"") 
	@Syntax("(substr <str> <start-index> <end-index>)")
	public static final Operator substr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_substr", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			LitString litStr = (LitString)args.get(0);
			LitInteger bgnIndex = (LitInteger)args.get(1);
			LitInteger endIndex = (LitInteger)args.get(2);
			
			String substr = litStr.value.substring((int)bgnIndex.value, (int)endIndex.value);
			
			return new LitString(substr);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeStringNative, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "substr";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			Method mthd;
			try {
				mthd = String.class.getMethod("substring", int.class, int.class);
			} catch (NoSuchMethodException | SecurityException e) {
				throw new RuntimeException(e);
			}
			this.wrapNaryMethod(method, mthd, mappedArgs, 2);
		}
	};
	
	@VelkaOperator
	@Description("Returns the length of this string. The length is equal to the number of Unicode code units in the string.") 
	@Example("(strlen \"hamburger\") ;; = 9") 
	@Syntax("(strlen <str>)")
	public static final Operator strlen = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			return ClojureHelper.unaryOperatorToFn(".length");
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_strlen", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			LitString litStr = (LitString)args.get(0);
			return new LitInteger(litStr.value.length());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "strlen";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			Method mthd;
			try {
				mthd = String.class.getMethod("length");
			} catch (NoSuchMethodException | SecurityException e) {
				throw new RuntimeException(e);
			}
			this.wrapNaryMethod(method, mthd, mappedArgs, 0);
		}
	};
	
	@VelkaOperator
	public static final Operator linFun = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var q = "_q";
			var k = "_k";
			var f = "_f";
			var x = "_x";
			var _rhis = "_rhis";
			var code = ClojureHelper.fnHelper(List.of(k, q),
					ClojureHelper.letHelper(
							ClojureHelper.reify(velka.types.typeSystem.VelkaAbstraction.class, 
									Pair.of("apply", Pair.of(List.of(_rhis, x), ClojureHelper.applyClojureFunction(".apply", f, 
											ClojureHelper.applyClojureFunction("first", x)))),
									Pair.of("getType", Pair.of(List.of(_rhis), new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), 
											TypeAtom.TypeDoubleNative).clojureTypeRepresentation()))), 
							Pair.of(f, ClojureHelper.applyClojureFunction("velka.util.Functions/linearFunction", k, q))));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_lin_fun", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var k = (LitDouble)args.get(0);
			var q = (LitDouble)args.get(1);
			
			final var f = Functions.linearFunction(k.value, q.value);
			
			var op = new Operator() {

				@Override
				protected String toClojureOperator(Environment env) throws AppendableException {
					return "";
				}

				@Override
				public Symbol getInternalSymbol() {
					return null;
				}

				@Override
				protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
					var x = (LitDouble)args.get(0);
					
					var r = f.apply(x.value);
					
					return new LitDouble(r);
				}

				@Override
				public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
					var type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative);
					return Pair.of(type, Substitution.EMPTY);
				}

				@Override
				protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {				
				}
				
			};
			
			return op;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative),
					new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "lin-fun";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var aClass = CodeModelInstance.instance().anonymousClass(VelkaAbstraction.class);
			aClass.method(JMod.PUBLIC, Type.class, "getType").body()
				._return(TypeUtil.instance().type2java(new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative)));
			
			var apply = aClass.method(JMod.PUBLIC, Object.class, "apply");
			
			var t = new Symbol("t");
			var innerArgs = Abstraction.convertAndDeclareParms(List.of(Pair.of(t, TypeAtom.TypeDoubleNative)), apply);
			
			var f = method.body().decl(CodeModelInstance.instance().ref(Function.class), "f", 
					CodeModelInstance.instance().ref(Functions.class).staticInvoke("linearFunction")
						.arg(mappedArgs.get(new Symbol("_0")))
						.arg(mappedArgs.get(new Symbol("_1"))));
			
			apply.body()._return(f.invoke("apply").arg(innerArgs.get(t)));
			
			method.body()._return(JExpr._new(aClass));
		}
	};
	
	@VelkaOperator
	public static final Operator linFunPoints = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var x1 = "_x1";
			var y1 = "_y1";
			var x2 = "_x2";
			var y2 = "_y2";
			var f = "_f";
			var x = "_x";
			var _rhis = "_rhis";
			var code = ClojureHelper.fnHelper(List.of(x1, y1, x2, y2),
					ClojureHelper.letHelper(
							ClojureHelper.reify(velka.types.typeSystem.VelkaAbstraction.class, 
									Pair.of("apply", Pair.of(List.of(_rhis, x), ClojureHelper.applyClojureFunction(".apply", f, 
											ClojureHelper.applyClojureFunction("first", x)))),
									Pair.of("getType", Pair.of(List.of(_rhis), new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), 
											TypeAtom.TypeDoubleNative).clojureTypeRepresentation()))), 
							Pair.of(f, ClojureHelper.applyClojureFunction("velka.util.Functions/linearFunctionFromPoints", x1, y1, x2, y2))));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_lin_fun_pts", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var x1 = (LitDouble)args.get(0);
			var y1 = (LitDouble)args.get(1);
			var x2 = (LitDouble)args.get(2);
			var y2 = (LitDouble)args.get(3);
			
			final var f = Functions.linearFunctionFromPoints(x1.value, y1.value, x2.value, y2.value);
			
			var op = new Operator() {

				@Override
				protected String toClojureOperator(Environment env) throws AppendableException {
					return "";
				}

				@Override
				public Symbol getInternalSymbol() {
					return null;
				}

				@Override
				protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
					var x = (LitDouble)args.get(0);
					
					var r = f.apply(x.value);
					
					return new LitDouble(r);
				}

				@Override
				public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
					var type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative);
					return Pair.of(type, Substitution.EMPTY);
				}

				@Override
				protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
				}
				
			};
			
			return op;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative, TypeAtom.TypeDoubleNative),
					new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative));
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "lin-fun-pts";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var aClass = CodeModelInstance.instance().anonymousClass(VelkaAbstraction.class);
			aClass.method(JMod.PUBLIC, Type.class, "getType").body()
				._return(TypeUtil.instance().type2java(new TypeArrow(new TypeTuple(TypeAtom.TypeDoubleNative), TypeAtom.TypeDoubleNative)));
			
			var apply = aClass.method(JMod.PUBLIC, Object.class, "apply");
			
			var t = new Symbol("t");
			var innerArgs = Abstraction.convertAndDeclareParms(List.of(Pair.of(t, TypeAtom.TypeDoubleNative)), apply);
			
			var f = method.body().decl(CodeModelInstance.instance().ref(Function.class), "f", 
					CodeModelInstance.instance().ref(Functions.class).staticInvoke("linearFunctionFromPoints")
						.arg(mappedArgs.get(new Symbol("_0")))
						.arg(mappedArgs.get(new Symbol("_1")))
						.arg(mappedArgs.get(new Symbol("_2")))
						.arg(mappedArgs.get(new Symbol("_3"))));
			
			apply.body()._return(f.invoke("apply").arg(innerArgs.get(t)));
			
			method.body()._return(JExpr._new(aClass));
		}
	};
	
	@VelkaOperator
	public static final Operator doall = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var arg = "_arg";
			var code = ClojureHelper.fnHelper(List.of(arg),
					ClojureHelper.applyClojureFunction("doall", arg));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("_velka_doall", Operators.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var arg = args.get(0);
			return arg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var tv = new TypeVariable(NameGenerator.next());
			var type = new TypeArrow(new TypeTuple(tv), tv);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "doall";
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(mappedArgs.get(new Symbol("_0")));
		}
	};
	
	/** Lists file names in folder */
	@VelkaOperator
	public static final Operator listFilepaths = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var path = "_path";
			var code = ClojureHelper.fnHelper(
					List.of(path),
					ClojureHelper.constructJavaClass(ArrayList.class,
							ClojureHelper.applyClojureFunction("map",
									"str",
									ClojureHelper.applyClojureFunction(".toList",
											ClojureHelper.applyClojureFunction("java.nio.file.Files/list", 
													ClojureHelper.applyClojureFunction("java.nio.file.Path/of",
															path,
															//Simulate empty varargs
															ClojureHelper.applyClojureFunction("into-array", 
																	"String",
																	"[]")))))));
			
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			String path = null;
			if(args.get(0) instanceof LitString l) {
				path = l.value;
			}
			
			List<Object> ret = null;
			try {
				
				ret = new ArrayList<Object>(Files.list(Path.of(path)).map(Path::toString).toList());
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
			
			return new LitInteropObject(ret, TypeAtom.TypeListNative);
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("list_filepaths", Operators.singleton().getNamespace());
		}

		@Override
		protected void modifyJavaMethod(JMethod method, Map<Symbol, JVar> mappedArgs) {
			var pathcl = CodeModelInstance.instance().ref(Path.class);
			var filescl = CodeModelInstance.instance().ref(Files.class);
			var lcl = CodeModelInstance.instance().ref(List.class);
			var alcl = CodeModelInstance.instance().ref(ArrayList.class);
			
			var path = method.body().decl(pathcl, "_path",
					pathcl.staticInvoke("of").arg(mappedArgs.get(new Symbol("_0"))));
			
			var list = method.body().decl(lcl, "_list", JExpr._null());
			
			var _try = method.body()._try();
			
			_try.body().assign(list,
					filescl.staticInvoke("list").arg(path)
					.invoke("map").arg(JExpr.direct("java.nio.file.Path::toString"))
					.invoke("toList"));
			
			var ioecl = CodeModelInstance.instance().ref(IOException.class);
			var rtecl = CodeModelInstance.instance().ref(RuntimeException.class);
			var _catch = _try._catch(ioecl);
			var _e = _catch.param("_e");
			_catch.body()._throw(JExpr._new(rtecl).arg(_e));
			
			method.body()._return(JExpr._new(alcl)
					.arg(list));			
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeStringNative), TypeAtom.TypeListNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "list-filepaths";
		}
		
	};
	
	@VelkaOperator
	public static final Operator strCmp = 
		Operator.wrapJavaMethod(String.class, "compareTo", "str-cmp", 
				Operators.singleton().getNamespace(), String.class);
	
	public static final String defaultCostFunction = "default-cost-function";
	public static final String defaultCostFunction_full = ClojureHelper.fullyQualifySymbol(Operators.singleton().getNamespace(), defaultCostFunction);

	/**
	 * Relative path to velka.clojure.operators file
	 */
	public static final Path VELKA_CLOJURE_OPERATORS_PATH = velka.core.util.Constants.LOCATION;

	/**
	 * Name of the velka.clojure.operators file
	 */
	public static final Path VELKA_CLOJURE_OPERAOTRS_NAME = Paths.get("operators");

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
	protected String clojureDefinitions(Class<?> clazz, String namespace) {
		StringBuilder sb = new StringBuilder(super.clojureDefinitions(clazz, namespace));
		
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
	@Override
	protected String name() {
		return "operators";
	}
}
