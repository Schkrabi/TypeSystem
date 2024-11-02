package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.List;

import velka.core.abstraction.Constructor;
import velka.core.abstraction.Conversion;
import velka.core.abstraction.Lambda;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.application.IfExpression;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitDouble;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.core.literal.Literal;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.BitSetHelper;
import velka.util.ClojureHelper;
import velka.util.Functions;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Header;
import velka.util.annotations.Name;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaConstructor;
import velka.util.annotations.VelkaConversion;
import velka.util.annotations.VelkaOperator;
import velka.util.annotations.VelkaOperatorBank;

/**
 * This class contains utilities to work with Bitset in velka
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
@VelkaOperatorBank
@Description("Operators for working with wrapped java.util.BitSet") 
@Header("Bit Set")
public class JavaBitSet extends OperatorBank {
	
	/**
	 * Clojure namespace for JavaArrayList
	 */
	public static final String NAMESPACE = "velka.clojure.bitSet";
	
	public static final Symbol constructorSymbol = new Symbol("velka-construct", NAMESPACE);
	
	
	@VelkaConstructor
	@Description("Constructs empty Set:BitSet.")
	@Name("Construct Empty bitset")
	@Syntax("(construct Set:BitSet)")
	public static final Constructor constructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String code = ClojureHelper.fnHelper(
							Arrays.asList(),
							ClojureHelper.applyClojureFunction("java.util.BitSet."));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			Expression e = new LitInteropObject(new BitSet(), TypeAtom.TypeSetBitSet);
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "construct Set BitSet";
		}
	};
	
	public static final Symbol nBitsConstructorSymbol = new Symbol("nbits-construct", NAMESPACE);
	
	@VelkaConstructor
	@Description("Creates a bit set whose initial size is large enough to explicitly represent bits with indices in the range 0 through nbits-1.")
	@Name("Construct bit set with capacity")
	@Syntax("(construct Set:BitSet <nbits>)")
	public static final Constructor nBitsConstructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String nbits = "_nbits";
			String code = ClojureHelper.fnHelper(
							Arrays.asList(nbits),
							ClojureHelper.applyClojureFunction(
									"java.util.BitSet.",
									nbits));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nBitsConstructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			LitInteger nbits = (LitInteger)args.get(0);
			Expression e = new LitInteropObject(new BitSet((int) nbits.value), TypeAtom.TypeSetBitSet);
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "construct Set BitSet";
		}		
	};
	
	@VelkaConstructor
	public static final Constructor copyConstructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			final String arg = "_arg";
			final String set = "_set";
			var code = ClojureHelper.fnHelper(List.of(arg),
					ClojureHelper.letHelper(set, 
							Pair.of(set, ClojureHelper.constructJavaClass(java.util.BitSet.class, ClojureHelper.applyClojureFunction(".length", arg))),
							Pair.of("tmp", ClojureHelper.applyClojureFunction(".or", set, arg))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("copy-construct", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			var bs = (java.util.BitSet)lio.javaObject;
			
			var ret = new java.util.BitSet(bs.length());
			ret.or(bs);
			
			return new LitInteropObject(ret, TypeAtom.TypeSetBitSet);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet), TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
	};
	
	/**
	 * Symbol for void and(BitSet set)
	 */
	private static final Symbol andSymbol = new Symbol("velka-and", NAMESPACE);
	public static final Symbol andSymbol_out = new Symbol("bit-set-and");

	/**
	 * Operator for void and(BitSet set)	
	 */
	@VelkaOperator
	@Description("Performs a logical AND of this target bit set with the argument bit set.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-and s1 s2)") 
	@Syntax("(bit-set-and <set1> <set2>)")
	public static final Operator and = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
								ClojureHelper.applyClojureFunction(
										".and",
										set1,
										set2),
								set1)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return andSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			
			LitInteropObject set2 = (LitInteropObject)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			bSet1.and(bSet2);
			
			return set1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeSetBitSet),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return andSymbol_out.toString();
		}
	};
	
	public static final Symbol andNotSymbol = new Symbol("andNot", NAMESPACE);
	public static final Symbol andNotSymbol_out = new Symbol("bit-set-and-not");
	
	@VelkaOperator
	@Description("Clears all of the bits in this BitSet whose corresponding bit is set in the specified BitSet.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-and-not s1 s2)") 
	@Syntax("(bit-set-and-not <set1> <set2>)")
	public static final Operator andNot = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
								ClojureHelper.applyClojureFunction(
										".andNot",
										set1,
										set2),
								set1)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return andNotSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			
			LitInteropObject set2 = (LitInteropObject)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			bSet1.andNot(bSet2);
			
			return set1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeSetBitSet),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		} 
		
		@Override
		public String toString() {
			return andNotSymbol_out.toString();
		}
	};
	
	public static final Symbol cardinalitySymbol = new Symbol("cardinality", NAMESPACE);
	public static final Symbol cardinalitySymbol_out = new Symbol("bit-set-cardinality");
	
	@VelkaOperator
	@Description("Returns the number of bits set to true in this BitSet.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-cardinality s1)") 
	@Syntax("(bit-set-cardinality <set>)")
	public static final Operator cardinality = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".cardinality",
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return cardinalitySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set = (LitInteropObject)args.get(0);
			
			BitSet bSet = (BitSet)set.javaObject;
			
			long card = (long)bSet.cardinality();
			
			return new LitInteger(card);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet),
					TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return cardinalitySymbol_out.toString();
		}
	};
	
	public static final Symbol clearSymbol = new Symbol("clear", NAMESPACE);
	public static final Symbol clearSymbol_out = new Symbol("bit-set-clear");
	
	@VelkaOperator
	@Description("Sets all of the bits in this BitSet to false.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-clear s1)") 
	@Syntax("(bit-set-clear <set>)")
	public static final Operator clear = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String code =
					ClojureHelper.fnHelper(
							Arrays.asList(set),
							ClojureHelper.applyClojureFunction(
									"second",
									ClojureHelper.clojureVectorHelper(
											ClojureHelper.applyClojureFunction(
													".clear",
													set),
											set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return clearSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set = (LitInteropObject)args.get(0);
			BitSet bSet = (BitSet)set.javaObject;
			bSet.clear();
			return set;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet), TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override 
		public String toString() {
			return clearSymbol_out.toString();
		}
	};
	
	public static final Symbol clearBitIndexSymbol = new Symbol("clear-bit-index", NAMESPACE);
	public static final Symbol clearBitIndexSymbol_out = new Symbol("bit-set-clear-bit-index");
	
	@VelkaOperator
	@Description("Sets the bit specified by the index to false.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-clear-bit-index s1 3)") 
	@Syntax("(bit-set-clear-bit-index <set> <index>)")
	public static final Operator clearBitIndex = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String bitIndex = "_bit-index";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, bitIndex),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".clear",
											set,
											bitIndex),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return clearBitIndexSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set = (LitInteropObject)args.get(0);
			LitInteger bitIndex = (LitInteger)args.get(1);
			BitSet bSet = (BitSet)set.javaObject;
			bSet.clear((int)bitIndex.value);
			return set;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return clearBitIndexSymbol_out.toString();
		}
	};
	
	public static final Symbol clearIntervalSymbol = new Symbol("clear-interval", NAMESPACE);
	public static final Symbol clearIntervalSymbol_out = new Symbol("bit-set-clear-interval");
	
	@VelkaOperator
	@Description("Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to false.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-clear-interval s1 2 4)") 
	@Syntax("(bit-set-clear-interval <set> <fromIndex> <toIndex>)")
	public static final Operator clearInterval = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String toIndex = "_toIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex, toIndex),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".clear",
											set,
											fromIndex,
											toIndex),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return clearIntervalSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set = (LitInteropObject)args.get(0);
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			BitSet bSet = (BitSet)set.javaObject;
			bSet.clear((int)fromIndex.value, (int)toIndex.value);
			return set;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return clearIntervalSymbol_out.toString();
		}		
	};
	
	public static final Symbol cloneSymbol = new Symbol("velka-clone", NAMESPACE);
	public static final Symbol cloneSymbol_out = new Symbol("bit-set-clone");
	
	@VelkaOperator
	@Description("Cloning this BitSet produces a new BitSet that is equal to it.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-clone s1)")
	@Syntax("(bit-set-clone <set>)")
	public static final Operator clone = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
							ClojureHelper.applyClojureFunction(
									".clone",
									set));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return cloneSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set = (LitInteropObject)args.get(0);
			BitSet bSet = (BitSet)set.javaObject;
			return new LitInteropObject(bSet.clone(), TypeAtom.TypeSetBitSet);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return cloneSymbol_out.toString();
		}
		
	};
	
	public static final Symbol equalsSymbol = new Symbol("velka-equal", NAMESPACE);
	public static final Symbol equalsSymbol_out = new Symbol("bit-set-equalp");
	
	@VelkaOperator
	@Description("Compares this object against the specified object.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-equalp s1 s2)")
	@Syntax("(bit-set-equalp <set1> <set2>)")
	public static final Operator equals = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".equals",
									set1,
									set2)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return equalsSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			
			LitInteropObject set2 = (LitInteropObject)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			return bSet1.equals(bSet2) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeSetBitSet),
					TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return equalsSymbol_out.toString();
		}
	};
	
	public static final Symbol flipSymbol = new Symbol("flip", NAMESPACE);
	public static final Symbol flipSymbol_out = new Symbol("bit-set-flip");
	
	@VelkaOperator
	@Description("Sets the bit at the specified index to the complement of its current value.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-flip s1 3)")
	@Syntax("(bit-set-flip <set> <bitIndex>)")
	public static final Operator flip = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String bitIndex = "_bitIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, bitIndex),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".flip",
											set,
											bitIndex),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return flipSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set = (LitInteropObject)args.get(0);
			BitSet bSet = (BitSet)set.javaObject;
			
			LitInteger bitIndex = (LitInteger)args.get(1);
			
			bSet.flip((int)bitIndex.value);
			return set;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return flipSymbol_out.toString();
		}
	};
	
	public static final Symbol flipIntervalSymbol = new Symbol("flip-interval", NAMESPACE);
	public static final Symbol flipIntervalSymbol_out = new Symbol("bit-set-flip-interval");
	
	@VelkaOperator
	@Description("Sets each bit from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to the complement of its current value.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-flip-interval s1 3 5)")
	@Syntax("(bit-set-flip-interval <set> <fromIndex> <toIndex>)")
	public static final Operator flipInterval = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String toIndex = "_toIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex, toIndex),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".flip",
											set,
											fromIndex,
											toIndex),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return flipIntervalSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set = (LitInteropObject)args.get(0);
			BitSet bSet = (BitSet)set.javaObject;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			
			bSet.flip((int)fromIndex.value, (int)toIndex.value);
			return set;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return flipIntervalSymbol_out.toString();
		}
	};
	
	public static final Symbol getSymbol = new Symbol("velka-get", NAMESPACE);
	public static final Symbol getSymbol_out = new Symbol("bit-set-get");
	
	@VelkaOperator
	@Description("Returns the value of the bit with the specified index.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-get s1 3)") 
	@Syntax("(bit-set-get <set> <index>)")
	public static final Operator get = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String index = "_index";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, index),
					LitBoolean.clojureLit(
					ClojureHelper.applyClojureFunction(
							".get",
							set,
							index)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return getSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set = (LitInteropObject)args.get(0);
			BitSet bSet = (BitSet)set.javaObject;
			
			LitInteger index = (LitInteger)args.get(1);
			return bSet.get((int)index.value) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative),
					TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return getSymbol_out.toString();
		}
	};
	
	public static final Symbol getIntervalSymbol = new Symbol("get-interval", NAMESPACE);
	public static final Symbol getIntervalSymbol_out = new Symbol("bit-set-get-interval");
	
	@VelkaOperator
	@Description("Returns a new BitSet composed of bits from this BitSet from fromIndex (inclusive) to toIndex (exclusive).") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-get-interval s1 2 4)") 
	@Syntax("(bit-set-get-interval <set> <fromIndex> <toIndex>)")
	public static final Operator getInterval = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String toIndex = "_toIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex, toIndex),
							ClojureHelper.applyClojureFunction(
									".get",
									set,
									fromIndex,
									toIndex));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return getIntervalSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set = (LitInteropObject)args.get(0);
			BitSet bSet = (BitSet)set.javaObject;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			return new LitInteropObject(bSet.get((int) fromIndex.value, (int) toIndex.value),
					TypeAtom.TypeSetBitSet);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return getIntervalSymbol_out.toString();
		}
	};
	
	public static final Symbol intersectsSymbol = new Symbol("intersects", NAMESPACE);
	public static final Symbol intersectsSymbol_out = new Symbol("bit-set-intersects");
	
	@VelkaOperator
	@Description("Returns true if the specified BitSet has any bits set to true that are also set to true in this BitSet.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-intersects s1 s2)") 
	@Syntax("(bit-set-intersects <set1> <set2>)")
	public static final Operator intersects = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".intersects",
									set1,
									set2)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return intersectsSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			
			LitInteropObject set2 = (LitInteropObject)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			return bSet1.intersects(bSet2) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeSetBitSet),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return intersectsSymbol_out.toString();
		}
	};
	
	public static final Symbol isEmptySymbol = new Symbol("velka-is-empty", NAMESPACE);
	public static final Symbol isEmptySymbol_out = new Symbol("bit-set-is-empty");
	
	@VelkaOperator
	@Description("Returns true if this BitSet contains no bits that are set to true.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-is-empty s1)\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-is-empty s1)") 
	@Syntax("(bit-set-is-empty <set>)")
	public static final Operator isEmpty = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".isEmpty",
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return isEmptySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return bSet1.isEmpty() ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet),
					TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return isEmptySymbol_out.toString();
		}
	};
	
	public static final Symbol lengthSymbol = new Symbol("velka-length", NAMESPACE);
	public static final Symbol lengthSymbol_out = new Symbol("bit-set-length");
	
	@VelkaOperator
	@Description("Returns the \"logical size\" of this BitSet: the index of the highest set bit in the BitSet plus one.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-length s1);; = 5") 
	@Syntax("(bit-set-length <set>)")
	public static final Operator length = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".length",
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return lengthSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.length());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet),
					TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return lengthSymbol_out.toString();
		}		
	};
	
	public static final Symbol nextClearBitSymbol = new Symbol("next-clear-bit", NAMESPACE);
	public static final Symbol nextClearBitSymbol_out = new Symbol("bit-set-next-clear-bit");
	
	//TODO Example
	@VelkaOperator
	@Description("Returns the index of the first bit that is set to false that occurs on or after the specified starting index.")  
	@Syntax("(bit-set-next-clear-bit <set> <fromIndex>)")
	public static final Operator nextClearBit = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".nextClearBit",
									set,
									fromIndex)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextClearBitSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.nextClearBit((int)fromIndex.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return nextClearBitSymbol_out.toString();
		}
	};
	
	public static final Symbol nextSetBitSymbol = new Symbol("next-set-bit", NAMESPACE);
	public static final Symbol nextSetBitSymbol_out = new Symbol("bit-set-next-set-bit");
	
	//TODO Example
	@VelkaOperator
	@Description("Returns the index of the first bit that is set to true that occurs on or after the specified starting index.") 
	@Syntax("(bit-set-next-set-bit <set> <fromIndex>)")
	public static final Operator nextSetBit = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".nextSetBit",
									set,
									fromIndex)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextSetBitSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.nextSetBit((int)fromIndex.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return nextSetBitSymbol_out.toString();
		}
	};
	
	public static final Symbol orSymbol = new Symbol("velka-or", NAMESPACE);
	public static final Symbol orSymbol_out = new Symbol("bit-set-or");
	
	@VelkaOperator
	@Description("Performs a logical OR of this bit set with the bit set argument.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-or s1 s2)") 
	@Syntax("(bit-set-or <set1> <set2>)")
	public static final Operator or = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".or",
											set1,
											set2),
									set1)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return orSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			
			LitInteropObject set2 = (LitInteropObject)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			bSet1.or(bSet2);
			
			return set1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeSetBitSet),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return orSymbol_out.toString();
		}
	};
	
	public static final Symbol previousClearBitSymbol = new Symbol("previous-clear-bit", NAMESPACE);
	public static final Symbol previousClearBitSymbol_out = new Symbol("bit-set-previous-clear-bit");
	
	//TODO Example
	@VelkaOperator
	@Description("Returns the index of the nearest bit that is set to false that occurs on or before the specified starting index.")  
	@Syntax("(bit-set-previous-clear-bit <set> <fromIndex>)")
	public static final Operator previousClearBit = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".previousClearBit",
									set,
									fromIndex)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return previousClearBitSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.previousClearBit((int)fromIndex.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return previousClearBitSymbol_out.toString();
		}
	};
	
	public static final Symbol previousSetBitSymbol = new Symbol("previous-set-bit", NAMESPACE);
	public static final Symbol previousSetBitSymbol_out = new Symbol("bit-set-previous-set-bit");
	
	//TODO Example
	@VelkaOperator
	@Description("Returns the index of the nearest bit that is set to true that occurs on or before the specified starting index.")  
	@Syntax("(bit-set-previous-set-bit <set> <fromIndex>)")
	public static final Operator previousSetBit = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".previousSetBit",
									set,
									fromIndex)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return previousSetBitSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.previousSetBit((int)fromIndex.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative),
					TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return previousSetBitSymbol_out.toString();
		}
	};
	
	public static final Symbol setSymbol = new Symbol("velka-set", NAMESPACE);
	public static final Symbol setSymbol_out = new Symbol("bit-set-set");
	
	@VelkaOperator
	@Description("Sets the bit at the specified index to true.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set s1 3)") 
	@Syntax("(bit-set-set <set> <bitIndex>)")
	public static final Operator set = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String bitIndex = "_bitIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, bitIndex),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".set",
											set,
											bitIndex),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return setSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			LitInteger bitIndex = (LitInteger)args.get(1);
			
			BitSet s = (BitSet)set1.javaObject;
			s.set((int)bitIndex.value);
			return set1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return setSymbol_out.toString();
		}
	};
	
	public static final Symbol setValueSymbol = new Symbol("set-value", NAMESPACE);
	public static final Symbol setValueSymbol_out = new Symbol("bit-set-set-value");
	
	@VelkaOperator
	@Description("Sets the bit at the specified index to the specified value.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-value s1 3 #t)") 
	@Syntax("(bit-set-set-value <set> <bitIndex> <value>)")
	public static final Operator setValue = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String bitIndex = "_bitIndex";
			String value = "_value";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, bitIndex, value),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".set",
											set,
											bitIndex,
											value),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return setValueSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			LitInteger bitIndex = (LitInteger)args.get(1);
			LitBoolean value = (LitBoolean)args.get(2);
			
			BitSet s = (BitSet)set1.javaObject;
			s.set((int)bitIndex.value, value.value);
			return set1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative, TypeAtom.TypeBoolNative),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return setValueSymbol_out.toString();
		}		
	};
	
	public static final Symbol setIntervalSymbol = new Symbol("set-interval", NAMESPACE);
	public static final Symbol setIntervalSymbol_out = new Symbol("bit-set-set-interval");
	
	@VelkaOperator
	@Description("Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to true.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 3 5)") 
	@Syntax("(bit-set-set-interval <set> <fromIndex> <toIndex>)")
	public static final Operator setInterval = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String toIndex = "_toIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex, toIndex),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".set",
											set,
											fromIndex,
											toIndex),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return setIntervalSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			
			BitSet s = (BitSet)set1.javaObject;
			s.set((int)fromIndex.value, (int)toIndex.value);
			return set1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return setIntervalSymbol_out.toString();
		}
	};
	
	public static final Symbol setIntervalValueSymbol = new Symbol("set-interval-value", NAMESPACE);
	public static final Symbol setIntervalValueSymbol_out = new Symbol("bit-set-set-interval-value");
	
	@VelkaOperator
	@Description("Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to the specified value.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval-value s1 3 5 #f)") 
	@Syntax("(bit-set-set-interval-value <set> <fromIndex> <toIndex> <value>)")	
	public static final Operator setIntervalValue = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String toIndex = "_toIndex";
			String value = "_value";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex, toIndex, value),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".set",
											set,
											fromIndex,
											toIndex,
											value),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return setIntervalValueSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			
			BitSet s = (BitSet)set1.javaObject;
			
			LitBoolean value = (LitBoolean)args.get(3);
			s.set((int)fromIndex.value, (int)toIndex.value, (boolean)value.value);
			return set1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative, TypeAtom.TypeBoolNative),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return setIntervalValueSymbol_out.toString();
		}
	};
	
	public static final Symbol sizeSymbol = new Symbol("velka-size", NAMESPACE);
	public static final Symbol sizeSymbol_out = new Symbol("bit-set-size");
	
	@VelkaOperator
	@Description("Returns the number of bits of space actually in use by this BitSet to represent bit values.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 3 5)\n"
					+ "(bit-set-size s1)") 
	@Syntax("(bit-set-size <set>)")
	public static final Operator size = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".size",
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return sizeSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			BitSet s = (BitSet)set1.javaObject;
			return new LitInteger(s.size());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet),
					TypeAtom.TypeIntNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return sizeSymbol_out.toString();
		}
	};
	
	public static final Symbol strSymbol = new Symbol("velka-str", NAMESPACE);
	public static final Symbol strSymbol_out = new Symbol("bit-set-str");
	
	@VelkaOperator
	@Description("Returns a string representation of this bit set.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 3 5)\n"
					+ "(bit-set-str s1)") 
	@Syntax("(bit-set-str <set>)")
	public static final Operator str = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
									".toString",
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return strSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			BitSet s = (BitSet)set1.javaObject;
			return new LitString(s.toString());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet),
					TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return strSymbol_out.toString();
		}		
	};
	
	public static final Symbol xorSymbol = new Symbol("velka-xor", NAMESPACE);
	public static final Symbol xorSymbol_out = new Symbol("bit-set-xor");
	
	@VelkaOperator
	@Description("Performs a logical XOR of this bit set with the bit set argument.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-xor s1 s2)") 
	@Syntax("(bit-set-xor <set1> <set2>)")
	public static final Operator xor = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
								ClojureHelper.applyClojureFunction(
										".xor",
										set1,
										set2),
								set1)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return xorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject set1 = (LitInteropObject)args.get(0);
			
			
			LitInteropObject set2 = (LitInteropObject)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			bSet1.xor(bSet2);
			
			return set1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeSetBitSet),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return xorSymbol_out.toString();
		}
	};
	
	public static final Symbol mapSymbol = new Symbol("velka-map", NAMESPACE);
	public static final Symbol mapSymbol_out = new Symbol("bit-set-map");
	
	@VelkaOperator
	@Description("Map function.") 
	@Example("(define s (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s 0 5)\n"
					+ "(bit-set-map s (lambda (x) (+ x 1)))") 
	@Syntax("(bit-set-map <set1> <fun>)")
	public static final Operator map = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var fun = "_fun";
			var set = "_set";
			var acc = "_acc";
			var idx = "_idx";
			var vl = "_vl";
			var tmp = "_tmp";
//			var code = ClojureHelper.fnHelper(
//					List.of(set, fun),
//					ClojureHelper.applyClojureFunction(
//							"map", fun,
//							ClojureHelper.applyClojureFunction("loop",
//									ClojureHelper.clojureVectorHelper(
//											acc, "'()",
//											idx, ClojureHelper.applyClojureFunction(".size", set)),
//									ClojureHelper.letHelper(
//											ClojureHelper.clojureIfHelper(
//													ClojureHelper.applyClojureFunction("=", vl, "-1"), 
//													acc, 
//													ClojureHelper.applyClojureFunction("recur", 
//															ClojureHelper.applyClojureFunction("conj", acc, vl),
//															ClojureHelper.applyClojureFunction("-", vl, "1"))), 
//											Pair.of(vl, ClojureHelper.applyClojureFunction(".previousSetBit", set, idx))))));
			var code = ClojureHelper.fnHelper(
					List.of(set, fun),
							ClojureHelper.applyClojureFunction("loop",
									ClojureHelper.clojureVectorHelper(
											acc, ClojureHelper.constructJavaClass(BitSet.class),
											idx, "0"),
									ClojureHelper.letHelper(
											ClojureHelper.clojureIfHelper(
													ClojureHelper.applyClojureFunction("=", vl, "-1"), 
													acc, 
													ClojureHelper.applyClojureFunction("recur", 
															ClojureHelper.letHelper(acc, 
																	Pair.of(tmp, ClojureHelper.applyClojureFunction(".set", 
																			acc, 
																			ClojureHelper.applyVelkaFunction(fun, vl)))),
															ClojureHelper.applyClojureFunction("+", vl, "1"))), 
											Pair.of(vl, ClojureHelper.applyClojureFunction(".nextSetBit", set, idx)))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return mapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var set = (LitInteropObject)args.get(0);
			var fun = args.get(1);
			var bSet = (BitSet)set.javaObject;
			
			var retSet = new BitSet();
			
			bSet.stream().forEach(i -> {
				var app = new AbstractionApplication(fun, new Tuple(new LitInteger((long)i)));
				try {
					var exp = app.interpret(env);
					if(exp instanceof LitInteger li) {
						retSet.set((int)li.value); 
					}
					else {
						throw new RuntimeException("Invalid mapping, got: " + exp);
					}
				}catch(Exception e) {
					throw new RuntimeException(e);
				}
			});
			
			return new LitInteropObject(retSet, TypeAtom.TypeSetBitSet);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet, new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeIntNative)),
					TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return mapSymbol_out.toString();
		}
		
	};
	
	public static Symbol containsAllSymbol = new Symbol("velka-contains-all", NAMESPACE);
	public static Symbol containsAllSymbol_out = new Symbol("bit-set-contains-all");
	
	@VelkaOperator
	public static final Operator containsAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var set = "_set";
			var lst = "_lst";
			var x = "_x";
			var y = "_y";
			var code = ClojureHelper.fnHelper(
					List.of(set, lst),
					ClojureHelper.applyClojureFunction(
							"reduce",
							ClojureHelper.fnHelper(
									List.of(x, y),
									ClojureHelper.applyClojureFunction("and", 
											x, 
											ClojureHelper.applyClojureFunction(".get", set, y))),
							"true",
							lst));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return containsAllSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var set = (LitInteropObject)args.get(0);
			var lst = (LitInteropObject)args.get(1);
			
			var bset = (java.util.BitSet)set.javaObject;
			@SuppressWarnings("unchecked")
			var l = (java.util.List<Expression>)lst.javaObject;
			
			var ret = l.stream().allMatch(e -> {
				if(e instanceof LitInteger li) {
					return bset.get((int)li.value);
				}
				throw new RuntimeException("Invalid set.");
			});
			
			
			return ret ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeListNative), TypeAtom.TypeBoolNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return containsAllSymbol_out.toString();
		}
	};
	
	public static Symbol setAllSymbol = new Symbol("set-all", NAMESPACE);
	public static Symbol setAllSymbol_out = new Symbol("bit-set-set-all");
	
	@VelkaOperator
	public static Operator setAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var set = "_set";
			var lst = "_lst";
			var x = "_x";
			var code = ClojureHelper.fnHelper(
					List.of(set, lst),
					ClojureHelper.letHelper(
							set,
							Pair.of("tmp", ClojureHelper.applyClojureFunction("doall", 
									ClojureHelper.applyClojureFunction("map", 
											ClojureHelper.fnHelper(List.of(x), ClojureHelper.applyClojureFunction(".set", set, x)),
													lst)))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return setAllSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var set = (LitInteropObject)args.get(0);
			var lst = (LitInteropObject)args.get(1);
			
			var bset = (java.util.BitSet)set.javaObject;
			@SuppressWarnings("unchecked")
			var l = (java.util.List<Expression>)lst.javaObject;
			
			l.stream().forEach(e ->{
				if(e instanceof LitInteger li) {
					bset.set((int)li.value);
				}
			});
			
			return set;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet, TypeAtom.TypeListNative), TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return setAllSymbol_out.toString();
		}
		
	};
	
	@VelkaOperator
	public static final Operator fromList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var lst = "_lst";
			var set = "_set";
			var e = "_e";
			var code = ClojureHelper.fnHelper(List.of(lst),
					ClojureHelper.letHelper(set,
							Pair.of(set,
									ClojureHelper.constructJavaClass(java.util.BitSet.class,
											ClojureHelper.applyClojureFunction("count", lst))),
							Pair.of("tmp",
									ClojureHelper.applyClojureFunction("doall",
											ClojureHelper.applyClojureFunction("map",
													ClojureHelper.fnHelper(List.of(e),
															ClojureHelper.applyClojureFunction(".set", set, e)),
													lst)))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("from-list", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lst = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l = (java.util.List<Expression>)lst.javaObject;
			
			var set = new java.util.BitSet(l.size());
			l.stream().forEach(e ->{
				if(e instanceof LitInteger li) {
					set.set((int)li.value);
					return;
				}
				throw new RuntimeException("All elements of initialization list must be Int:Native, got " + e);
			}
			);
			
			return new LitInteropObject(set, TypeAtom.TypeSetBitSet);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "bit-set-from-list";                                                                                                                                         
		}
		
	};
	
	@VelkaOperator
	public static final Operator toList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var set = "_set";
			var i = "_i";
			var acc = "_acc";
			var code = ClojureHelper.fnHelper(
					List.of(set),
					ClojureHelper.applyClojureFunction("loop", 
							ClojureHelper.clojureVectorHelper(i, ClojureHelper.applyClojureFunction(".nextSetBit", set, "0"), 
									acc, ClojureHelper.tupleHelper()),
							ClojureHelper.clojureIfHelper(
									ClojureHelper.applyClojureFunction("<", i, "0"), 
									ClojureHelper.applyClojureFunction("seq", acc), 
									ClojureHelper.applyClojureFunction("recur", 
											ClojureHelper.applyClojureFunction(".nextSetBit", set, ClojureHelper.applyClojureFunction("+", i, "1")),
											ClojureHelper.applyClojureFunction("conj", acc, i)))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("to-list", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			var bset = (java.util.BitSet)lio.javaObject;
			
			var l = new ArrayList<Expression>();
			
			int i = bset.nextSetBit(0);
			while(i >= 0) {
				l.add(new LitInteger((long)i));
				
				i = bset.nextSetBit(i + 1);
			}
			
			return new LitInteropObject(l, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet), TypeAtom.TypeListNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "bit-set-to-list";
		}
		
	};
	
	@VelkaConversion
	@Description("Converts Set:BitSet into Set:Tree.") 
	@Example("(convert Set:BitSet Set:Tree (bit-set-set (bit-set-set (bit-set-set (construct Set:BitSet) 3) 6) 9))") 
	@Syntax("(convert Set:BitSet Set:Tree <arg>)")
	public static final Conversion bitSetToTreeSet = new Conversion() {

		Double costX1 = 0d;
		Double costY1 = 0.8d;
		Double costX2 = 1000d;
		Double costY2 = 0.5d;
		
		@Override
		public Expression cost() {
			final var f = Functions.linearFunctionFromPoints(costX1, costY1, costX2, costY2);
			var l = new Operator() {

				@Override
				protected String toClojureOperator(Environment env) throws AppendableException {
					var arg = "_arg";
					var code = ClojureHelper.fnHelper(List.of(arg),
							ClojureHelper.applyClojureFunction("min", costY1.toString(),
									ClojureHelper.applyClojureFunction("max", costY2.toString(),
											ClojureHelper.applyClojureFunction(".apply",
													ClojureHelper.applyClojureFunction(
															"velka.util.Functions/linearFunctionFromPoints",
															costX1.toString(), costY1.toString(), costX2.toString(),
															costY2.toString()),
													ClojureHelper.applyClojureFunction("double", ClojureHelper.applyClojureFunction(".cardinality", arg))))));
					return code;
				}

				@Override
				public Symbol getClojureSymbol() {
					return new Symbol(NameGenerator.next());
				}

				@Override
				protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
					var lio = (LitInteropObject)args.get(0);
					var set = (java.util.BitSet)lio.javaObject;
					
					var cost = Math.max(0.5d, Math.min(0.8d, f.apply((double) set.cardinality())));
					
					return new LitDouble(cost);
				}

				@Override
				public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
					var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet), TypeAtom.TypeDoubleNative);
					return Pair.of(type, Substitution.EMPTY);
				}
				
			};
			
			return l;
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			final var arg = "_arg";
			final var code = ClojureHelper.fnHelper(
					List.of(arg),
					ClojureHelper.applyClojureFunction("velka.util.BitSetHelper/bitset2treeset", arg)); 
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("bit-set-2-tree-set", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			var bitSet = (java.util.BitSet)lio.javaObject;
			
			var treeSet = new java.util.TreeSet<Object>(new java.util.Comparator<Object>() {

				@Override
				public int compare(Object o1, Object o2) {
					try {
						var arg1 = Literal.objectToLiteral(o1);
						var arg2 = Literal.objectToLiteral(o2);
						var a1 = new Symbol(NameGenerator.next());
						var a2 = new Symbol(NameGenerator.next());
						
						var cmp = new Lambda(new Tuple(a1, a2),
								new TypeTuple(TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
								new velka.core.application.IfExpression(
										new velka.core.application.AbstractionApplication(Operators.Equals,
												new Tuple(a1, a2)),
										new LitInteger(0),
										new IfExpression(
												new AbstractionApplication(Operators.LesserThan, new Tuple(a1, a2)),
												new LitInteger(-1), new LitInteger(1))));
						
						var appl = new velka.core.application.AbstractionApplication(cmp,
								new Tuple(arg1, arg2));

						var ret = appl.interpret(env);

						if (ret instanceof LitInteger li) {
							return (int) li.value;
						}
						throw new RuntimeException("Invalid result of comparator " + ret);

					} catch (AppendableException ae) {
						throw new RuntimeException(ae);
					}
				}
				
			});
			
			bitSet.stream().forEach(x -> treeSet.add(Long.valueOf(x)));
			
			return new LitInteropObject(treeSet, TypeAtom.TypeSetTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet), TypeAtom.TypeSetTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
	};
	
	@VelkaConversion
	public static Conversion toHashSet = new Conversion() {

		@Override
		public Expression cost() {
			var arg = new Symbol(NameGenerator.next());
			
			var cost = new Lambda(
					new Tuple(arg), 
					new TypeTuple(TypeAtom.TypeSetHash),
					new AbstractionApplication(
							new AbstractionApplication(
									Operators.linFunPoints, 
									new Tuple(new LitDouble(0d), new LitDouble(0.8d), new LitDouble(1000d), new LitDouble(0.5d))), 
							new Tuple(
									new AbstractionApplication(Operators.IntToDouble,
											new Tuple(new AbstractionApplication(HashSet.size, new Tuple(arg)))))));
			return cost;
		}

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var bs = "_bit-set";
			var code = ClojureHelper.fnHelper(
					List.of(bs),
					ClojureHelper.applyClojureFunction("velka.util.BitSetHelper/bitset2hashset", bs));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("bit-set-2-hash-set", NAMESPACE);
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			var bs = (java.util.BitSet)lio.javaObject;
			
			var hs = BitSetHelper.bitset2hashset(bs);
			
			return new LitInteropObject(hs, TypeAtom.TypeSetHash);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet), TypeAtom.TypeSetHash);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "bit-set-2-hash-set";
		}
	};
	
	public static final Path VELKA_CLOJURE_BITSET_PATH = Paths.get("velka", "clojure");
	public static final Path VELKA_CLOJURE_BITSET_NAME = Paths.get("bitSet.clj");
	public static final Path RELATIVE_PATH = VELKA_CLOJURE_BITSET_PATH.resolve(VELKA_CLOJURE_BITSET_NAME);

	@Override
	public String getNamespace() {
		return NAMESPACE;
	}

	@Override
	public Path getPath() {
		return VELKA_CLOJURE_BITSET_PATH;
	}

	@Override
	public Path getFileName() {
		return VELKA_CLOJURE_BITSET_NAME;
	}
	
	private JavaBitSet() {}
	private static JavaBitSet instance = null;
	public static JavaBitSet singleton() {
		if(instance == null) {
			instance = new JavaBitSet();
		}
		return instance;
	}
}
