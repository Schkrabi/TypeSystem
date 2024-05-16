package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.BitSet;

import velka.core.abstraction.Constructor;
import velka.core.abstraction.Operator;
import velka.core.exceptions.DuplicateTypeDefinitionException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.core.literal.LitString;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;
import velka.util.annotations.Description;
import velka.util.annotations.Example;
import velka.util.annotations.Header;
import velka.util.annotations.Name;
import velka.util.annotations.Syntax;
import velka.util.annotations.VelkaConstructor;
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			Expression e = new LitInteropObject(new BitSet());
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String nbits = "_nbits";
			String code = ClojureHelper.fnHelper(
							Arrays.asList(nbits),
							ClojureHelper.applyClojureFunction(
									"java.util.BitSet.",
									ClojureHelper.getLiteralInnerValue(nbits)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nBitsConstructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitInteger nbits = (LitInteger)args.get(0);
			Expression e = new LitInteropObject(new BitSet((int) nbits.value));
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeSetBitSet);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return "construct Set BitSet";
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
								ClojureHelper.applyClojureFunction(
										".and",
										ClojureHelper.getLiteralInnerValue(set1),
										ClojureHelper.getLiteralInnerValue(set2)),
								set1)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return andSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitComposite lc2 = (LitComposite)args.get(1);
			LitInteropObject set2 = (LitInteropObject)lc2.value;
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			bSet1.and(bSet2);
			
			return lc1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
								ClojureHelper.applyClojureFunction(
										".andNot",
										ClojureHelper.getLiteralInnerValue(set1),
										ClojureHelper.getLiteralInnerValue(set2)),
								set1)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return andNotSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitComposite lc2 = (LitComposite)args.get(1);
			LitInteropObject set2 = (LitInteropObject)lc2.value;
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			bSet1.andNot(bSet2);
			
			return lc1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".cardinality",
									ClojureHelper.getLiteralInnerValue(set))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return cardinalitySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject set = (LitInteropObject)lc.value;
			
			BitSet bSet = (BitSet)set.javaObject;
			
			long card = (long)bSet.cardinality();
			
			return new LitInteger(card);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String code =
					ClojureHelper.fnHelper(
							Arrays.asList(set),
							ClojureHelper.applyClojureFunction(
									"second",
									ClojureHelper.clojureVectorHelper(
											ClojureHelper.applyClojureFunction(
													".clear",
													ClojureHelper.getLiteralInnerValue(set)),
											set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return clearSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject set = (LitInteropObject)lc.value;
			BitSet bSet = (BitSet)set.javaObject;
			bSet.clear();
			return lc;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String bitIndex = "_bit-index";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, bitIndex),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".clear",
											ClojureHelper.getLiteralInnerValue(set),
											ClojureHelper.getLiteralInnerValue(bitIndex)),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return clearBitIndexSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject set = (LitInteropObject)lc.value;
			LitInteger bitIndex = (LitInteger)args.get(1);
			BitSet bSet = (BitSet)set.javaObject;
			bSet.clear((int)bitIndex.value);
			return lc;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
											ClojureHelper.getLiteralInnerValue(set),
											ClojureHelper.getLiteralInnerValue(fromIndex),
											ClojureHelper.getLiteralInnerValue(toIndex)),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return clearIntervalSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject set = (LitInteropObject)lc.value;
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			BitSet bSet = (BitSet)set.javaObject;
			bSet.clear((int)fromIndex.value, (int)toIndex.value);
			return lc;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitComposite.clojureLit(
							TypeAtom.TypeSetBitSet,
							ClojureHelper.applyClojureFunction(
									".clone",
									ClojureHelper.getLiteralInnerValue(set))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return cloneSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject set = (LitInteropObject)lc.value;
			BitSet bSet = (BitSet)set.javaObject;
			return new LitComposite(new LitInteropObject(bSet.clone()), TypeAtom.TypeSetBitSet);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".equals",
									ClojureHelper.getLiteralInnerValue(set1),
									ClojureHelper.getLiteralInnerValue(set2))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return equalsSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitComposite lc2 = (LitComposite)args.get(1);
			LitInteropObject set2 = (LitInteropObject)lc2.value;
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			return bSet1.equals(bSet2) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String bitIndex = "_bitIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, bitIndex),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".flip",
											ClojureHelper.getLiteralInnerValue(set),
											ClojureHelper.getLiteralInnerValue(bitIndex)),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return flipSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject set = (LitInteropObject)lc.value;
			BitSet bSet = (BitSet)set.javaObject;
			
			LitInteger bitIndex = (LitInteger)args.get(1);
			
			bSet.flip((int)bitIndex.value);
			return lc;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
											ClojureHelper.getLiteralInnerValue(set),
											ClojureHelper.getLiteralInnerValue(fromIndex),
											ClojureHelper.getLiteralInnerValue(toIndex)),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return flipIntervalSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject set = (LitInteropObject)lc.value;
			BitSet bSet = (BitSet)set.javaObject;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			
			bSet.flip((int)fromIndex.value, (int)toIndex.value);
			return lc;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String index = "_index";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, index),
					LitBoolean.clojureLit(
					ClojureHelper.applyClojureFunction(
							".get",
							ClojureHelper.getLiteralInnerValue(set),
							ClojureHelper.getLiteralInnerValue(index))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return getSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject set = (LitInteropObject)lc.value;
			BitSet bSet = (BitSet)set.javaObject;
			
			LitInteger index = (LitInteger)args.get(1);
			return bSet.get((int)index.value) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String toIndex = "_toIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex, toIndex),
					LitComposite.clojureLit(
							TypeAtom.TypeSetBitSet,
							ClojureHelper.applyClojureFunction(
									".get",
									ClojureHelper.getLiteralInnerValue(set),
									ClojureHelper.getLiteralInnerValue(fromIndex),
									ClojureHelper.getLiteralInnerValue(toIndex))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return getIntervalSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc = (LitComposite)args.get(0);
			LitInteropObject set = (LitInteropObject)lc.value;
			BitSet bSet = (BitSet)set.javaObject;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			return new LitComposite(new LitInteropObject(bSet.get((int) fromIndex.value, (int) toIndex.value)),
					TypeAtom.TypeSetBitSet);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".intersects",
									ClojureHelper.getLiteralInnerValue(set1),
									ClojureHelper.getLiteralInnerValue(set2))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return intersectsSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitComposite lc2 = (LitComposite)args.get(1);
			LitInteropObject set2 = (LitInteropObject)lc2.value;
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			return bSet1.intersects(bSet2) ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".isEmpty",
									ClojureHelper.getLiteralInnerValue(set))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return isEmptySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return bSet1.isEmpty() ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".length",
									ClojureHelper.getLiteralInnerValue(set))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return lengthSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.length());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".nextClearBit",
									ClojureHelper.getLiteralInnerValue(set),
									ClojureHelper.getLiteralInnerValue(fromIndex))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextClearBitSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.nextClearBit((int)fromIndex.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".nextSetBit",
									ClojureHelper.getLiteralInnerValue(set),
									ClojureHelper.getLiteralInnerValue(fromIndex))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return nextSetBitSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.nextSetBit((int)fromIndex.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".or",
											ClojureHelper.getLiteralInnerValue(set1),
											ClojureHelper.getLiteralInnerValue(set2)),
									set1)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return orSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitComposite lc2 = (LitComposite)args.get(1);
			LitInteropObject set2 = (LitInteropObject)lc2.value;
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			bSet1.or(bSet2);
			
			return lc1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".previousClearBit",
									ClojureHelper.getLiteralInnerValue(set),
									ClojureHelper.getLiteralInnerValue(fromIndex))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return previousClearBitSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.previousClearBit((int)fromIndex.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String fromIndex = "_fromIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, fromIndex),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".previousSetBit",
									ClojureHelper.getLiteralInnerValue(set),
									ClojureHelper.getLiteralInnerValue(fromIndex))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return previousSetBitSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			return new LitInteger(bSet1.previousSetBit((int)fromIndex.value));
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String bitIndex = "_bitIndex";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set, bitIndex),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
									ClojureHelper.applyClojureFunction(
											".set",
											ClojureHelper.getLiteralInnerValue(set),
											ClojureHelper.getLiteralInnerValue(bitIndex)),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return setSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitInteger bitIndex = (LitInteger)args.get(1);
			
			BitSet s = (BitSet)set1.javaObject;
			s.set((int)bitIndex.value);
			return lc1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
											ClojureHelper.getLiteralInnerValue(set),
											ClojureHelper.getLiteralInnerValue(bitIndex),
											ClojureHelper.getLiteralInnerValue(value)),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return setValueSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitInteger bitIndex = (LitInteger)args.get(1);
			LitBoolean value = (LitBoolean)args.get(2);
			
			BitSet s = (BitSet)set1.javaObject;
			s.set((int)bitIndex.value, value.value);
			return lc1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
											ClojureHelper.getLiteralInnerValue(set),
											ClojureHelper.getLiteralInnerValue(fromIndex),
											ClojureHelper.getLiteralInnerValue(toIndex)),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return setIntervalSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			
			BitSet s = (BitSet)set1.javaObject;
			s.set((int)fromIndex.value, (int)toIndex.value);
			return lc1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
											ClojureHelper.getLiteralInnerValue(set),
											ClojureHelper.getLiteralInnerValue(fromIndex),
											ClojureHelper.getLiteralInnerValue(toIndex),
											ClojureHelper.getLiteralInnerValue(value)),
									set)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return setIntervalValueSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitInteger fromIndex = (LitInteger)args.get(1);
			LitInteger toIndex = (LitInteger)args.get(2);
			
			BitSet s = (BitSet)set1.javaObject;
			
			LitBoolean value = (LitBoolean)args.get(3);
			s.set((int)fromIndex.value, (int)toIndex.value, (boolean)value.value);
			return lc1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".size",
									ClojureHelper.getLiteralInnerValue(set))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return sizeSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			BitSet s = (BitSet)set1.javaObject;
			return new LitInteger(s.size());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set = "_set";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
									".toString",
									ClojureHelper.getLiteralInnerValue(set))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return strSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			BitSet s = (BitSet)set1.javaObject;
			return new LitString(s.toString());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String set1 = "_set1";
			String set2 = "_set2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(set1, set2),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.clojureVectorHelper(
								ClojureHelper.applyClojureFunction(
										".xor",
										ClojureHelper.getLiteralInnerValue(set1),
										ClojureHelper.getLiteralInnerValue(set2)),
								set1)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return xorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitComposite lc1 = (LitComposite)args.get(0);
			LitInteropObject set1 = (LitInteropObject)lc1.value;
			
			LitComposite lc2 = (LitComposite)args.get(1);
			LitInteropObject set2 = (LitInteropObject)lc2.value;
			
			BitSet bSet1 = (BitSet)set1.javaObject;
			BitSet bSet2 = (BitSet)set2.javaObject;
			
			bSet1.xor(bSet2);
			
			return lc1;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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

	@Override
	public void initTypes(TypeEnvironment typeEnv) throws DuplicateTypeDefinitionException {
		typeEnv.addType(TypeName.SET);
		typeEnv.addRepresentation(TypeAtom.TypeSetBitSet);		
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
