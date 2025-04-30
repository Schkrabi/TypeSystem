package velka.core.langbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.List;
import java.util.Map;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JMethod;
import com.sun.codemodel.JMod;
import com.sun.codemodel.JVar;

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
import velka.core.literal.Literal;
import velka.java.CodeModelInstance;
import velka.java.runtime.VelkaTuple;
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
import velka.util.RankAggregation;
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
	
	public static final Symbol constructorSymbol = new Symbol("velka_construct", JavaBitSet.singleton().getNamespace());
	
	
	@VelkaConstructor
	@Description("Constructs empty Set:BitSet.")
	@Name("Construct Empty bitset")
	@Syntax("(construct Set:BitSet)")
	public static final Constructor constructor = Constructor.wrapJavaConstructor(BitSet.class, JavaBitSet.singleton().getNamespace());
	
	@VelkaConstructor
	@Description("Creates a bit set whose initial size is large enough to explicitly represent bits with indices in the range 0 through nbits-1.")
	@Name("Construct bit set with capacity")
	@Syntax("(construct Set:BitSet <nbits>)")
	public static final Constructor nBitsConstructor = Constructor.wrapJavaConstructor(BitSet.class, JavaBitSet.singleton().getNamespace(), int.class); 
	
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
		public Symbol getInternalSymbol() {
			return new Symbol("copy_construct", JavaBitSet.singleton().getNamespace());
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
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var bscl = CodeModelInstance.instance()._ref(BitSet.class);
			var _s = method.body().decl(bscl, "_s",
					JExpr._new(bscl).arg(mappedArgs.get(new Symbol("_0")).invoke("length")));
			method.body().add(_s.invoke("or").arg(mappedArgs.get(new Symbol("_0"))));
			method.body()._return(_s);
		}
	};
	
	/**
	 * Symbol for void and(BitSet set)
	 */
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
	public static final Operator and = Operator.wrapJavaMethod(BitSet.class, "and", "bit-set-and",
			JavaBitSet.singleton().getNamespace(), BitSet.class);
	
	public static final Symbol andNotSymbol = new Symbol("and_not", JavaBitSet.singleton().getNamespace());
	public static final Symbol andNotSymbol_out = new Symbol("bit-set-and-not");
	
	@VelkaOperator
	@Description("Clears all of the bits in this BitSet whose corresponding bit is set in the specified BitSet.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-and-not s1 s2)") 
	@Syntax("(bit-set-and-not <set1> <set2>)")
	public static final Operator andNot = Operator.wrapJavaMethod(BitSet.class, "andNot", "bit-set-and-not",
			JavaBitSet.singleton().getNamespace(), BitSet.class);
	
	public static final Symbol cardinalitySymbol = new Symbol("cardinality", JavaBitSet.singleton().getNamespace());
	public static final Symbol cardinalitySymbol_out = new Symbol("bit-set-cardinality");
	
	@VelkaOperator
	@Description("Returns the number of bits set to true in this BitSet.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-cardinality s1)") 
	@Syntax("(bit-set-cardinality <set>)")
	public static final Operator cardinality = Operator.wrapJavaMethod(BitSet.class, "cardinality",
			"bit-set-cardinality", JavaBitSet.singleton().getNamespace()); 
	
	public static final Symbol clearSymbol = new Symbol("clear", JavaBitSet.singleton().getNamespace());
	public static final Symbol clearSymbol_out = new Symbol("bit-set-clear");
	
	@VelkaOperator
	@Description("Sets all of the bits in this BitSet to false.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-clear s1)") 
	@Syntax("(bit-set-clear <set>)")
	public static final Operator clear = Operator.wrapJavaMethod(BitSet.class, "clear", "bit-set-clear",
			JavaBitSet.singleton().getNamespace());
	
	public static final Symbol clearBitIndexSymbol = new Symbol("clear_bit_index", JavaBitSet.singleton().getNamespace());
	public static final Symbol clearBitIndexSymbol_out = new Symbol("bit-set-clear-bit-index");
	
	@VelkaOperator
	@Description("Sets the bit specified by the index to false.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-clear-bit-index s1 3)") 
	@Syntax("(bit-set-clear-bit-index <set> <index>)")
	public static final Operator clearBitIndex = Operator.wrapJavaMethod(BitSet.class, "clear",
			"bit-set-clear-bit-index", JavaBitSet.singleton().getNamespace(), int.class);
	
	public static final Symbol clearIntervalSymbol = new Symbol("clear_interval", JavaBitSet.singleton().getNamespace());
	public static final Symbol clearIntervalSymbol_out = new Symbol("bit-set-clear-interval");
	
	@VelkaOperator
	@Description("Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to false.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-clear-interval s1 2 4)") 
	@Syntax("(bit-set-clear-interval <set> <fromIndex> <toIndex>)")
	public static final Operator clearInterval = Operator.wrapJavaMethod(BitSet.class, "clear",
			"bit-set-clear-interval", JavaBitSet.singleton().getNamespace(), int.class, int.class);
	
	public static final Symbol cloneSymbol = new Symbol("velka_clone", JavaBitSet.singleton().getNamespace());
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
		public Symbol getInternalSymbol() {
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
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var bscl = CodeModelInstance.instance()._ref(BitSet.class);
			var _s = method.body().decl(bscl, "_s",
					JExpr._new(bscl).arg(mappedArgs.get(new Symbol("_0")).invoke("length")));
			method.body().add(_s.invoke("or").arg(mappedArgs.get(new Symbol("_0"))));
			method.body()._return(_s);
		}
	};
	
	public static final Symbol equalsSymbol = new Symbol("velka_equal", JavaBitSet.singleton().getNamespace());
	public static final Symbol equalsSymbol_out = new Symbol("bit-set-equalp");
	
	@VelkaOperator
	@Description("Compares this object against the specified object.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-equalp s1 s2)")
	@Syntax("(bit-set-equalp <set1> <set2>)")
	public static final Operator equals = Operator.wrapJavaMethod(BitSet.class, "equals",
			"bit-set-equalp", JavaBitSet.singleton().getNamespace(), Object.class);
	
	public static final Symbol flipSymbol = new Symbol("flip", JavaBitSet.singleton().getNamespace());
	public static final Symbol flipSymbol_out = new Symbol("bit-set-flip");
	
	@VelkaOperator
	@Description("Sets the bit at the specified index to the complement of its current value.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-flip s1 3)")
	@Syntax("(bit-set-flip <set> <bitIndex>)")
	public static final Operator flip = Operator.wrapJavaMethod(BitSet.class, "flip",
			"bit-set-flip", JavaBitSet.singleton().getNamespace(), int.class);
	
	public static final Symbol flipIntervalSymbol = new Symbol("flip_interval", JavaBitSet.singleton().getNamespace());
	public static final Symbol flipIntervalSymbol_out = new Symbol("bit-set-flip-interval");
	
	@VelkaOperator
	@Description("Sets each bit from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to the complement of its current value.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-flip-interval s1 3 5)")
	@Syntax("(bit-set-flip-interval <set> <fromIndex> <toIndex>)")
	public static final Operator flipInterval = Operator.wrapJavaMethod(BitSet.class, "flip",
			"bit-set-flip-interval", JavaBitSet.singleton().getNamespace(), int.class, int.class);
	
	public static final Symbol getSymbol = new Symbol("velka_get", JavaBitSet.singleton().getNamespace());
	public static final Symbol getSymbol_out = new Symbol("bit-set-get");
	
	@VelkaOperator
	@Description("Returns the value of the bit with the specified index.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-get s1 3)") 
	@Syntax("(bit-set-get <set> <index>)")
	public static final Operator get = Operator.wrapJavaMethod(BitSet.class, "get", "bit-set-get",
			JavaBitSet.singleton().getNamespace(), int.class);
	
	public static final Symbol getIntervalSymbol = new Symbol("get_interval", JavaBitSet.singleton().getNamespace());
	public static final Symbol getIntervalSymbol_out = new Symbol("bit-set-get-interval");
	
	@VelkaOperator
	@Description("Returns a new BitSet composed of bits from this BitSet from fromIndex (inclusive) to toIndex (exclusive).") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-get-interval s1 2 4)") 
	@Syntax("(bit-set-get-interval <set> <fromIndex> <toIndex>)")
	public static final Operator getInterval = Operator.wrapJavaMethod(BitSet.class, "get",
			"bit-set-get-interval", JavaBitSet.singleton().getNamespace(), int.class, int.class);
	
	public static final Symbol intersectsSymbol = new Symbol("intersects", JavaBitSet.singleton().getNamespace());
	public static final Symbol intersectsSymbol_out = new Symbol("bit-set-intersects");
	
	@VelkaOperator
	@Description("Returns true if the specified BitSet has any bits set to true that are also set to true in this BitSet.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-intersects s1 s2)") 
	@Syntax("(bit-set-intersects <set1> <set2>)")
	public static final Operator intersects = Operator.wrapJavaMethod(java.util.BitSet.class, "intersects", "bit-set-intersects",
			JavaBitSet.singleton().getNamespace(), java.util.BitSet.class);
	
	public static final Symbol isEmptySymbol = new Symbol("velka_is_empty", JavaBitSet.singleton().getNamespace());
	public static final Symbol isEmptySymbol_out = new Symbol("bit-set-is-empty");
	
	@VelkaOperator
	@Description("Returns true if this BitSet contains no bits that are set to true.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-is-empty s1)\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-is-empty s1)") 
	@Syntax("(bit-set-is-empty <set>)")
	public static final Operator isEmpty = Operator.wrapJavaMethod(BitSet.class, "isEmpty", "bit-set-is-empty",
			JavaBitSet.singleton().getNamespace());
	
	public static final Symbol lengthSymbol = new Symbol("velka_length", JavaBitSet.singleton().getNamespace());
	public static final Symbol lengthSymbol_out = new Symbol("bit-set-length");
	
	@VelkaOperator
	@Description("Returns the \"logical size\" of this BitSet: the index of the highest set bit in the BitSet plus one.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(bit-set-length s1);; = 5") 
	@Syntax("(bit-set-length <set>)")
	public static final Operator length = Operator.wrapJavaMethod(BitSet.class, "length", "bit-set-length",
			JavaBitSet.singleton().getNamespace());
	
	public static final Symbol nextClearBitSymbol = new Symbol("next_clear_bit", JavaBitSet.singleton().getNamespace());
	public static final Symbol nextClearBitSymbol_out = new Symbol("bit-set-next-clear-bit");
	
	//TODO Example
	@VelkaOperator
	@Description("Returns the index of the first bit that is set to false that occurs on or after the specified starting index.")  
	@Syntax("(bit-set-next-clear-bit <set> <fromIndex>)")
	public static final Operator nextClearBit = Operator.wrapJavaMethod(BitSet.class, "nextClearBit", "bit-set-next-clear-bit",
			JavaBitSet.singleton().getNamespace(), int.class);
	
	public static final Symbol nextSetBitSymbol = new Symbol("next_set_bit", JavaBitSet.singleton().getNamespace());
	public static final Symbol nextSetBitSymbol_out = new Symbol("bit-set-next-set-bit");
	
	//TODO Example
	@VelkaOperator
	@Description("Returns the index of the first bit that is set to true that occurs on or after the specified starting index.") 
	@Syntax("(bit-set-next-set-bit <set> <fromIndex>)")
	public static final Operator nextSetBit = Operator.wrapJavaMethod(BitSet.class, "nextSetBit", "bit-set-next-set-bit",
			JavaBitSet.singleton().getNamespace(), int.class);
	
	public static final Symbol orSymbol = new Symbol("velka_or", JavaBitSet.singleton().getNamespace());
	public static final Symbol orSymbol_out = new Symbol("bit-set-or");
	
	@VelkaOperator
	@Description("Performs a logical OR of this bit set with the bit set argument.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-or s1 s2)") 
	@Syntax("(bit-set-or <set1> <set2>)")
	public static final Operator or = Operator.wrapJavaMethod(BitSet.class, "or", "bit-set-or",
			JavaBitSet.singleton().getNamespace(), BitSet.class);
	
	public static final Symbol previousClearBitSymbol = new Symbol("previous_clear_bit", JavaBitSet.singleton().getNamespace());
	public static final Symbol previousClearBitSymbol_out = new Symbol("bit-set-previous-clear-bit");
	
	//TODO Example
	@VelkaOperator
	@Description("Returns the index of the nearest bit that is set to false that occurs on or before the specified starting index.")  
	@Syntax("(bit-set-previous-clear-bit <set> <fromIndex>)")
	public static final Operator previousClearBit = Operator.wrapJavaMethod(BitSet.class, "previousClearBit", "bit-set-previous-clear-bit",
			JavaBitSet.singleton().getNamespace(), int.class);
	
	public static final Symbol previousSetBitSymbol = new Symbol("previous_set_bit", JavaBitSet.singleton().getNamespace());
	public static final Symbol previousSetBitSymbol_out = new Symbol("bit-set-previous-set-bit");
	
	//TODO Example
	@VelkaOperator
	@Description("Returns the index of the nearest bit that is set to true that occurs on or before the specified starting index.")  
	@Syntax("(bit-set-previous-set-bit <set> <fromIndex>)")
	public static final Operator previousSetBit = Operator.wrapJavaMethod(BitSet.class, "previousSetBit", "bit-set-previous-set-bit",
			JavaBitSet.singleton().getNamespace(), int.class);
	
	public static final Symbol setSymbol = new Symbol("velka_set", JavaBitSet.singleton().getNamespace());
	public static final Symbol setSymbol_out = new Symbol("bit-set-set");
	
	@VelkaOperator
	@Description("Sets the bit at the specified index to true.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set s1 3)") 
	@Syntax("(bit-set-set <set> <bitIndex>)")
	public static final Operator set = Operator.wrapJavaMethod(BitSet.class, "set", "bit-set-set",
			JavaBitSet.singleton().getNamespace(), int.class);
	
	public static final Symbol setValueSymbol = new Symbol("set_value", JavaBitSet.singleton().getNamespace());
	public static final Symbol setValueSymbol_out = new Symbol("bit-set-set-value");
	
	@VelkaOperator
	@Description("Sets the bit at the specified index to the specified value.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-value s1 3 #t)") 
	@Syntax("(bit-set-set-value <set> <bitIndex> <value>)")
	public static final Operator setValue = Operator.wrapJavaMethod(BitSet.class, "set", "bit-set-set-value",
			JavaBitSet.singleton().getNamespace(), int.class, boolean.class);
	
	public static final Symbol setIntervalSymbol = new Symbol("set_interval", JavaBitSet.singleton().getNamespace());
	public static final Symbol setIntervalSymbol_out = new Symbol("bit-set-set-interval");
	
	@VelkaOperator
	@Description("Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to true.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 3 5)") 
	@Syntax("(bit-set-set-interval <set> <fromIndex> <toIndex>)")
	public static final Operator setInterval = Operator.wrapJavaMethod(BitSet.class, "set",
			"bit-set-set-interval", JavaBitSet.singleton().getNamespace(), int.class, int.class);
	
	public static final Symbol setIntervalValueSymbol = new Symbol("set_interval_value", JavaBitSet.singleton().getNamespace());
	public static final Symbol setIntervalValueSymbol_out = new Symbol("bit-set-set-interval-value");
	
	@VelkaOperator
	@Description("Sets the bits from the specified fromIndex (inclusive) to the specified toIndex (exclusive) to the specified value.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval-value s1 3 5 #f)") 
	@Syntax("(bit-set-set-interval-value <set> <fromIndex> <toIndex> <value>)")	
	public static final Operator setIntervalValue = Operator.wrapJavaMethod(BitSet.class, "set",
			"bit-set-set-interval-value", JavaBitSet.singleton().getNamespace(), int.class, int.class, boolean.class);
	
	public static final Symbol sizeSymbol = new Symbol("velka_size", JavaBitSet.singleton().getNamespace());
	public static final Symbol sizeSymbol_out = new Symbol("bit-set-size");
	
	@VelkaOperator
	@Description("Returns the number of bits of space actually in use by this BitSet to represent bit values.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 3 5)\n"
					+ "(bit-set-size s1)") 
	@Syntax("(bit-set-size <set>)")
	public static final Operator size = Operator.wrapJavaMethod(BitSet.class, "size", "bit-set-size",
			JavaBitSet.singleton().getNamespace());
	
	public static final Symbol strSymbol = new Symbol("velka_str", JavaBitSet.singleton().getNamespace());
	public static final Symbol strSymbol_out = new Symbol("bit-set-str");
	
	@VelkaOperator
	@Description("Returns a string representation of this bit set.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 3 5)\n"
					+ "(bit-set-str s1)") 
	@Syntax("(bit-set-str <set>)")
	public static final Operator str = Operator.wrapJavaMethod(BitSet.class, "toString", "bit-set-str",
			JavaBitSet.singleton().getNamespace());
	
	public static final Symbol xorSymbol = new Symbol("velka_xor", JavaBitSet.singleton().getNamespace());
	public static final Symbol xorSymbol_out = new Symbol("bit-set-xor");
	
	@VelkaOperator
	@Description("Performs a logical XOR of this bit set with the bit set argument.") 
	@Example("(define s1 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s1 0 5)\n"
					+ "(define s2 (construct Set:BitSet))\n"
					+ "(bit-set-set-interval s2 3 7)\n"
					+ "(bit-set-xor s1 s2)") 
	@Syntax("(bit-set-xor <set1> <set2>)")
	public static final Operator xor = Operator.wrapJavaMethod(BitSet.class, "xor", "bit-set-xor",
			JavaBitSet.singleton().getNamespace(), BitSet.class);
	
	public static final Symbol mapSymbol = new Symbol("velka_map", JavaBitSet.singleton().getNamespace());
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
		public Symbol getInternalSymbol() {
			return mapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var set = (LitInteropObject)args.get(0);
			var fun = args.get(1);
			var bSet = (BitSet)set.javaObject;
			
			var retSet = new BitSet();
			
			bSet.stream().forEach(i -> {
				var app = new AbstractionApplication(fun, new Tuple(new LitInteger(i)));
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
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var bitSetCl = CodeModelInstance.instance()._ref(BitSet.class);
			var _newBitSet = method.body().decl(bitSetCl, "newBitSet",
					JExpr._new(bitSetCl));
			
			
			var consCl = CodeModelInstance.instance().anonymousClass(java.util.function.IntConsumer.class);
			
			var acceptMth = consCl.method(JMod.PUBLIC, void.class, "accept");
			var valueParm = acceptMth.param(int.class, "value");
			
			var valueInt = acceptMth.body().decl(CodeModelInstance.instance()._ref(Integer.class), "lvalue", 
					CodeModelInstance.instance().ref(Integer.class).staticInvoke("valueOf").arg(valueParm));
			
			var retVal = acceptMth.body().decl(CodeModelInstance.instance().INT, "retVal",
					JExpr.cast(CodeModelInstance.instance()._ref(Integer.class),
							mappedArgs.get(new Symbol("_1")).invoke("apply").arg(VelkaTuple._velkaTuple(
									new TypeTuple(TypeAtom.TypeIntNative), valueInt)))
							.invoke("intValue"));
			
			acceptMth.body().add(_newBitSet.invoke("set").arg(retVal));
			
			method.body()
					.add(mappedArgs.get(new Symbol("_0")).invoke("stream").invoke("forEach").arg(JExpr._new(consCl)));
			
			method.body()._return(_newBitSet);
		}
	};
	
	public static Symbol containsAllSymbol = new Symbol("velka_contains_all", JavaBitSet.singleton().getNamespace());
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
		public Symbol getInternalSymbol() {
			return containsAllSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var set = (LitInteropObject)args.get(0);
			var lst = (LitInteropObject)args.get(1);
			
			var bset = (java.util.BitSet)set.javaObject;
			@SuppressWarnings("unchecked")
			var l = (java.util.List<Object>)lst.javaObject;
			
			var ret = l.stream().allMatch(e -> {
				if(e instanceof Integer i) {
					return bset.get(i);
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
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var _foreach = method.body().forEach(CodeModelInstance.instance()._ref(Object.class), "val", mappedArgs.get(new Symbol("_1")));
			var _if = _foreach.body()._if(mappedArgs.get(new Symbol("_0")).invoke("get")
					.arg(JExpr.cast(CodeModelInstance.instance()._ref(Integer.class), _foreach.var()))
					.not());
			_if._then()._return(JExpr.FALSE);
			method.body()._return(JExpr.TRUE);
		}
	};
	
	public static Symbol setAllSymbol = new Symbol("set_all", JavaBitSet.singleton().getNamespace());
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
		public Symbol getInternalSymbol() {
			return setAllSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var set = (LitInteropObject)args.get(0);
			var lst = (LitInteropObject)args.get(1);
			
			var bset = (java.util.BitSet)set.javaObject;
			@SuppressWarnings("unchecked")
			var l = (java.util.List<Object>)lst.javaObject;
			
			l.stream().forEach(e ->{
				if(e instanceof Integer i) {
					bset.set(i);
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
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var _foreach = method.body().forEach(CodeModelInstance.instance()._ref(Object.class), "val", mappedArgs.get(new Symbol("_1")));
			_foreach.body().add(mappedArgs.get(new Symbol("_0")).invoke("set")
					.arg(JExpr.cast(CodeModelInstance.instance()._ref(Integer.class), _foreach.var())));
			
			method.body()._return(mappedArgs.get(new Symbol("_0")));
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
		public Symbol getInternalSymbol() {
			return new Symbol("from_list", JavaBitSet.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lst = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			var l = (java.util.List<Object>)lst.javaObject;
			
			var set = new java.util.BitSet(l.size());
			l.stream().forEach(e ->{
				if(e instanceof Integer i) {
					set.set(i);
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
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var bsCl = CodeModelInstance.instance()._ref(BitSet.class);
			var bset = method.body().decl(bsCl, "_bs",
					JExpr._new(bsCl));
			
			var _foreach = method.body().forEach(CodeModelInstance.instance()._ref(Object.class), "val", mappedArgs.get(new Symbol("_0")));
			_foreach.body().add(bset.invoke("set")
					.arg(JExpr.cast(CodeModelInstance.instance()._ref(Integer.class), _foreach.var())));
			
			method.body()._return(bset);
		}
	};
	
	@VelkaOperator
	public static final Operator toList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			var set = "_set";
			var acc = "_acc";
			var indices = "_indices";
			var index = "_index";
			var code = ClojureHelper.fnHelper(
					List.of(set),
					ClojureHelper.letHelper(
							ClojureHelper.constructJavaClass(ArrayList.class, indices),
							Pair.of(indices,
									ClojureHelper.applyClojureFunction("loop",
											ClojureHelper.clojureVectorHelper(
													index, ClojureHelper.applyClojureFunction(".nextSetBit", set, "0"),
													acc, ClojureHelper.clojureVectorHelper()),
											ClojureHelper.clojureIfHelper(
													ClojureHelper.applyClojureFunction("neg?", index),
													acc,
													ClojureHelper.applyClojureFunction(
															"recur",
															ClojureHelper.applyClojureFunction(
																	".nextSetBit",
																	set, 
																	ClojureHelper.applyClojureFunction("inc", index)),
															ClojureHelper.applyClojureFunction("conj", acc, index)))))));
			return code;
		}

		@Override
		public Symbol getInternalSymbol() {
			return new Symbol("to_list", JavaBitSet.singleton().getNamespace());
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject)args.get(0);
			var bset = (java.util.BitSet)lio.javaObject;
			
			var l = new ArrayList<Object>();
			
			int i = bset.nextSetBit(0);
			while(i >= 0) {
				l.add(Integer.valueOf(i));
				
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
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var lCl = CodeModelInstance.instance().ref(ArrayList.class);
			var lst = method.body().decl(lCl, "_lst", JExpr._new(lCl));
			
			var i = method.body().decl(CodeModelInstance.instance().INT, "_i",
						mappedArgs.get(new Symbol("_0")).invoke("nextSetBit").arg(JExpr.lit(0)));
			
			var _while = method.body()._while(i.gte(JExpr.lit(0)));
			_while.body().add(lst.invoke("add").arg(i));
			_while.body().assign(i, mappedArgs.get(new Symbol("_0")).invoke("nextSetBit").arg(i.plus(JExpr.lit(1))));
			
			method.body()._return(lst);
		}
	};
	
	@VelkaConversion
	@Description("Converts Set:BitSet into Set:Tree.") 
	@Example("(convert Set:BitSet Set:Tree (bit-set-set (bit-set-set (bit-set-set (construct Set:BitSet) 3) 6) 9))") 
	@Syntax("(convert Set:BitSet Set:Tree <arg>)")
	public static final Conversion toTreeSet = new Conversion() {

		Double costX1 = 0d;
		Double costY1 = 0.8d;
		Double costX2 = 1000d;
		Double costY2 = 0.5d;
		
		@Override
		public Expression cost() {
			return Lambda.constFun(1, new LitDouble(0.7d));
//			
//			var arg = new Symbol(NameGenerator.next());
//			
//			var cost = new Lambda(
//					new AbstractionApplication(
//							new AbstractionApplication(
//									Operators.linFunPoints, 
//									new Tuple(new LitDouble(0d), new LitDouble(0.8d), new LitDouble(1000d), new LitDouble(0.5d))), 
//							new Tuple(
//									new AbstractionApplication(Operators.IntToDouble,
//											new Tuple(new AbstractionApplication(JavaBitSet.cardinality, new Tuple(arg)))))),
//					List.of(Pair.of(arg, TypeAtom.TypeSet)));
//			return cost;
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
		public Symbol getInternalSymbol() {
			return new Symbol("bit-set-2-tree-set", JavaBitSet.singleton().getNamespace());
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
						
						var cmp = new Lambda(
								new velka.core.application.IfExpression(
										new velka.core.application.AbstractionApplication(Operators.Equals,
												new Tuple(a1, a2)),
										new LitInteger(0),
										new IfExpression(
												new AbstractionApplication(Operators.LesserThan, new Tuple(a1, a2)),
												new LitInteger(-1), new LitInteger(1))),
								List.of(Pair.of(a1, TypeAtom.TypeIntNative), Pair.of(a2, TypeAtom.TypeIntNative)));
						
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
			
			bitSet.stream().forEach(x -> treeSet.add(Integer.valueOf(x)));
			
			return new LitInteropObject(treeSet, TypeAtom.TypeSetTree);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			var type = new TypeArrow(new TypeTuple(TypeAtom.TypeSetBitSet), TypeAtom.TypeSetTree);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var tsCl = CodeModelInstance.instance().ref(java.util.TreeSet.class);
			var ts = method.body().decl(tsCl, "_ts", JExpr._new(tsCl));
			
			
			var i = method.body().decl(CodeModelInstance.instance().INT, "_i",
						mappedArgs.get(new Symbol("_0")).invoke("nextSetBit").arg(JExpr.lit(0)));
			
			var _while = method.body()._while(i.gte(JExpr.lit(0)));
			_while.body().add(ts.invoke("add").arg(i));
			_while.body().assign(i, mappedArgs.get(new Symbol("_0")).invoke("nextSetBit").arg(i.plus(JExpr.lit(1))));
			
			method.body()._return(ts);
		}
	};
	
	@VelkaConversion
	public static Conversion toHashSet = new Conversion() {

		@Override
		public Expression cost() {
			return Lambda.constFun(1, new LitDouble(0.7d));
//			var arg = new Symbol(NameGenerator.next());
//			
//			var cost = new Lambda(
//					new AbstractionApplication(
//							new AbstractionApplication(
//									Operators.linFunPoints, 
//									new Tuple(new LitDouble(0d), new LitDouble(0.8d), new LitDouble(1000d), new LitDouble(0.5d))), 
//							new Tuple(
//									new AbstractionApplication(Operators.IntToDouble,
//											new Tuple(new AbstractionApplication(JavaBitSet.cardinality, new Tuple(arg)))))),
//					List.of(Pair.of(arg, TypeAtom.TypeSet)));
//			return cost;
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
		public Symbol getInternalSymbol() {
			return new Symbol("bit_set_2_hash_set", JavaBitSet.singleton().getNamespace());
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
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var hsCl = CodeModelInstance.instance().ref(java.util.HashSet.class);
			var hs = method.body().decl(hsCl, "_hs", JExpr._new(hsCl));
			
			
			var i = method.body().decl(CodeModelInstance.instance().INT, "_i",
						mappedArgs.get(new Symbol("_0")).invoke("nextSetBit").arg(JExpr.lit(0)));
			
			var _while = method.body()._while(i.gte(JExpr.lit(0)));
			_while.body().add(hs.invoke("add").arg(i));
			_while.body().assign(i, mappedArgs.get(new Symbol("_0")).invoke("nextSetBit").arg(i.plus(JExpr.lit(1))));
			
			method.body()._return(hs);
		}
	};
	
	
	private JavaBitSet() {}
	private static JavaBitSet instance = null;
	public static JavaBitSet singleton() {
		if(instance == null) {
			instance = new JavaBitSet();
		}
		return instance;
	}

	@Override
	protected String name() {
		return "bitSet";
	}
}
