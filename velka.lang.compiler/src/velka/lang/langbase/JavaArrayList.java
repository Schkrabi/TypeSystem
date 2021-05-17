package velka.lang.langbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Optional;
import java.util.stream.Collectors;

import velka.lang.abstraction.Abstraction;
import velka.lang.abstraction.Operator;
import velka.lang.application.AbstractionApplication;
import velka.lang.expression.Expression;
import velka.lang.expression.Symbol;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.literal.LitBoolean;
import velka.lang.literal.LitComposite;
import velka.lang.literal.LitInteger;
import velka.lang.literal.LitInteropObject;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeName;
import velka.lang.types.TypeRepresentation;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;
import velka.lang.util.ThrowingBinaryOperator;
import velka.lang.util.ThrowingFunction;

/**
 * This class contains utilities to work with ArrayList in velka
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class JavaArrayList {

	/**
	 * Type Variable used for list elements
	 */
	private static final TypeVariable A = new TypeVariable(NameGenerator.next());

	/**
	 * Type Variable used for list elements
	 */
	private static final TypeVariable B = new TypeVariable(NameGenerator.next());

	/**
	 * Type Variable used for list elements
	 */
	private static final TypeVariable C = new TypeVariable(NameGenerator.next());

	/**
	 * Type of java array list in velka
	 */
	public static final TypeAtom TypeListJavaArray = new TypeAtom(TypeName.LIST, new TypeRepresentation("JavaArray"));

	/**
	 * Constructor
	 */
	public static final Operator constructor = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [] (java.util.ArrayList.))";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression e = new LitInteropObject(new ArrayList<Object>());
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(TypeTuple.EMPTY_TUPLE, JavaArrayList.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	};

	/**
	 * Symbol for boolean add(E e)
	 */
	public static final Symbol addToEndSymbol = new Symbol("java-array-list-add-to-end");

	/**
	 * Operator for boolean add(E e)
	 */
	public static final Operator addToEnd = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _e] " + LitBoolean.clojureBooleanToClojureLitBoolean("(.add (first _list) _e)")
					+ ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;
			Expression e = args.get(1);

			@SuppressWarnings("unchecked")
			ArrayList<Object> l = (ArrayList<Object>) list.javaObject;
			l.add(e);

			return LitBoolean.TRUE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray, JavaArrayList.A),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for void add(int index, E element)
	 */
	public static final Symbol addToIndexSymbol = new Symbol("java-array-list-add-to-index");

	/**
	 * Operator for void add(int index, E element)
	 */
	public static final Operator addToIndex = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _index _e] (second (doall [(.add (first _list) (first _index) _e) "
					+ Expression.EMPTY_EXPRESSION.toClojureCode(env, typeEnv) + "])))";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			LitInteger index = (LitInteger) args.get(1);

			Expression e = args.get(2);

			@SuppressWarnings("unchecked")
			ArrayList<Object> l = (ArrayList<Object>) list.javaObject;
			l.add((int)index.value, e);

			return Expression.EMPTY_EXPRESSION;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(JavaArrayList.TypeListJavaArray, TypeAtom.TypeIntNative, JavaArrayList.A),
					TypeTuple.EMPTY_TUPLE);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for boolean addAll(Collection<? extends E> c)
	 */
	public static final Symbol addAllSymbol = new Symbol("java-array-list-add-all");

	/**
	 * operator for boolean addAll(Collection<? extends E> c)
	 */
	public static final Operator addAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _collection] "
					+ LitBoolean.clojureBooleanToClojureLitBoolean("(.add (first _list) (first _collection))") + ")";

			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			LitComposite lc2 = (LitComposite) args.get(1);
			LitInteropObject collection = (LitInteropObject) lc2.value;

			@SuppressWarnings("unchecked")
			ArrayList<Object> l = (ArrayList<Object>) list.javaObject;
			@SuppressWarnings("unchecked")
			ArrayList<Object> c = (ArrayList<Object>) collection.javaObject;

			if (l.addAll(c)) {
				return LitBoolean.TRUE;
			}

			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(JavaArrayList.TypeListJavaArray, JavaArrayList.TypeListJavaArray),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for boolean contains(Object o)
	 */
	public static final Symbol containsSymbol = new Symbol("java-array-list-contains");

	/**
	 * Operator for boolean contains(Object o)
	 */
	public static final Operator contains = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _object] "
					+ LitBoolean.clojureBooleanToClojureLitBoolean("(.contains (first _list) _object)") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			@SuppressWarnings("unchecked")
			ArrayList<Object> l = (ArrayList<Object>) list.javaObject;

			Expression e = args.get(1);

			if (l.contains(e)) {
				return LitBoolean.TRUE;
			}

			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray, A), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for boolean containsAll(Collection<?> c)
	 */
	public static final Symbol containsAllSymbol = new Symbol("java-array-list-contains-all");

	/**
	 * Operator for boolean containsAll(Collection<?> c)
	 */
	public static final Operator containsAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _collection] "
					+ LitBoolean.clojureBooleanToClojureLitBoolean("(.containsAll (first _list) (first _collection))")
					+ ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			LitComposite lc2 = (LitComposite) args.get(1);
			LitInteropObject collection = (LitInteropObject) lc2.value;

			@SuppressWarnings("unchecked")
			ArrayList<Object> l = (ArrayList<Object>) list.javaObject;
			@SuppressWarnings("unchecked")
			ArrayList<Object> c = (ArrayList<Object>) collection.javaObject;

			if (l.containsAll(c)) {
				return LitBoolean.TRUE;
			}

			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(JavaArrayList.TypeListJavaArray, JavaArrayList.TypeListJavaArray),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for E get(int index)
	 */
	public static final Symbol getSymbol = new Symbol("java-array-list-get");

	/**
	 * Operator for E get(int index)
	 */
	public static final Operator get = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _index] (.get (first _list) (first _index)))";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			LitInteger index = (LitInteger) args.get(1);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) list.javaObject;

			return l.get((int)index.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray, TypeAtom.TypeIntNative), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for int indexOf(Object o)
	 */
	public static final Symbol indexOfSymbol = new Symbol("java-array-list-index-of");

	/**
	 * Operator for int indexOf(Object o)
	 */
	public static final Operator indexOf = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _object] "
					+ LitInteger.clojureIntToClojureLitInteger("(.indexOf (first _list) _object)") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			Expression e = args.get(1);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) list.javaObject;

			int ret = l.indexOf(e);

			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray, A), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for boolean isEmpty()
	 */
	public static final Symbol isEmptySymbol = new Symbol("java-array-list-is-empty");

	/**
	 * Operator for boolean isEmpty()
	 */
	public static final Operator isEmpty = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list] " + LitBoolean.clojureBooleanToClojureLitBoolean("(.isEmpty (first _list))")
					+ ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) list.javaObject;

			if (l.isEmpty()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for int lastIndexOf(E e)
	 */
	public static final Symbol lastIndexOfSymbol = new Symbol("java-array-list-last-index-of");

	/**
	 * Operator for int lastIndexOf(E e)
	 */
	public static final Operator lastIndexOf = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _object] "
					+ LitInteger.clojureIntToClojureLitInteger("(.lastIndexOf (first _list) _object)") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			Expression e = args.get(1);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) list.javaObject;

			int ret = l.lastIndexOf(e);

			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray, A), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/*
	 * //E remove(int index) public static final Symbol removeSymbol = new
	 * Symbol("java-array-list-remove"); public static final Operator remove = new
	 * Operator() {
	 * 
	 * @Override protected String toClojureOperator(Environment env, TypeEnvironment
	 * typeEnv) throws AppendableException { String code =
	 * "(fn [_list _index] (.remove (first _list) ^Integer (first _index)))"; return
	 * code; }
	 * 
	 * @Override protected Expression doSubstituteAndEvaluate(Tuple args,
	 * Environment env, TypeEnvironment typeEnv, Optional<Expression>
	 * rankingFunction) throws AppendableException { // Need to extract LitComposite
	 * carrying type info first LitComposite lc = (LitComposite) args.get(0); // Now
	 * I can get to LitInteropObject carrying java.util.ArrayList LitInteropObject
	 * list = (LitInteropObject) lc.value;
	 * 
	 * LitInteger index = (LitInteger) args.get(1);
	 * 
	 * @SuppressWarnings("rawtypes") ArrayList al = (ArrayList) list.javaObject;
	 * 
	 * Expression e = (Expression) al.remove(index.value); return e; }
	 * 
	 * @Override public Pair<Type, Substitution> infer(Environment env,
	 * TypeEnvironment typeEnv) throws AppendableException { TypeArrow type = new
	 * TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray,
	 * TypeAtom.TypeIntNative), A); return new Pair<Type, Substitution>(type,
	 * Substitution.EMPTY); }
	 * 
	 * };
	 */

	/**
	 * Symbol for boolean remove(Object o)
	 */
	public static final Symbol removeSymbol = new Symbol("java-array-list-remove");

	/**
	 * Operator for boolean remove(Object o)
	 */
	public static final Operator remove = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _o] " + LitBoolean.clojureBooleanToClojureLitBoolean("(.remove (first _list) _o)")
					+ ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			Expression o = args.get(1);

			@SuppressWarnings("rawtypes")
			ArrayList al = (ArrayList) list.javaObject;

			if (al.remove(o)) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray, A), TypeAtom.TypeBoolNative);
			;
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * symbol for boolean removeAll(Collection<?> c)
	 */
	public static final Symbol removeAllSymbol = new Symbol("java-array-list-remove-all");

	/**
	 * Operator for boolean removeAll(Collection<?> c)
	 */
	public static final Operator removeAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _c] "
					+ LitBoolean.clojureBooleanToClojureLitBoolean("(.removeAll (first _list) (first _c))") + ")";
			return code;
		}

		@SuppressWarnings("unchecked")
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			// Need to extract LitComposite carrying type info first
			LitComposite lc2 = (LitComposite) args.get(1);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject ec = (LitInteropObject) lc2.value;

			@SuppressWarnings("rawtypes")
			ArrayList al = (ArrayList) list.javaObject;
			@SuppressWarnings("rawtypes")
			ArrayList c = (ArrayList) ec.javaObject;

			if (al.removeAll(c)) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(JavaArrayList.TypeListJavaArray, JavaArrayList.TypeListJavaArray),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for boolean retainAll(Collection<?> c)
	 */
	public static final Symbol retainAllSymbol = new Symbol("java-array-list-retain-all");

	/**
	 * Operator for boolean retainAll(Collection<?> c)
	 */
	public static final Operator retainAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _c] "
					+ LitBoolean.clojureBooleanToClojureLitBoolean("(.retainAll (first _list) (first _c))") + ")";
			return code;
		}

		@SuppressWarnings("unchecked")
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			// Need to extract LitComposite carrying type info first
			LitComposite lc2 = (LitComposite) args.get(1);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject ec = (LitInteropObject) lc2.value;

			@SuppressWarnings("rawtypes")
			ArrayList al = (ArrayList) list.javaObject;
			@SuppressWarnings("rawtypes")
			ArrayList c = (ArrayList) ec.javaObject;

			if (al.retainAll(c)) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(JavaArrayList.TypeListJavaArray, JavaArrayList.TypeListJavaArray),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for E set(int index, E element)
	 */
	public static final Symbol setSymbol = new Symbol("java-array-list-set");

	/**
	 * Operator for E set(int index, E element)
	 */
	public static final Operator set = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _index _element] (.set (first _list) (first _index) _element))";
			return code;
		}

		@SuppressWarnings("unchecked")
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			LitInteger index = (LitInteger) args.get(1);
			Expression element = args.get(2);

			ArrayList<Expression> al = (ArrayList<Expression>) list.javaObject;

			return al.set((int)index.value, element);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray, TypeAtom.TypeIntNative, A),
					A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for int size()
	 */
	public static final Symbol sizeSymbol = new Symbol("java-array-list-size");

	/**
	 * Operator for int size()
	 */
	public static final Operator size = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list] " + LitInteger.clojureIntToClojureLitInteger("(.size (first _list))") + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			@SuppressWarnings("unchecked")
			ArrayList<Expression> al = (ArrayList<Expression>) list.javaObject;

			int size = al.size();

			return new LitInteger(size);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for List<E> subList(int fromIndex, int toIndex)
	 */
	public static final Symbol sublistSymbol = new Symbol("java-array-list-sublist");

	/**
	 * Operator for List<E> subList(int fromIndex, int toIndex)
	 */
	public static final Operator sublist = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _from _to] " + LitComposite.clojureValueToClojureLiteral(
					"(java.util.ArrayList. (.subList (first _list) (first _from) (first _to)))",
					JavaArrayList.TypeListJavaArray) + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			LitInteger from = (LitInteger) args.get(1);
			LitInteger to = (LitInteger) args.get(2);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> al = (ArrayList<Expression>) list.javaObject;

			ArrayList<Expression> sublist = new ArrayList<Expression>(al.subList((int)from.value, (int)to.value));

			return new LitComposite(new LitInteropObject(sublist), JavaArrayList.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(JavaArrayList.TypeListJavaArray, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					JavaArrayList.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for List<T> map(Function<T, E>)
	 */
	public static final Symbol mapSymbol = new Symbol("java-array-list-map");

	/**
	 * Operator for List<T> map(Function<T, E>)
	 */
	public static final Operator map = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list _abst] "
					+ LitComposite
							.clojureValueToClojureLiteral("(java.util.ArrayList. (map (fn [_e] ("
									+ ClojureCodeGenerator.eapplyClojureSymbol + " _abst "
									+ ClojureCodeGenerator.addTypeMetaInfo_str("[_e]",
											"(velka.lang.types.TypeTuple. [("
													+ ClojureCodeGenerator.getTypeClojureSymbol + " _e)])")
									+ ")) (first _list)))", JavaArrayList.TypeListJavaArray)
					+ ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			Abstraction abst = (Abstraction) args.get(1);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> al = (ArrayList<Expression>) list.javaObject;
			ArrayList<Expression> rslt = null;
			try {
				rslt = new ArrayList<Expression>(al.stream().map(ThrowingFunction.wrapper(e -> {
					AbstractionApplication appl = new AbstractionApplication(abst, new Tuple(e));
					return appl.interpret(env, typeEnv);
				})).collect(Collectors.toList()));
			} catch (RuntimeException re) {
				if (re.getCause() instanceof AppendableException) {
					AppendableException e = (AppendableException) re.getCause();
					throw e;
				}
				throw re;
			}

			return new LitComposite(new LitInteropObject(rslt), JavaArrayList.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(JavaArrayList.TypeListJavaArray, new TypeArrow(new TypeTuple(A), B)),
					JavaArrayList.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for List<T> map2(List<E2> other, Function<T, E1, E2>)
	 */
	public static final Symbol map2Symbol = new Symbol("java-array-list-map2");

	/**
	 * Operator for List<T> map2(List<E2> other, Function<T, E1, E2>)
	 */
	public static final Operator map2 = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list1 _list2 _abst] " + LitComposite.clojureValueToClojureLiteral(
					"(java.util.ArrayList. (map (fn [_e1 _e2] (" + ClojureCodeGenerator.eapplyClojureSymbol + " _abst "
							+ ClojureCodeGenerator.addTypeMetaInfo_str("[_e1 _e2]",
									"(velka.lang.types.TypeTuple. [(" + ClojureCodeGenerator.getTypeClojureSymbol
											+ " _e1) (" + ClojureCodeGenerator.getTypeClojureSymbol + " _e2)])")
							+ ")) (first _list1) (first _list2)))",
					JavaArrayList.TypeListJavaArray) + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			LitComposite lc = (LitComposite) args.get(0);
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) lc.value;

			LitComposite lc2 = (LitComposite) args.get(1);
			LitInteropObject list2 = (LitInteropObject) lc2.value;

			Abstraction abst = (Abstraction) args.get(2);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> l1 = (ArrayList<Expression>) list.javaObject;
			@SuppressWarnings("unchecked")
			ArrayList<Expression> l2 = (ArrayList<Expression>) list2.javaObject;
			ArrayList<Expression> rslt = new ArrayList<Expression>();

			Iterator<Expression> i1 = l1.iterator();
			Iterator<Expression> i2 = l2.iterator();

			while (i1.hasNext() && i2.hasNext()) {
				Expression e1 = i1.next();
				Expression e2 = i2.next();

				AbstractionApplication appl = new AbstractionApplication(abst, new Tuple(e1, e2));

				rslt.add(appl.interpret(env, typeEnv));
			}

			return new LitComposite(new LitInteropObject(rslt), JavaArrayList.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray,
					JavaArrayList.TypeListJavaArray, new TypeArrow(new TypeTuple(A, B), C)),
					JavaArrayList.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for T foldl(Function<T, E, T>)
	 */
	public static final Symbol foldlSymbol = new Symbol("java-array-list-foldl");

	/**
	 * Operator for T foldl(Function<T, E, T>)
	 */
	public static final Operator foldl = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_abst _term _list] (reduce (fn [_agg _element] ("
					+ ClojureCodeGenerator.eapplyClojureSymbol + " _abst "
					+ ClojureCodeGenerator.addTypeMetaInfo_str("[_agg _element]",
							"(velka.lang.types.TypeTuple. [(" + ClojureCodeGenerator.getTypeClojureSymbol + " _agg) ("
									+ ClojureCodeGenerator.getTypeClojureSymbol + " _element)])")
					+ ")) _term (first _list)))";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Abstraction abst = (Abstraction) args.get(0);
			Expression terminator = args.get(1);
			LitComposite lc = (LitComposite) args.get(2);
			LitInteropObject io = (LitInteropObject) lc.value;
			@SuppressWarnings("unchecked")
			ArrayList<Expression> list = (ArrayList<Expression>) io.javaObject;

			Expression rslt = Expression.EMPTY_EXPRESSION;
			try {
				rslt = list.stream().reduce(terminator, ThrowingBinaryOperator.wrapper((agg, element) -> {
					AbstractionApplication app = new AbstractionApplication(abst, new Tuple(agg, element));
					return app.interpret(env, typeEnv);
				}));
			} catch (RuntimeException re) {
				if (re.getCause() instanceof AppendableException) {
					AppendableException e = (AppendableException) re.getCause();
					throw e;
				}
				throw re;
			}

			return rslt;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays
					.asList(new TypeArrow(new TypeTuple(Arrays.asList(A, A)), A), A, JavaArrayList.TypeListJavaArray)),
					A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for T foldr(Function<T, E, T>)
	 */
	public static final Symbol foldrSymbol = new Symbol("java-array-list-foldr");

	/**
	 * Operator for T foldr(Function<T, E, T>)
	 */
	public static final Operator foldr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_abst _term _list] (reduce (fn [_agg _element] ("
					+ ClojureCodeGenerator.eapplyClojureSymbol + " _abst "
					+ ClojureCodeGenerator.addTypeMetaInfo_str("[_agg _element]",
							"(velka.lang.types.TypeTuple. [(" + ClojureCodeGenerator.getTypeClojureSymbol + " _agg) ("
									+ ClojureCodeGenerator.getTypeClojureSymbol + " _element)])")
					+ ")) _term (reverse (first _list))))";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Abstraction abst = (Abstraction) args.get(0);
			Expression terminator = args.get(1);
			LitComposite lc = (LitComposite) args.get(2);
			LitInteropObject io = (LitInteropObject) lc.value;
			@SuppressWarnings("unchecked")
			ArrayList<Expression> list = (ArrayList<Expression>) io.javaObject;

			Expression agg = terminator;
			ListIterator<Expression> i = list.listIterator(list.size());
			while (i.hasPrevious()) {
				Expression element = i.previous();
				AbstractionApplication app = new AbstractionApplication(abst, new Tuple(agg, element));
				agg = app.interpret(env, typeEnv);
			}

			return agg;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays
					.asList(new TypeArrow(new TypeTuple(Arrays.asList(A, A)), A), A, JavaArrayList.TypeListJavaArray)),
					A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Conversion ArrayList 2 LinkedList
	 */
	public static Operator ArrayListToLinkedList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_arrayList] " + LitComposite.clojureValueToClojureLiteral(
					"(java.util.LinkedList. (first _arrayList))", JavaLinkedList.TypeListJavaLinked) + ")";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite lc = (LitComposite) args.get(0);
			LitInteropObject lio = (LitInteropObject) lc.value;
			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) lio.javaObject;

			return new LitComposite(new LitInteropObject(new LinkedList<Expression>(l)),
					JavaLinkedList.TypeListJavaLinked);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray),
					JavaLinkedList.TypeListJavaLinked);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};
	
	/**
	 * Conversion ArrayList 2 NativeList
	 */
	public static Operator ArrayListToNativeList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list] (reduce (fn [_l _e] " 
							+ LitComposite.clojureValueToClojureLiteral(
									ClojureCodeGenerator.addTypeMetaInfo_str("[_e, _l]", 
											"(velka.lang.types.TypeTuple. [(" + ClojureCodeGenerator.getTypeClojureSymbol  + " _e) " 
									+ TypeAtom.TypeListNative.clojureTypeRepresentation() + "])")
									, TypeAtom.TypeListNative) + ") " 
							+ ListNative.EMPTY_LIST_NATIVE.toClojureCode(env, typeEnv) + " (reverse (first _list))))";
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite lc = (LitComposite) args.get(0);
			LitInteropObject lio = (LitInteropObject) lc.value;
			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) lio.javaObject;
			
			LitComposite converted = ListNative.EMPTY_LIST_NATIVE;
			
			ListIterator<Expression> i = l.listIterator(l.size());
			while(i.hasPrevious()) {
				Expression e = i.previous();
				converted = new LitComposite(new Tuple(e, converted), TypeAtom.TypeListNative );
			}
			
			return converted;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(JavaArrayList.TypeListJavaArray), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}};

	/**
	 * Initializes values for java array list in environment
	 * 
	 * @param env initialized environment
	 */
	public static void initializeInEnvironment(Environment env) {
		env.put(addToEndSymbol, addToEnd);
		env.put(addToIndexSymbol, addToIndex);
		env.put(addAllSymbol, addAll);
		env.put(containsSymbol, contains);
		env.put(containsAllSymbol, containsAll);
		env.put(getSymbol, get);
		env.put(indexOfSymbol, indexOf);
		env.put(isEmptySymbol, isEmpty);
		env.put(lastIndexOfSymbol, lastIndexOf);
		env.put(removeSymbol, remove);
		env.put(removeAllSymbol, removeAll);
		env.put(retainAllSymbol, retainAll);
		env.put(setSymbol, set);
		env.put(sizeSymbol, size);
		env.put(sublistSymbol, sublist);
		env.put(mapSymbol, map);
		env.put(map2Symbol, map2);
		env.put(foldlSymbol, foldl);
		env.put(foldrSymbol, foldr);
	}
}
