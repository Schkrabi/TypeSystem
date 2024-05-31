package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.stream.Collectors;

import velka.core.abstraction.Abstraction;
import velka.core.abstraction.Constructor;
import velka.core.abstraction.Conversion;
import velka.core.abstraction.Lambda;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.application.Application;
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
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.ThrowingBinaryOperator;
import velka.util.ThrowingFunction;
import velka.util.ThrowingPredicate;
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
 * This class contains utilities to work with ArrayList in velka
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
@VelkaOperatorBank
@Description("Operators for working with wrapped java.util.ArrayList.") 
@Header("Array List")
public class JavaArrayList extends OperatorBank{

	/**
	 * Clojure namespace for JavaArrayList
	 */
	public static final String NAMESPACE = "velka.clojure.arrayList";

	public static final Symbol constructorSymbol = new Symbol("velka-construct", NAMESPACE);

	/**
	 * Constructor
	 */
	@VelkaConstructor
	@Description("Constructs empty List:JavaArray.") 
	@Name("Construct Empty List") 
	@Syntax("(construct List:JavaArray)")
	public static final Constructor constructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = ClojureHelper.fnHelper(
					Arrays.asList(),
					ClojureHelper.applyClojureFunction("java.util.ArrayList."));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Expression e = new LitInteropObject(new ArrayList<Object>(), TypeAtom.TypeListJavaArray);
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorSymbol;
		}
		
		@Override
		public String toString() {
			return "construct List JavaArray";
		}
	};

	/**
	 * Symbol for constructor from list
	 */
	public static Symbol constructorFromListSymbol = new Symbol("velka-construct-from-list", NAMESPACE);

	/**
	 * Operator for contructor from list
	 */
	@VelkaConstructor
	@Description("Construct List:JavaArray from existing list inserting all its elements.") 
	@Name("Construct from list") 
	@Syntax("(construct List:JavaArray <list>)")
	public static Constructor constructorFromList = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			final String list = "_list";
			final String code = ClojureHelper.fnHelper(Arrays.asList(list),
					ClojureHelper.applyClojureFunction("java.util.ArrayList.",
							ClojureHelper.applyClojureFunction("doall", list)));

			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorFromListSymbol;
		}
		
		@Override
		public String toString() {
			return "construct List JavaArray";
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteropObject interop = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			ArrayList<Expression> al = new ArrayList<Expression>(l);

			return new LitInteropObject(al, TypeAtom.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeListJavaArray);

			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for Capacity Constructor
	 */
	public static final Symbol constructorCapacitySymbol = new Symbol("velka-contruct-capacity", NAMESPACE);

	/**
	 * Operator for capacity constructor
	 */
	@VelkaConstructor
	@Description("Constructs List:JavaArray with specified pre-allocated capacity.") 
	@Name("Construct with capacity") 
	@Syntax("(construc List JavaArray <capacity>)")
	public static Constructor constructorCapacity = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			final String capacity = "_capacity";
			final String code = ClojureHelper.fnHelper(
					Arrays.asList(capacity), 
					ClojureHelper.applyClojureFunction(
							"java.util.ArrayList.", 
							capacity));

			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorCapacitySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			LitInteger lit = (LitInteger) args.get(0);

			ArrayList<Expression> al = new ArrayList<Expression>((int) lit.value);

			return new LitInteropObject(al, TypeAtom.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), TypeAtom.TypeListJavaArray);

			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for boolean add(E e)
	 */
	private static final Symbol addToEndSymbol = new Symbol("add-to-end", NAMESPACE);
	public static final Symbol addToEndSymbol_out = new Symbol("java-array-list-add-to-end");

	/**
	 * Operator for boolean add(E e)
	 */
	@VelkaOperator
	@Description("Appends the specified element to the end of list.") 
	@Example("(java-array-list-add-to-end (construct List:JavaArray) 42)") 
	@Syntax("(java-array-list-add-to-end <list> <element>)")
	public static final Operator addToEnd = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String e = "_e";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, e),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".add",
									list,
									e)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);
			Expression e = args.get(1);

			@SuppressWarnings("unchecked")
			ArrayList<Object> l = (ArrayList<Object>) list.javaObject;
			l.add(e);

			return LitBoolean.TRUE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray, A), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return addToEndSymbol;
		}
		
		@Override
		public String toString() {
			return addToEndSymbol_out.toString();
		}

	};

	/**
	 * Symbol for void add(int index, E element)
	 */
	private static final Symbol addToIndexSymbol = new Symbol("add-to-index", NAMESPACE);
	public static final Symbol addToIndexSymbol_out = new Symbol("java-array-list-add-to-index");

	/**
	 * Operator for void add(int index, E element)
	 */
	@VelkaOperator
	@Description("Inserts the specified element at the specified position in list.") 
	@Example("(java-array-list-to-index (construct List:JavaArray) 0 42)") 
	@Syntax("(java-array-list-to-index <list> <index> <element>)")
	public static final Operator addToIndex = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String index = "_index";
			String e = "_e";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, index, e),
					ClojureHelper.applyClojureFunction(
							"second",
							ClojureHelper.applyClojureFunction(
									"doall",
									ClojureHelper.clojureVectorHelper(
											ClojureHelper.applyClojureFunction(
													".add",
													list,
													index,
													e),
											Expression.EMPTY_EXPRESSION.toClojureCode(env, typeEnv)))));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteger index = (LitInteger) args.get(1);

			Expression e = args.get(2);

			@SuppressWarnings("unchecked")
			ArrayList<Object> l = (ArrayList<Object>) list.javaObject;
			l.add((int) index.value, e);

			return Expression.EMPTY_EXPRESSION;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray, TypeAtom.TypeIntNative, A),
					TypeTuple.EMPTY_TUPLE);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return addToIndexSymbol;
		}

		@Override
		public String toString() {
			return addToIndexSymbol_out.toString();
		}
	};

	/**
	 * Symbol for boolean addAll(Collection<? extends E> c)
	 */
	private static final Symbol addAllSymbol = new Symbol("add-all", NAMESPACE);
	public static final Symbol addAllSymbol_out = new Symbol("java-array-list-add-all");

	/**
	 * operator for boolean addAll(Collection<? extends E> c)
	 */
	@VelkaOperator
	@Description("Appends all of the elements in the specified collection to the end of this list, in the order that they are returned by the specified collection's Iterator.")
	@Example("(def l (construct List:JavaArray))\n" + "(java-array-list-add l 42)\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n" + "(println l)\n" + ";;(42 0 1 2)")
	@Syntax("(java-array-list-add-all <list1> <list2>)")
	public static final Operator addAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String collection = "_collection";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, collection),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".addAll",
									list, 
									collection)));

			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteropObject collection = (LitInteropObject) args.get(1);

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
					new TypeTuple(TypeAtom.TypeListJavaArray, TypeAtom.TypeListJavaArray),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return addAllSymbol;
		}
		
		@Override
		public String toString() {
			return addAllSymbol_out.toString();
		}

	};

	/**
	 * Symbol for boolean contains(Object o)
	 */
	private static final Symbol containsSymbol = new Symbol("velka-contains", NAMESPACE);
	public static final Symbol containsSymbol_out = new Symbol("java-array-list-contains");

	/**
	 * Operator for boolean contains(Object o)
	 */
	@VelkaOperator
	@Description("Returns true if this list contains the specified element.") 
	@Example("(java-array-list-contains (construct List:JavaArray) 42) ; = #f") 
	@Syntax("(java-array-list-contains <list> <element>)")
	public static final Operator contains = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String object = "_object";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, object),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".contains",
									list,
									object)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

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
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray, A), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return containsSymbol;
		}

		@Override
		public String toString() {
			return containsSymbol_out.toString();
		}
	};

	/**
	 * Symbol for boolean containsAll(Collection<?> c)
	 */
	private static final Symbol containsAllSymbol = new Symbol("contains-all", NAMESPACE);
	public static final Symbol containsAllSymbol_out = new Symbol("java-array-list-contains-all");

	/**
	 * Operator for boolean containsAll(Collection<?> c)
	 */
	@VelkaOperator
	@Description("Returns true if this list contains all of the elements in the specified list.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
			+ "(java-array-list-contains-all k (build-list-native 2 (lambda (x) x))) ;; = #t")
	@Syntax("(java-array-list-contains-all <list1> <list2>)")
	public static final Operator containsAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String collection = "_collection";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, collection),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".containsAll",
									list,
									collection)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteropObject collection = (LitInteropObject) args.get(1);

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
					new TypeTuple(TypeAtom.TypeListJavaArray, TypeAtom.TypeListJavaArray),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return containsAllSymbol;
		}
		
		@Override
		public String toString() {
			return containsAllSymbol_out.toString();
		}

	};

	/**
	 * Symbol for E get(int index)
	 */
	private static final Symbol getSymbol = new Symbol("velka-get", NAMESPACE);
	public static final Symbol getSymbol_out = new Symbol("java-array-list-get");

	/**
	 * Operator for E get(int index)
	 */
	@VelkaOperator
	@Description("Returns the element at the specified position in this list.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n" + "(java-array-list-get l 1) ;; = 1")
	@Syntax("(java-array-list-get <list> <index>)")
	public static final Operator get = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String index = "_index";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, index),
					ClojureHelper.applyClojureFunction(
							".get",
							list,
							index));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteger index = (LitInteger) args.get(1);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) list.javaObject;

			return l.get((int) index.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray, TypeAtom.TypeIntNative), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return getSymbol;
		}
		
		@Override
		public String toString() {
			return getSymbol_out.toString();
		}

	};

	/**
	 * Symbol for int indexOf(Object o)
	 */
	private static final Symbol indexOfSymbol = new Symbol("index-of", NAMESPACE);
	public static final Symbol indexOfSymbol_out = new Symbol("java-array-list-index-of");

	/**
	 * Operator for int indexOf(Object o)
	 */
	@VelkaOperator
	@Description("Returns the index of the first occurrence of the specified element in this list, or -1 if this list does not contain the element.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
			+ "(java-array-list-index-of l 1) ;; = 1")
	@Syntax("(java-array-list-index-of <list> <element>)")
	public static final Operator indexOf = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String object = "_object";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, object),
					LitInteger.clojureLit(
						ClojureHelper.applyClojureFunction(
								".indexOf",
								list,
								object)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			Expression e = args.get(1);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) list.javaObject;

			int ret = l.indexOf(e);

			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray, A), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return indexOfSymbol;
		}
		
		@Override
		public String toString() {
			return indexOfSymbol_out.toString();
		}

	};

	/**
	 * Symbol for boolean isEmpty()
	 */
	private static final Symbol isEmptySymbol = new Symbol("is-empty", NAMESPACE);
	public static final Symbol isEmptySymbol_out = new Symbol("java-array-list-is-empty");

	/**
	 * Operator for boolean isEmpty()
	 */
	@VelkaOperator
	@Description("Returns true if this list contains no elements.") 
	@Example("(java-array-list-is-empty (construct List:JavaArray)) ;; = #t") 
	@Syntax("(java-array-list-is-empty <list>)")
	public static final Operator isEmpty = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".isEmpty",
									list)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) list.javaObject;

			if (l.isEmpty()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return isEmptySymbol;
		}
		
		@Override
		public String toString() {
			return isEmptySymbol_out.toString();
		}

	};

	/**
	 * Symbol for int lastIndexOf(E e)
	 */
	private static final Symbol lastIndexOfSymbol = new Symbol("last-index-of", NAMESPACE);
	public static final Symbol lastIndexOfSymbol_out = new Symbol("java-array-list-last-index-of");

	/**
	 * Operator for int lastIndexOf(E e)
	 */
	@VelkaOperator
	@Description("Returns the index of the last occurrence of the specified element in this list, or -1 if this list does not contain the element.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) 1)))\n"
			+ "(java-array-list-last-index-of l 1) ;; = 2")
	@Syntax("(java-array-list-last-index-of <list> <element>)")
	public static final Operator lastIndexOf = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String object = "_object";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, object),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".lastIndexOf",
									list,
									object)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			Expression e = args.get(1);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) list.javaObject;

			int ret = l.lastIndexOf(e);

			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray, A), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return lastIndexOfSymbol;
		}
		
		@Override
		public String toString() {
			return lastIndexOfSymbol_out.toString();
		}

	};

	/**
	 * Symbol for boolean remove(Object o)
	 */
	private static final Symbol removeSymbol = new Symbol("velka-remove", NAMESPACE);
	public static final Symbol removeSymbol_out = new Symbol("java-array-list-remove");

	/**
	 * Operator for boolean remove(Object o)
	 */
	@VelkaOperator
	@Description("Removes the first occurrence of the specified element from this list, if it is present.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n" + "(java-array-list-remove l 1)\n"
			+ "(println l)\n" + "(0 2)")
	@Syntax("(java-array-list-remove <list> <element>)")
	public static final Operator remove = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String o = "_o";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, o),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".remove",
									list,
									o)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

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
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray, A), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return removeSymbol;
		}

		@Override
		public String toString() {
			return removeSymbol_out.toString();
		}
	};

	/**
	 * symbol for boolean removeAll(Collection<?> c)
	 */
	private static final Symbol removeAllSymbol = new Symbol("remove-all", NAMESPACE);
	public static final Symbol removeAllSymbol_out = new Symbol("java-array-list-remove-all");

	/**
	 * Operator for boolean removeAll(Collection<?> c)
	 */
	@VelkaOperator
	@Description("Removes from this list all of its elements that are contained in the specified collection.")
	@Example("(def l (construct List:JavaArray))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
			+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) 1)))\n" + "(println l)\n" + "(0 1 2 1 1 1)\n"
			+ "(java-array-list-remove l 1)\n" + "(println l)\n" + "(0 2)")
	@Syntax("(java-array-list-remove <list> <element>)")
	public static final Operator removeAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String c = "_c";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, c),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".removeAll",
									list,
									c)));
			return code;
		}

		@SuppressWarnings("unchecked")
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject ec = (LitInteropObject) args.get(1);

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
					new TypeTuple(TypeAtom.TypeListJavaArray, TypeAtom.TypeListJavaArray),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return removeAllSymbol;
		}
		
		@Override
		public String toString() {
			return removeAllSymbol_out.toString();
		}

	};

	/**
	 * Symbol for boolean retainAll(Collection<?> c)
	 */
	private static final Symbol retainAllSymbol = new Symbol("retain-all", NAMESPACE);
	public static final Symbol retainAllSymbol_out = new Symbol("java-array-list-retain-all");

	/**
	 * Operator for boolean retainAll(Collection<?> c)
	 */
	@VelkaOperator
	@Description("Retains only the elements in this list that are contained in the specified collection.") 
	@Example("(def l (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) 1)))\n"
					+ "(java-array-list-retain-all l (build-list-native 2 (lambda (x) (+ 1 x))))\n"
					+ "(println l)\n"
					+ "(2 3)") 
	@Syntax("(java-array-list-retain-all <retained-list> <retainee-list>)")
	public static final Operator retainAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String c = "_c";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, c),
					LitBoolean.clojureLit(
							ClojureHelper.applyClojureFunction(
									".retainAll",
									list,
									c)));
			return code;
		}

		@SuppressWarnings("unchecked")
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject ec = (LitInteropObject) args.get(1);

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
					new TypeTuple(TypeAtom.TypeListJavaArray, TypeAtom.TypeListJavaArray),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return retainAllSymbol;
		}
		
		@Override
		public String toString() {
			return retainAllSymbol_out.toString();
		}

	};

	/**
	 * Symbol for E set(int index, E element)
	 */
	private static final Symbol setSymbol = new Symbol("velka-set", NAMESPACE);
	public static final Symbol setSymbol_out = new Symbol("java-array-list-set");

	/**
	 * Operator for E set(int index, E element)
	 */
	@VelkaOperator
	@Description("Replaces the element at the specified position in this list with the specified element.") 
	@Example("(def l (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-array-list-set l 1 42)\n"
					+ "(println l)\n"
					+ "(0 42 2)") 
	@Syntax("(java-array-list-set <list> <index> <element>)")
	public static final Operator set = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String index = "_index";
			String element = "_element";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, index, element),
					ClojureHelper.applyClojureFunction(
							".set",
							list,
							index,
							element));
			return code;
		}

		@SuppressWarnings("unchecked")
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteger index = (LitInteger) args.get(1);
			Expression element = args.get(2);

			ArrayList<Expression> al = (ArrayList<Expression>) list.javaObject;

			return al.set((int) index.value, element);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray, TypeAtom.TypeIntNative, A),
					A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return setSymbol;
		}
		
		@Override
		public String toString() {
			return setSymbol_out.toString();
		}

	};

	/**
	 * Symbol for int size()
	 */
	private static final Symbol sizeSymbol = new Symbol("velka-size", NAMESPACE);
	public static final Symbol sizeSymbol_out = new Symbol("java-array-list-size");

	/**
	 * Operator for int size()
	 */
	@VelkaOperator
	@Description("Returns the number of elements in this list.") 
	@Example("(def l (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-array-list-size l) ;; = 3") 
	@Syntax("(java-array-list-size <list>)")
	public static final Operator size = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list),
					LitInteger.clojureLit(
							ClojureHelper.applyClojureFunction(
									".size",
									list)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> al = (ArrayList<Expression>) list.javaObject;

			int size = al.size();

			return new LitInteger(size);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return sizeSymbol;
		}
		
		@Override
		public String toString() {
			return sizeSymbol_out.toString();
		}

	};

	/**
	 * Symbol for List<E> subList(int fromIndex, int toIndex)
	 */
	private static final Symbol sublistSymbol = new Symbol("sublist", NAMESPACE);
	public static final Symbol sublistSymbol_out = new Symbol("java-array-list-sublist");

	/**
	 * Operator for List<E> subList(int fromIndex, int toIndex)
	 */
	@VelkaOperator
	@Description("Returns a view of the portion of this list between the specified fromIndex, inclusive, and toIndex, exclusive.") 
	@Example("(def l (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 10 (lambda (x) x)))\n"
					+ "(java-array-list-sublist l 3 7)\n"
					+ ";; = (2 3 4 5 6 7)")
	@Syntax("(java-array-list-sublist <list> <fromIndex> <toIndex>)")
	public static final Operator sublist = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String from = "_from";
			String to = "_to";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, from, to),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction(
									"java.util.ArrayList.",
									ClojureHelper.applyClojureFunction(
											".subList",
											list,
											from,
											to)),
							TypeAtom.TypeListJavaArray));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteger from = (LitInteger) args.get(1);
			LitInteger to = (LitInteger) args.get(2);

			@SuppressWarnings("unchecked")
			ArrayList<Expression> al = (ArrayList<Expression>) list.javaObject;

			ArrayList<Expression> sublist = new ArrayList<Expression>(al.subList((int) from.value, (int) to.value));

			return new LitInteropObject(sublist, TypeAtom.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaArray, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return sublistSymbol;
		}

		@Override
		public String toString() {
			return sublistSymbol_out.toString();
		}
	};

	/**
	 * Symbol for List<T> map(Function<T, E>)
	 */
	private static final Symbol mapSymbol = new Symbol("velka-map", NAMESPACE);
	public static final Symbol mapSymbol_out = new Symbol("java-array-list-map");

	/**
	 * Operator for List<T> map(Function<T, E>)
	 */
	@VelkaOperator
	@Description("Returns a List:JavaArray consisting of the results of applying the given function to the elements of list.") 
	@Example("(def l (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-array-list-map l (lambda (x) (+ x 2)))\n"
					+ ";; = (2 3 4)") 
	@Syntax("(java-array-list-map <list> <function>)")
	public static final Operator map = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String abst = "_abst";
			String e = "_e";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, abst),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction(
									"java.util.ArrayList.",
									ClojureHelper.applyClojureFunction(
											"map",
											ClojureHelper.fnHelper(
													Arrays.asList(e),
													ClojureHelper.applyVelkaFunction(
															abst,
															e)),
											list)),
							TypeAtom.TypeListJavaArray));
		
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

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

			return new LitInteropObject(rslt, TypeAtom.TypeListJavaArray);
		}
		
		private TypeVariable A = new TypeVariable(NameGenerator.next());
		private TypeVariable B = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaArray, new TypeArrow(new TypeTuple(A), B)),
					TypeAtom.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return mapSymbol;
		}

		@Override
		public String toString() {
			return mapSymbol_out.toString();
		}
	};

	/**
	 * Symbol for List<T> map2(List<E2> other, Function<T, E1, E2>)
	 */
	private static final Symbol map2Symbol = new Symbol("velka-map2", NAMESPACE);
	public static final Symbol map2Symbol_out = new Symbol("java-array-list-map2");

	/**
	 * Operator for List<T> map2(List<E2> other, Function<T, E1, E2>)
	 */
	@VelkaOperator
	@Description("Returns a List:JavaArray consisting of the results of applying the given function to the elements of list1 and list2.") 
	@Example("(def l1 (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l1 (build-list-native 3 (lambda (x) x)))\n"
					+ "(def l2 (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l2 (build-list-native 3 (lambda (x) (+ x 1))))\n"
					+ "(java-array-list-map2 l1 l2 +)\n"
					+ ";; = (1 3 5)") 
	@Syntax("(java-array-list-map2 <list1> <list2> <function>)")
	public static final Operator map2 = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list1 = "_list1";
			String list2 = "_list2";
			String abst = "_abst";
			String e1 = "_e1";
			String e2 = "_e2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list1, list2, abst),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction(
									"java.util.ArrayList.",
									ClojureHelper.applyClojureFunction(
											"map",
											ClojureHelper.fnHelper(
													Arrays.asList(e1, e2),
													ClojureHelper.applyVelkaFunction(
															abst,
															e1,
															e2)),
											list1,
											list2)),
							TypeAtom.TypeListJavaArray));

			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

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

			return new LitInteropObject(rslt,TypeAtom.TypeListJavaArray);
		}
		
		private TypeVariable A = new TypeVariable(NameGenerator.next());
		private TypeVariable B = new TypeVariable(NameGenerator.next());
		private TypeVariable C = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray,
					TypeAtom.TypeListJavaArray, new TypeArrow(new TypeTuple(A, B), C)),
					TypeAtom.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return map2Symbol;
		}
		
		@Override
		public String toString() {
			return map2Symbol_out.toString();
		}

	};

	/**
	 * Symbol for T foldl(Function<T, E, T>)
	 */
	private static final Symbol foldlSymbol = new Symbol("foldl", NAMESPACE);
	public static final Symbol foldlSymbol_out = new Symbol("java-array-list-foldl");

	/**
	 * Operator for T foldl(Function<T, E, T>)
	 */
	@VelkaOperator
	@Description("Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the beginning.") 
	@Example("(def l1 (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) (+ x 1))))\n"
					+ "(java-array-list-foldl / 0 l) ;; = 0.16666666666666666666666666666667") 
	@Syntax("(java-array-list-foldl <function> <terminator> <list>)")
	public static final Operator foldl = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String abst = "_abst";
			String term = "_term";
			String list = "_list";
			String agg = "_agg";
			String element = "_element";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(abst, term, list),
					ClojureHelper.applyClojureFunction(
							"reduce",
							ClojureHelper.fnHelper(
									Arrays.asList(agg, element),
									ClojureHelper.applyVelkaFunction(
											abst,
											agg,
											element)),
							term,
							list));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
		
		private TypeVariable A = new TypeVariable(NameGenerator.next());
		private TypeVariable B = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays
					.asList(new TypeArrow(new TypeTuple(Arrays.asList(A, B)), A), A, TypeAtom.TypeListJavaArray)),
					A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return foldlSymbol;
		}

		@Override
		public String toString() {
			return foldlSymbol_out.toString();
		}
	};

	/**
	 * Symbol for T foldr(Function<T, E, T>)
	 */
	private static final Symbol foldrSymbol = new Symbol("foldr", NAMESPACE);
	public static final Symbol foldrSymbol_out = new Symbol("java-array-list-foldr");

	/**
	 * Operator for T foldr(Function<T, E, T>)
	 */
	@VelkaOperator
	@Description("Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the end.") 
	@Example("(def l1 (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 3 (lambda (x) (+ x 1))))\n"
					+ "(java-array-list-foldr / 0 l) ;; = 1.5") 
	@Syntax("(java-array-list-foldr <function> <terminator> <list>)")
	public static final Operator foldr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String abst = "_abst";
			String term = "_term";
			String list = "_list";
			String agg = "_agg";
			String element = "_element";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(abst, term, list),
					ClojureHelper.applyClojureFunction(
							"reduce",
							ClojureHelper.fnHelper(
									Arrays.asList(agg, element),
									ClojureHelper.applyVelkaFunction(
											abst,
											agg,
											element)),
							term,
							ClojureHelper.applyClojureFunction(
									"reverse",
									list)));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
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

		private TypeVariable A = new TypeVariable(NameGenerator.next());
		private TypeVariable B = new TypeVariable(NameGenerator.next());
		
		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays
					.asList(new TypeArrow(new TypeTuple(Arrays.asList(A, B)), A), A, TypeAtom.TypeListJavaArray)),
					A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return foldrSymbol;
		}
		
		@Override
		public String toString() {
			return foldrSymbol_out.toString();
		}

	};

	public static final Symbol ArrayListToLinkedListSymbol = new Symbol("to-linked-list", NAMESPACE);
	public static final Symbol ArrayListToLinkedListSymbol_out = new Symbol("array-list-2-linked-list");

	/**
	 * Conversion ArrayList 2 LinkedList
	 */
	@VelkaConversion
	@Description("Converts List:JavaArray to List:JavaLinked.") 
	@Example("(array-list-2-linked-list (construct List:JavaArray))") 
	@Syntax("(array-list-2-linked-list <array list>)")
	public static Conversion ArrayListToLinkedList = new Conversion() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction(
									"java.util.LinkedList.",
									list),
							 TypeAtom.TypeListJavaLinked));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			
			LitInteropObject lio = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) lio.javaObject;

			return new LitInteropObject(new LinkedList<Expression>(l), TypeAtom.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray),
					TypeAtom.TypeListJavaLinked);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return ArrayListToLinkedListSymbol;
		}
		
		@Override
		public String toString() {
			return ArrayListToLinkedListSymbol_out.toString();
		}

		@Override
		public Expression cost() {
			var arg = new Symbol(NameGenerator.next());
			return new Lambda(new Tuple(arg), new TypeTuple(TypeAtom.TypeListJavaArray),
					new AbstractionApplication(JavaArrayList.size, new Tuple(arg)));
		}

	};

	public static final Symbol ArrayListToNativeListSymbol = new Symbol("to-velka-list", NAMESPACE);
	public static final Symbol ArrayListToNativeListSymbol_out = new Symbol("array-list-2-native-list");

	/**
	 * Conversion ArrayList 2 NativeList
	 */
	@VelkaConversion
	@Description("Converts List:JavaArray to List:Native.") 
	@Example("(array-list-2-native-list (construct List:JavaArray))") 
	@Syntax("(array-list-2-native-list <array list>)")
	public static Conversion ArrayListToNativeList = new Conversion() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";

			String code = ClojureHelper
					.fnHelper(Arrays.asList(list),
							LitComposite.clojureValueToClojureLiteral(
									ClojureHelper.applyClojureFunction("lazy-seq",
											ClojureHelper.applyClojureFunction("seq",
													list)),
									TypeAtom.TypeListNative));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			
			var lio = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) lio.javaObject;

			LinkedList<Expression> ll = new LinkedList<Expression>(l);

			return new LitInteropObject(ll, TypeAtom.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaArray), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return ArrayListToNativeListSymbol;
		}
		
		@Override
		public String toString() {
			return ArrayListToNativeListSymbol_out.toString();
		}

		@Override
		public Expression cost() {
			var arg = new Symbol(NameGenerator.next());
			return new Lambda(new Tuple(arg), new TypeTuple(TypeAtom.TypeListJavaArray),
					new AbstractionApplication(JavaArrayList.size, new Tuple(arg)));
		}
	};

	public static final Symbol everypSymbol = new Symbol("velka-everyp", NAMESPACE);
	public static final Symbol everypSymbol_out = new Symbol("java-array-list-everyp");

	@VelkaOperator
	@Description("Returns true if every element of this list returns true for the predicate. Otherwise returns false.") 
	@Example("(define l (construct List:JavaArray))\n"
					+ "(java-array-list-add-all l (build-list-native 10 (* 2 x)))\n"
					+ "(java-array-list-everyp l (lambda (x) (= (mod x 2) 0))) ;; = #t\n"
					+ "(java-array-list-everyp l (lambda (x) (= x 1))) ;; = #f") 
	@Syntax("(java-array-list-everyp <list> <predicate>)")
	public static final Operator everyp = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String pred = "_pred";
			String pred_arg = "_arg";
			String code = ClojureHelper.fnHelper(Arrays.asList(list, pred),
					LitBoolean.clojureLit(ClojureHelper.applyClojureFunction("every?",
							ClojureHelper.fnHelper(Arrays.asList(pred_arg),
									ClojureHelper.applyVelkaFunction(pred, pred_arg)),
							list)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return everypSymbol;
		}
		
		@Override
		public String toString() {
			return everypSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
			@SuppressWarnings("unchecked")
			ArrayList<Expression> l = (ArrayList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			Expression pred = args.get(1);

			Boolean ret = l.stream().allMatch(ThrowingPredicate.wrapper(expr -> {
				AbstractionApplication appl = new AbstractionApplication(pred, new Tuple((Expression) expr));
				Expression rslt = appl.interpret(env, typeEnv);
				return rslt.equals(LitBoolean.TRUE);
			}));

			return ret ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaArray, new TypeArrow(
							new TypeTuple(new TypeVariable(NameGenerator.next())), TypeAtom.TypeBoolNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	};

	private static final Symbol buildListSymbol = new Symbol("build-list", NAMESPACE);
	public static final Symbol buildListSymbol_out = new Symbol("java-array-list-build");
	
	@VelkaOperator
	@Description("Builds java array list") 
	@Example("(java-array-list-build 5 (lambda (x) x))") 
	@Syntax("(java-array-list-build <n> <build-function>)")
	public static final Operator buildList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String n = "_n";
			String buildFun = "_build-fun";
			String x = "_x";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(n, buildFun),
							ClojureHelper.instantiateJavaClass(
									ArrayList.class,
									ClojureHelper.applyClojureFunction(
											"map",
											ClojureHelper.fnHelper(
													Arrays.asList(x),
													ClojureHelper.applyVelkaFunction(
															buildFun,
															LitInteger.clojureLit(x))),
											ClojureHelper.applyClojureFunction(
													"range",
													n))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return buildListSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv)
				throws AppendableException {
			LitInteger n = (LitInteger)args.get(0);
			Expression buildFun = args.get(1);
			
			ArrayList<Expression> l = new ArrayList<Expression>((int)n.value);
			for(int i = 0; i < n.value; i++) {
				Application appl = new AbstractionApplication(buildFun, new Tuple(new LitInteger(i)));
				Expression e = appl.interpret(env, typeEnv);
				l.add(e);
			}
			
			return new LitInteropObject(l, TypeAtom.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeVariable T = new TypeVariable(NameGenerator.next());
			Type type = new TypeArrow(new TypeTuple(
								TypeAtom.TypeIntNative, 
								new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), T)),
									TypeAtom.TypeListJavaArray);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return buildListSymbol_out.toString();
		}
		
	};

	public static final Path VELKA_CLOJURE_ARRAYLIST_PATH = Paths.get("velka", "clojure");

	public static final Path VELKA_CLOJURE_ARRAYLIST_NAME = Paths.get("arrayList.clj");

	@Override
	public String getNamespace() {
		return NAMESPACE;
	}

	@Override
	public Path getPath() {
		return VELKA_CLOJURE_ARRAYLIST_PATH;
	}

	@Override
	public Path getFileName() {
		return VELKA_CLOJURE_ARRAYLIST_NAME;
	}

	@Override
	public void initTypes(TypeEnvironment typeEnv) throws DuplicateTypeDefinitionException {
		typeEnv.addRepresentation(TypeAtom.TypeListJavaArray);
	}
	
	private static JavaArrayList instance = null;
	private JavaArrayList() {}
	public static JavaArrayList singleton() {
		if(instance == null) {
			instance = new JavaArrayList();
		}
		return instance;
	}
	
	public static Expression of(Expression ...vals) {
		return new LitInteropObject(new ArrayList<Expression>(List.of(vals)), TypeAtom.TypeListJavaArray);
	}
}
