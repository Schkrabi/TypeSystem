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
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
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
 * 
 * This class contains utilities to work with LinkedList in velka
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
@VelkaOperatorBank
@Description("Operators for working with wrapped java.util.LinkedList.") 
@Header("Linked List")
public class JavaLinkedList extends OperatorBank {

	/**
	 * Clojure namespace symbol for JavaLinkedList
	 */
	public static final String NAMESPACE = "velka.clojure.linkedList";

	private static final Symbol constructorSymbol = new Symbol("velka-construct", NAMESPACE);
	public static final Symbol constructorSymbol_out = new Symbol("construct-linked-list");

	/**
	 * Constructor
	 */
	@VelkaConstructor
	@Description("Constructs empty List:Linked.") 
	@Name("Construct empty list") 
	@Syntax("(construct List:JavaLinked)")
	public static final Constructor constructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String code = ClojureHelper.fnHelper(
					Arrays.asList(),
					ClojureHelper.applyClojureFunction("java.util.LinkedList."));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression e = new LitInteropObject(new LinkedList<Object>(), TypeAtom.TypeListJavaLinked);
			return e;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeListJavaLinked);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorSymbol;
		}
	};

	/**
	 * Symbol for boolean add(E e)
	 */
	private static final Symbol addToEndSymbol = new Symbol("add-to-end", NAMESPACE);
	public static final Symbol addToEndSymbol_out = new Symbol("java-linked-list-add-to-end");

	/**
	 * Operator for boolean add(E e)
	 */
	@VelkaOperator
	@Description("Appends the specified element to the end of this list.") 
	@Example("(java-linked-list-add-to-end (construct List:JavaLinked) 42)") 
	@Syntax("(java-linked-list-add-to-end <list> <element>)")
	public static final Operator addToEnd = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);
			Expression e = args.get(1);

			@SuppressWarnings("unchecked")
			LinkedList<Object> l = (LinkedList<Object>) list.javaObject;
			l.add(e);

			return LitBoolean.TRUE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked, A),
					TypeAtom.TypeBoolNative);
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
	public static final Symbol addToIndexSymbol_out = new Symbol("java-linked-list-add-to-index");

	/**
	 * Operator for void add(int index, E element)
	 */
	@VelkaOperator
	@Description("Inserts the specified element at the specified position in this list.") 
	@Example("(java-linked-list-add-to-index (construct List:JavaLinked) 0 42)") 
	@Syntax("(java-linked-list-add-to-index <list> <index> <element>)")
	public static final Operator addToIndex = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
											Expression.EMPTY_EXPRESSION.toClojureCode(env)))));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteger index = (LitInteger) args.get(1);

			Expression e = args.get(2);

			@SuppressWarnings("unchecked")
			LinkedList<Object> l = (LinkedList<Object>) list.javaObject;
			l.add((int) index.value, e);

			return Expression.EMPTY_EXPRESSION;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked, TypeAtom.TypeIntNative, A),
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
	public static final Symbol addAllSymbol_out = new Symbol("java-linked-list-add-all");

	/**
	 * operator for boolean addAll(Collection<? extends E> c)
	 */
	@VelkaOperator
	@Description("Appends all of the elements in the specified collection to the end of this list, in the order that they are returned by the specified collection's Iterator.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add l 42)\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(println l)\n"
					+ ";;(42 0 1 2)") 
	@Syntax("(java-linked-list-add-all <list1> <list2>)")
	public static final Operator addAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String collection = "_collection";
			String code = ClojureHelper.fnHelper(Arrays.asList(list, collection),
					LitBoolean.clojureLit(ClojureHelper.applyClojureFunction(".addAll",
							list, collection)));

			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			
			LitInteropObject collection = (LitInteropObject) args.get(1);

			@SuppressWarnings("unchecked")
			LinkedList<Object> l = (LinkedList<Object>) list.javaObject;
			@SuppressWarnings("unchecked")
			LinkedList<Object> c = (LinkedList<Object>) collection.javaObject;

			if (l.addAll(c)) {
				return LitBoolean.TRUE;
			}

			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaLinked, TypeAtom.TypeListJavaLinked),
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
	public static final Symbol containsSymbol_out = new Symbol("java-linked-list-contains");

	/**
	 * Operator for boolean contains(Object o)
	 */
	@VelkaOperator
	@Description("Returns true if this list contains the specified element.") 
	@Example("(java-linked-list-contains (construct List:JavaLinked) 42) ; = #f") 
	@Syntax("(java-linked-list-contains <list> <element>)")
	public static final Operator contains = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			@SuppressWarnings("unchecked")
			LinkedList<Object> l = (LinkedList<Object>) list.javaObject;

			Expression e = args.get(1);

			if (l.contains(e)) {
				return LitBoolean.TRUE;
			}

			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked, A),
					TypeAtom.TypeBoolNative);
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
	public static final Symbol containsAllSymbol_out = new Symbol("java-linked-list-contains-all");

	/**
	 * Operator for boolean containsAll(Collection<?> c)
	 */
	@VelkaOperator
	@Description("Returns true if this list contains all of the elements in the specified list.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-linked-list-contains-all k (build-list-native 2 (lambda (x) x))) ;; = #t") 
	@Syntax("(java-linked-list-contains-all <list1> <list2>)")
	public static final Operator containsAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			
			LitInteropObject collection = (LitInteropObject) args.get(1);

			@SuppressWarnings("unchecked")
			LinkedList<Object> l = (LinkedList<Object>) list.javaObject;
			@SuppressWarnings("unchecked")
			LinkedList<Object> c = (LinkedList<Object>) collection.javaObject;

			if (l.containsAll(c)) {
				return LitBoolean.TRUE;
			}

			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaLinked, TypeAtom.TypeListJavaLinked),
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
	public static final Symbol getSymbol_out = new Symbol("java-linked-list-get");

	/**
	 * Operator for E get(int index)
	 */
	@VelkaOperator
	@Description("Returns the element at the specified position in this list.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-linked-list-get l 1) ;; = 1") 
	@Syntax("(java-linked-list-get <list> <index>)")
	public static final Operator get = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteger index = (LitInteger) args.get(1);

			@SuppressWarnings("unchecked")
			var l = (List<Expression>) list.javaObject;

			return l.get((int) index.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked, TypeAtom.TypeIntNative), A);
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
	private static final Symbol indexOfSymbol = new Symbol("velka-index-of", NAMESPACE);
	public static final Symbol indexOfSymbol_out = new Symbol("java-linked-list-index-of");

	/**
	 * Operator for int indexOf(Object o)
	 */
	@VelkaOperator
	@Description("Returns the index of the first occurrence of the specified element in this list, or -1 if this list does not contain the element.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-linked-list-index-of l 1) ;; = 1") 
	@Syntax("(java-linked-list-index-of <list> <element>)")
	public static final Operator indexOf = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			Expression e = args.get(1);

			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) list.javaObject;

			int ret = l.indexOf(e);

			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked, A), TypeAtom.TypeIntNative);
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
	public static final Symbol isEmptySymbol_out = new Symbol("java-linked-list-is-empty");

	/**
	 * Operator for boolean isEmpty()
	 */
	@VelkaOperator
	@Description("Returns true if this list contains no elements.") 
	@Example("(java-linked-list-is-empty (construct List:JavaLinked)) ;; = #t") 
	@Syntax("(java-linked-list-is-empty <list>)")
	public static final Operator isEmpty = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) list.javaObject;

			if (l.isEmpty()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked), TypeAtom.TypeBoolNative);
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
	public static final Symbol lastIndexOfSymbol_out = new Symbol("java-linked-list-last-index-of");

	/**
	 * Operator for int lastIndexOf(E e)
	 */
	@VelkaOperator
	@Description("Returns the index of the last occurrence of the specified element in this list, or -1 if this list does not contain the element.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) 1)))\n"
					+ "(java-linked-list-last-index-of l 1) ;; = 2") 
	@Syntax("(java-linked-list-last-index-of <list> <element>)")
	public static final Operator lastIndexOf = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			Expression e = args.get(1);

			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) list.javaObject;

			int ret = l.lastIndexOf(e);

			return new LitInteger(ret);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked, A), TypeAtom.TypeIntNative);
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
	public static final Symbol removeSymbol_out = new Symbol("java-linked-list-remove");

	/**
	 * Operator for boolean remove(Object o)
	 */
	@VelkaOperator
	@Description("Removes the first occurrence of the specified element from this list, if it is present.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-linked-list-remove l 1)\n"
					+ "(println l)\n"
					+ "(0 2)") 
	@Syntax("(java-linked-list-remove <list> <element>)")
	public static final Operator remove = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			Expression o = args.get(1);

			@SuppressWarnings("rawtypes")
			LinkedList al = (LinkedList) list.javaObject;

			if (al.remove(o)) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked, A),
					TypeAtom.TypeBoolNative);
			;
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
	public static final Symbol removeAllSymbol_out = new Symbol("java-linked-list-remove-all");

	/**
	 * Operator for boolean removeAll(Collection<?> c)
	 */
	@VelkaOperator
	@Description("Removes from this list all of its elements that are contained in the specified collection.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) 1)))\n"
					+ "(println l)\n"
					+ "(0 1 2 1 1 1)\n"
					+ "(java-linked-list-remove l 1)\n"
					+ "(println l)\n"
					+ "(0 2)")
	@Syntax("(java-linked-list-remove <list> <element>)")
	public static final Operator removeAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject ec = (LitInteropObject) args.get(1);

			@SuppressWarnings("rawtypes")
			LinkedList al = (LinkedList) list.javaObject;
			@SuppressWarnings("rawtypes")
			LinkedList c = (LinkedList) ec.javaObject;

			if (al.removeAll(c)) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaLinked, TypeAtom.TypeListJavaLinked),
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
	public static final Symbol retainAllSymbol_out = new Symbol("java-linked-list-retain-all");

	/**
	 * Operator for boolean retainAll(Collection<?> c)
	 */
	@VelkaOperator
	@Description("Retains only the elements in this list that are contained in the specified collection.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) 1)))\n"
					+ "(java-linked-list-retain-all l (build-list-native 2 (lambda (x) (+ 1 x))))\n"
					+ "(println l)\n"
					+ "(2 3)") 
	@Syntax("(java-linked-list-retain-all <retained-list> <retainee-list>)")
	public static final Operator retainAll = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject ec = (LitInteropObject) args.get(1);

			@SuppressWarnings("rawtypes")
			LinkedList al = (LinkedList) list.javaObject;
			@SuppressWarnings("rawtypes")
			LinkedList c = (LinkedList) ec.javaObject;

			if (al.retainAll(c)) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaLinked, TypeAtom.TypeListJavaLinked),
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
	public static final Symbol setSymbol_out = new Symbol("java-linked-list-set");

	/**
	 * Operator for E set(int index, E element)
	 */
	@VelkaOperator
	@Description("Replaces the element at the specified position in this list with the specified element.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-linked-list-set l 1 42)\n"
					+ "(println l)\n"
					+ "(0 42 2)") 
	@Syntax("(java-linked-list-set <list> <index> <element>)")
	public static final Operator set = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteger index = (LitInteger) args.get(1);
			Expression element = args.get(2);

			LinkedList<Expression> al = (LinkedList<Expression>) list.javaObject;

			return al.set((int) index.value, element);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked, TypeAtom.TypeIntNative, A),
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
	public static final Symbol sizeSymbol_out = new Symbol("java-linked-list-size");

	/**
	 * Operator for int size()
	 */
	@VelkaOperator
	@Description("Returns the number of elements in this list.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-linked-list-size l) ;; = 3") 
	@Syntax("(java-linked-list-size <list>)")
	public static final Operator size = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			@SuppressWarnings("unchecked")
			LinkedList<Expression> al = (LinkedList<Expression>) list.javaObject;

			int size = al.size();

			return new LitInteger(size);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked), TypeAtom.TypeIntNative);
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
	private static final Symbol sublistSymbol = new Symbol("velka-sublist", NAMESPACE);
	public static final Symbol sublistSymbol_out = new Symbol("java-linked-list-sublist");

	/**
	 * Operator for List<E> subList(int fromIndex, int toIndex)
	 */
	@VelkaOperator
	@Description("Returns a view of the portion of this list between the specified fromIndex, inclusive, and toIndex, exclusive.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) x)))\n"
					+ "(java-linked-list-sublist l 3 7)\n"
					+ ";; = (2 3 4 5 6 7)") 
	@Syntax("(java-linked-list-sublist <list> <fromIndex> <toIndex>)")
	public static final Operator sublist = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String from = "_from";
			String to = "_to";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, from, to),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction(
									"java.util.LinkedList.",
									ClojureHelper.applyClojureFunction(
											".subList",
											list,
											from,
											to)),
							TypeAtom.TypeListJavaLinked));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			LitInteger from = (LitInteger) args.get(1);
			LitInteger to = (LitInteger) args.get(2);

			@SuppressWarnings("unchecked")
			LinkedList<Expression> al = (LinkedList<Expression>) list.javaObject;

			LinkedList<Expression> sublist = new LinkedList<Expression>(al.subList((int) from.value, (int) to.value));

			return new LitInteropObject(sublist, TypeAtom.TypeListJavaLinked);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaLinked, TypeAtom.TypeIntNative, TypeAtom.TypeIntNative),
					TypeAtom.TypeListJavaLinked);
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
	public static final Symbol mapSymbol_out = new Symbol("java-linked-list-map");

	/**
	 * Operator for List<T> map(Function<T, E>)
	 */
	@VelkaOperator
	@Description("Returns a List:JavaLinked consisting of the results of applying the given function to the elements of list.") 
	@Example("(def l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) x)))\n"
					+ "(java-linked-list-map l (lambda (x) (+ x 2)))\n"
					+ ";; = (2 3 4)") 
	@Syntax("(java-linked-list-map <list> <function>)")
	public static final Operator map = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String abst = "_abst";
			String e = "_e";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, abst),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction(
									"java.util.LinkedList.",
									ClojureHelper.applyClojureFunction(
											"map",
											ClojureHelper.fnHelper(
													Arrays.asList(e),
													ClojureHelper.applyVelkaFunction(
															abst,
															e)),
											list)),
							TypeAtom.TypeListJavaLinked));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			Abstraction abst = (Abstraction) args.get(1);

			@SuppressWarnings("unchecked")
			LinkedList<Expression> al = (LinkedList<Expression>) list.javaObject;
			LinkedList<Expression> rslt = null;
			try {
				rslt = new LinkedList<Expression>(al.stream().map(ThrowingFunction.wrapper(e -> {
					AbstractionApplication appl = new AbstractionApplication(abst, new Tuple(e));
					return appl.interpret(env);
				})).collect(Collectors.toList()));
			} catch (RuntimeException re) {
				if (re.getCause() instanceof AppendableException) {
					AppendableException e = (AppendableException) re.getCause();
					throw e;
				}
				throw re;
			}

			return new LitInteropObject(rslt, TypeAtom.TypeListJavaLinked);
		}
		
		private TypeVariable A = new TypeVariable(NameGenerator.next());
		private TypeVariable B = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaLinked, new TypeArrow(new TypeTuple(A), B)),
					TypeAtom.TypeListJavaLinked);
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
	public static final Symbol map2Symbol_out = new Symbol("java-linked-list-map2");

	/**
	 * Operator for List<T> map2(List<E2> other, Function<T, E1, E2>)
	 */
	@VelkaOperator
	@Description("Returns a List:JavaLinked consisting of the results of applying the given function to the elements of list1 and list2.") 
	@Example("(def l1 (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l1 (build-list-native 3 (lambda (x) x)))\n"
					+ "(def l2 (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l2 (build-list-native 3 (lambda (x) (+ x 1))))\n"
					+ "(java-linked-list-map2 l1 l2 +)\n"
					+ ";; = (1 3 5)") 
	@Syntax("(java-linked-list-map2 <list1> <list2> <function>)")
	public static final Operator map2 = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list1 = "_list1";
			String list2 = "_list2";
			String abst = "_abst";
			String e1 = "_e1";
			String e2 = "_e2";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list1, list2, abst),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction(
									"java.util.LinkedList.",
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
							TypeAtom.TypeListJavaLinked));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			// Need to extract LitComposite carrying type info first
			
			// Now I can get to LitInteropObject carrying java.util.ArrayList
			LitInteropObject list = (LitInteropObject) args.get(0);

			
			LitInteropObject list2 = (LitInteropObject) args.get(1);

			Abstraction abst = (Abstraction) args.get(2);

			@SuppressWarnings("unchecked")
			LinkedList<Expression> l1 = (LinkedList<Expression>) list.javaObject;
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l2 = (LinkedList<Expression>) list2.javaObject;
			LinkedList<Expression> rslt = new LinkedList<Expression>();

			Iterator<Expression> i1 = l1.iterator();
			Iterator<Expression> i2 = l2.iterator();

			while (i1.hasNext() && i2.hasNext()) {
				Expression e1 = i1.next();
				Expression e2 = i2.next();

				AbstractionApplication appl = new AbstractionApplication(abst, new Tuple(e1, e2));

				rslt.add(appl.interpret(env));
			}

			return new LitInteropObject(rslt, TypeAtom.TypeListJavaLinked);
		}

		private TypeVariable A = new TypeVariable(NameGenerator.next());
		private TypeVariable B = new TypeVariable(NameGenerator.next());
		private TypeVariable C = new TypeVariable(NameGenerator.next());
		
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked,
					TypeAtom.TypeListJavaLinked, new TypeArrow(new TypeTuple(A, B), C)),
					TypeAtom.TypeListJavaLinked);
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
	public static final Symbol foldlSymbol_out = new Symbol("java-linked-list-foldl");

	/**
	 * Operator for T foldl(Function<T, E, T>)
	 */
	@VelkaOperator
	@Description("Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the beginning.") 
	@Example("(def l1 (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) (+ x 1))))\n"
					+ "(java-linked-list-foldl / 0 l) ;; = 0.16666666666666666666666666666667") 
	@Syntax("(java-linked-list-foldl <function> <terminator> <list>)")
	public static final Operator foldl = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Abstraction abst = (Abstraction) args.get(0);
			Expression terminator = args.get(1);
			LitInteropObject io = (LitInteropObject) args.get(2);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> list = (LinkedList<Expression>) io.javaObject;

			Expression rslt = Expression.EMPTY_EXPRESSION;
			try {
				rslt = list.stream().reduce(terminator, ThrowingBinaryOperator.wrapper((agg, element) -> {
					AbstractionApplication app = new AbstractionApplication(abst, new Tuple(agg, element));
					return app.interpret(env);
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
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(
					new TypeArrow(new TypeTuple(Arrays.asList(A, B)), A), A, TypeAtom.TypeListJavaLinked)), A);
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
	public static final Symbol foldrSymbol_out = new Symbol("java-linked-list-foldr");

	/**
	 * Operator for T foldr(Function<T, E, T>)
	 */
	@VelkaOperator
	@Description("Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the end.") 
	@Example("(def l1 (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 3 (lambda (x) (+ x 1))))\n"
					+ "(java-linked-list-foldr / 0 l) ;; = 1.5") 
	@Syntax("(java-linked-list-foldr <function> <terminator> <list>)")
	public static final Operator foldr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Abstraction abst = (Abstraction) args.get(0);
			Expression terminator = args.get(1);
			LitInteropObject io = (LitInteropObject) args.get(2);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> list = (LinkedList<Expression>) io.javaObject;

			Expression agg = terminator;
			ListIterator<Expression> i = list.listIterator(list.size());
			while (i.hasPrevious()) {
				Expression element = i.previous();
				AbstractionApplication app = new AbstractionApplication(abst, new Tuple(agg, element));
				agg = app.interpret(env);
			}

			return agg;
		}
		
		private TypeVariable A = new TypeVariable(NameGenerator.next());
		private TypeVariable B = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(
					new TypeArrow(new TypeTuple(Arrays.asList(A, B)), A), A, TypeAtom.TypeListJavaLinked)), A);
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

	public static final Symbol LinkedListToArrayListSymbol = new Symbol("to-array-list", NAMESPACE);
	public static final Symbol LinkedListToArrayListSymbol_out = new Symbol("linked-list-2-array-list");

	/**
	 * Conversion LinkedList 2 ArrayList
	 */
	@VelkaConversion
	@Description("Converts List:JavaLinked to List:JavaArray)") 
	@Example("(linked-list-2-array-list (construct List:JavaLinked))") 
	@Syntax("(linked-list-2-array-list <linked-list>)")
	public static Conversion LinkedListToArrayList = new Conversion() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list),
						ClojureHelper.applyClojureFunction(
									"java.util.ArrayList.",
									list));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			
			LitInteropObject lio = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) lio.javaObject;

			return new LitInteropObject(new ArrayList<Expression>(l), TypeAtom.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked),
					TypeAtom.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return LinkedListToArrayListSymbol;
		}

		@Override
		public String toString() {
			return LinkedListToArrayListSymbol_out.toString();
		}

		@Override
		public Expression cost() {
			var arg = new Symbol(NameGenerator.next());
			return new Lambda(new Tuple(arg), new TypeTuple(TypeAtom.TypeListJavaLinked),
					new AbstractionApplication(JavaLinkedList.size, new Tuple(arg)));
		}
	};

	public static final Symbol LinkedListToNativeListSymbol = new Symbol("to-list-native", NAMESPACE);
	public static final Symbol LinkedListToNativeListSymbol_out = new Symbol("linked-list-2-native-list");

	/**
	 * Conversion LinkedList 2 NativeList
	 */
	@VelkaConversion
	@Description("Converts List:JavaLinked to List:Native.") 
	@Example("(linked-list-2-native-list (construct List:JavaLinked))") 
	@Syntax("(linked-list-2-native-list <linked list>)")
	public static Conversion LinkedListToNativeList = new Conversion() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction(
									"seq", 
									list),
							TypeAtom.TypeListNative));
			return code;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			
			LitInteropObject lio = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) lio.javaObject;

			LinkedList<Expression> ll = new LinkedList<Expression>(l);

			return new LitInteropObject(ll, TypeAtom.TypeListJavaLinked);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Symbol getClojureSymbol() {
			return LinkedListToNativeListSymbol;
		}
		
		@Override
		public String toString() {
			return LinkedListToNativeListSymbol_out.toString();
		}

		@Override
		public Expression cost() {
			var arg = new Symbol(NameGenerator.next());
			return new Lambda(new Tuple(arg), new TypeTuple(TypeAtom.TypeListJavaLinked),
					new AbstractionApplication(JavaLinkedList.size, new Tuple(arg)));
		}
	};

	public static final Symbol everypSymbol = new Symbol("velka-everyp", NAMESPACE);
	public static final Symbol everypSymbol_out = new Symbol("java-linked-list-everyp");

	@VelkaOperator
	@Description("Returns true if every element of this list returns true for the predicate. Otherwise returns false.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(java-linked-list-everyp l (lambda (x) (= (mod x 2) 0))) ;; = #t\n"
					+ "(java-linked-list-everyp l (lambda (x) (= x 1))) ;; = #f") 
	@Syntax("(java-linked-list-everyp <list> <predicate>)")
	public static final Operator everyp = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var lio = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) lio.javaObject;
			Expression pred = args.get(1);

			Boolean ret = l.stream().allMatch(ThrowingPredicate.wrapper(expr -> {
				AbstractionApplication appl = new AbstractionApplication(pred, new Tuple((Expression) expr));
				Expression rslt = appl.interpret(env);
				return rslt.equals(LitBoolean.TRUE);
			}));

			return ret ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaLinked, new TypeArrow(
							new TypeTuple(new TypeVariable(NameGenerator.next())), TypeAtom.TypeBoolNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	};
	
	public static final Symbol toStrSymbol = new Symbol("velka-to-str", NAMESPACE);
	public static final Symbol toStrSymbol_out = new Symbol("java-linked-list-to-str");
	
	@VelkaOperator
	@Description("Returns readable string representation of list.")
	@Example("(define l (construct List:JavaLinked))\n"
			+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
			+ "(java-linked-list-to-str l)")
	@Syntax("(java-linked-list-to-str <list>)")
	public static final Operator toStr = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list),
					LitString.clojureLit(
							ClojureHelper.applyClojureFunction(
									".toString",
									list)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return toStrSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject list_io = (LitInteropObject)args.get(0);
			LinkedList<?> list = (LinkedList<?>)list_io.javaObject;
			return new LitString(list.toString());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeListJavaLinked),
					TypeAtom.TypeStringNative);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return toStrSymbol_out.toString();
		}
		
	};
	
	public static final Symbol listIteratorSymbol = new Symbol("list-iterator", NAMESPACE);
	public static final Symbol listIteratorSymbol_out = new Symbol("java-linked-list-iterator");
	@VelkaOperator
	@Description("Returns a list-iterator of the elements in this list (in proper sequence), starting at the specified position in the list.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(java-linked-list-iterator l 0)") 
	@Syntax("(java-linked-list-iterator <list> <index>)")
	public static final Operator listIterator = new Operator() {
	
		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String index = "_index";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, index),
							ClojureHelper.applyClojureFunction(
									".listIterator",
									list,
									index));
			return code;
		}
	
		@Override
		public Symbol getClojureSymbol() {
			return listIteratorSymbol;
		}
	
		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env)
				throws AppendableException {
			
			LitInteropObject list_io = (LitInteropObject)args.get(0);
			LinkedList<?> list = (LinkedList<?>)list_io.javaObject;
			
			LitInteger index = (LitInteger)args.get(1);
			
			ListIterator<?> li = list.listIterator((int) index.value);
			
			return new LitInteropObject(li, TypeAtom.TypeListIterator);
		}
	
		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaLinked, TypeAtom.TypeIntNative),
					TypeAtom.TypeListIterator);
			return Pair.of(type, Substitution.EMPTY);
		}
		
		@Override
		public String toString() {
			return listIteratorSymbol_out.toString();
		}
	};
	
	public static final Path VELKA_CLOJURE_LINKEDLIST_PATH = Paths.get("velka", "clojure");

	public static final Path VELKA_CLOJURE_LINKEDLIST_NAME = Paths.get("linkedList.clj");

	@Override
	public String getNamespace() {
		return NAMESPACE;
	}

	@Override
	public Path getPath() {
		return VELKA_CLOJURE_LINKEDLIST_PATH;
	}

	@Override
	public Path getFileName() {
		return VELKA_CLOJURE_LINKEDLIST_NAME;
	}
	
	private static JavaLinkedList instance = null;
	private JavaLinkedList() {}
	public static JavaLinkedList singleton() {
		if(instance == null) {
			instance = new JavaLinkedList();
		}
		return instance;
	}
	
	public static Expression of(Expression ...vals) {
		return new LitInteropObject(new LinkedList<Expression>(List.of(vals)), TypeAtom.TypeListJavaLinked);
	}
}
