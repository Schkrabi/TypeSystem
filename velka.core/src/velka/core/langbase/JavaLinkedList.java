package velka.core.langbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.stream.Collectors;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JMod;

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
import velka.core.literal.LitInteropObject;
import velka.core.literal.Literal;
import velka.java.CodeModelInstance;
import velka.java.TypeUtil;
import velka.java.runtime.JavaTypeSystem;
import velka.java.runtime.VelkaTuple;
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
import velka.util.ThrowingFunction;
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
	public static final Symbol constructorSymbol_out = new Symbol("construct-linked-list");

	/**
	 * Constructor
	 */
	@VelkaConstructor
	@Description("Constructs empty List:Linked.") 
	@Name("Construct empty list") 
	@Syntax("(construct List:JavaLinked)")
	public static final Constructor constructor = Constructor.wrapJavaContructorToType(LinkedList.class, JavaLinkedList.singleton().getNamespace(),
			TypeAtom.TypeListJavaLinked);
	
	@VelkaConstructor
	public static final Constructor copyConstructor = Constructor.wrapJavaContructorToType(LinkedList.class, JavaLinkedList.singleton().getNamespace(), 
			TypeAtom.TypeListJavaLinked, java.util.Collection.class);

	public static final Symbol addToEndSymbol_out = new Symbol("java-linked-list-add-to-end");

	/**
	 * Operator for boolean add(E e)
	 */
	@VelkaOperator
	@Description("Appends the specified element to the end of this list.") 
	@Example("(java-linked-list-add-to-end (construct List:JavaLinked) 42)") 
	@Syntax("(java-linked-list-add-to-end <list> <element>)")
	public static final Operator addToEnd = Operator.wrapJavaMethod(LinkedList.class, "add", "java-linked-list-add-to-end",
			JavaLinkedList.singleton().getNamespace(), Object.class);

	public static final Symbol addToIndexSymbol_out = new Symbol("java-linked-list-add-to-index");

	/**
	 * Operator for void add(int index, E element)
	 */
	@VelkaOperator
	@Description("Inserts the specified element at the specified position in this list.") 
	@Example("(java-linked-list-add-to-index (construct List:JavaLinked) 0 42)") 
	@Syntax("(java-linked-list-add-to-index <list> <index> <element>)")
	public static final Operator addToIndex = Operator.wrapJavaMethod(LinkedList.class, "add", "java-linked-list-add-to-index",
			JavaLinkedList.singleton().getNamespace(), int.class, Object.class);

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
	public static final Operator addAll = Operator.wrapJavaMethod(LinkedList.class, "addAll", "java-linked-list-add-all",
			JavaLinkedList.singleton().getNamespace(), java.util.Collection.class);

	public static final Symbol containsSymbol_out = new Symbol("java-linked-list-contains");

	/**
	 * Operator for boolean contains(Object o)
	 */
	@VelkaOperator
	@Description("Returns true if this list contains the specified element.") 
	@Example("(java-linked-list-contains (construct List:JavaLinked) 42) ; = #f") 
	@Syntax("(java-linked-list-contains <list> <element>)")
	public static final Operator contains = Operator.wrapJavaMethod(LinkedList.class, "contains", "java-linked-list-contains",
			JavaLinkedList.singleton().getNamespace(), Object.class);

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
	public static final Operator containsAll = Operator.wrapJavaMethod(LinkedList.class, "containsAll", "java-linked-list-contains-all",
			JavaLinkedList.singleton().getNamespace(), java.util.Collection.class);

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
	public static final Operator get = Operator.wrapJavaMethod(LinkedList.class, "get", "java-linked-list-get",
			JavaLinkedList.singleton().getNamespace(), int.class);

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
	public static final Operator indexOf = Operator.wrapJavaMethod(LinkedList.class, "indexOf", "java-linked-list-index-of",
			JavaLinkedList.singleton().getNamespace(), Object.class);

	public static final Symbol isEmptySymbol_out = new Symbol("java-linked-list-is-empty");

	/**
	 * Operator for boolean isEmpty()
	 */
	@VelkaOperator
	@Description("Returns true if this list contains no elements.") 
	@Example("(java-linked-list-is-empty (construct List:JavaLinked)) ;; = #t") 
	@Syntax("(java-linked-list-is-empty <list>)")
	public static final Operator isEmpty = Operator.wrapJavaMethod(LinkedList.class, "isEmpty", "java-linked-list-is-empty",
			JavaLinkedList.singleton().getNamespace());

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
	public static final Operator lastIndexOf = Operator.wrapJavaMethod(LinkedList.class, "lastIndexOf", "java-linked-list-last-index-of",
			JavaLinkedList.singleton().getNamespace(), Object.class);

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
	public static final Operator remove = Operator.wrapJavaMethod(LinkedList.class, "remove", "java-linked-list-remove",
			JavaLinkedList.singleton().getNamespace(), Object.class);

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
	public static final Operator removeAll = Operator.wrapJavaMethod(LinkedList.class, "removeAll", "java-linked-list-remove-all",
			JavaLinkedList.singleton().getNamespace(), Collection.class);

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
	public static final Operator retainAll = Operator.wrapJavaMethod(LinkedList.class, "retainAll", "java-linked-list-retain-all",
			JavaLinkedList.singleton().getNamespace(), Collection.class);

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
	public static final Operator set = Operator.wrapJavaMethod(LinkedList.class, "set", "java-linked-list-set",
			JavaLinkedList.singleton().getNamespace(), int.class, Object.class);
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
	public static final Operator size = Operator.wrapJavaMethod(LinkedList.class, "size", "java-linked-list-size",
			JavaLinkedList.singleton().getNamespace());

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
	public static final Operator sublist = Operator.wrapJavaMethod(LinkedList.class, "subList", "java-linked-list-sublist",
			JavaLinkedList.singleton().getNamespace(), int.class, int.class);

	/**
	 * Symbol for List<T> map(Function<T, E>)
	 */
	private static final Symbol mapSymbol = new Symbol("velka_map", JavaLinkedList.singleton().getNamespace());
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
			var list = (LitInteropObject) args.get(0);

			var abst = args.get(1);

			@SuppressWarnings("unchecked")
			var al = (LinkedList<Object>) list.javaObject;
			LinkedList<Object> rslt = null;
			try {
				rslt = new LinkedList<Object>(al.stream().map(ThrowingFunction.wrapper(e -> {
					var appl = new AbstractionApplication(abst, new Tuple(
							e instanceof Expression lit ? lit : Literal.objectToLiteral(e)));
					
					var ret = appl.interpret(env);
					if(ret instanceof Literal lit){
						var lto = Literal.literalToObject(ret);
						return lto;
					}
					return ret;					
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
		public Symbol getInternalSymbol() {
			return mapSymbol;
		}
		
		@Override
		public String toString() {
			return mapSymbol_out.toString();
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod _method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var objCl = CodeModelInstance.instance()._ref(Object.class);
			var aCl = CodeModelInstance.instance().anonymousClass(java.util.function.Function.class);
			var applyMth = aCl.method(JMod.PUBLIC, objCl, "apply");
			var applyArg = applyMth.param(objCl, "_arg");
			
			applyMth.body()._return(
					mappedArgs.get(new Symbol("_1")).invoke("apply")
						.arg(VelkaTuple._of(applyArg)));
			
			_method.body()._return(
					JExpr._new(CodeModelInstance.instance()._ref(LinkedList.class)).arg(
						mappedArgs.get(new Symbol("_0"))
							.invoke("stream")
							.invoke("map").arg(JExpr._new(aCl))
							.invoke("toList")));
		}
	};

	/**
	 * Symbol for List<T> map2(List<E2> other, Function<T, E1, E2>)
	 */
	private static final Symbol map2Symbol = new Symbol("velka_map2", JavaLinkedList.singleton().getNamespace());
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
			var list = (LitInteropObject) args.get(0);

			
			var list2 = (LitInteropObject) args.get(1);

			var abst = args.get(2);

			@SuppressWarnings("unchecked")
			var l1 = (LinkedList<Object>) list.javaObject;
			@SuppressWarnings("unchecked")
			var l2 = (LinkedList<Object>) list2.javaObject;
			var rslt = new LinkedList<Object>();

			var i1 = l1.iterator();
			var i2 = l2.iterator();

			while (i1.hasNext() && i2.hasNext()) {
				var o1 = i1.next();
				var o2 = i2.next();
				
				Expression e1, e2;
				if(o1 instanceof Expression expr) {
					e1 = expr;
				}
				else {
					e1 = Literal.objectToLiteral(o1);
				}
				
				if(o2 instanceof Expression expr) {
					e2 = expr;
				}
				else {
					e2 = Literal.objectToLiteral(o2);
				}

				AbstractionApplication appl = new AbstractionApplication(abst, new Tuple(e1, e2));

				var ret = appl.interpret(env);
				
				if(ret instanceof Literal lit){
					var lto = Literal.literalToObject(ret);
					rslt.add(lto);
					continue;
				}
				rslt.add(ret);
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
		public Symbol getInternalSymbol() {
			return map2Symbol;
		}

		@Override
		public String toString() {
			return map2Symbol_out.toString();
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod _method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var objCl = CodeModelInstance.instance().ref(Object.class);
			var itCl = CodeModelInstance.instance().ref(java.util.Iterator.class);
			var llCl = CodeModelInstance.instance().ref(LinkedList.class);
			
			var ret = _method.body().decl(llCl, "_ret", JExpr._new(llCl));
			
			var it1 = _method.body().decl(itCl, "_it1", mappedArgs.get(new Symbol("_0")).invoke("iterator"));
			var it2 = _method.body().decl(itCl, "_it2", mappedArgs.get(new Symbol("_1")).invoke("iterator"));
			
			var _while = _method.body()._while(it1.invoke("hasNext").band(it2.invoke("hasNext")));
			var e1 = _while.body().decl(objCl, "_e1", it1.invoke("next"));
			var e2 = _while.body().decl(objCl, "_e2", it2.invoke("next"));
			
			var rslt = _while.body().decl(objCl, "_rslt", mappedArgs.get(new Symbol("_2")).invoke("apply")
							.arg(VelkaTuple._of(e1, e2)));
			
			_while.body().add(ret.invoke("add").arg(rslt));
			
			_method.body()._return(ret);
		}
	};

	/**
	 * Symbol for T foldl(Function<T, E, T>)
	 */
	private static final Symbol foldlSymbol = new Symbol("foldl", JavaLinkedList.singleton().getNamespace());
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
			var abst = args.get(0);
			var agg = args.get(1);
			var io = (LitInteropObject) args.get(2);			
			@SuppressWarnings("unchecked")
			var list = (LinkedList<Object>) io.javaObject;
			
			var i = list.iterator();
			while(i.hasNext()) {
				var o = i.next();
				Expression e;
				if(o instanceof Expression expr) {
					e = expr;
				}
				else {
					e = Literal.objectToLiteral(o);
				}
				
				var app = new AbstractionApplication(abst, new Tuple(agg, e));
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
		public Symbol getInternalSymbol() {
			return foldlSymbol;
		}
		
		@Override
		public String toString() {
			return foldlSymbol_out.toString();
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod _method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var objCl = CodeModelInstance.instance().ref(Object.class);
			
			var ret = _method.body().decl(objCl, "_ret", mappedArgs.get(new Symbol("_1")));
			var rettype = _method.body().decl(TypeUtil.instance().typeJType(), "_retType", 
					JExpr.direct("_cparm").invoke("getType").arg(JExpr.lit(1)));
			
			var _for = _method.body()._for();
			var _i = _for.init(CodeModelInstance.instance().INT, "_i", JExpr.lit(0));
			_for.test(_i.lt(mappedArgs.get(new Symbol("_2")).invoke("size")));
			_for.update(_i.incr());
			
			var _e = _for.body().decl(objCl, "_e", mappedArgs.get(new Symbol("_2")).invoke("get").arg(_i));
			_for.body()
					.assign(ret,
							mappedArgs.get(new Symbol("_0")).invoke("apply")
									.arg(VelkaTuple._velkaTupleTypeExpr(
											JExpr._new(TypeUtil.instance().typeTupleJClass()).arg(rettype)
													.arg(JavaTypeSystem.codeInstance().invoke("getType").arg(_e)),
											ret, _e)));
			
			_method.body()._return(ret);
		}
	};

	/**
	 * Symbol for T foldr(Function<T, E, T>)
	 */
	private static final Symbol foldrSymbol = new Symbol("foldr", JavaLinkedList.singleton().getNamespace());
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
			var abst = args.get(0);
			var terminator = args.get(1);
			var io = (LitInteropObject) args.get(2);
			@SuppressWarnings("unchecked")
			var list = (LinkedList<Object>) io.javaObject;

			var agg = terminator;
			var i = list.listIterator(list.size());
			while (i.hasPrevious()) {
				Object o = i.previous();
				Expression element;
				
				if(i instanceof Expression expr) {
					element = expr;
				}
				else {
					element = Literal.objectToLiteral(o);
				}
				
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
		public Symbol getInternalSymbol() {
			return foldrSymbol;
		}
		
		@Override
		public String toString() {
			return foldrSymbol_out.toString();
		}

		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod _method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var objCl = CodeModelInstance.instance().ref(Object.class);
			
			var l = mappedArgs.get(new Symbol("_2"));			
			
			var ret = _method.body().decl(objCl, "_ret", mappedArgs.get(new Symbol("_1")));
			var rettype = _method.body().decl(TypeUtil.instance().typeJType(), "_retType", 
					JExpr.direct("_cparm").invoke("getType").arg(JExpr.lit(1)));
			
			var _for = _method.body()._for();
			var _i = _for.init(CodeModelInstance.instance().INT, "_i", l.invoke("size").minus(JExpr.lit(1)));
			_for.test(_i.gte(JExpr.lit(0)));
			_for.update(_i.decr());
			
			var _e = _for.body().decl(objCl, "_e", l.invoke("get").arg(_i));
			_for.body().assign(ret,
					mappedArgs.get(new Symbol("_0")).invoke("apply").arg(VelkaTuple._velkaTupleTypeExpr(
							JExpr._new(TypeUtil.instance().typeTupleJClass()).arg(rettype)
								.arg(JavaTypeSystem.codeInstance().invoke("getType").arg(_e)),
							ret, _e)));
			
			_method.body()._return(ret);
		}
	};

	public static final Symbol LinkedListToNativeListSymbol = new Symbol("to_list_native", JavaLinkedList.singleton().getNamespace());
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
			String code = ClojureHelper.fnHelper(List.of(list),
					ClojureHelper.constructJavaClass(ArrayList.class,
							list));
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
		public Symbol getInternalSymbol() {
			return LinkedListToNativeListSymbol;
		}
		
		@Override
		public String toString() {
			return LinkedListToNativeListSymbol_out.toString();
		}

		@Override
		public Expression cost() {
			var arg = new Symbol(NameGenerator.next());
			return new Lambda(new AbstractionApplication(JavaLinkedList.size, new Tuple(arg)),
					List.of(Pair.of(arg, TypeAtom.TypeList)));
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			method.body()._return(JExpr._new(CodeModelInstance.instance()._ref(ArrayList.class))
					.arg(mappedArgs.get(new Symbol("_0"))));
		}
	};

	public static final Symbol everypSymbol = new Symbol("velka_everyp", JavaLinkedList.singleton().getNamespace());
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
		public Symbol getInternalSymbol() {
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
			var l = (LinkedList<Object>) lio.javaObject;
			var pred = args.get(1);

			var i = l.iterator();
			while(i.hasNext()) {
				var o = i.next();
				Expression e;
				if(o instanceof Expression expr) {
					e = expr;
				}
				else {
					e = Literal.objectToLiteral(o);
				}
				
				var appl = new AbstractionApplication(pred, new Tuple(e));
				var ret = appl.interpret(env);
				if(ret.equals(LitBoolean.FALSE)) {
					return LitBoolean.FALSE;
				}
			}

			return LitBoolean.TRUE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListJavaLinked, new TypeArrow(
							new TypeTuple(new TypeVariable(NameGenerator.next())), TypeAtom.TypeBoolNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
		@Override
		protected void modifyJavaMethod(com.sun.codemodel.JMethod method, Map<Symbol, com.sun.codemodel.JVar> mappedArgs) {
			var objCl = CodeModelInstance.instance().ref(Object.class);
			
			var i = method.body().decl(CodeModelInstance.instance()._ref(Iterator.class), "_i",
					mappedArgs.get(new Symbol("_0")).invoke("iterator"));
			
			var _while = method.body()._while(i.invoke("hasNext"));
			var e = _while.body().decl(objCl, "_e", i.invoke("next"));
			
			var _if = _while.body()
					._if(JExpr.cast(CodeModelInstance.instance()._ref(Boolean.class),
							mappedArgs.get(new Symbol("_1")).invoke("apply").arg(VelkaTuple._of(e))).not());
			_if._then()._return(JExpr.FALSE);
			
			method.body()._return(JExpr.TRUE);
		}
	};
	
	public static final Symbol toStrSymbol = new Symbol("velka_to_str", JavaLinkedList.singleton().getNamespace());
	public static final Symbol toStrSymbol_out = new Symbol("java-linked-list-to-str");
	
	@VelkaOperator
	@Description("Returns readable string representation of list.")
	@Example("(define l (construct List:JavaLinked))\n"
			+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
			+ "(java-linked-list-to-str l)")
	@Syntax("(java-linked-list-to-str <list>)")
	public static final Operator toStr = Operator.wrapJavaMethod(LinkedList.class, "toString", "java-linked-list-to-str",
			JavaLinkedList.singleton().getNamespace());
	
	public static final Symbol listIteratorSymbol = new Symbol("list_iterator", JavaLinkedList.singleton().getNamespace());
	public static final Symbol listIteratorSymbol_out = new Symbol("java-linked-list-iterator");
	@VelkaOperator
	@Description("Returns a list-iterator of the elements in this list (in proper sequence), starting at the specified position in the list.") 
	@Example("(define l (construct List:JavaLinked))\n"
					+ "(java-linked-list-add-all l (build-list-native 10 (lambda (x) (* 2 x))))\n"
					+ "(java-linked-list-iterator l 0)") 
	@Syntax("(java-linked-list-iterator <list> <index>)")
	public static final Operator listIterator = Operator.wrapJavaMethod(LinkedList.class, "listIterator", "java-linked-list-iterator",
			JavaLinkedList.singleton().getNamespace(), int.class);
	
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

	@Override
	protected String name() {
		return "linkedList";
	}
}
