package velka.core.langbase;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.stream.Collectors;

import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.core.abstraction.Constructor;
import velka.core.abstraction.Conversion;
import velka.core.abstraction.Lambda;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
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

@VelkaOperatorBank
@Description("Operators for working with velka Lists") 
@Header("List")
public class ListNative extends OperatorBank{

	/**
	 * Name of velka.clojure.list namespace
	 */
	public static final String NAMESPACE = "velka.clojure.list";

	/**
	 * Empty list native
	 */
	public static final Expression EMPTY_LIST_NATIVE = 
			new LitInteropObject(new LinkedList<Expression>(), TypeAtom.TypeListNative);

	/**
	 * Clojure code for empty list
	 */
	public static final String EMPTY_LIST_NATIVE_CLOJURE = LitComposite.clojureValueToClojureLiteral(
			Type.addTypeMetaInfo("'()", TypeAtom.TypeListNative), TypeAtom.TypeListNative);

	/**
	 * Symbol for empty constructor
	 */
	public static final Symbol constructorEmptySymbol = new Symbol("velka-construct-empty-list-native", NAMESPACE);

	/**
	 * Construtor for empty list
	 */
	@VelkaConstructor
	@Description("Constructs Empty List:Native.")
	@Name("Construct Empty List") 
	@Syntax("(construct List:Native)")
	public static final Constructor constructorEmpty = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String code = ClojureHelper.fnHelper(Arrays.asList(),
					Type.addTypeMetaInfo("'()", TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorEmptySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			return new LitInteropObject(new LinkedList<Expression>(), TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol constructorSymbol = new Symbol("velka-construct-list-native", NAMESPACE);

	/**
	 * Constructor for non-empty list
	 */
	@VelkaConstructor
	@Description("Constructs new List:Native adding element as head to list.") 
	@Name("Construct by cons") 
	@Syntax("(construct List:Native <element> <list>)")
	public static final Constructor constructor = new Constructor() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String val = "_value";
			String rest = "_rest";
			String code = ClojureHelper.fnHelper(Arrays.asList(val, rest),
					ClojureHelper.applyClojureFunction("lazy-seq", Type.addTypeMetaInfo(
							ClojureHelper.applyClojureFunction("cons", val, rest),
							TypeAtom.TypeListNative)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression val = args.get(0);
			LitInteropObject interop = (LitInteropObject) args.get(1);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			LinkedList<Expression> ll = new LinkedList<Expression>();
			ll.add(val);
			ll.addAll(l);

			return new LitInteropObject(ll, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * is-list-native-empty symbol
	 */
	private static final Symbol isEmptySymbol = new Symbol("is-empty", NAMESPACE);
	public static final Symbol isEmptySymbol_out = new Symbol("is-list-native-empty");

	/**
	 * is-list-native-empty operator
	 */
	@VelkaOperator
	@Description("Returns _true_ if list is empty. Returns _false_ otherwise.") 
	@Example("(is-list-native-empty (construct List:Native)) ;; = #t") 
	@Syntax("(is-list-native-empty <list>)")
	public static final Operator isEmpty = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					ClojureHelper.clojureIfHelper(
							ClojureHelper.applyClojureFunction("empty?", list),
							LitBoolean.TRUE.toClojureCode(env), LitBoolean.FALSE.toClojureCode(env)));
			return code;
		}

		@Override
		public String toString() {
			return isEmptySymbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return isEmptySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteropObject interop = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			if (l.isEmpty()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * head-list-native symbol
	 */
	private static final Symbol headSymbol = new Symbol("head", NAMESPACE);
	public static final Symbol headSymbol_out = new Symbol("head-list-native");

	/**
	 * head-list-native operator
	 */
	@VelkaOperator
	@Description("Returns first element in this list.") 
	@Example("(head-list-native (build-list-native 5 (lambda (x) x))) ;; = 0") 
	@Syntax("(head-list-native <list>)")
	public static final Operator headListNativeOperator = new Operator() {

		private final String errorMsg = "Cannot take head of empty list.";

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					ClojureHelper.clojureIfHelper(
							ClojureHelper.applyClojureFunction("empty?", list),
							ClojureHelper.errorHelper(ClojureHelper.stringHelper(errorMsg)),
							ClojureHelper.applyClojureFunction("first", list)));
			return code;
		}

		@Override
		public String toString() {
			return headSymbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return headSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			
			LitInteropObject interop = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			if (l.isEmpty()) {
				throw new UserException(errorMsg);
			}

			return l.get(0);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * tail-list-native symbol
	 */
	private static final Symbol tailSymbol = new Symbol("tail", NAMESPACE);
	public static final Symbol tailSymbol_out = new Symbol("tail-list-native");

	@VelkaOperator
	@Description("Returns list consisting of all elements of original list, except the first element.") 
	@Example("(tail-list-native (build-list-native 5 (lambda (x) x))) ;; = (1 2 3 4)") 
	@Syntax("(tail-list-native <list>)")
	public static final Operator tailListNativeOperator = new Operator() {

		private final String errorMsg = "Cannot take tail of empty list.";

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("lazy-seq",
									ClojureHelper.clojureIfHelper(
											ClojureHelper.applyClojureFunction("empty?",
													list),
											ClojureHelper.errorHelper(ClojureHelper.stringHelper(errorMsg)),
											ClojureHelper.applyClojureFunction("rest",
													list))),
							TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public String toString() {
			return tailSymbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return tailSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			
			LitInteropObject interop = (LitInteropObject) args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			if (l.isEmpty()) {
				throw new UserException(errorMsg);
			}

			LinkedList<Expression> ll = new LinkedList<Expression>(l.subList(1, l.size()));
			return new LitInteropObject(ll, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * map-list-native symbol
	 */
	private static final Symbol mapSymbol = new Symbol("velka-map", NAMESPACE);
	public static final Symbol mapSymbol_out = new Symbol("map-list-native");

	/**
	 * map-list-native operator
	 */
	@VelkaOperator
	@Description("Returns a List:Native consisting of the results of applying the given function to the elements of list.") 
	@Example("(map-list-native\n"
					+ "    (build-list-native 5 (lambda (x)))\n"
					+ "    (lambda (y) (* y 2))) ;; = (0 2 4 6 8)") 
	@Syntax("(map-list-native <list> <function>)")
	public static final Operator mapListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String fn = "_fn";
			String arg = "_arg";
			String code = ClojureHelper.fnHelper(Arrays.asList(fn, list),
					LitComposite
							.clojureValueToClojureLiteral(
									ClojureHelper.applyClojureFunction("lazy-seq",
											ClojureHelper.applyClojureFunction("map",
													ClojureHelper.fnHelper(Arrays.asList(arg),
															ClojureHelper.applyVelkaFunction(fn, arg)),
													list)),
									TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.mapSymbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return ListNative.mapSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression f = args.get(0);
			LitInteropObject interop = (LitInteropObject) args.get(1);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			LinkedList<Expression> agg = new LinkedList<Expression>();

			for (Expression e : l) {
				AbstractionApplication appl = new AbstractionApplication(f, new Tuple(e));
				Expression res = appl.interpret(env);
				agg.add(res);
			}

			return new LitInteropObject(agg, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeVariable B = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(
					new TypeTuple(
							Arrays.asList(new TypeArrow(new TypeTuple(Arrays.asList(A)), B), TypeAtom.TypeListNative)),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for map2-list-native
	 */
	private static final Symbol map2Symbol = new Symbol("map2", NAMESPACE);
	public static final Symbol map2Symbol_out = new Symbol("map2-list-native");

	/**
	 * map2-list-native operator
	 */
	@VelkaOperator
	@Description("Returns a List:Native consisting of the results of applying the given function to the elements of list1 and list2.") 
	@Example("(map2-list-native\n"
					+ "    (build-list-native 5 (lambda (x) x))\n"
					+ "    (build-list-native 5 (lambda (x) x))\n"
					+ "    +) ;; = (0 2 4 6 8)") 
	@Syntax("(map2-list-native <list1> <list2> <function>)")
	public static final Operator map2ListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list1 = "_list1";
			String list2 = "_list2";
			String fn = "_fn";
			String arg1 = "_arg1";
			String arg2 = "_arg2";

			String code = ClojureHelper
					.fnHelper(Arrays.asList(fn, list1, list2),
							LitComposite.clojureValueToClojureLiteral(
									ClojureHelper.applyClojureFunction("lazy-seq",
											ClojureHelper.applyClojureFunction("map",
													ClojureHelper.fnHelper(Arrays.asList(arg1, arg2),
															ClojureHelper.applyVelkaFunction(fn, arg1, arg2)),
													list1,
													list2)),
									TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.map2Symbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return ListNative.map2Symbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression f = args.get(0);
			var iOp1 = (LitInteropObject) args.get(1);
			var iOp2 = (LitInteropObject) args.get(2);
			
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l1 = (LinkedList<Expression>) iOp1.javaObject;
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l2 = (LinkedList<Expression>) iOp2.javaObject;

			LinkedList<Expression> agg = new LinkedList<Expression>();

			Iterator<Expression> i1 = l1.iterator();
			Iterator<Expression> i2 = l2.iterator();
			while (i1.hasNext() && i2.hasNext()) {
				Expression e1 = i1.next();
				Expression e2 = i2.next();

				AbstractionApplication appl = new AbstractionApplication(f, new Tuple(e1, e2));
				Expression ret = appl.interpret(env);
				agg.add(ret);
			}

			return new LitInteropObject(agg, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeVariable B = new TypeVariable(NameGenerator.next());
			TypeVariable C = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(new TypeArrow(new TypeTuple(Arrays.asList(A, B)), C),
					TypeAtom.TypeListNative, TypeAtom.TypeListNative), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for foldl-list-native
	 */
	private static final Symbol foldlSymbol = new Symbol("foldl", NAMESPACE);
	public static final Symbol foldlSymbol_out = new Symbol("foldl-list-native");

	/**
	 * foldl-list-native operator
	 */
	@VelkaOperator
	@Description("Performs a reduction on the elements of list, using the terminator value and an associative accumulation function, and returns the reduced value. Processes list from the beginning.") 
	@Example("(foldl-list-native / 0 (build-list-native 3 (lambda (x) (+ x 1)))) ;; = 0.16666666666666666666666666666667") 
	@Syntax("(foldl-list-native <function> <terminator> <list>)")
	public static final Operator foldlListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String terminator = "_term";
			String fn = "_fn";
			String arg1 = "_arg1";
			String arg2 = "_arg2";
			String code = ClojureHelper.fnHelper(Arrays.asList(fn, terminator, list),
					ClojureHelper.applyClojureFunction("reduce",
							ClojureHelper.fnHelper(Arrays.asList(arg1, arg2),
									ClojureHelper.applyVelkaFunction(fn, arg1, arg2)),
							terminator, list));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.foldlSymbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return ListNative.foldlSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			Expression f = args.get(0);
			Expression term = args.get(1);
			var iOp = (LitInteropObject)args.get(2);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;

			for (Expression e : l) {
				AbstractionApplication appl = new AbstractionApplication(f, new Tuple(term, e));
				term = appl.interpret(env);
			}

			return term;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeVariable B = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(
					new TypeTuple(new TypeArrow(new TypeTuple(A, B), A), A, TypeAtom.TypeListNative), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for add-to-end function
	 */
	private static final Symbol addToEndSymbol = new Symbol("add-to-end", NAMESPACE);
	public static final Symbol addToEndSymbol_out = new Symbol("add-to-end-list-native");

	/**
	 * add-to-end operator
	 */
	@VelkaOperator
	@Description("Creates new list with appended the specified element to the end of list.") 
	@Example("(add-to-end-list-native (construct List:Native) 42) ;; = (42)") 
	@Syntax("(add-to-end-list-native <list> <element>)")
	public static final Operator addToEndOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String element = "_element";

			String code = ClojureHelper.fnHelper(Arrays.asList(list, element),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("lazy-seq",
									ClojureHelper.applyClojureFunction("concat",
											list, Type
													.addTypeMetaInfo("'(" + element + ")", TypeAtom.TypeListNative))),
							TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.addToEndSymbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return ListNative.addToEndSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;
			Expression e = args.get(1);

			LinkedList<Expression> ll = new LinkedList<Expression>(l);
			ll.add(e);

			return new LitInteropObject(ll, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, A), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol listNativeToArrayListSymbol = new Symbol("to-array_list", NAMESPACE);
	public static final Symbol listNativeToArrayListSymbol_out = new Symbol("list-native-2-array-list");

	/**
	 * list-native-2-array-list operator
	 */
	@VelkaConversion
	@Description("Converts List:Native to List:JavaArray.") 
	@Example("(list-native-2-array-list (build-list-native 5 (lambda (x) x)))") 
	@Syntax("(list-native-2-array-list <list native>)")
	public static final Conversion ListNativeToArrayListOperator = new Conversion() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(list),
							LitComposite.clojureValueToClojureLiteral(
									ClojureHelper.applyClojureFunction("java.util.ArrayList.",
											list),
									TypeAtom.TypeListJavaArray));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.listNativeToArrayListSymbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return listNativeToArrayListSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;
			ArrayList<Expression> a = new ArrayList<Expression>(l);

			return new LitInteropObject(a, TypeAtom.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Expression cost() {
			var arg = new Symbol(NameGenerator.next());
			return new Lambda(new Tuple(arg), new TypeTuple(TypeAtom.TypeListNative),
					new AbstractionApplication(ListNative.size, new Tuple(arg)));
		}

	};

	public static final Symbol ListNativeToLinkedListSymbol = new Symbol("to-linked-list", NAMESPACE);
	public static final Symbol ListNativeToLinkedListSymbol_out = new Symbol("list-native-2-linked-list");

	@VelkaConversion
	@Description("Converts List:Native to List:JavaLinked.") 
	@Example("(list-native-2-linked-list (build-list-native 5 (lambda (x) x)))") 
	@Syntax("(list-native-2-linked-list <list native>)")
	public static final Conversion ListNativeToLinkedListOperator = new Conversion() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(list),
							LitComposite.clojureValueToClojureLiteral(
									ClojureHelper.applyClojureFunction("java.util.LinkedList.",
											list),
									TypeAtom.TypeListJavaLinked));
			return code;
		}

		@Override
		public String toString() {
			return ListNative.ListNativeToLinkedListSymbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return ListNativeToLinkedListSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;
			LinkedList<Expression> a = new LinkedList<Expression>(l);

			return new LitInteropObject(a, TypeAtom.TypeListJavaLinked);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeListJavaLinked);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

		@Override
		public Expression cost() {
			var arg = new Symbol(NameGenerator.next());
			return new Lambda(new Tuple(arg), new TypeTuple(TypeAtom.TypeListNative),
					new AbstractionApplication(ListNative.size, new Tuple(arg)));
		}

	};

	/**
	 * namespace symbol for contains
	 */
	public static final Symbol containsSymbol = new Symbol("contains", NAMESPACE);
	/**
	 * public symbol for contains
	 */
	public static final Symbol containsSymbol_out = new Symbol("contains-list-native");

	/**
	 * contains operator
	 */
	@VelkaOperator
	@Description("Returns true if this list contains the specified element.") 
	@Example("(contains-list-native (build-list-native 3 (lambda (x) x)) 0) ; = #t\n"
					+ "(contains-list-native (build-list-native 3 (lambda (x) x)) 5) ; = #f") 
	@Syntax("(contains-list-native <list> <element>)")
	public static final Operator contains = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			final String list = "_list";
			final String element = "_element";
			final String arg = "_arg";
			final String code = ClojureHelper.fnHelper(Arrays.asList(list, element),
					LitBoolean.clojureLit(ClojureHelper.applyClojureFunction("boolean",
							ClojureHelper.applyClojureFunction("some",
									ClojureHelper.fnHelper(Arrays.asList(arg),
											ClojureHelper.applyClojureFunction("=", arg, element)),
									list))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return containsSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.containsSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;
			Expression element = args.get(1);

			if (l.contains(element)) {
				return LitBoolean.TRUE;
			}

			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, A), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol filterSymbol = new Symbol("velka-filter", NAMESPACE);
	public static final Symbol filterSymbol_out = new Symbol("filter-list-native");

	@VelkaOperator
	@Description("Returns new List:Native containing only those elements of list, for which predicate returns true.") 
	@Example("(filter-list-native (build-list-native 5 (lambda (x) x)) (lambda (y) (= (mod y 2) 0))) ;; = (0 2 4)") 
	@Syntax("(filter-list-native <list> <predicate>)")
	public static final Operator filter = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String pred = "_pred";
			String arg = "_arg";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(list, pred),
							LitComposite
									.clojureValueToClojureLiteral(
											ClojureHelper.applyClojureFunction("lazy-seq",
													ClojureHelper.applyClojureFunction("filter",
															ClojureHelper.fnHelper(Arrays.asList(arg),
																	ClojureHelper
																			.applyVelkaFunction(pred, arg)),
															list)),
											TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return filterSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.filterSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;
			Expression pred = args.get(1);

			LinkedList<Expression> aux = new LinkedList<Expression>();

			for (Expression e : l) {
				AbstractionApplication app = new AbstractionApplication(pred, new Tuple(e));
				Expression rsl = app.interpret(env);
				if (rsl.equals(LitBoolean.TRUE)) {
					aux.add(e);
				}
			}

			return new LitInteropObject(aux, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			final TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListNative, new TypeArrow(new TypeTuple(A), TypeAtom.TypeBoolNative)),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * Symbol for get operator
	 */
	public static final Symbol getSymbol = new Symbol("velka-get", NAMESPACE);
	/**
	 * Public symbol for get operator
	 */
	public static final Symbol getSymbol_out = new Symbol("get-list-native");

	/**
	 * Get operator
	 */
	@VelkaOperator
	@Description("Returns the element at the specified position in this list.") 
	@Example("(get-list-native (build-list-native 5 (lambda (x) (* 2 x))) 1) ;; = 2") 
	@Syntax("(get-list-native <list> <index>)")
	public static final Operator get = new Operator() {

		private static final String errorMsg = "Index out of range";

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String index = "_index";
			String code = ClojureHelper.fnHelper(Arrays.asList(list, index), ClojureHelper.applyClojureFunction("nth",
					list, index));

			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return getSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.getSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;
			LitInteger index = (LitInteger) args.get(1);

			if (index.value >= l.size()) {
				throw new UserException(errorMsg);
			}

			return l.get((int) index.value);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeIntNative), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol buildListSymbol = new Symbol("build-list", NAMESPACE);
	public static final Symbol buildListSymbol_out = new Symbol("build-list-native");

	@VelkaOperator
	@Description("Creates a List:Native of n elements by applying function to the integers from 0 to (- n 1) in order.\n"
					+ "If lst is the resulting list, then (get-list-native lst i) is the value produced by (function i).") 
	@Example("(build-list-native 5 (lambda (x) (* x x))) ;; = (0 1 4 9 16)") 
	@Syntax("(build-list-native <n> <function>)")
	public static final Operator buildList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String n = "_n";
			String fn = "_fn";
			String arg = "_arg";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(n, fn),
							LitComposite
									.clojureValueToClojureLiteral(
											ClojureHelper.applyClojureFunction("lazy-seq",
													ClojureHelper.applyClojureFunction("map",
															ClojureHelper.fnHelper(Arrays.asList(arg),
																	ClojureHelper.applyVelkaFunction(fn,
																			LitInteger.clojureLit(
																					arg))),
															ClojureHelper.applyClojureFunction("range",
																	n))),
											TypeAtom.TypeListNative));

			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return buildListSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.buildListSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			LitInteger n = (LitInteger) args.get(0);
			Expression fn = args.get(1);

			LinkedList<Expression> l = new LinkedList<Expression>();

			for (long i = 0; i < n.value; i++) {
				AbstractionApplication appl = new AbstractionApplication(fn, new Tuple(new LitInteger(i)));

				Expression expr = appl.interpret(env);

				l.add(expr);
			}

			return new LitInteropObject(l, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable buildListNative_A = new TypeVariable(NameGenerator.next());
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeIntNative,
							new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), buildListNative_A)),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol removeSymbol = new Symbol("velka-remove", NAMESPACE);
	public static final Symbol removeSymbol_out = new Symbol("remove-list-native");

	@VelkaOperator
	@Description("Removes the first occurrence of the specified element from this list, if it is present.") 
	@Example("(remove-list-native build-list-native 3 (lambda (x) x)) 1) ;; = (0 2)") 
	@Syntax("(remove-list-native <list> <element>)")
	public static final Operator remove = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String element = "_element";
			String arg = "_arg";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(list, element),
							LitComposite
									.clojureValueToClojureLiteral(
											ClojureHelper.applyClojureFunction("lazy-seq",
													ClojureHelper.applyClojureFunction("filter",
															ClojureHelper.fnHelper(Arrays.asList(arg),
																	ClojureHelper.applyClojureFunction("not=", arg,
																			element)),
															list)),
											TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return removeSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.removeSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;
			Expression e = args.get(1);

			LinkedList<Expression> ll = new LinkedList<Expression>(l);
			ll.remove(e);

			return new LitInteropObject(ll, TypeAtom.TypeListNative);
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, A), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol sizeSymbol = new Symbol("velka-size", NAMESPACE);
	public static final Symbol sizeSymbol_out = new Symbol("size-list-native");

	@VelkaOperator
	@Description("Returns the number of elements in this list.") 
	@Example("(size-list-native (build-list-native 3 (lambda (x) x))) ;; = 3") 
	@Syntax("(size-list-native <list>)")
	public static final Operator size = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list), LitInteger.clojureLit(
					ClojureHelper.applyClojureFunction("count", list)));

			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return sizeSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.sizeSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;

			return new LitInteger(l.size());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol appendSymbol = new Symbol("append", NAMESPACE);
	public static final Symbol appendSymbol_out = new Symbol("append-list-native");

	@VelkaOperator
	@Description("Creates a new List:Native where contents of list2 are appended after contents of list1.") 
	@Example("(append-list-native (build-list-native 2 (lambda (x) x)) (build-list-native 3 (lambda (x) (+ x 2)))) ;; = (0 1 2 3 4)") 
	@Syntax("(append-list-native <list1> <list2>)")
	public static final Operator append = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list1 = "_list1";
			String list2 = "_list2";
			String code = ClojureHelper.fnHelper(Arrays.asList(list1, list2), LitComposite.clojureValueToClojureLiteral(
					ClojureHelper.applyClojureFunction("lazy-seq", ClojureHelper.applyClojureFunction("concat",
							list1, list2)),
					TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return appendSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.appendSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp1 = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l0 = (LinkedList<Expression>) iOp1.javaObject;
			var iOp2 = (LitInteropObject)args.get(1);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l1 = (LinkedList<Expression>) iOp2.javaObject;

			LinkedList<Expression> aux = new LinkedList<Expression>(l0);
			aux.addAll(l1);

			return new LitInteropObject(aux, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol reverseSymbol = new Symbol("velka-reverse", NAMESPACE);
	public static final Symbol reverseSymbol_out = new Symbol("reverse-list-native");

	@VelkaOperator
	@Description("Creates new List:Native with the same elements as list, but in reversed (last to first) order.") 
	@Example("(reverse-list-native (build-list-native 5 (lambda (x) x))) ;; = (4 3 2 1 0)") 
	@Syntax("(reverse-list-native <list>)")
	public static final Operator reverse = new Operator() {

		@Override
		protected String toClojureOperator(Environment env) throws AppendableException {
			String list = "list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("reverse", list),
							TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return reverseSymbol;
		}
		
		@Override
		public String toString() {
			return ListNative.reverseSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;
			ListIterator<Expression> li = l.listIterator(l.size());
			List<Expression> r = new LinkedList<Expression>();
			while (li.hasPrevious()) {
				r.add(li.previous());
			}

			return new LitInteropObject(r, TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol everypSymbol = new Symbol("velka-everyp", NAMESPACE);
	public static final Symbol everypSymbol_out = new Symbol("everyp-list-native");

	@VelkaOperator
	@Description("Returns true if every element of this list returns true for the predicate. Otherwise returns false.") 
	@Example("(everyp-list-native (build-list-native 10 (* 2 x)) (lambda (x) (= (mod x 2) 0))) ;; = #t\n"
					+ "(everyp-list-native (build-list-native 10 (* 2 x)) (lambda (x) (= x 1))) ;; = #f") 
	@Syntax("(everyp-list-native <list> <predicate>)")
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
			return ListNative.everypSymbol_out.toString();
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env) throws AppendableException {
			var iOp = (LitInteropObject)args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) iOp.javaObject;
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
					new TypeTuple(TypeAtom.TypeListNative, new TypeArrow(
							new TypeTuple(new TypeVariable(NameGenerator.next())), TypeAtom.TypeBoolNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
	};

	/**
	 * Creates a native list from collection
	 * 
	 * @param col collection
	 * @return LitComposite with native list
	 */
	public static Expression collectionToListNative(Collection<? extends Expression> col) {
		List<Expression> l = col.stream().collect(Collectors.toList());
		return ListNative.makeListNativeExpression(l);
	}

	/**
	 * Converts tuple into equvivalent list
	 * 
	 * @param t converted tuple
	 * @return LitComposite object containing native list
	 */
	public static Expression tupleToListNative(Tuple t) {
		return 
				new LitInteropObject(new LinkedList<Expression>(t.stream().collect(Collectors.toList())),
				TypeAtom.TypeListNative);
	}

	/**
	 * Converts java list into list native expression
	 * 
	 * @param l converted list
	 * @return list native literal (LitComposite instance)
	 */
	public static Expression makeListNativeExpression(List<Expression> l) {
		LinkedList<Expression> ll = new LinkedList<Expression>(l);
		return new LitInteropObject(ll, TypeAtom.TypeListNative);
	}

	/**
	 * Converts expression into List Native expression
	 * 
	 * @param exprs expression
	 * @return list native literal (LitComposite instance)
	 */
	public static Expression of(Expression... exprs) {
		return makeListNativeExpression(Arrays.asList(exprs));
	}

	/**
	 * Converts (at interpretation runtime) list into tuple
	 * 
	 * @param list converted list
	 * @return tuple
	 */
	public static Tuple listNativeToTuple(LitComposite list) {
		@SuppressWarnings("unchecked")
		LinkedList<Expression> l = (LinkedList<Expression>) ((LitInteropObject) list.value).javaObject;

		return new Tuple(l);
	}

	/**
	 * Prints list in clojure style
	 * 
	 * @param l printed list
	 * @return string with printed list
	 */
	public static String toStringListNative(List<Expression> l) {
		StringBuilder sb = new StringBuilder("(");
		Iterator<Expression> i = l.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			sb.append(e.toString());
			if (i.hasNext()) {
				sb.append(" ");
			}
		}
		sb.append(")");
		return sb.toString();
	}
	
	public static String listNativeClojure(String clojureCode) {
		String code = ClojureHelper.applyClojureFunction("lazy-seq", clojureCode);
		return LitComposite.clojureValueToClojureLiteral(code, TypeAtom.TypeListNative);
	}

	/**
	 * Creates code for ListNative value in clojure
	 * @param members code for members of list
	 * @return code for list native
	 */
	public static String listNativeClojure(Collection<String> members) {
		String code =  listNativeClojure(ClojureHelper.applyClojureFunction("list", members));
		return code;
	}
	
	/**
	 * Creates code for ListNative value in clojure
	 * @param members members of the list
	 * @return code for list
	 */
	public static String listNativeClojure(String ...members) {
		return listNativeClojure(Arrays.asList(members));
	}

	/**
	 * Relative path to velka.clojure.list file
	 */
	public static final Path VELKA_CLOJURE_LIST_PATH = Paths.get("velka", "clojure");

	/**
	 * Name of the velka.clojure.list file
	 */
	public static final Path VELKA_CLOJURE_LIST_NAME = Paths.get("list.clj");

	@Override
	public String getNamespace() {
		return NAMESPACE;
	}

	@Override
	public Path getPath() {
		return VELKA_CLOJURE_LIST_PATH;
	}

	@Override
	public Path getFileName() {
		return VELKA_CLOJURE_LIST_NAME;
	}
	
	private ListNative() {}
	private static ListNative instance = null;
	public static ListNative singleton() {
		if(instance == null) {
			instance = new ListNative();
		}
		return instance;
	}
}
