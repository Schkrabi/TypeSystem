package velka.core.langbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Optional;
import java.util.stream.Collectors;

import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.core.abstraction.Operator;
import velka.core.application.AbstractionApplication;
import velka.core.application.Construct;
import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.core.literal.LitInteropObject;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.ThrowingPredicate;

public class ListNative {

	/**
	 * Name of velka.clojure.list namespace
	 */
	public static final String NAMESPACE = "velka.clojure.list";

	/**
	 * Empty list native
	 */
	public static final LitComposite EMPTY_LIST_NATIVE = new LitComposite(
			new LitInteropObject(new LinkedList<Expression>()), TypeAtom.TypeListNative);

	/**
	 * Clojure code for empty list
	 */
	public static final String EMPTY_LIST_NATIVE_CLOJURE = LitComposite.clojureValueToClojureLiteral(
			ClojureHelper.addTypeMetaInfo("'()", TypeAtom.TypeListNative), TypeAtom.TypeListNative);

	/**
	 * Symbol for empty constructor
	 */
	public static final Symbol constructorEmptySymbol = new Symbol("velka-construct-empty-list-native", NAMESPACE);

	/**
	 * Construtor for empty list
	 */
	public static final Operator constructorEmpty = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = ClojureHelper.fnHelper(Arrays.asList(),
					ClojureHelper.addTypeMetaInfo("'()", TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorEmptySymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			return new LitInteropObject(new LinkedList<Expression>());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(TypeTuple.EMPTY_TUPLE, TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol constructorSymbol = new Symbol("velka-construct-list-native", NAMESPACE);

	/**
	 * Constructor for non-empty list
	 */
	public static final Operator constructor = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String val = "_value";
			String rest = "_rest";
			String code = ClojureHelper.fnHelper(Arrays.asList(val, rest),
					ClojureHelper.applyClojureFunction("lazy-seq", ClojureHelper.addTypeMetaInfo(
							ClojureHelper.applyClojureFunction("cons", val, ClojureHelper.getLiteralInnerValue(rest)),
							TypeAtom.TypeListNative)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return constructorSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression val = args.get(0);
			LitComposite rest = (LitComposite) args.get(1);
			LitInteropObject interop = (LitInteropObject) rest.value;
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			LinkedList<Expression> ll = new LinkedList<Expression>();
			ll.add(val);
			ll.addAll(l);

			return new LitInteropObject(ll);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	public static final Operator isEmpty = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					ClojureHelper.clojureIfHelper(
							ClojureHelper.applyClojureFunction("empty?", ClojureHelper.getLiteralInnerValue(list)),
							LitBoolean.TRUE.toClojureCode(env, typeEnv), LitBoolean.FALSE.toClojureCode(env, typeEnv)));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite list = ((LitComposite) args.get(0));
			LitInteropObject interop = (LitInteropObject) list.value;
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			if (l.isEmpty()) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	public static final Operator headListNativeOperator = new Operator() {

		private final String errorMsg = "Cannot take head of empty list.";

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					ClojureHelper.clojureIfHelper(ClojureHelper.applyClojureFunction("empty?", ClojureHelper.getLiteralInnerValue(list)),
							ClojureHelper.errorHelper(ClojureHelper.stringHelper(errorMsg)),
							ClojureHelper.applyClojureFunction("first", ClojureHelper.getLiteralInnerValue(list))));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite list = ((LitComposite) args.get(0));
			LitInteropObject interop = (LitInteropObject) list.value;
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			if (l.isEmpty()) {
				throw new UserException(errorMsg);
			}

			return l.get(0);
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	/**
	 * tail-list-native symbol
	 */
	private static final Symbol tailSymbol = new Symbol("tail", NAMESPACE);
	public static final Symbol tailSymbol_out = new Symbol("tail-list-native");

	public static final Operator tailListNativeOperator = new Operator() {

		private final String errorMsg = "Cannot take tail of empty list.";

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("lazy-seq",
									ClojureHelper.clojureIfHelper(
											ClojureHelper.applyClojureFunction("empty?",
													ClojureHelper.getLiteralInnerValue(list)),
											ClojureHelper.errorHelper(ClojureHelper.stringHelper(errorMsg)),
											ClojureHelper.applyClojureFunction("rest",
													ClojureHelper.getLiteralInnerValue(list)))),
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite list = ((LitComposite) args.get(0));
			LitInteropObject interop = (LitInteropObject) list.value;
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			if (l.isEmpty()) {
				throw new UserException(errorMsg);
			}

			LinkedList<Expression> ll = new LinkedList<Expression>(l.subList(1, l.size()));
			return new LitComposite(new LitInteropObject(ll), TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	public static final Operator mapListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
													ClojureHelper.getLiteralInnerValue(list))),
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression f = args.get(0);
			LitComposite list = ((LitComposite) args.get(1));
			LitInteropObject interop = (LitInteropObject) list.value;
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) interop.javaObject;

			LinkedList<Expression> agg = new LinkedList<Expression>();

			for (Expression e : l) {
				AbstractionApplication appl = new AbstractionApplication(f, new Tuple(e));
				Expression res = appl.interpret(env, typeEnv);
				agg.add(res);
			}

			return new LitComposite(new LitInteropObject(agg), TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(Arrays.asList(
					new TypeArrow(new TypeTuple(Arrays.asList(new TypeVariable("A"))), new TypeVariable("B")),
					TypeAtom.TypeListNative)), TypeAtom.TypeListNative);
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
	public static final Operator map2ListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
													ClojureHelper.getLiteralInnerValue(list1),
													ClojureHelper.getLiteralInnerValue(list2))),
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression f = args.get(0);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l1 = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(1)).value).javaObject);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l2 = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(2)).value).javaObject);

			LinkedList<Expression> agg = new LinkedList<Expression>();

			Iterator<Expression> i1 = l1.iterator();
			Iterator<Expression> i2 = l2.iterator();
			while (i1.hasNext() && i2.hasNext()) {
				Expression e1 = i1.next();
				Expression e2 = i2.next();

				AbstractionApplication appl = new AbstractionApplication(f, new Tuple(e1, e2));
				Expression ret = appl.interpret(env, typeEnv);
				agg.add(ret);
			}

			return new LitComposite(new LitInteropObject(agg), TypeAtom.TypeListNative);
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());
		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable B = new TypeVariable(NameGenerator.next());
		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable C = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	public static final Operator foldlListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String terminator = "_term";
			String fn = "_fn";
			String arg1 = "_arg1";
			String arg2 = "_arg2";
			String code = ClojureHelper.fnHelper(Arrays.asList(fn, terminator, list),
					ClojureHelper.applyClojureFunction("reduce",
							ClojureHelper.fnHelper(Arrays.asList(arg1, arg2),
									ClojureHelper.applyVelkaFunction(fn, arg1, arg2)),
							terminator, ClojureHelper.getLiteralInnerValue(list)));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression f = args.get(0);
			Expression term = args.get(1);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(2)).value).javaObject);

			for (Expression e : l) {
				AbstractionApplication appl = new AbstractionApplication(f, new Tuple(term, e));
				term = appl.interpret(env, typeEnv);
			}

			return term;
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());
		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable B = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	public static final Operator addToEndOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String element = "_element";

			String code = ClojureHelper.fnHelper(Arrays.asList(list, element),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("lazy-seq",
									ClojureHelper.applyClojureFunction("concat",
											ClojureHelper.getLiteralInnerValue(list), ClojureHelper
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			Expression e = args.get(1);

			LinkedList<Expression> ll = new LinkedList<Expression>(l);
			ll.add(e);

			return new LitComposite(new LitInteropObject(ll), TypeAtom.TypeListNative);
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, A), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol listNativeToArrayListSymbol = new Symbol("to-array_list", NAMESPACE);
	public static final Symbol listNativeToArrayListSymbol_out = new Symbol("list-native-2-array-list");

	/**
	 * list-native-2-array-list operator
	 */
	public static final Operator ListNativeToArrayListOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(list),
							LitComposite.clojureValueToClojureLiteral(
									ClojureHelper.applyClojureFunction("java.util.ArrayList.",
											ClojureHelper.getLiteralInnerValue(list)),
									JavaArrayList.TypeListJavaArray));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			ArrayList<Expression> a = new ArrayList<Expression>(l);

			return new LitComposite(new LitInteropObject(a), JavaArrayList.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), JavaArrayList.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol ListNativeToLinkedListSymbol = new Symbol("to-linked-list", NAMESPACE);
	public static final Symbol ListNativeToLinkedListSymbol_out = new Symbol("list-native-2-linked-list");

	public static final Operator ListNativeToLinkedListOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(list),
							LitComposite.clojureValueToClojureLiteral(
									ClojureHelper.applyClojureFunction("java.util.LinkedList.",
											ClojureHelper.getLiteralInnerValue(list)),
									JavaArrayList.TypeListJavaArray));
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
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			LinkedList<Expression> a = new LinkedList<Expression>(l);

			return new LitComposite(new LitInteropObject(a), JavaLinkedList.TypeListJavaLinked);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), JavaLinkedList.TypeListJavaLinked);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
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
	public static final Operator contains = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			final String list = "_list";
			final String element = "_element";
			final String arg = "_arg";
			final String code = ClojureHelper.fnHelper(Arrays.asList(list, element),
					LitBoolean.clojureBooleanToClojureLitBoolean(ClojureHelper.applyClojureFunction("boolean",
							ClojureHelper.applyClojureFunction("some",
									ClojureHelper.fnHelper(Arrays.asList(arg),
											ClojureHelper.applyClojureFunction("=", arg, element)),
									ClojureHelper.getLiteralInnerValue(list)))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return containsSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			Expression element = args.get(1);

			if (l.contains(element)) {
				return LitBoolean.TRUE;
			}

			return LitBoolean.FALSE;
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, A), TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol filterSymbol = new Symbol("velka-filter", NAMESPACE);
	public static final Symbol filterSymbol_out = new Symbol("filter-list-native");

	public static final Operator filter = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String pred = "_pred";
			String arg = "_arg";
			String code = ClojureHelper
					.fnHelper(Arrays.asList(list, pred),
						LitComposite.clojureValueToClojureLiteral(
								ClojureHelper.applyClojureFunction("lazy-seq",
									ClojureHelper.applyClojureFunction("filter",
											ClojureHelper.fnHelper(Arrays.asList(arg),
													ClojureHelper.getLiteralInnerValue(ClojureHelper
															.applyVelkaFunction(pred, arg))),
											ClojureHelper.getLiteralInnerValue(list))),
								TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return filterSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			Expression pred = args.get(1);

			LinkedList<Expression> aux = new LinkedList<Expression>();

			for (Expression e : l) {
				AbstractionApplication app = new AbstractionApplication(pred, new Tuple(e));
				Expression rsl = app.interpret(env, typeEnv);
				if (rsl.equals(LitBoolean.TRUE)) {
					aux.add(e);
				}
			}

			return new LitComposite(new LitInteropObject(aux), TypeAtom.TypeListNative);
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
	public static final Operator get = new Operator() {

		private static final String errorMsg = "Index out of range";

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String index = "_index";
			String code = ClojureHelper.fnHelper(Arrays.asList(list, index), ClojureHelper.applyClojureFunction("nth",
					ClojureHelper.getLiteralInnerValue(list), ClojureHelper.getLiteralInnerValue(index)));

			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return getSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			LitInteger index = (LitInteger) args.get(1);

			if (index.value >= l.size()) {
				throw new UserException(errorMsg);
			}

			return l.get((int) index.value);
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeIntNative), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol buildListSymbol = new Symbol("build-list", NAMESPACE);
	public static final Symbol buildListSymbol_out = new Symbol("build-list-native");

	public static final Operator buildList = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String n = "_n";
			String fn = "_fn";
			String arg = "_arg";
			String code = ClojureHelper.fnHelper(Arrays.asList(n, fn),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("lazy-seq", ClojureHelper.applyClojureFunction("map",
									ClojureHelper.fnHelper(Arrays.asList(arg),
											ClojureHelper.applyVelkaFunction(fn, LitInteger.clojureIntToClojureLitInteger(arg))),
									ClojureHelper.applyClojureFunction("range", ClojureHelper.getLiteralInnerValue(n)))),
							TypeAtom.TypeListNative));

			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return buildListSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger n = (LitInteger) args.get(0);
			Expression fn = args.get(1);

			LinkedList<Expression> l = new LinkedList<Expression>();

			for (long i = 0; i < n.value; i++) {
				AbstractionApplication appl = new AbstractionApplication(fn, new Tuple(new LitInteger(i)));

				Expression expr = appl.interpret(env, typeEnv);

				l.add(expr);
			}

			return new LitComposite(new LitInteropObject(l), TypeAtom.TypeListNative);
		}

		private final TypeVariable buildListNative_A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeIntNative,
							new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), buildListNative_A)),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol removeSymbol = new Symbol("velka-remove", NAMESPACE);
	public static final Symbol removeSymbol_out = new Symbol("remove-list-native");

	public static final Operator remove = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
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
															ClojureHelper.getLiteralInnerValue(list))),
											TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return removeSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			Expression e = args.get(1);

			LinkedList<Expression> ll = new LinkedList<Expression>(l);
			ll.remove(e);

			return new LitComposite(new LitInteropObject(ll), TypeAtom.TypeListNative);
		}

		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, A), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol sizeSymbol = new Symbol("velka-size", NAMESPACE);
	public static final Symbol sizeSymbol_out = new Symbol("size-list-native");

	public static final Operator size = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String code = ClojureHelper.fnHelper(Arrays.asList(list), LitInteger.clojureIntToClojureLitInteger(
					ClojureHelper.applyClojureFunction("count", ClojureHelper.getLiteralInnerValue(list))));

			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return sizeSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);

			return new LitInteger(l.size());
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};

	public static final Symbol appendSymbol = new Symbol("append", NAMESPACE);
	public static final Symbol appendSymbol_out = new Symbol("append-list-native");

	public static final Operator append = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list1 = "_list1";
			String list2 = "_list2";
			String code = ClojureHelper.fnHelper(Arrays.asList(list1, list2), LitComposite.clojureValueToClojureLiteral(
					ClojureHelper.applyClojureFunction("lazy-seq", ClojureHelper.applyClojureFunction("concat",
							ClojureHelper.getLiteralInnerValue(list1), ClojureHelper.getLiteralInnerValue(list2))),
					TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return appendSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l0 = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l1 = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(1)).value).javaObject);

			LinkedList<Expression> aux = new LinkedList<Expression>(l0);
			aux.addAll(l1);

			return new LitComposite(new LitInteropObject(aux), TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}

	};
	
	public static final Symbol reverseSymbol = new Symbol("velka-reverse", NAMESPACE);
	public static final Symbol reverseSymbol_out = new Symbol("reverse-list-native");
	
	public static final Operator reverse = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "list";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list),
					LitComposite.clojureValueToClojureLiteral(
							ClojureHelper.applyClojureFunction("reverse",
									ClojureHelper.getLiteralInnerValue(list)),
							TypeAtom.TypeListNative));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return reverseSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			ListIterator<Expression> li = l.listIterator(l.size());
			List<Expression> r = new LinkedList<Expression>();
			while(li.hasPrevious()) {
				r.add(li.previous());
			}
			
			return new LitComposite(new LitInteropObject(r), TypeAtom.TypeListNative);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};
	
	public static final Symbol everypSymbol = new Symbol("velka-everyp", NAMESPACE);
	public static final Symbol everypSymbol_out = new Symbol("everyp-list-native");
	
	public static final Operator everyp = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String pred = "_pred";
			String pred_arg = "_arg";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, pred),
					LitBoolean.clojureBooleanToClojureLitBoolean(
							ClojureHelper.applyClojureFunction(
									"every?",
									ClojureHelper.fnHelper(
											Arrays.asList(pred_arg),
											ClojureHelper.getLiteralInnerValue(ClojureHelper.applyVelkaFunction(pred, pred_arg))),
									ClojureHelper.getLiteralInnerValue(list))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return everypSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			@SuppressWarnings("unchecked")
			LinkedList<Expression> l = (LinkedList<Expression>) (((LitInteropObject) ((LitComposite) args
					.get(0)).value).javaObject);
			Expression pred = args.get(1);
			
			Boolean ret = l.stream().allMatch(ThrowingPredicate.wrapper(
					expr -> {
						AbstractionApplication appl = new AbstractionApplication(pred, new Tuple((Expression)expr));
						Expression rslt = appl.interpret(env, typeEnv);
						return rslt.equals(LitBoolean.TRUE);
					}
					));
			
			return ret ? LitBoolean.TRUE : LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type type = new TypeArrow(
					new TypeTuple(TypeAtom.TypeListNative, new TypeArrow(
							new TypeTuple(new TypeVariable(NameGenerator.next())), TypeAtom.TypeBoolNative)),
					TypeAtom.TypeBoolNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}};

	/**
	 * Initializes list functions in environment
	 */
	public static void initializeInEnvironment(Environment env) {

		env.put(isEmptySymbol_out, isEmpty);
		env.put(headSymbol_out, headListNativeOperator);
		env.put(tailSymbol_out, tailListNativeOperator);
		env.put(mapSymbol_out, mapListNativeOperator);
		env.put(map2Symbol_out, map2ListNativeOperator);
		env.put(foldlSymbol_out, foldlListNativeOperator);
		env.put(addToEndSymbol_out, addToEndOperator);
		env.put(containsSymbol_out, contains);
		env.put(filterSymbol_out, filter);
		env.put(getSymbol_out, get);
		env.put(buildListSymbol_out, buildList);
		env.put(removeSymbol_out, remove);
		env.put(sizeSymbol_out, size);
		env.put(appendSymbol_out, append);
		env.put(reverseSymbol_out, reverse);
		env.put(everypSymbol_out, everyp);
	}

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
	public static LitComposite tupleToListNative(Tuple t) {
		return new LitComposite(
				new LitInteropObject(new LinkedList<Expression>(t.stream().collect(Collectors.toList()))),
				TypeAtom.TypeListNative);
	}

	/**
	 * Converts java list into list native expression
	 * 
	 * @param l converted list
	 * @return list native literal (LitComposite instance)
	 */
	public static Expression makeListNativeExpression(List<Expression> l) {
		ListIterator<Expression> i = l.listIterator(l.size());

		Expression agg = new Construct(TypeAtom.TypeListNative, Tuple.EMPTY_TUPLE);

		while (i.hasPrevious()) {
			Expression e = i.previous();

			agg = new Construct(TypeAtom.TypeListNative, new Tuple(e, agg));
		}

		return agg;
	}

	/**
	 * Converts exprssion into List Native expression
	 * 
	 * @param exprs expression
	 * @return list native literal (LitComposite instance)
	 */
	public static Expression makeListNativeExpression(Expression... exprs) {
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
	 * @param l printed list
	 * @return string with printed list
	 */
	public static String toStringListNative(List<Expression> l) {
		StringBuilder sb = new StringBuilder("(");
		Iterator<Expression> i = l.iterator();
		while(i.hasNext()) {
			Expression e = i.next();
			sb.append(e.toString());
			if(i.hasNext()) {
				sb.append(" ");
			}
		}
		sb.append(")");
		return sb.toString();
	}
}
