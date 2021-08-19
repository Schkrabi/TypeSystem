package velka.core.langbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
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
import velka.core.abstraction.Lambda;
import velka.core.abstraction.Operator;
import velka.core.abstraction.Operators;
import velka.core.application.AbstractionApplication;
import velka.core.application.Construct;
import velka.core.application.ExceptionExpr;
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
import velka.core.literal.LitString;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

public class ListNative {

	/**
	 * Name of velka.clojure.list namespace
	 */
	public static final String NAMESPACE = "velka.clojure.list";
	
	/**
	 * Symbol for empty constructor
	 */
	public static final Symbol constructorEmptySymbol = new Symbol("velka-construct-empty-list-native", NAMESPACE);
	
	/**
	 * Construtor for empty list
	 */
	public static final Lambda constructorEmpty = new Lambda(Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE,
			Tuple.EMPTY_TUPLE);

	public static final Symbol constructorSymbol = new Symbol("velka-construct-list-native", NAMESPACE);
	
	/**
	 * Constructor for non-empty list
	 */
	public static final Lambda constructor = new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("l"))),
			new TypeTuple(Arrays.asList(new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative)),
			new Tuple(Arrays.asList(new Symbol("x"), new Symbol("l"))));
	
	/**
	 * Empty list native
	 */
	public static final LitComposite EMPTY_LIST_NATIVE = new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative);
	
	/**
	 * Clojure code for empty list
	 */
	public static final String EMPTY_LIST_NATIVE_CLOJURE = LitComposite.clojureValueToClojureLiteral(Tuple.EMPTY_TUPLE_CLOJURE, TypeAtom.TypeListNative);

	/**
	 * is-list-native-empty symbol
	 */
	private static final Symbol isEmptySymbol = new Symbol("is-empty", NAMESPACE);
	public static final Symbol isEmptySymbol_out = new Symbol("is-list-native-empty");

//	/**
//	 * is-list-native-empty function
//	 */
//	public static final Lambda isListNativeEmpty = new Lambda(new Tuple(Arrays.asList(new Symbol("l"))),
//			new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
//			new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE));
	
	/**
	 * is-list-native-empty operator
	 */
	public static final Operator isEmpty = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list] (if (empty? (first _list)) " + LitBoolean.TRUE.toClojureCode(env, typeEnv) + " " + LitBoolean.FALSE.toClojureCode(env, typeEnv)
			 + "))";
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
			LitComposite list = ((LitComposite)args.get(0));
			
			if(list.equals(ListNative.EMPTY_LIST_NATIVE)) {
				return LitBoolean.TRUE;
			}
			return LitBoolean.FALSE;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
					TypeAtom.TypeBoolNative
					);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};

	/**
	 * head-list-native symbol
	 */
	private static final Symbol headSymbol = new Symbol("head", NAMESPACE);
	public static final Symbol headSymbol_out = new Symbol("head-list-native");

//	/**
//	 * head-list-native function
//	 */
//	public static final Lambda headListNative = new Lambda(new Tuple(Arrays.asList(new Symbol("l"))),
//			new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
//			new IfExpression(
//					new AbstractionApplication(ListNative.isListNativeEmptySymbol,
//							new Tuple(Arrays.asList(new Symbol("l")))),
//					new ExceptionExpr(new LitString("Cannot take head of empty list.")),
//					new AbstractionApplication(Operators.Car,
//							new Tuple(new Deconstruct(new Symbol("l"), new TypeTuple(
//									new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative))))));
	
	/**
	 * head-list-native operator
	 */
	public static final Operator headListNativeOperator = new Operator() {

		private final String errorMsg = "Cannot take head of empty list.";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list] (if (= " 
											+ LitBoolean.TRUE.toClojureCode(env, typeEnv) + " " 
											+ ClojureHelper.applyVelkaFunction(isEmptySymbol.toClojureCode(env, typeEnv), "_list") + ")"
										+ new ExceptionExpr(new LitString(errorMsg)).toClojureCode(env, typeEnv) + " "
										+ "(first (first _list))))";
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
			LitComposite l = (LitComposite)args.get(0);
			
			if(l.equals(ListNative.EMPTY_LIST_NATIVE)) {
				throw new UserException(errorMsg);
			}
			
			Tuple t = (Tuple)l.value;
			return t.get(0);
		}
		
		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
					A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};

	/**
	 * tail-list-native symbol
	 */
	private static final Symbol tailSymbol = new Symbol("tail", NAMESPACE);
	public static final Symbol tailSymbol_out = new Symbol("tail-list-native");

//	/**
//	 * tail-list-native function
//	 */
//	public static final Lambda tailListNative = new Lambda(new Tuple(Arrays.asList(new Symbol("l"))),
//			new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
//			new IfExpression(
//					new AbstractionApplication(ListNative.isListNativeEmptySymbol,
//							new Tuple(Arrays.asList(new Symbol("l")))),
//					new ExceptionExpr(new LitString("Cannot take tail of empty list.")),
//					new AbstractionApplication(Operators.Cdr,
//							new Tuple(Arrays.asList(new Deconstruct(new Symbol("l"), new TypeTuple(Arrays
//									.asList(new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative))))))));;

	public static final Operator tailListNativeOperator = new Operator() {

		private final String errorMsg = "Cannot take tail of empty list.";
		
		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_list] (if (= " 
					+ LitBoolean.TRUE.toClojureCode(env, typeEnv) + " " 
					+ ClojureHelper.applyVelkaFunction(isEmptySymbol.toClojureCode(env, typeEnv), "_list") + ")"
				+ new ExceptionExpr(new LitString(errorMsg)).toClojureCode(env, typeEnv) + " "
				+ "(second (first _list))))";
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
			LitComposite l = (LitComposite)args.get(0);
			
			if(l.equals(ListNative.EMPTY_LIST_NATIVE)) {
				throw new UserException(errorMsg);
			}
			
			Tuple t = (Tuple)l.value;
			return t.get(1);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
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
	 * map-list-native function
	 */
//	public static final Lambda mapListNative = new Lambda(new Tuple(Arrays.asList(new Symbol("f"), new Symbol("l"))),
//			new TypeTuple(Arrays
//					.asList(new TypeArrow(new TypeTuple(Arrays.asList(new TypeVariable("A"))), new TypeVariable("B")),
//							TypeAtom.TypeListNative)),
//			new IfExpression(
//					new AbstractionApplication(ListNative.isListNativeEmptySymbol,
//							new Tuple(Arrays.asList(new Symbol("l")))),
//					new Construct(TypeAtom.TypeListNative, Tuple.EMPTY_TUPLE),
//					new Construct(TypeAtom.TypeListNative, new Tuple(Arrays.asList(
//							new AbstractionApplication(new Symbol("f"),
//									new Tuple(Arrays.asList(new AbstractionApplication(ListNative.headListNativeSymbol,
//											new Tuple(Arrays.asList(new Symbol("l"))))))),
//							new AbstractionApplication(ListNative.mapListNativeSymbol,
//									new Tuple(Arrays.asList(new Symbol("f"),
//											new AbstractionApplication(ListNative.tailListNativeSymbol,
//													new Tuple(Arrays.asList(new Symbol("l"))))))))))));
	
	/**
	 * map-list-native operator
	 */
	public static final Operator mapListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_f _l] " + "(if (first " + ClojureHelper.applyVelkaFunction(ListNative.isEmptySymbol.toClojureCode(env, typeEnv), "_l") + ")" 
												+ ListNative.EMPTY_LIST_NATIVE.toClojureCode(env, typeEnv) + " " 
												+ LitComposite.clojureValueToClojureLiteral(
														ClojureHelper.tupleHelper(
																ClojureHelper.applyVelkaFunction(
																		"_f", 
																		ClojureHelper.applyVelkaFunction(
																				ListNative.headSymbol.toClojureCode(env, typeEnv), 
																				"_l")),
																ClojureHelper.applyVelkaFunction(
																		ListNative.mapSymbol.toClojureCode(env, typeEnv), 
																		"_f", 
																		ClojureHelper.applyVelkaFunction(
																				ListNative.tailSymbol.toClojureCode(env, typeEnv), 
																				"_l"))),
														TypeAtom.TypeListNative) + "))";
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
			LitComposite l = (LitComposite)args.get(1);
			
			List<Expression> agg = new LinkedList<Expression>();
			
			while(!l.equals(ListNative.EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l.value;
				AbstractionApplication appl = new AbstractionApplication(f, new Tuple(pair.get(0)));
				Expression res = appl.interpret(env, typeEnv);
				agg.add(res);
				l = (LitComposite)pair.get(1);
			}
			
			LitComposite res = ListNative.EMPTY_LIST_NATIVE;			
			ListIterator<Expression> i = agg.listIterator(agg.size());
			while(i.hasPrevious()) {
				Expression e = i.previous();
				res = new LitComposite(new Tuple(e, res), TypeAtom.TypeListNative);
			}
			
			return res;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
					new TypeTuple(Arrays
							.asList(new TypeArrow(new TypeTuple(Arrays.asList(new TypeVariable("A"))), new TypeVariable("B")),
									TypeAtom.TypeListNative)),
					TypeAtom.TypeListNative
					);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};

	/**
	 * Symbol for map2-list-native
	 */
	private static final Symbol map2Symbol = new Symbol("map2", NAMESPACE);
	public static final Symbol map2Symbol_out = new Symbol("map2-list-native");

	/**
	 * map2-list-native function
	 */
//	public static final Lambda map2ListNative = new Lambda(
//			new Tuple(new Symbol("f"), new Symbol("l1"), new Symbol("l2")),
//			new TypeTuple(new TypeArrow(new TypeTuple(Arrays.asList(A, B)), C), TypeAtom.TypeListNative,
//					TypeAtom.TypeListNative),
//			new IfExpression(
//					new OrExpression(
//							new Tuple(
//								new AbstractionApplication(isListNativeEmptySymbol,
//										new Tuple(new Symbol("l1"))),
//								new AbstractionApplication(isListNativeEmptySymbol,
//										new Tuple(new Symbol("l2"))))),
//					new Construct(TypeAtom.TypeListNative, Tuple.EMPTY_TUPLE),
//					new Construct(TypeAtom.TypeListNative,
//							new Tuple(
//								new AbstractionApplication(new Symbol("f"),
//										new Tuple(
//											new AbstractionApplication(ListNative.headListNativeSymbol,
//												new Tuple(new Symbol("l1"))),
//											new AbstractionApplication(ListNative.headListNativeSymbol,
//												new Tuple(new Symbol("l2"))))),
//								new AbstractionApplication(ListNative.map2ListNativeSymbol,
//										new Tuple(new Symbol("f"),
//											new AbstractionApplication(ListNative.tailListNativeSymbol,
//												new Tuple(new Symbol("l1"))),
//											new AbstractionApplication(ListNative.tailListNativeSymbol,
//												new Tuple(new Symbol("l2")))))))));
	
	/**
	 * map2-list-native operator
	 */
	public static final Operator map2ListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_f _l1 _l2] " + 
					"(if (or (first " + ClojureHelper.applyVelkaFunction(ListNative.isEmptySymbol.toClojureCode(env, typeEnv), "_l1") + ")" 
							+ "(first " + ClojureHelper.applyVelkaFunction(ListNative.isEmptySymbol.toClojureCode(env, typeEnv), "_l1") + ")) " 
						+ ListNative.EMPTY_LIST_NATIVE.toClojureCode(env, typeEnv) + " " +
						LitComposite.clojureValueToClojureLiteral(
								ClojureHelper.tupleHelper(
										ClojureHelper.applyVelkaFunction(
												"_f", 
												ClojureHelper.applyVelkaFunction(ListNative.headSymbol.toClojureCode(env, typeEnv), "_l1"),
												ClojureHelper.applyVelkaFunction(ListNative.headSymbol.toClojureCode(env, typeEnv), "_l2")),
										ClojureHelper.applyVelkaFunction(
												ListNative.map2Symbol.toClojureCode(env, typeEnv), 
												"_f",
												ClojureHelper.applyVelkaFunction(ListNative.tailSymbol.toClojureCode(env, typeEnv), "_l1"),
												ClojureHelper.applyVelkaFunction(ListNative.tailSymbol.toClojureCode(env, typeEnv), "_l2"))), 
								TypeAtom.TypeListNative) + "))";
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
			LitComposite l1 = (LitComposite)args.get(1);
			LitComposite l2 = (LitComposite)args.get(2);
			
			List<Expression> agg = new LinkedList<Expression>();
			
			while(!l1.equals(ListNative.EMPTY_LIST_NATIVE) || !l2.equals(ListNative.EMPTY_LIST_NATIVE)) {
				Tuple p1 = (Tuple)l1.value;
				Tuple p2 = (Tuple)l2.value;
				
				AbstractionApplication appl = new AbstractionApplication(f, new Tuple(p1.get(0), p2.get(0)));
				Expression ret = appl.interpret(env, typeEnv);
				agg.add(ret);
				l1 = (LitComposite)p1.get(1);
				l2 = (LitComposite)p2.get(1);
			}
			
			LitComposite res = ListNative.EMPTY_LIST_NATIVE;			
			ListIterator<Expression> i = agg.listIterator(agg.size());
			while(i.hasPrevious()) {
				Expression e = i.previous();
				res = new LitComposite(new Tuple(e, res), TypeAtom.TypeListNative);
			}
			
			return res;
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
			TypeArrow type = new TypeArrow(
					new TypeTuple(new TypeArrow(new TypeTuple(Arrays.asList(A, B)), C), TypeAtom.TypeListNative,
							TypeAtom.TypeListNative),
					TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};

	/**
	 * Symbol for foldl-list-native
	 */
	private static final Symbol foldlSymbol = new Symbol("foldl", NAMESPACE);
	public static final Symbol foldlSymbol_out = new Symbol("foldl-list-native");

//	public static final Lambda foldlListNative = new Lambda(
//			new Tuple(new Symbol("f"), new Symbol("term"), new Symbol("l")),
//			new TypeTuple(
//					new TypeArrow(new TypeTuple(A, B), A), A, TypeAtom.TypeListNative),
//			new IfExpression(
//					new AbstractionApplication(
//							ListNative.isListNativeEmptySymbol, new Tuple(new Symbol("l"))),
//					new Symbol("term"),
//					new AbstractionApplication(foldlListNativeSymbol,
//							new Tuple(
//									new Symbol("f"),
//									new AbstractionApplication(new Symbol("f"),
//											new Tuple(	
//													new Symbol("term"), 
//													new AbstractionApplication(ListNative.headListNativeSymbol, 
//															new Tuple(new Symbol("l"))))),
//									new AbstractionApplication(ListNative.tailListNativeSymbol,
//											new Tuple(new Symbol("l")))))));
	
	/**
	 * foldl-list-native operator
	 */
	public static final Operator foldlListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_f _term _l] (if (first " + ClojureHelper.applyVelkaFunction(ListNative.isEmptySymbol.toClojureCode(env, typeEnv), "_l") + ")"
							+ "_term"
							+ ClojureHelper.applyVelkaFunction(
									ListNative.foldlSymbol.toClojureCode(env, typeEnv), 
									"_f",
									ClojureHelper.applyVelkaFunction(
											"_f", 
											"_term", 
											ClojureHelper.applyVelkaFunction(ListNative.headSymbol.toClojureCode(env, typeEnv), "_l")),
									ClojureHelper.applyVelkaFunction(ListNative.tailSymbol.toClojureCode(env, typeEnv), "_l"))
							+ ")"  + ")";
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
			LitComposite l = (LitComposite)args.get(2);
			
			while(!l.equals(ListNative.EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l.value;
				AbstractionApplication appl = new AbstractionApplication(f, new Tuple(term, pair.get(0)));
				term = appl.interpret(env, typeEnv);
				
				l = (LitComposite)pair.get(1);
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
			TypeArrow type = new TypeArrow(new TypeTuple(
					new TypeArrow(new TypeTuple(A, B), A), A, TypeAtom.TypeListNative), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};

	/**
	 * Symbol for foldr-list-native
	 */
	private static final Symbol foldrSymbol = new Symbol("foldr", NAMESPACE);
	public static final Symbol foldrSymbol_out = new Symbol("foldr-list-native");

//	/**
//	 * foldr-list-native function
//	 */
//	public static final Lambda foldrListNative = new Lambda(
//			new Tuple(new Symbol("f"), new Symbol("term"), new Symbol("l")),
//			new TypeTuple(new TypeArrow(new TypeTuple(A, B), A), A,
//					TypeAtom.TypeListNative),
//			new IfExpression(
//					new AbstractionApplication(ListNative.isListNativeEmptySymbol, new Tuple(
//							new Symbol("l"))),
//					new Symbol("term"),
//					new AbstractionApplication(new Symbol("f"),
//							new Tuple(
//									new AbstractionApplication(foldrListNativeSymbol, new Tuple(
//											new Symbol("f"), new Symbol("term"),
//											new AbstractionApplication(
//													ListNative.tailListNativeSymbol,
//													new Tuple(new Symbol("l"))))),
//									new AbstractionApplication(
//											ListNative.headListNative,
//											new Tuple(new Symbol("l")))))));
	
	/**
	 * foldr-list-native operator
	 */
	public static final Operator foldrListNativeOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_f _term _l] (if (first " + ClojureHelper.applyVelkaFunction(ListNative.isEmptySymbol.toClojureCode(env, typeEnv), "_l") + ")"
								+ " _term "
								+ ClojureHelper.applyVelkaFunction("_f", 
										ClojureHelper.applyVelkaFunction(
												ListNative.foldrSymbol.toClojureCode(env, typeEnv), 
												"_f",
												"_term",
												ClojureHelper.applyVelkaFunction(
														ListNative.tailSymbol.toClojureCode(env, typeEnv), 
														"_l")),
										ClojureHelper.applyVelkaFunction(ListNative.headSymbol.toClojureCode(env, typeEnv), "_l"))
							+ "))";
			return code;
		}
		
		@Override
		public String toString() {
			return ListNative.foldlSymbol_out.toString();
		}

		@Override
		public Symbol getClojureSymbol() {
			return ListNative.foldrSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			Expression f = args.get(0);
			Expression term = args.get(1);
			LitComposite l = (LitComposite)args.get(2);
			
			List<Expression> aux = new LinkedList<Expression>();
			
			while(!l.equals(ListNative.EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l.value;
				aux.add(pair.get(0));
				l = (LitComposite)pair.get(1);
			}
			
			ListIterator<Expression> i = aux.listIterator(aux.size());
			while(i.hasPrevious()) {
				Expression e = i.previous();
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
					new TypeTuple(new TypeArrow(new TypeTuple(A, B), A), A,	TypeAtom.TypeListNative), A);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};
	
//	(lambda (_list _e)
//			(if (isEmpty _list)
//				(construct List Native _e _list)
//				(construct List Native
//						(head _list)
//						(add-to-end 
//							(tail _list)
//							_e
//							))
//				))
	/**
	 * Symbol for add-to-end function
	 */
	private static final Symbol addToEndSymbol = new Symbol("add-to-end", NAMESPACE);
	public static final Symbol addToEndSymbol_out = new Symbol("add-to-end-list-native");
	
	/**
	 * add-to-end function definition
	 */
//	public static final Lambda addToEnd = new Lambda(
//			new Tuple(new Symbol("_list"), new Symbol("_e")),
//			new TypeTuple(TypeAtom.TypeListNative, A),
//			new IfExpression(
//					new AbstractionApplication(ListNative.isListNativeEmptySymbol, new Tuple(new Symbol("_list"))),
//					new Construct(TypeAtom.TypeListNative, new Tuple(new Symbol("_e"), new Symbol("_list"))),
//					new Construct(
//							TypeAtom.TypeListNative,
//							new Tuple(
//									new AbstractionApplication(ListNative.headListNativeSymbol, new Tuple(new Symbol("_list"))),
//					new AbstractionApplication(ListNative.addToEndSymbol,
//							new Tuple(
//									new AbstractionApplication(
//											ListNative.tailListNativeSymbol,
//											new Tuple(
//													new Symbol("_list"))),
//									new Symbol("_e")))))));
	
	/**
	 * add-to-end operator
	 */
	public static final Operator addToEndOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_l _e] (if (first " + ClojureHelper.applyVelkaFunction(ListNative.isEmptySymbol.toClojureCode(env, typeEnv), "_l") + ") "
								+ LitComposite.clojureValueToClojureLiteral(ClojureHelper.tupleHelper("_e", "_l"), TypeAtom.TypeListNative) + " "
								+ LitComposite.clojureValueToClojureLiteral(
										ClojureHelper.tupleHelper(
											ClojureHelper.applyVelkaFunction(ListNative.headSymbol.toClojureCode(env, typeEnv), "_l"),
											ClojureHelper.applyVelkaFunction(
												ListNative.addToEndSymbol.toClojureCode(env, typeEnv), 
												ClojureHelper.applyVelkaFunction(
														ListNative.tailSymbol.toClojureCode(env, typeEnv), 
														"_l"),
												"_e")), TypeAtom.TypeListNative) + "))";
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
			LitComposite l = (LitComposite)args.get(0);
			Expression e = args.get(1);
			
			List<Expression> aux = new LinkedList<Expression>();
			
			while(!l.equals(ListNative.EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l.value;
				aux.add(pair.get(0));
				l = (LitComposite)pair.get(1);
			}
			aux.add(e);
			LitComposite term = ListNative.EMPTY_LIST_NATIVE;
			
			ListIterator<Expression> i = aux.listIterator(aux.size());
			while(i.hasPrevious()) {
				term = new LitComposite(new Tuple(i.previous(), term), TypeAtom.TypeListNative);
			}
			
			return term;
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
	
	/**
	 * Conversion lambda List:Native 2 List:JavaArray
	 */
	public static final Lambda ListNativeToArrayList = new Lambda(
			new Tuple(new Symbol("_list")),
			new TypeTuple(TypeAtom.TypeListNative),
			new AbstractionApplication(
					new Lambda(
							new Tuple(new Symbol("_agg")),
							new TypeTuple(JavaArrayList.TypeListJavaArray),
							new AbstractionApplication(
									Operators.Cdr,
									new Tuple(
											new Tuple(
													new AbstractionApplication(
															ListNative.foldlSymbol_out,
															new Tuple(
																	new Lambda(
																			new Tuple(
																					new Symbol("_l"),
																					new Symbol("_e")
																					),
																			new TypeTuple(
																					JavaArrayList.TypeListJavaArray,
																					new TypeVariable(NameGenerator.next())
																					),
																			new AbstractionApplication(
																					Operators.Cdr,
																					new Tuple(
																							new Tuple(
																									new AbstractionApplication(
																											JavaArrayList.addToEndSymbol_out,
																											new Tuple(
																													new Symbol("_l"),
																													new Symbol("_e")
																													)
																											),
																									new Symbol("_l")
																									)
																							)
																					)
																			),
																	new Symbol("_agg"),
																	new Symbol("_list")
																	)
															),
													new Symbol("_agg")
													)
											)
									)
							),
					new Tuple(new Construct(JavaArrayList.TypeListJavaArray, Tuple.EMPTY_TUPLE))
					)
			);
	
	private static final Symbol listNativeToArrayListSymbol = new Symbol("to-array_list", NAMESPACE);
	public static final Symbol listNativeToArrayListSymbol_out = new Symbol("list-native-2-array-list");
	
	/**
	 * list-native-2-array-list operator
	 */
	public static final Operator ListNativeToArrayListOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_l] (let [arrList " + ClojureHelper.addTypeMetaInfo("(java.util.ArrayList.)", JavaArrayList.TypeListJavaArray) + " "
											+ "aux " + ClojureHelper.applyVelkaFunction(ListNative.mapSymbol.toClojureCode(env, typeEnv), "velka.clojure.arrayList/java-array-list-add-to-end", "_l") + "] " 
										+ "arrList))";	
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
			LitComposite l = (LitComposite)args.get(0);
			ArrayList<Expression> a = new ArrayList<Expression>();
			
			while(!l.equals(ListNative.EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l.value;
				a.add(pair.get(0));
				l = (LitComposite)pair.get(1);
			}
			
			return new LitComposite(new LitInteropObject(a), JavaArrayList.TypeListJavaArray);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative), JavaArrayList.TypeListJavaArray);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};
	
	/**
	 * Conversion lambda List:Native 2 List:JavaArray
	 */
	public static final Lambda ListNativeToLinkedList = new Lambda(
			new Tuple(new Symbol("_list")),
			new TypeTuple(TypeAtom.TypeListNative),
			new AbstractionApplication(
					new Lambda(
							new Tuple(new Symbol("_agg")),
							new TypeTuple(JavaLinkedList.TypeListJavaLinked),
							new AbstractionApplication(
									Operators.Cdr,
									new Tuple(
											new Tuple(
													new AbstractionApplication(
															ListNative.foldlSymbol_out,
															new Tuple(
																	new Lambda(
																			new Tuple(
																					new Symbol("_l"),
																					new Symbol("_e")
																					),
																			new TypeTuple(
																					JavaLinkedList.TypeListJavaLinked,
																					new TypeVariable(NameGenerator.next())
																					),
																			new AbstractionApplication(
																					Operators.Cdr,
																					new Tuple(
																							new Tuple(
																									new AbstractionApplication(
																											JavaLinkedList.addToEndSymbol_out,
																											new Tuple(
																													new Symbol("_l"),
																													new Symbol("_e")
																													)
																											),
																									new Symbol("_l")
																									)
																							)
																					)
																			),
																	new Symbol("_agg"),
																	new Symbol("_list")
																	)
															),
													new Symbol("_agg")
													)
											)
									)
							),
					new Tuple(new Construct(JavaLinkedList.TypeListJavaLinked, Tuple.EMPTY_TUPLE))
					)
			);
	
	public static final Symbol ListNativeToLinkedListSymbol = new Symbol("to-linked-list", NAMESPACE);
	public static final Symbol ListNativeToLinkedListSymbol_out = new Symbol("list-native-2-linked-list");
	
	public static final Operator ListNativeToLinkedListOperator = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String code = "(fn [_l] (let [linkedList " + ClojureHelper.addTypeMetaInfo("(java.util.LinkedList.)", JavaLinkedList.TypeListJavaLinked) + " "
					+ "aux " + ClojureHelper.applyVelkaFunction(ListNative.mapSymbol.toClojureCode(env, typeEnv), "velka.clojure.linkedList/java-linked-list-add-to-end", "_l") + "] " 
				+ "linkedList))";	
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
			LitComposite l = (LitComposite)args.get(0);
			LinkedList<Expression> a = new LinkedList<Expression>();
			
			while(!l.equals(ListNative.EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l.value;
				a.add(pair.get(0));
				l = (LitComposite)pair.get(1);
			}
			
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
			final String code = ClojureHelper.fnHelper(
					Arrays.asList(list, element), 
					ClojureHelper.velkaIfHelper(
							ClojureHelper.applyVelkaFunction(
									isEmptySymbol.toClojureCode(env, typeEnv), 
									list), 
							LitBoolean.FALSE.toClojureCode(env, typeEnv), 
							ClojureHelper.velkaIfHelper(
									ClojureHelper.applyVelkaFunction(
											Operators.Equals.toClojureCode(env, typeEnv), 
											ClojureHelper.applyVelkaFunction(
													headSymbol.toClojureCode(env, typeEnv), 
													list),
											element), 
									LitBoolean.TRUE.toClojureCode(env, typeEnv), 
									ClojureHelper.applyVelkaFunction(
											containsSymbol.toClojureCode(env, typeEnv), 
											ClojureHelper.applyVelkaFunction(
													tailSymbol.toClojureCode(env, typeEnv), 
													list),
											element))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return containsSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite l = (LitComposite)args.get(0);
			Expression element = args.get(1);
			
			while(!l.equals(EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l.value;
				Expression current = pair.get(0);
				if(current.equals(element)) {
					return LitBoolean.TRUE;
				}
				l = (LitComposite)pair.get(1);
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
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, pred), 
					ClojureHelper.velkaIfHelper(
							ClojureHelper.applyVelkaFunction(
									isEmptySymbol.toClojureCode(env, typeEnv), 
									list), 
							list, 
							ClojureHelper.velkaIfHelper(
									ClojureHelper.applyVelkaFunction(
											pred, 
											ClojureHelper.applyVelkaFunction(
													headSymbol.toClojureCode(env, typeEnv),
													list)), 
									ClojureHelper.litCompositeHelper(
											TypeAtom.TypeListNative, 
											ClojureHelper.tupleHelper(
													ClojureHelper.applyVelkaFunction(headSymbol.toClojureCode(env, typeEnv), list),
													ClojureHelper.applyVelkaFunction(
															filterSymbol.toClojureCode(env, typeEnv), 
															ClojureHelper.applyVelkaFunction(tailSymbol.toClojureCode(env, typeEnv), list),
															pred))), 
									ClojureHelper.applyVelkaFunction(tailSymbol.toClojureCode(env, typeEnv), list))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return filterSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite l = (LitComposite)args.get(0);
			Expression pred = args.get(1);
			
			LinkedList<Expression> aux = new LinkedList<Expression>();
			
			while(!l.equals(EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l.value;
				Expression current = pair.get(0);
				
				AbstractionApplication appl = new AbstractionApplication(pred, new Tuple(current));
				Expression rslt = appl.interpret(env, typeEnv);
				if(rslt.equals(LitBoolean.TRUE)) {
					aux.add(current);
				}				
				
				l = (LitComposite)pair.get(1);
			}
			
			LitComposite term = ListNative.EMPTY_LIST_NATIVE;
			
			ListIterator<Expression> i = aux.listIterator(aux.size());
			while(i.hasPrevious()) {
				term = new LitComposite(new Tuple(i.previous(), term), TypeAtom.TypeListNative);
			}
			
			return term;
		}
		
		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			final TypeArrow type = new TypeArrow(new TypeTuple(
													TypeAtom.TypeListNative,
													new TypeArrow(
															new TypeTuple(A), 
															TypeAtom.TypeBoolNative)), 
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
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, index), 
					ClojureHelper.velkaIfHelper(
							ClojureHelper.applyVelkaFunction(isEmptySymbol.toClojureCode(env, typeEnv), list), 
							ClojureHelper.errorHelper(ClojureHelper.stringHelper(errorMsg)), 
							ClojureHelper.velkaIfHelper(
									ClojureHelper.applyVelkaFunction(
											Operators.NumericEqual.toClojureCode(env, typeEnv), 
											index,
											new LitInteger(0).toClojureCode(env, typeEnv)), 
									ClojureHelper.applyVelkaFunction(headSymbol.toClojureCode(env, typeEnv), list), 
									ClojureHelper.applyVelkaFunction(
											getSymbol.toClojureCode(env, typeEnv), 
											ClojureHelper.applyVelkaFunction(tailSymbol.toClojureCode(env, typeEnv), list),
											ClojureHelper.applyVelkaFunction(Operators.Subtraction.toClojureCode(env, typeEnv), index, new LitInteger(1).toClojureCode(env, typeEnv))))));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return getSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite l = (LitComposite)args.get(0);
			LitInteger index = (LitInteger)args.get(1);
			
			long i = index.value;
			
			while(!(l.equals(EMPTY_LIST_NATIVE))) {
				Tuple pair = (Tuple)l.value;
				if(i == 0) {
					return pair.get(0);
				}
				i--;
				l = (LitComposite)pair.get(1);
			}
			
			throw new UserException(errorMsg);
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
	
	public static final Symbol buildListAuxSymbol = new Symbol("build-list-aux", NAMESPACE);
	public static final Operator buildListAux = new Operator() {

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String n = "_n";
			String fn = "_fn";
			String i = "_i";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(n, fn, i),
					ClojureHelper.velkaIfHelper(
							ClojureHelper.applyVelkaFunction(Operators.LesserThan.toClojureCode(env, typeEnv), i, n), 
							ClojureHelper.litCompositeHelper(
									TypeAtom.TypeListNative, 
									ClojureHelper.tupleHelper(
											ClojureHelper.applyVelkaFunction(fn, i),
											ClojureHelper.applyVelkaFunction(
													buildListAuxSymbol.toClojureCode(env, typeEnv), 
													n, 
													fn,
													ClojureHelper.applyVelkaFunction(
															Operators.Addition.toClojureCode(env, typeEnv), 
															i, 
															LitInteger.clojureIntToClojureLitInteger("1"))))), 
							EMPTY_LIST_NATIVE.toClojureCode(env, typeEnv)));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return buildListAuxSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			throw new AppendableException("Helper operator " + this.getClass().getCanonicalName() + " is not supported for interpretation");
		}
		
		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative,
					new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative), A), TypeAtom.TypeIntNative),
					TypeAtom.TypeListNative);
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
			String code = ClojureHelper.fnHelper(
					Arrays.asList(n, fn), 
					ClojureHelper.applyVelkaFunction(
							buildListAuxSymbol.toClojureCode(env, typeEnv), 
							n,
							fn,
							LitInteger.clojureIntToClojureLitInteger("0")));
					
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return buildListSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitInteger n = (LitInteger)args.get(0);
			Expression fn = args.get(1);
			
			LitComposite l = ListNative.EMPTY_LIST_NATIVE;
			
			for(long i = n.value - 1; i >= 0 ; i--) {
				AbstractionApplication appl = new AbstractionApplication(fn, new Tuple(new LitInteger(i)));
				
				Expression expr = appl.interpret(env, typeEnv);
				
				l = new LitComposite(new Tuple(expr, l), TypeAtom.TypeListNative);
			}
			
			return l;
		}
		
		private final TypeVariable buildListNative_A = new TypeVariable(NameGenerator.next());

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(
								new TypeTuple(
										TypeAtom.TypeIntNative, 
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
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list, element),
					ClojureHelper.velkaIfHelper(
							ClojureHelper.applyVelkaFunction(isEmptySymbol.toClojureCode(env, typeEnv), list), 
							list, 
							ClojureHelper.velkaIfHelper(
									ClojureHelper.applyVelkaFunction(
											Operators.Equals.getClojureSymbol().toClojureCode(env, typeEnv),
											ClojureHelper.applyVelkaFunction(headSymbol.toClojureCode(env, typeEnv), list),
											element), 
									ClojureHelper.applyVelkaFunction(tailSymbol.toClojureCode(env, typeEnv), list), 
									ClojureHelper.litCompositeHelper(
											TypeAtom.TypeListNative, 
											ClojureHelper.tupleHelper(
													ClojureHelper.applyVelkaFunction(headSymbol.toClojureCode(env, typeEnv), list),
													ClojureHelper.applyVelkaFunction(
															removeSymbol.toClojureCode(env, typeEnv), 
															ClojureHelper.applyVelkaFunction(tailSymbol.toClojureCode(env, typeEnv), list),
															element))))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return removeSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite l = (LitComposite)args.get(0);
			Expression e = args.get(1);
			
			LinkedList<Expression> aux = new LinkedList<Expression>();
			while(!l.equals(EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l.value;
				Expression current = pair.get(0);
				
				if(!current.equals(e)) {
					aux.add(current);
				}
				l = (LitComposite)pair.get(1);
			}
			
			LitComposite term = ListNative.EMPTY_LIST_NATIVE;
			
			ListIterator<Expression> i = aux.listIterator(aux.size());
			while(i.hasPrevious()) {
				term = new LitComposite(new Tuple(i.previous(), term), TypeAtom.TypeListNative);
			}
			
			return term;
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
		
		/**
		 * Type variable for use in lambda
		 */
		private final TypeVariable A = new TypeVariable(NameGenerator.next());

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			String list = "_list";
			String x = "_x";
			String y = "_y";
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list), 
					ClojureHelper.applyVelkaFunction(
							foldrSymbol.toClojureCode(env, typeEnv), 
							ClojureHelper.addTypeMetaInfo(
							ClojureHelper.lambdaHelper(ClojureHelper.addTypeMetaInfo(
									ClojureHelper.fnHelper(
										Arrays.asList(x, y), 
										ClojureHelper.applyVelkaFunction(
												Operators.Addition.getClojureSymbol().toClojureCode(env, typeEnv), 
												x, 
												LitInteger.clojureIntToClojureLitInteger("1"))), 
									new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, A), TypeAtom.TypeIntNative))),
							new TypeArrow(new TypeTuple(TypeAtom.TypeIntNative, A), TypeAtom.TypeIntNative)),
							LitInteger.clojureIntToClojureLitInteger("0"),
							list));
			
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return sizeSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite l = (LitComposite)args.get(0);
			long n = 0;
			while(!l.equals(EMPTY_LIST_NATIVE)) {
				n++;
				Tuple pair = (Tuple)l.value;
				l = (LitComposite)pair.get(1);
			}
			
			return new LitInteger(n);
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
			String code = ClojureHelper.fnHelper(
					Arrays.asList(list1, list2), 
					ClojureHelper.velkaIfHelper(
							ClojureHelper.applyVelkaFunction(isEmptySymbol.toClojureCode(env, typeEnv), list2), 
							list1, 
							ClojureHelper.velkaIfHelper(
									ClojureHelper.applyVelkaFunction(isEmptySymbol.toClojureCode(env, typeEnv), list1), 
									list2, 
									ClojureHelper.litCompositeHelper(
											TypeAtom.TypeListNative, 
											ClojureHelper.tupleHelper(
													ClojureHelper.applyVelkaFunction(headSymbol.toClojureCode(env, typeEnv), list1),
													ClojureHelper.applyVelkaFunction(
															appendSymbol.toClojureCode(env, typeEnv), 
															ClojureHelper.applyVelkaFunction(tailSymbol.toClojureCode(env, typeEnv), list1),
															list2))))));
			return code;
		}

		@Override
		public Symbol getClojureSymbol() {
			return appendSymbol;
		}

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite l1 = (LitComposite)args.get(0);
			LitComposite l2 = (LitComposite)args.get(1);
			
			LinkedList<Expression> aux = new LinkedList<Expression>();
			while(!l1.equals(EMPTY_LIST_NATIVE)) {
				Tuple pair = (Tuple)l1.value;
				Expression current = pair.get(0);
				aux.add(current);
				l1 = (LitComposite)pair.get(1);
			}
			
			LitComposite term = l2;
			
			ListIterator<Expression> i = aux.listIterator(aux.size());
			while(i.hasPrevious()) {
				term = new LitComposite(new Tuple(i.previous(), term), TypeAtom.TypeListNative);
			}
			
			return term;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};

	/**
	 * Initializes list functions in environment
	 */
	public static void initializeInEnvironment(Environment env, TypeEnvironment typeEnv) {
		
		env.put(isEmptySymbol_out, isEmpty);
		env.put(headSymbol_out, headListNativeOperator);
		env.put(tailSymbol_out, tailListNativeOperator);
		env.put(mapSymbol_out, mapListNativeOperator);
		env.put(map2Symbol_out, map2ListNativeOperator);
		env.put(foldlSymbol_out, foldlListNativeOperator);
		env.put(foldrSymbol_out, foldrListNativeOperator);
		env.put(addToEndSymbol_out, addToEndOperator);
		env.put(containsSymbol_out, contains);
		env.put(filterSymbol_out, filter);
		env.put(getSymbol_out, get);
		env.put(buildListSymbol_out, buildList);
		env.put(removeSymbol_out, remove);
		env.put(sizeSymbol_out, size);
		env.put(appendSymbol_out, append);
	}
	
	/**
	 * Creates a native list from collection
	 * @param col collection
	 * @return LitComposite with native list
	 */
	public static LitComposite collectionToListNative(Collection<? extends Expression> col) {
		List<Expression> l = col.stream().collect(Collectors.toList());
		return ListNative.makeListNativeExpression(l);
	}
	
	/**
	 * Converts tuple into equvivalent list
	 * @param t converted tuple
	 * @return LitComposite object containing native list
	 */
	public static LitComposite tupleToListNative(Tuple t) {
		return t.reverse().stream().reduce(
				ListNative.EMPTY_LIST_NATIVE, 
				(x, y) -> new LitComposite(new Tuple(Arrays.asList(y, x)), TypeAtom.TypeListNative), 
				(x, y) -> y);
	}
	
	/**
	 * Converts java list into list native expression
	 * @param l converted list
	 * @return list native literal (LitComposite instance)
	 */
	public static LitComposite makeListNativeExpression(List<Expression> l) {
		LitComposite res = ListNative.EMPTY_LIST_NATIVE;
		
		ListIterator<Expression> i = l.listIterator(l.size());
		while(i.hasPrevious()) {
			Expression e = i.previous();
			res = new LitComposite(new Tuple(e, res), TypeAtom.TypeListNative);
		}
		
		return res;
	}
	
	/**
	 * Converts exprssion into List Native expression
	 * @param exprs expression
	 * @return list native literal (LitComposite instance)
	 */
	public static LitComposite makeListNativeExpression(Expression ...exprs) {
		return makeListNativeExpression(Arrays.asList(exprs));
	}
	
	/**
	 * Converts (at interpretation runtime) list into tuple
	 * @param list converted list
	 * @return tuple
	 */
	public static Tuple listNativeToTuple(LitComposite list) {
		List<Expression> l = new LinkedList<Expression>();
		
		LitComposite it = list;
		
		while(!it.equals(ListNative.EMPTY_LIST_NATIVE)) {
			Tuple pair = (Tuple)it.value;
			Expression current = pair.get(0);
			l.add(current);
			
			it = (LitComposite)pair.get(1);
		}
		
		return new Tuple(l);
	}
}
