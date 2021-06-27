package velka.lang.langbase;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Optional;

import velka.lang.abstraction.Lambda;
import velka.lang.abstraction.Operator;
import velka.lang.abstraction.Operators;
import velka.lang.application.Construct;
import velka.lang.application.ExceptionExpr;
import velka.lang.exceptions.UserException;
import velka.lang.expression.Expression;
import velka.lang.expression.Symbol;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.ClojureHelper;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.interpretation.VelkaClojureList;
import velka.lang.literal.LitBoolean;
import velka.lang.literal.LitComposite;
import velka.lang.literal.LitInteropObject;
import velka.lang.literal.LitString;
import velka.lang.application.AbstractionApplication;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;

public class ListNative {
	
	public static final Symbol constructorEmptySymbol = new Symbol("velka-construct-empty-list-native", VelkaClojureList.NAMESPACE);
	
	/**
	 * Construtor for empty list
	 */
	public static final Lambda constructorEmpty = new Lambda(Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE,
			Tuple.EMPTY_TUPLE);

	public static final Symbol constructorSymbol = new Symbol("velka-construct-list-native", VelkaClojureList.NAMESPACE);
	
	/**
	 * Constructor for non-empty list
	 */
	public static final Lambda constructor = new Lambda(new Tuple(Arrays.asList(new Symbol("x"), new Symbol("l"))),
			new TypeTuple(Arrays.asList(new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative)),
			new Tuple(Arrays.asList(new Symbol("x"), new Symbol("l"))));

	/**
	 * Type variable for use in lambda
	 */
	private static TypeVariable A = new TypeVariable(NameGenerator.next());
	/**
	 * Type variable for use in lambda
	 */
	private static TypeVariable B = new TypeVariable(NameGenerator.next());
	/**
	 * Type variable for use in lambda
	 */
	private static TypeVariable C = new TypeVariable(NameGenerator.next());
	
	/**
	 * Empty list native
	 */
	public static final LitComposite EMPTY_LIST_NATIVE = new LitComposite(Tuple.EMPTY_TUPLE, TypeAtom.TypeListNative);

	/**
	 * is-list-native-empty symbol
	 */
	private static final Symbol isEmptySymbol = new Symbol("is-empty", VelkaClojureList.NAMESPACE);
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
	private static final Symbol headSymbol = new Symbol("head", VelkaClojureList.NAMESPACE);
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
	private static final Symbol tailSymbol = new Symbol("tail", VelkaClojureList.NAMESPACE);
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
	private static final Symbol mapSymbol = new Symbol("velka-map", VelkaClojureList.NAMESPACE);
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
	private static final Symbol map2Symbol = new Symbol("map2", VelkaClojureList.NAMESPACE);
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
	private static final Symbol foldlSymbol = new Symbol("foldl", VelkaClojureList.NAMESPACE);
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
	private static final Symbol foldrSymbol = new Symbol("foldr", VelkaClojureList.NAMESPACE);
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
	private static final Symbol addToEndSymbol = new Symbol("add-to-end", VelkaClojureList.NAMESPACE);
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

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			TypeArrow type = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, A), TypeAtom.TypeListNative);
			return new Pair<Type, Substitution>(type, Substitution.EMPTY);
		}
		
	};
	
//	(lambda (_list _index _e)
//			(if (is-empty _list)
//				(error "Index out of bounds exception")
//				(if (= _index 0)
//					(construct List Native)
//						)
//				)
//			)
	
	
//	(lambda (_list)
//			(foldl-list-native
//					java-array-list-add-to-end
//					(construct List JavaArray)
//					_list))
	
//	(lambda (_list)
//			((lambda (_agg)
//					(cdr
//						(cons
//							(foldl-list-native
//									(lambda (_l _e)
//										(cdr 
//											(cons 
//												(java-array-list-add-to-end _l _e)
//												_l)))
//									_agg
//									_list)
//							_agg)))
//			(construt List JavaArray)
//			))
	
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
	
	private static final Symbol listNativeToArrayListSymbol = new Symbol("to-array_list", VelkaClojureList.NAMESPACE);
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
	
	public static final Symbol ListNativeToLinkedListSymbol = new Symbol("to-linked-list", VelkaClojureList.NAMESPACE);
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
//		try {
////			(new DefineSymbol(isListNativeEmptySymbol, isListNativeEmpty)).interpret(env, typeEnv);
////			(new DefineSymbol(headListNativeSymbol, headListNative)).interpret(env, typeEnv);
////			(new DefineSymbol(tailListNativeSymbol, tailListNative)).interpret(env, typeEnv);
////			(new DefineSymbol(mapListNativeSymbol, mapListNative)).interpret(env, typeEnv);
////			(new DefineSymbol(map2ListNativeSymbol, map2ListNative)).interpret(env, typeEnv);
////			(new DefineSymbol(foldlListNativeSymbol, foldlListNative)).interpret(env, typeEnv);
//			//(new DefineSymbol(foldrListNativeSymbol, foldrListNative)).interpret(env, typeEnv);
////			(new DefineSymbol(addToEndSymbol, addToEnd)).interpret(env, typeEnv);
//			
//			
//			
//		} catch (AppendableException e) {
//			System.err.println("Interpretation error " + e.getMessage() + " occured in " + ListNative.class.getName());
//		}
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
}
