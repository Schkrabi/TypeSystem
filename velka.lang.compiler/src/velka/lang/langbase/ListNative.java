package velka.lang.langbase;

import java.util.Arrays;

import velka.lang.abstraction.Lambda;
import velka.lang.abstraction.Operator;
import velka.lang.application.CanDeconstructAs;
import velka.lang.application.Construct;
import velka.lang.application.Deconstruct;
import velka.lang.application.DefineSymbol;
import velka.lang.application.ExceptionExpr;
import velka.lang.application.IfExpression;
import velka.lang.application.OrExpression;
import velka.lang.expression.Symbol;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.literal.LitString;
import velka.lang.application.AbstractionApplication;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;

public class ListNative {
	/**
	 * Construtor for empty list
	 */
	public static final Lambda constructorEmpty = new Lambda(Tuple.EMPTY_TUPLE, TypeTuple.EMPTY_TUPLE,
			Tuple.EMPTY_TUPLE);

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
	 * is-list-native-empty symbol
	 */
	public static final Symbol isListNativeEmptySymbol = new Symbol("is-list-native-empty");

	/**
	 * is-list-native-empty function
	 */
	public static final Lambda isListNativeEmpty = new Lambda(new Tuple(Arrays.asList(new Symbol("l"))),
			new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
			new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE));

	/**
	 * head-list-native symbol
	 */
	public static final Symbol headListNativeSymbol = new Symbol("head-list-native");

	/**
	 * head-list-native function
	 */
	public static final Lambda headListNative = new Lambda(new Tuple(Arrays.asList(new Symbol("l"))),
			new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
			new IfExpression(
					new AbstractionApplication(ListNative.isListNativeEmptySymbol,
							new Tuple(Arrays.asList(new Symbol("l")))),
					new ExceptionExpr(new LitString("Cannot take head of empty list.")),
					new AbstractionApplication(Operator.Car,
							new Tuple(Arrays.asList(new Deconstruct(new Symbol("l"), new TypeTuple(Arrays
									.asList(new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative))))))));

	/**
	 * tail-list-native symbol
	 */
	public static final Symbol tailListNativeSymbol = new Symbol("tail-list-native");

	/**
	 * tail-list-native function
	 */
	public static final Lambda tailListNative = new Lambda(new Tuple(Arrays.asList(new Symbol("l"))),
			new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
			new IfExpression(
					new AbstractionApplication(ListNative.isListNativeEmptySymbol,
							new Tuple(Arrays.asList(new Symbol("l")))),
					new ExceptionExpr(new LitString("Cannot take tail of empty list.")),
					new AbstractionApplication(Operator.Cdr,
							new Tuple(Arrays.asList(new Deconstruct(new Symbol("l"), new TypeTuple(Arrays
									.asList(new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative))))))));;

	/**
	 * map-list-native symbol
	 */
	public static final Symbol mapListNativeSymbol = new Symbol("map-list-native");

	/**
	 * map-list-native function
	 */
	public static final Lambda mapListNative = new Lambda(new Tuple(Arrays.asList(new Symbol("f"), new Symbol("l"))),
			new TypeTuple(Arrays
					.asList(new TypeArrow(new TypeTuple(Arrays.asList(new TypeVariable("A"))), new TypeVariable("B")),
							TypeAtom.TypeListNative)),
			new IfExpression(
					new AbstractionApplication(ListNative.isListNativeEmptySymbol,
							new Tuple(Arrays.asList(new Symbol("l")))),
					new Construct(TypeAtom.TypeListNative, Tuple.EMPTY_TUPLE),
					new Construct(TypeAtom.TypeListNative, new Tuple(Arrays.asList(
							new AbstractionApplication(new Symbol("f"),
									new Tuple(Arrays.asList(new AbstractionApplication(ListNative.headListNativeSymbol,
											new Tuple(Arrays.asList(new Symbol("l"))))))),
							new AbstractionApplication(ListNative.mapListNativeSymbol,
									new Tuple(Arrays.asList(new Symbol("f"),
											new AbstractionApplication(ListNative.tailListNativeSymbol,
													new Tuple(Arrays.asList(new Symbol("l"))))))))))));

	/**
	 * Symbol for map2-list-native
	 */
	public static final Symbol map2ListNativeSymbol = new Symbol("map2-list-native");

	/**
	 * map2-list-native function
	 */
	public static final Lambda map2ListNative = new Lambda(
			new Tuple(Arrays.asList(new Symbol("f"), new Symbol("l1"), new Symbol("l2"))),
			new TypeTuple(Arrays.asList(new TypeArrow(new TypeTuple(Arrays.asList(A, B)), C), TypeAtom.TypeListNative,
					TypeAtom.TypeListNative)),
			new IfExpression(
					new OrExpression(
							new Tuple(
									Arrays.asList(
											new AbstractionApplication(isListNativeEmptySymbol,
													new Tuple(Arrays.asList(new Symbol("l1")))),
											new AbstractionApplication(isListNativeEmptySymbol,
													new Tuple(Arrays.asList(new Symbol("l2"))))))),
					new Construct(TypeAtom.TypeListNative, Tuple.EMPTY_TUPLE),
					new Construct(TypeAtom.TypeListNative,
							new Tuple(Arrays.asList(
									new AbstractionApplication(new Symbol("f"),
											new Tuple(Arrays.asList(
													new AbstractionApplication(ListNative.headListNativeSymbol,
															new Tuple(Arrays.asList(new Symbol("l1")))),
													new AbstractionApplication(ListNative.headListNativeSymbol,
															new Tuple(Arrays.asList(new Symbol("l2"))))))),
									new AbstractionApplication(ListNative.map2ListNativeSymbol,
											new Tuple(Arrays.asList(new Symbol("f"),
													new AbstractionApplication(ListNative.tailListNativeSymbol,
															new Tuple(Arrays.asList(new Symbol("l1")))),
													new AbstractionApplication(ListNative.tailListNativeSymbol,
															new Tuple(Arrays.asList(new Symbol("l2"))))))))))));

	/**
	 * Symbol for foldl-list-native
	 */
	public static final Symbol foldlListNativeSymbol = new Symbol("foldl-list-native");

	public static final Lambda foldlListNative = new Lambda(
			new Tuple(Arrays.asList(new Symbol("f"), new Symbol("term"), new Symbol("l"))),
			new TypeTuple(
					Arrays.asList(new TypeArrow(new TypeTuple(Arrays.asList(A, A)), A), A, TypeAtom.TypeListNative)),
			new IfExpression(
					new AbstractionApplication(
							ListNative.isListNativeEmptySymbol, new Tuple(Arrays.asList(new Symbol("l")))),
					new Symbol("term"),
					new AbstractionApplication(foldlListNativeSymbol,
							new Tuple(Arrays.asList(new Symbol("f"), new AbstractionApplication(new Symbol("f"),
									new Tuple(Arrays.asList(new Symbol("term"), new AbstractionApplication(Operator.Car,
											new Tuple(Arrays.asList(new Deconstruct(new Symbol("l"),
													new TypeTuple(Arrays.asList(A, TypeAtom.TypeListNative))))))))),
									new AbstractionApplication(Operator.Cdr,
											new Tuple(Arrays.asList(new Deconstruct(new Symbol("l"),
													new TypeTuple(Arrays.asList(A, TypeAtom.TypeListNative)))))))))));

	/**
	 * Symbol for foldr-list-native
	 */
	public static final Symbol foldrListNativeSymbol = new Symbol("foldr-list-native");

	/**
	 * foldr-list-native function
	 */
	public static final Lambda foldrListNative = new Lambda(
			new Tuple(Arrays.asList(new Symbol("f"), new Symbol("term"), new Symbol("l"))),
			new TypeTuple(Arrays.asList(new TypeArrow(new TypeTuple(Arrays.asList(A, A)), A), A,
					TypeAtom.TypeListNative)),
			new IfExpression(
					new AbstractionApplication(ListNative.isListNativeEmptySymbol, new Tuple(
							Arrays.asList(new Symbol("l")))),
					new Symbol("term"),
					new AbstractionApplication(new Symbol("f"),
							new Tuple(
									Arrays.asList(
											new AbstractionApplication(foldrListNativeSymbol, new Tuple(Arrays.asList(
													new Symbol("f"), new Symbol("term"),
													new AbstractionApplication(Operator.Cdr,
															new Tuple(Arrays.asList(new Deconstruct(new Symbol("l"),
																	new TypeTuple(Arrays.asList(A,
																			TypeAtom.TypeListNative))))))))),
											new AbstractionApplication(Operator.Car,
													new Tuple(Arrays
															.asList(new Deconstruct(new Symbol("l"), new TypeTuple(
																	Arrays.asList(A, TypeAtom.TypeListNative)))))))))));

	/**
	 * Generates code for clojure regarding Native List
	 * 
	 * @return
	 */
	public static String makeClojureCode(Environment env, TypeEnvironment typeEnv) {
		StringBuilder s = new StringBuilder();

		try {
			s.append((new DefineSymbol(isListNativeEmptySymbol, isListNativeEmpty)).toClojureCode(env, typeEnv));
			s.append('\n');
			s.append((new DefineSymbol(headListNativeSymbol, headListNative)).toClojureCode(env, typeEnv));
			s.append('\n');
			s.append((new DefineSymbol(tailListNativeSymbol, tailListNative)).toClojureCode(env, typeEnv));
			s.append("\n");
			s.append((new DefineSymbol(mapListNativeSymbol, mapListNative)).toClojureCode(env, typeEnv));
			s.append('\n');
			s.append((new DefineSymbol(map2ListNativeSymbol, map2ListNative)).toClojureCode(env, typeEnv));
			s.append('\n');
			s.append((new DefineSymbol(foldlListNativeSymbol, foldlListNative)).toClojureCode(env, typeEnv));
			s.append('\n');
			s.append((new DefineSymbol(foldrListNativeSymbol, foldrListNative)).toClojureCode(env, typeEnv));
			s.append('\n');
		} catch (AppendableException e) {
			System.err.println("Compilation error " + e.getMessage() + " occured in " + ListNative.class.getName());
		}

		return s.toString();
	}

	/**
	 * Initializes list functions in environment
	 */
	public static void initializeInEnvironment(Environment env, TypeEnvironment typeEnv) {
		try {
			(new DefineSymbol(isListNativeEmptySymbol, isListNativeEmpty)).interpret(env, typeEnv);
			(new DefineSymbol(headListNativeSymbol, headListNative)).interpret(env, typeEnv);
			(new DefineSymbol(tailListNativeSymbol, tailListNative)).interpret(env, typeEnv);
			(new DefineSymbol(mapListNativeSymbol, mapListNative)).interpret(env, typeEnv);
			(new DefineSymbol(map2ListNativeSymbol, map2ListNative)).interpret(env, typeEnv);
			(new DefineSymbol(foldlListNativeSymbol, foldlListNative)).interpret(env, typeEnv);
			(new DefineSymbol(foldrListNativeSymbol, foldrListNative)).interpret(env, typeEnv);
		} catch (AppendableException e) {
			System.err.println("Interpretation error " + e.getMessage() + " occured in " + ListNative.class.getName());
		}
	}
}
