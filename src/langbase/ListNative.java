package langbase;

import java.util.Arrays;

import abstraction.Lambda;
import abstraction.Operator;
import application.CanDeconstructAs;
import application.Construct;
import application.Deconstruct;
import application.DefineSymbol;
import application.ExceptionExpr;
import application.IfExpression;
import application.OrExpression;
import expression.Symbol;
import expression.Tuple;
import interpretation.Environment;
import literal.LitString;
import application.AbstractionApplication;
import types.TypeArrow;
import types.TypeAtom;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;

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

	public static final Symbol map2ListNativeSymbol = new Symbol("map2-list-native");

	private static TypeVariable A = new TypeVariable(NameGenerator.next());
	private static TypeVariable B = new TypeVariable(NameGenerator.next());
	private static TypeVariable C = new TypeVariable(NameGenerator.next());

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
	 * Generates code for clojure regarding Native List
	 * 
	 * @return
	 */
	public static String makeClojureCode() {
		StringBuilder s = new StringBuilder();

		try {
			s.append((new DefineSymbol(isListNativeEmptySymbol, isListNativeEmpty)).toClojureCode());
			s.append('\n');
			s.append((new DefineSymbol(headListNativeSymbol, headListNative)).toClojureCode());
			s.append('\n');
			s.append((new DefineSymbol(tailListNativeSymbol, tailListNative)).toClojureCode());
			s.append("\n");
			s.append((new DefineSymbol(mapListNativeSymbol, mapListNative)).toClojureCode());
			s.append('\n');
			s.append((new DefineSymbol(map2ListNativeSymbol, map2ListNative)).toClojureCode());
			s.append('\n');
		} catch (AppendableException e) {
			System.err.println("Compilation error " + e.getMessage() + " occured in " + ListNative.class.getName());
		}

		return s.toString();
	}

	/**
	 * Initializes list functions in environment
	 */
	public static void initializeInEnvironment(Environment env) {
		try {
			(new DefineSymbol(isListNativeEmptySymbol, isListNativeEmpty)).interpret(env);
			(new DefineSymbol(headListNativeSymbol, headListNative)).interpret(env);
			(new DefineSymbol(tailListNativeSymbol, tailListNative)).interpret(env);
			(new DefineSymbol(mapListNativeSymbol, mapListNative)).interpret(env);
			(new DefineSymbol(map2ListNativeSymbol, map2ListNative)).interpret(env);
		} catch (AppendableException e) {
			System.err.println("Interpretation error " + e.getMessage() + " occured in " + ListNative.class.getName());
		}
	}
}
