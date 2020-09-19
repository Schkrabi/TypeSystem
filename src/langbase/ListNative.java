package langbase;

import java.util.Arrays;

import abstraction.Lambda;
import abstraction.Operator;
import application.CanDeconstructAs;
import application.Deconstruct;
import application.DefineSymbol;
import application.ExceptionExpr;
import application.IfExpression;
import expression.Symbol;
import expression.Tuple;
import literal.LitString;
import application.AbstractionApplication;
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
	 * is-list-native function
	 */
	public static final Lambda isListNativeEmpty = new Lambda(new Tuple(Arrays.asList(new Symbol("l"))),
			new TypeTuple(Arrays.asList(TypeAtom.TypeListNative)),
			new CanDeconstructAs(new Symbol("l"), TypeTuple.EMPTY_TUPLE));

	/**
	 * is-list-native symbol
	 */
	public static final Symbol isListNativeEmptySymbol = new Symbol("is-list-native-empty");

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
							new Tuple(Arrays.asList(new Deconstruct(new Symbol("l"), new TypeTuple(
									Arrays.asList(new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative))))))));
	/**
	 * head-list-native symbol
	 */
	public static final Symbol headListNativeSymbol = new Symbol("head-list-native");
	
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
							new Tuple(Arrays.asList(new Deconstruct(new Symbol("l"), new TypeTuple(
									Arrays.asList(new TypeVariable(NameGenerator.next()), TypeAtom.TypeListNative))))))));;
									
	/**
	 * tail-list-native symbol
	 */
	public static final Symbol tailListNativeSymbol = new Symbol("tail-list-native");
	
	/**
	 * Generates code for clojure regarding Native List
	 * @return
	 */
	public static String makeClojureCode() {
		StringBuilder s = new StringBuilder();

		try {
			s.append((new DefineSymbol(isListNativeEmptySymbol, isListNativeEmpty)).toClojureCode());
			s.append('\n');
			s.append((new DefineSymbol(headListNativeSymbol, headListNative)).toClojureCode());
			s.append('\n');
		} catch (AppendableException e) {
			System.err.println("Compilation error occured in " + ListNative.class.getName());
		}

		return s.toString();
	}
}
