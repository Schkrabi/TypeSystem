package main;

import java.io.Reader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.TreeMap;

import org.antlr.runtime.tree.CommonTree;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import parser.ExtendedTypeSystemListener;
import parser.SchemeBaseListener;
import parser.SchemeLexer;
import parser.SchemeListener;
import parser.SchemeParser;
import parser.SchemeParser.ExprsContext;

import types.Type;
import types.TypeConcrete;
import types.TypeRepresentation;
import types.TypeTuple;
import expression.Addition;
import expression.Application;
import expression.Expression;
import expression.ExtendedLambda;
import expression.IfExpression;
import expression.IntBinary;
import expression.IntRoman;
import expression.IntString;
import expression.Lambda;
import expression.LitBoolean;
import expression.LitInteger;
import expression.Sequence;
import expression.Subtraction;
import expression.Tuple;
import expression.Variable;
import interpretation.Environment;

/**
 * Main entry point for testing
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Main {

	/**
	 * Main entrypoint for testing
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		Scanner input = new Scanner(System.in);
		Environment topLevel = Main.initTopLevelEnvironment();
		
		System.out.println("Parser interactive test");
		try {
			while (true) {
				CharStream charStream = new ANTLRInputStream(input.nextLine());
				TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
				SchemeParser parser = new SchemeParser(tokens);

				ExprsContext exprsContext = parser.exprs();

				for (Expression e : exprsContext.val) {
					Expression expr = e.substituteTopLevelVariables(topLevel);
					System.out.println(expr);
					Type t = expr.infer();
					System.out.println(t.getRep());
					System.out.println(expr.interpret(topLevel));
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			input.close();
		}
	}

	/**
	 * Old entrypoint
	 * 
	 * @param args
	 */
	public static void main_legacy(String[] args) {
		// Expression expr = new Application(Addition.singleton, new Tuple(new
		// Expression[]{LitInteger.initializeDefaultImplementation(4),
		// LitInteger.initializeDefaultImplementation(5)}));
		// Expression expr = new Application(Subtraction.singleton, new
		// Tuple(new Expression[]{LitInteger.initializeDefaultImplementation(4),
		// LitInteger.initializeDefaultImplementation(5)}));
		// Expression expr = new IfExpression(LitBoolean.FALSE, new
		// IntBinary(3), new IntBinary(2));

		Expression romanId = new Expression() {

			@Override
			public Expression interpret(Environment env) throws Exception {
				Expression e = env.get(new Variable("x")).interpret(env);
				System.out.println("Roman Implementation");
				return e;
			}

			@Override
			public Type infer() throws Exception {
				return TypeRepresentation.TypeIntRoman;
			}

			@Override
			public Expression substituteTopLevelVariables(Environment topLevel) {
				return this;
			}

			@Override
			public String toClojureCode() throws Exception {
				return "(fn [x] x)";
			}

		};

		Expression binId = new Expression() {

			@Override
			public Expression interpret(Environment env) throws Exception {
				IntBinary e = (IntBinary) env.get(new Variable("x")).interpret(env);
				System.out.println("Binary Implementation");
				return new IntBinary(e.value + 5);
			}

			@Override
			public Type infer() throws Exception {
				return TypeConcrete.TypeInt;
			}

			@Override
			public Expression substituteTopLevelVariables(Environment topLevel) {
				return this;
			}
			
			@Override
			public String toClojureCode() throws Exception {
				return "(fn [x] x)";
			}

		};

		Map<TypeTuple, Expression> impls = new TreeMap<TypeTuple, Expression>();
		impls.put(new TypeTuple(new Type[] { TypeRepresentation.TypeIntRoman }), romanId);

		ExtendedLambda elambda = new ExtendedLambda(new Tuple(new Variable[] { new Variable("x") }), binId, impls);

		// Expression expr = new Application(elambda, new Tuple(new
		// Expression[]{LitInteger.initializeDefaultImplementation(1024)}));
		// Expression expr = new Application(elambda, new Tuple(new
		// Expression[]{new IntRoman("MCM")}));
		Expression expr = new Application(elambda, new Tuple(new Expression[] { new IntString("2048") }));

		System.out.println(expr);
		try {
			System.out.println(expr.infer());
			Environment env = new Environment();
			System.out.println(expr.interpret(env));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private static Environment initTopLevelEnvironment() {
		Environment env = new Environment();
		env.put(new Variable("+"), Addition.singleton);
		env.put(new Variable("-"), Subtraction.singleton);
		
		return env;
	}
}
