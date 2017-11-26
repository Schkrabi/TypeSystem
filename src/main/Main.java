package main;

import java.io.Reader;
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
		System.out.println("Parser interactive test");
		try {
			while(true){
				CharStream charStream = new ANTLRInputStream(input.nextLine());	
				TokenStream tokens = new CommonTokenStream(new SchemeLexer(charStream));
				SchemeParser parser = new SchemeParser(tokens);
				
				ExprsContext exprsContext = parser.exprs();
				
				/*ParseTreeWalker walker = new ParseTreeWalker();
				
				SchemeListener listener = new SchemeBaseListener();
				walker.walk(listener, exprsContext);*/
				
				for(Expression e : exprsContext.val){
					System.out.println(e);
					System.out.println(e.infer());
					System.out.println(e.interpret(new Environment()));
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
				return TypeConcrete.TypeIntRoman;
			}

		};

		Expression binId = new Expression() {

			@Override
			public Expression interpret(Environment env) throws Exception {
				IntBinary e = (IntBinary) env.get(new Variable("x")).interpret(
						env);
				System.out.println("Binary Implementation");
				return new IntBinary(e.value + 5);
			}

			@Override
			public Type infer() throws Exception {
				return TypeConcrete.TypeIntBinary;
			}

		};

		Map<TypeTuple, Expression> impls = new TreeMap<TypeTuple, Expression>();
		impls.put(new TypeTuple(new Type[] { TypeConcrete.TypeIntRoman }),
				romanId);

		ExtendedLambda elambda = new ExtendedLambda(new Tuple(
				new Variable[] { new Variable("x") }), binId, impls);

		// Expression expr = new Application(elambda, new Tuple(new
		// Expression[]{LitInteger.initializeDefaultImplementation(1024)}));
		// Expression expr = new Application(elambda, new Tuple(new
		// Expression[]{new IntRoman("MCM")}));
		Expression expr = new Application(elambda, new Tuple(
				new Expression[] { new IntString("2048") }));

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

}
