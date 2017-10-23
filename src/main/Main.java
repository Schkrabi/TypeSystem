package main;

import java.util.Map;
import java.util.TreeMap;

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

public class Main {

	public static void main(String[] args) {
		//Expression expr = new Application(Addition.singleton, new Tuple(new Expression[]{LitInteger.initializeDefaultImplementation(4), LitInteger.initializeDefaultImplementation(5)}));
		//Expression expr = new Application(Subtraction.singleton, new Tuple(new Expression[]{LitInteger.initializeDefaultImplementation(4), LitInteger.initializeDefaultImplementation(5)}));
		//Expression expr = new IfExpression(LitBoolean.FALSE, new IntBinary(3), new IntBinary(2));
		
		Expression romanId = new Expression(){

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
		
		Expression binId = new Expression(){

			@Override
			public Expression interpret(Environment env) throws Exception {
				IntBinary e = (IntBinary)env.get(new Variable("x")).interpret(env);
				System.out.println("Binary Implementation");
				return new IntBinary(e.value + 5);
			}

			@Override
			public Type infer() throws Exception {
				return TypeConcrete.TypeIntBinary;
			}
			
		};
		
		Map<TypeTuple, Expression> impls = new TreeMap<TypeTuple, Expression>();
		impls.put(new TypeTuple(new Type[]{TypeConcrete.TypeIntRoman}), romanId);
		
		ExtendedLambda elambda = new ExtendedLambda(new Tuple(new Variable[]{new Variable("x")}), binId, impls);
		
		//Expression expr = new Application(elambda, new Tuple(new Expression[]{LitInteger.initializeDefaultImplementation(1024)}));
		//Expression expr = new Application(elambda, new Tuple(new Expression[]{new IntRoman("MCM")}));
		Expression expr = new Application(elambda, new Tuple(new Expression[] {new IntString("2048")}));
		
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
