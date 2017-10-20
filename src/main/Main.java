package main;

import expression.Addition;
import expression.Application;
import expression.Expression;
import expression.IfExpression;
import expression.IntBinary;
import expression.IntRoman;
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
		Expression expr = new Application(Subtraction.singleton, new Tuple(new Expression[]{LitInteger.initializeDefaultImplementation(4), LitInteger.initializeDefaultImplementation(5)}));
		//Expression expr = new IfExpression(LitBoolean.FALSE, new IntBinary(3), new IntBinary(2));
		
		System.out.println(expr);
		try {
			Environment env = new Environment();
			System.out.println(expr.interpret(env));
			System.out.println(expr.infer());
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
