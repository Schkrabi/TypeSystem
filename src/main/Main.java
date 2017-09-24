package main;

import expression.Add;
import expression.Addition;
import expression.Application;
import expression.Expression;
import expression.IfExpression;
import expression.Lambda;
import expression.LitBoolean;
import expression.LitInteger;
import expression.Subtraction;
import expression.Tuple;
import expression.Variable;
import interpretation.Environment;

public class Main {

	public static void main(String[] args) {
		//Expression expr = new Application(Addition.singleton, new Tuple(new Expression[]{new LitInteger(4), new LitInteger(5)}));
		//Expression expr = new Application(Subtraction.singleton, new Tuple(new Expression[]{new LitInteger(4), new LitInteger(5)}));
		//Expression expr = new Application(new Lambda(new Variable("_y"), new Application(new Lambda(new Variable("_x"), Add.singleton), new LitInteger(4))), new LitInteger(5));
		Expression expr = new IfExpression(LitBoolean.FALSE, new LitInteger(1), new LitInteger(2));
		
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
