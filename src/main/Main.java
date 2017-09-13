package main;

import expression.Addition;
import expression.Application;
import expression.Expression;
import expression.LitInteger;
import interpretation.Environment;

public class Main {

	public static void main(String[] args) {
		Expression expr = new Application(new Application(new Addition(), new LitInteger(4)), new LitInteger(5));
		
		System.out.println(expr);
		try {
			System.out.println(expr.interpret(new Environment()));
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
