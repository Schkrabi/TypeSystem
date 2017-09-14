package expression;

public class Subtraction extends Application {
	public Subtraction(Expression larg, Expression rarg) {
		super(new Lambda(new Variable("_y"), new Application(new Lambda(new Variable("_x"), Sub.singleton), larg)), rarg);
	}
}
