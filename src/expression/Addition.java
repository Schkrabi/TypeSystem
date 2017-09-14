package expression;

public class Addition extends Application {
	
	public Addition(Expression larg, Expression rarg) {
		super(new Lambda(new Variable("_y"), new Application(new Lambda(new Variable("_x"), Add.singleton), larg)), rarg);
	}
}
