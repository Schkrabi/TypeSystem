package expression;

public class Subtraction extends Lambda {
	public Subtraction() {
		super(new Variable("_x"), new Lambda(new Variable("_y"), Sub.singleton));
	}
}
