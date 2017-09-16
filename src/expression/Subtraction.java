package expression;

public class Subtraction extends Lambda {
	public Subtraction() {
		super(new Tuple(new Variable[]{new Variable("_x"), new Variable("_y")}), Sub.singleton);
	}
}
