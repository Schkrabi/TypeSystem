package expression;

public class Addition extends Lambda {
	
	public Addition() {
		super(new Variable("_x"), new Lambda(new Variable("_y"), Add.singleton));
	}
}
