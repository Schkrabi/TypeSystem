package expression;

public class Addition extends Lambda {
	
	public Addition() {
		super(new Tuple(new Variable[]{new Variable("_x"), new Variable("_y")}), Add.singleton);
	}
}
