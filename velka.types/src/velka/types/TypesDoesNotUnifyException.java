package velka.types;

import velka.util.AppendableException;

public class TypesDoesNotUnifyException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = 2740517979949832018L;

	public final Type first;
	public final Type second;

	public TypesDoesNotUnifyException(Type first, Type second) {
		super("Types " + first.toString() + " and " + second.toString() + " does not unify");
		this.first = first;
		this.second = second;
	}

}
