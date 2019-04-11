package types;

import java.util.Set;

import util.AppendableException;

public class TypeSetDoesNotUnifyException extends AppendableException {
	/**
	 * 
	 */
	private static final long serialVersionUID = -9168569362545986567L;
	public final Set<Type> types;
	
	public TypeSetDoesNotUnifyException(Set<Type> types) {
		super("Set of types " + types.toString() + " does not unify.");
		this.types = types;
	}
}
