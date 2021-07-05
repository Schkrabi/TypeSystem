package velka.types;

import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import velka.types.Type;
import velka.util.AppendableException;

/**
 * Exception thrown when set of types cannot be unified
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeSetDoesNotUnifyException extends AppendableException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -7938135357323862971L;
	
	/**
	 * Types that does not unify
	 */
	private Set<Type> types;
	
	public TypeSetDoesNotUnifyException(Collection<? extends Type> types) {
		super("Set of types cannot be unified: " + types.toString());
		types = new TreeSet<Type>(types);
	}
	
	/**
	 * Gets types that does not unify
	 * @return
	 */
	public Set<Type> getTypes() {
		return new TreeSet<Type>(this.types);
	}
}
