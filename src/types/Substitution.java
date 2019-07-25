package types;

import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

public class Substitution extends TreeMap<TypeVariable, Type> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -125490028169063398L;

	public Substitution compose(Substitution other) throws TypesDoesNotUnifyException {
		Set<TypeVariable> intersection = new TreeSet<TypeVariable>();
		intersection.addAll(this.keySet());
		intersection.retainAll(other.keySet());
		
		Substitution composition = new Substitution();
		composition.putAll(this);
		composition.putAll(other);
		for(TypeVariable v : intersection) {
			composition.remove(v);
		}
		
		for(TypeVariable v : intersection) {
			Optional<Substitution> mgu = Type.unify(this.get(v), other.get(v));
			if(!mgu.isPresent()) {
				throw new TypesDoesNotUnifyException(this.get(v), other.get(v));
			}
			composition.put(v, this.get(v).apply(mgu.get()));
			composition = composition.compose(mgu.get());
		}
		
		return composition;
	}
}
