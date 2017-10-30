package types;

import java.util.Set;
import java.util.TreeSet;

/**
 * Tuple of types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class TypeTuple extends Type {

	/**
	 * Values of the tuple
	 */
	public final Type[] values;

	/**
	 * Empty type tuple object
	 */
	public static final TypeTuple EMPTY_TUPLE = new TypeTuple(new Type[] {});

	public TypeTuple(Type[] values) {
		this.values = values;
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("[");
		for (int i = 0; i < this.values.length; i++) {
			s.append(this.values[i].toString());
			if (i != this.values.length - 1) {
				s.append(", ");
			}
		}
		s.append("]");
		return s.toString();
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof TypeTuple)) {
			return false;
		}
		TypeTuple other = (TypeTuple) o;
		if (this.values.length != other.values.length) {
			return false;
		}

		for (int i = 0; i < this.values.length; i++) {
			if (!this.values[i].equals(other.values[i])) {
				return false;
			}
		}
		return true;
	}

	@Override
	public Set<TypeVariable> getUnconstrainedVariables() {
		Set<TypeVariable> s = new TreeSet<TypeVariable>();
		if (this.getRep() != this) {
			s.addAll(this.getRep().getUnconstrainedVariables());
			return s;
		}

		for (int i = 0; i < this.values.length; i++) {
			s.addAll(this.values[i].getUnconstrainedVariables());
		}
		return s;
	}

	@Override
	public int compareTo(Type o) {
		if (!(o instanceof TypeTuple)) {
			return super.compareTo(o);
		}
		TypeTuple other = (TypeTuple) o;
		if (this.values.length != other.values.length) {
			return Integer.compare(this.values.length, other.values.length);
		}
		for (int i = 0; i < this.values.length; i++) {
			int cmp = this.values[i].compareTo(other.values[i]);
			if (cmp != 0) {
				return cmp;
			}
		}
		return 0;
	}
}
