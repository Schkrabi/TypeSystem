package types;

import java.util.List;
import java.util.Set;
import java.util.Arrays;

/**
 * Abstract class for types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Type implements Comparable<Type> {
	/**
	 * Type representative
	 */
	private Type rep;

	/**
	 * Ordering of subclasses for Comparable interface
	 */
	private static List<Class<? extends Type>> ordering = Arrays.asList(TypeConcrete.class, TypeRepresentation.class, TypeVariable.class,
			ForallType.class, TypeArrow.class, TypeTuple.class);

	public Type() {
		this.rep = this;
	}

	/**
	 * Returns set of all the variables in the type expression that are not
	 * constrained by quantifier
	 * 
	 * @return
	 */
	public abstract Set<TypeVariable> getUnconstrainedVariables();

	/**
	 * Returns true if this is an applicable type, otherwise returns false
	 * 
	 * @return true or false
	 */
	public boolean isApplicableType() {
		return false;
	}

	/**
	 * Returns true if two types unifies, otherwise returns false.
	 * 
	 * @warning Might change the type representatives if unification is succesfull
	 * @param m
	 *            Type
	 * @param n
	 *            Type
	 * @return true or false
	 */
	public static boolean unify(Type m, Type n) {
		Type s = m.getRep();
		Type t = n.getRep();

		if (s == t) {
			return true;
		} else if (t instanceof TypeConcrete && s instanceof TypeConcrete) {
			return ((TypeConcrete)s).isSameBasicType((TypeConcrete)t);
		} else if (s instanceof TypeArrow && t instanceof TypeArrow) {
			Type.union(s, t);
			TypeArrow as = (TypeArrow) s;
			TypeArrow at = (TypeArrow) t;
			return Type.unify(as.ltype, at.ltype) && Type.unify(as.rtype, at.rtype);
		} else if (s instanceof TypeVariable || t instanceof TypeVariable) {
			Type.union(s, t);
			return true;
		} else if (s instanceof TypeTuple && t instanceof TypeTuple) {
			TypeTuple ts = (TypeTuple) s;
			TypeTuple tt = (TypeTuple) t;

			if (ts.values.length != tt.values.length) {
				return false;
			}

			for (int i = 0; i < ts.values.length; i++) {
				if (!Type.unify(ts.values[i], tt.values[i])) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/**
	 * Gets the representative of this type
	 * 
	 * @return Type
	 */
	public Type getRep() {
		if (this.rep == this) {
			return this.rep;
		}
		return this.rep.getRep();
	}

	@Override
	public int compareTo(Type other) {
		return (int) Math.signum(ordering.indexOf(this.getClass()) - ordering.indexOf(other.getClass()));
	}

	/**
	 * Unions the representatives of two types
	 * 
	 * @param t1
	 *            Type
	 * @param t2
	 *            Type
	 */
	private static void union(Type t1, Type t2) {
		if (t1 instanceof TypeConcrete) {
			t2.rep = t1.rep;
			return;
		}
		if (t2 instanceof TypeConcrete) {
			t1.rep = t2.rep;
		}
		t2.rep = t1.rep;
		return;
	}
}
