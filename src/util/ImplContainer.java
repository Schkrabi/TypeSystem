package util;

import expression.Expression;
import types.TypeTuple;

/**
 * Untility container class for type representation tuples asociated with lambda
 * body implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ImplContainer implements Comparable<ImplContainer> {
	/**
	 * Type representation tuple
	 */
	public final TypeTuple typeSpec;
	/**
	 * Expression - body of the lambda
	 */
	public final Expression implementation;

	public ImplContainer(TypeTuple typeSpec, Expression implementation) {
		this.typeSpec = typeSpec;
		this.implementation = implementation;
	}

	@Override
	public int compareTo(ImplContainer o) {
		return this.typeSpec.compareTo(o.typeSpec);
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof ImplContainer)) {
			return false;
		}
		ImplContainer other = (ImplContainer) o;
		return typeSpec.equals(other.typeSpec);
	}

	@Override
	public String toString() {
		return "I: " + this.typeSpec.toString() + " :: " + this.implementation.toString();
	}

}
