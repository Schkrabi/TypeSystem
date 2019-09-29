package types;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Set;

import expression.Expression;
import interpretation.Environment;
import util.AppendableException;
import util.Pair;

/**
 * Abstract class for types
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Type implements Comparable<Type> {

	/**
	 * Returns set of all the variables in the type expression that are not
	 * constrained by quantifier
	 * 
	 * @return
	 */
	public abstract Set<TypeVariable> getUnconstrainedVariables();

	/**
	 * Applies substitution to this type
	 * 
	 * @param s
	 *            applied substitution
	 * @return new Type with applied substitution
	 * @throws TypeVariableNotSubstitutedException 
	 * @throws AppendableException 
	 */
	public abstract Type apply(Substitution s);
	
	/**
	 * Creates expression that converts expr in different type (if possible)
	 * 
	 * @param expr
	 *            Expression to be converted
	 * @param toType
	 *            target type
	 * @return a new expression that will interpret/infer into a targeted type
	 * @throws Exception
	 */
	public abstract Expression convertTo(Expression expr, Type toType) throws AppendableException;

	/**
	 * Replaces all type Representations in this type with its basic types
	 * 
	 * @return new Type with representations removed
	 */
	public abstract Type removeRepresentationInfo();
	
	@Override
	public int compareTo(Type other) {
		return this.getClass().getName().compareTo(other.getClass().getName());
	}

	/**
	 * Returns unified type if the expression if two types unifies, otherwise
	 * throws
	 * 
	 * @param m
	 *            first unified type
	 * @param n
	 *            second unified type
	 * @return MGU of given types
	 * @throws AppendableException if types are not unifiable
	 */
	public static Substitution unify(Type m, Type n) throws AppendableException {
		if (m instanceof TypeAtom && n instanceof TypeAtom) {
			TypeAtom mc = (TypeAtom) m;
			TypeAtom nc = (TypeAtom) n;

			if (TypeAtom.isSameBasicType(mc, nc)) {
				return Substitution.EMPTY;
			}
		} else if (m instanceof TypeArrow && n instanceof TypeArrow) {
			TypeArrow ma = (TypeArrow) m;
			TypeArrow na = (TypeArrow) n;

			Substitution left = Type.unify(ma.ltype, na.ltype);
			Substitution right = Type.unify(ma.rtype, na.rtype);
			
			return left.union(right);
		} else if (m instanceof TypeVariable) {
			return new Substitution(Arrays.asList(new Pair<TypeVariable, Type>((TypeVariable)m, n)));
		} else if (n instanceof TypeVariable) {
			return new Substitution(Arrays.asList(new Pair<TypeVariable, Type>((TypeVariable)n, m)));
		} else if (m instanceof TypeTuple && n instanceof TypeTuple) {
			TypeTuple mt = (TypeTuple) m;
			TypeTuple nt = (TypeTuple) n;

			if (mt.size() == nt.size()) {
				Substitution s = Substitution.EMPTY;

				Iterator<Type> i = mt.iterator();
				Iterator<Type> j = nt.iterator();
				while (i.hasNext()) {
					Type t = i.next();
					Type u = j.next();

					Substitution ot = Type.unify(t, u);

					s = s.union(ot);
				}
				return s;
			}
		}

		throw new TypesDoesNotUnifyException(m, n);
	}
}
