package velka.lang.abstraction;

import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;

import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.types.RepresentationOr;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeTuple;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;
import velka.lang.util.ThrowingBinaryOperator;

/**
 * Expression for interpreted function with various implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExtendedFunction extends ExtendedLambda {

	public final Environment creationEnvironment;

	private ExtendedFunction(Collection<Function> implementations, Environment createdEnvironment) {
		super(implementations);
		creationEnvironment = createdEnvironment;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			/*Set<Pair<Type, Substitution>> s = this.implementations.stream()
					.map(ThrowingFunction.wrapper(x -> x.infer(env))).collect(Collectors.toSet());
			*/
			Set<Pair<Type, Substitution>> s = new HashSet<Pair<Type, Substitution>>();
			for(Expression e : this.implementations) {
				Pair<Type, Substitution> p = e.infer(env, typeEnv);
				s.add(p);
			}
			
			return new Pair<Type, Substitution>(
					RepresentationOr.makeRepresentationOr(s.stream().map(x -> x.first).collect(Collectors.toSet())),
					s.stream().map(x -> x.second).reduce(Substitution.EMPTY,
							ThrowingBinaryOperator.wrapper((x, y) -> x.union(y))));

		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	public PriorityQueue<Lambda> getSortedImplementations(Comparator<? super Lambda> c) {
		PriorityQueue<Lambda> q = new PriorityQueue<Lambda>(c);
		q.addAll(this.implementations);
		return q;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof ExtendedFunction) {
			int cmp = this.creationEnvironment.compareTo(((ExtendedFunction) other).creationEnvironment);
			if (cmp != 0)
				return cmp;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ExtendedFunction) {
			boolean envEquals = this.creationEnvironment.equals(((ExtendedFunction) other).creationEnvironment);
			
			if(!envEquals) {
				return false;
			}
			
			//Have to compare implementation by implementation to get unification of argtypes
			for(Lambda l : this.implementations) {
				boolean found = false;
				for(Lambda k : ((ExtendedFunction) other).implementations) {
					if(k.equals(l)) {
						found = true;
						break;
					}
				}
				if(!found) {
					return false;
				}
			}
			
			for(Lambda k : ((ExtendedFunction) other).implementations) {
				boolean found = false;
				for(Lambda l : this.implementations) {
					if(l.equals(k)) {
						found = true;
						break;
					}
				}
				if(!found) {
					return false;
				}
			}
			
			return true;
		}
		return false;
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(ExFunctionInternal (");

		// Arguments
		Iterator<? extends Lambda> k = this.implementations.iterator();
		while (k.hasNext()) {
			Lambda f = k.next();
			s.append('(');
			s.append(f.argsType.toString().replace('[', '(').replace(']', ')'));
			s.append(' ');
			s.append(f.body.toString());
			s.append(')');
			if (k.hasNext()) {
				s.append(' ');
			}
		}

		s.append("))");

		return s.toString();
	}

	@Override
	public int hashCode() {
		return super.hashCode() * this.implementations.hashCode();
	}

	/**
	 * Creates new extended function
	 * 
	 * @param implementations    function implementations
	 * @param createdEnvironment environment where function was created
	 * @return new ExtendedFunction object
	 * @throws AppendableException thrown if argument types of function does not
	 *                             unify
	 */
	public static ExtendedFunction makeExtendedFunction(Collection<Function> implementations,
			Environment createdEnvironment) throws AppendableException {
		Type.unifyMany(implementations.stream().map(x -> x.argsType).collect(Collectors.toSet()));
		return new ExtendedFunction(implementations, createdEnvironment);
	}
	
	@Override
	protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {	
		TypeTuple argsType = (TypeTuple)args.infer(env, typeEnv).first;
		Lambda l = this.getMostFitLambda(argsType);
		return l.doSubstituteAndEvaluate(args, env, typeEnv);
	}
	
	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) {
		return this;
	}
}