package expression;

import interpretation.Environment;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.stream.Collectors;

import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;
import util.ThrowingBinaryOperator;
import util.ThrowingFunction;

/**
 * Expression for interpreted function with various implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExtendedFunction extends MetaFunction {

	/**
	 * Implementations of the function
	 */
	private final Set<Function> implementations;

	/**
	 * Type of arguments
	 */
	public final TypeTuple argsType;

	public ExtendedFunction(TypeTuple argsType, Collection<Function> implementations, Environment createdEnvironment) {
		super(createdEnvironment);
		this.argsType = argsType;
		this.implementations = implementations.stream()
				.map(x -> new Function(x.argsType, x.args, x.body, createdEnvironment)).collect(Collectors.toSet());
	}

	@Override
	public Function getFunction(Comparator<? super Function> c) {
		return this.getSortedImplementations(c).peek();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		final Environment crEnv = this.creationEnvironment;
		final TypeVariable v = new TypeVariable(NameGenerator.next());
		try {
			try {
				Pair<Type, Substitution> p = this.implementations.stream()
						.map(ThrowingFunction.wrapper(x -> x.infer(crEnv)))
						.reduce(new Pair<Type, Substitution>(v, new Substitution()),
								ThrowingBinaryOperator.wrapper((x, y) -> {
									Substitution s = Type.unify(x.first, y.first).get();
									s = s.compose(x.second).compose(y.second);
									return new Pair<Type, Substitution>(v.apply(s), s);
								}));

				return new Pair<Type, Substitution>(p.first.removeRepresentationInfo(),
						p.second.compose(Type.unify(((TypeArrow) (p.first)).ltype, this.argsType).get()));
			} catch (RuntimeException e) {
				AppendableException ae = (AppendableException) e.getCause();
				throw ae;
			}
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	public PriorityQueue<Function> getSortedImplementations(Comparator<? super Function> c) {
		PriorityQueue<Function> q = new PriorityQueue<Function>(c);
		q.addAll(this.implementations);
		return q;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof ExtendedFunction) {
			int cmp = this.argsType.compareTo(((ExtendedFunction) other).argsType);
			if (cmp != 0)
				return cmp;
			cmp = this.creationEnvironment.compareTo(((ExtendedFunction) other).creationEnvironment);
			if (cmp != 0)
				return cmp;

			for (Function f : this.implementations) {				
				if (!((ExtendedFunction) other).implementations.contains(f)) {
					return 1;
				}
			}
			for (Function f : ((ExtendedFunction) other).implementations) {
				if (!this.implementations.contains(f)) {
					return -1;
				}
			}
			return 0;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ExtendedFunction) {
			return this.argsType.equals(((ExtendedFunction) other).argsType)
					&& this.implementations.equals(((ExtendedFunction) other).implementations);
		}
		return false;
	}
	
	@Override
	public String toString() {
		StringBuilder s = new StringBuilder("(ExFunctionInternal (");
		Function sample = this.implementations.stream().findAny().get();

		// Arguments
		Iterator<Expression> i = sample.args.iterator();
		Iterator<Type> j = this.argsType.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			Type t = j.next();
			s.append('(');
			s.append(t.toString());
			s.append(' ');
			s.append(e.toString());
			s.append(')');
		}
		s.append(") ");

		Iterator<Function> k = this.implementations.iterator();
		while(k.hasNext()) {
			Function f = k.next();
			s.append('(');
			s.append(f.argsType.toString().replace('[', '(').replace(']', ')'));
			s.append(' ');
			s.append(f.body.toString());
			s.append(')');
			if(k.hasNext()) {
				s.append(' ');
			}
		}

		s.append(')');

		return s.toString();
	}
	
	@Override
	public int hashCode() {
		return super.hashCode() * this.argsType.hashCode() * this.implementations.hashCode();
	}
}
