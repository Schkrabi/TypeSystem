package expression;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Optional;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;

import interpretation.Environment;
import types.Substitution;
import types.Type;
import util.AppendableException;
import util.Pair;

/**
 * Extended lambda expression allowing for the different implementation of body
 * based on the arguments representation
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class ExtendedLambda extends MetaLambda {

	/**
	 * Various implementations of this function
	 */
	public final Set<Lambda> implementations;

	public ExtendedLambda(Tuple args, Expression expr) {
		Set<Lambda> aux = new TreeSet<Lambda>();
		aux.add(new Lambda(args, expr));
		this.implementations = aux;
	}

	public ExtendedLambda(Tuple args, Expression defaultImplementation, Set<Lambda> specificImplementations) {
		Set<Lambda> aux = new TreeSet<Lambda>();
		aux.add(new Lambda(args, defaultImplementation));
		aux.addAll(specificImplementations);
		this.implementations = aux;
	}

	public ExtendedLambda(Set<Lambda> implementations) {
		this.implementations = implementations;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Set<Function> s = new TreeSet<Function>();

		for (Lambda l : this.implementations) {
			Function f = (Function) l.interpret(env);
			s.add(f);
		}

		ExtendedFunction ef = new ExtendedFunction(s, env);
		ef.infer(env);
		return ef;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			//TODO implement
			throw new AppendableException("Not implemented!");
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	/**
	 * Returns the priority queue with natural ordering of the optimized
	 * implementations
	 * 
	 * @return priority queue of ImplContainers
	 */
	public PriorityQueue<Lambda> getSortedImplementations() {
		PriorityQueue<Lambda> q = new PriorityQueue<Lambda>();
		q.addAll(this.implementations);
		return q;
	}

	/**
	 * Returns the priority queue with ordering of the optimized implementations
	 * given by comparator
	 * 
	 * @param c
	 *            comparator determining the ordering of the implementations
	 * @return priority queue of ImplContainers
	 */
	public PriorityQueue<Lambda> getSortedImplementations(Comparator<? super Lambda> c) {
		PriorityQueue<Lambda> q = new PriorityQueue<Lambda>(c);
		q.addAll(this.implementations);
		return q;
	}

	public Lambda defaultImplementation() {
		Optional<Lambda> o = this.implementations.stream().filter(new Predicate<Lambda>() {

			@Override
			public boolean test(Lambda arg0) {
				return arg0.argsType == null;
			}
		}).findAny();

		return o.get();
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();

		Lambda di = this.defaultImplementation();

		sb.append("ExtendedLabmbda ");
		sb.append(di.args);
		sb.append(" ");
		sb.append(di.body);
		if (this.implementations.size() > 1) {
			sb.append(" ");
		}

		Iterator<Lambda> i = this.implementations.iterator();

		while (i.hasNext()) {
			Lambda c = i.next();
			if (c.argsType != null) {
				sb.append("(");
				sb.append(c.argsType);
				sb.append(" ");
				sb.append(c.body);
				sb.append(")");
				if (i.hasNext()) {
					sb.append(" ");
				}
			}
		}

		sb.append(")");

		return sb.toString();
	}

	@Override
	public String toClojureCode() throws Exception {
		Lambda l = this.getSortedImplementations().peek(); // Comparator?
		return l.toClojureCode();
	}

	@Override
	public Lambda getLambda(Comparator<? super Lambda> c) {
		PriorityQueue<Lambda> queue = this.getSortedImplementations(c);
		return queue.peek();
	}

	@Override
	public Lambda getLambda() {
		return this.defaultImplementation();
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof ExtendedLambda) {
			ExtendedLambda o = (ExtendedLambda)other;
			
			int c = (int)Math.signum(this.implementations.size() - o.implementations.size());
			if(c != 0)
				return c;
			
			Set<Lambda> tmp = new TreeSet<Lambda>();
			tmp.addAll(this.implementations);
			tmp.addAll(o.implementations);
			
			if(tmp.size() == this.implementations.size())
				return 0;
			
			Set<Lambda> thisSubOther = new TreeSet<Lambda>();
			thisSubOther.addAll(tmp);
			thisSubOther.removeAll(o.implementations);
			
			Set<Lambda> otherSubThis = tmp;
			otherSubThis.removeAll(this.implementations);
			
			Iterator<Lambda> i = thisSubOther.iterator();
			Iterator<Lambda> j = otherSubThis.iterator();
			
			while(i.hasNext() && j.hasNext()) {
				Lambda f = i.next();
				Lambda g = j.next();
				c = f.compareTo(g);
				if(c != 0)
					return c;
			}
			
			return 0;
		}
		return super.compareTo(other);
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof ExtendedLambda) {
			return this.implementations.equals(((ExtendedLambda) other).implementations);
		}
		return false;
	}
}
