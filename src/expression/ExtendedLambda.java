package expression;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Optional;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeTuple;
import types.TypeVariable;

/**
 * Extended lambda expression allowing for the different implementation of body
 * based on the arguments representation
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class ExtendedLambda extends Expression {

	/**
	 * Various implementations of this function
	 */
	public final Set<Lambda> implementations;

	public ExtendedLambda(Tuple args, Expression expr) {
		Set<Lambda> aux = new TreeSet<Lambda>();
		aux.add(new Lambda(args, expr));
		this.implementations = aux;
	}

	public ExtendedLambda(Tuple args, Expression defaultImplementation,
			Set<Lambda> specificImplementations) {
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
		return this;
	}

	@Override
	public Type infer() throws Exception {
		Type lastType = null;

		for (Lambda l : this.implementations) {
			Type currentType = l.infer();
			
			if(lastType == null) {
				lastType = currentType;
				continue;
			}
			
			//Maybe add some exception for the Forall type
			if(!Type.unify(lastType, currentType)) {
				throw new Exception("Types " + lastType + " and " + currentType + " in " + this + " does not unify");
			}
			
			//Is this transitive?
			lastType = currentType;
		}
		
		Type t = lastType.getRep();
		
		//Might want to add comparator into the scope...
		
		for (TypeVariable v : t.getUnconstrainedVariables()) {
			t = new ForallType(v, t);
		}

		this.setType(t);

		return t;
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
	public PriorityQueue<Lambda> getSortedImplementations(
			Comparator<? super Lambda> c) {
		PriorityQueue<Lambda> q = new PriorityQueue<Lambda>(c);
		q.addAll(this.implementations);
		return q;
	}

	/**
	 * Transforms this extended lambda to normal lambda based its implementation
	 * 
	 * @param impl
	 *            implementation to be used in tranformation
	 * @return new Lambda expression
	 * @throws Exception
	 *             if the impl is not default implementation or one of the
	 *             specific implementations
	 */
	/*public Lambda transformStaticDispatch(ImplContainer impl) throws Exception {
		if (impl.implementation == this.defaultImplementation) {
			return new Lambda(this.args, this.defaultImplementation);
		}
		if (this.implementations.contains(impl)) {
			return new Lambda(this.args, impl.implementation);
		}
		throw new Exception("Implementation " + impl.toString()
				+ " do not belong to extended lambda " + this.toString());
	}*/

	/**
	 * Transforms this extended lambda to normal lambda based its implementation
	 * 
	 * @param c
	 *            comparator used to select the implementation
	 * @return new Lambda expression
	 * @throws Exception
	 *             Exception if the impl is not default implementation or one of
	 *             the specific implementations
	 */
	public Lambda transformStaticDispatch(Comparator<? super Lambda> c)
			throws Exception {
		PriorityQueue<Lambda> queue = this.getSortedImplementations(c);
		return queue.peek();
	}

	/**
	 * Transforms this extended lambda to normal lambda based its implementation
	 * 
	 * @param argsType
	 *            Type of arguments to search the implementation
	 * @return new Lambda expression
	 * @throws Exception
	 *             Exception if the impl is not default implementation or one of
	 *             the specific implementations
	 */
	public Lambda transformStaticDispatch(TypeTuple argsType) throws Exception {
		Lambda impl = null;

		// Maybe better search in Java 8?
		for (Lambda i : this.implementations) {
			if (i.argsType == argsType) {
				impl = i;
				break;
			}
		}

		if (impl == null) {
			throw new Exception("Extended lambda " + this.toString()
					+ " has no implementation for argument types "
					+ argsType.toString());
		}

		return impl;
	}
	
	public Lambda defaultImplementation() {
		Optional<Lambda> o = this.implementations.stream().filter(new Predicate<Lambda>() {

			@Override
			public boolean test(Lambda arg0) {
				return arg0.argsType == null;
			}}).findAny();
		
		return o.get();
	}
	
	@Override
	public String toString(){
		StringBuilder sb = new StringBuilder();
		
		Lambda di = this.defaultImplementation();
		
		sb.append("ExtendedLabmbda ");
		sb.append(di.args);
		sb.append(" ");
		sb.append(di.body);
		if(this.implementations.size() > 1) {
			sb.append(" ");
		}
		
		Iterator<Lambda> i = this.implementations.iterator();
		
		while(i.hasNext()){
			Lambda c = i.next();
			if(c.argsType != null) {
				sb.append("(");
				sb.append(c.argsType);
				sb.append(" ");
				sb.append(c.body);
				sb.append(")");
				if(i.hasNext()){
					sb.append(" ");
				}
			}
		}
		
		sb.append(")");
		
		return sb.toString();
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		Set<Lambda> tmp = new TreeSet<Lambda>();
		for(Lambda l : this.implementations){
			tmp.add((Lambda)l.substituteTopLevelVariables(topLevel));
		}
		
		return new ExtendedLambda(tmp);
	}

	@Override
	public String toClojureCode() throws Exception {
		Lambda l = this.getSortedImplementations().peek(); //Comparator?
		return l.toClojureCode();
	}
}
