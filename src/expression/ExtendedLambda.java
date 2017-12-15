package expression;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeSet;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.ImplContainer;

/**
 * Extended lambda expression allowing for the different implementation of body
 * based on the arguments representation
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class ExtendedLambda extends Expression {

	/**
	 * Formal arguments (names) of the lambda expression
	 */
	public final Tuple args;

	/**
	 * Basic fall back implementation of the lambda body
	 */
	public final Expression defaultImplementation;

	/**
	 * Alternative implementations of the lambda body associated with the
	 * argument representation types
	 */
	public final Set<ImplContainer> implementations;

	public ExtendedLambda(Tuple args, Expression expr) {
		this.args = args;
		this.implementations = new TreeSet<ImplContainer>();
		this.defaultImplementation = expr;
	}

	public ExtendedLambda(Tuple args, Expression defaultImplementation,
			Set<ImplContainer> specificImplementations) {
		this.args = args;
		this.implementations = new TreeSet<ImplContainer>();
		this.implementations.addAll(specificImplementations);
		this.defaultImplementation = defaultImplementation;
	}

	/**
	 * Backward compatibility constructor
	 * 
	 * @param args
	 * @param defaultImplementation
	 * @param specificImplementations
	 */
	public ExtendedLambda(Tuple args, Expression defaultImplementation,
			Map<TypeTuple, Expression> specificImplementations) {
		this.args = args;
		this.implementations = new TreeSet<ImplContainer>();
		for (Map.Entry<TypeTuple, Expression> e : specificImplementations
				.entrySet()) {
			this.implementations
					.add(new ImplContainer(e.getKey(), e.getValue()));
		}
		this.defaultImplementation = defaultImplementation;
	}

	@Deprecated
	public Expression getDefaultUmplementation() {
		return this.defaultImplementation;
	}

	@Deprecated
	public Expression getImplementation(TypeTuple prefferedType) {
		for (ImplContainer c : this.implementations) {
			if (c.typeSpec.equals(prefferedType)) {
				return c.implementation;
			}
		}
		return null;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		return this;
	}

	@Override
	public Type infer() throws Exception {
		Type argsType = this.args.infer();
		Type defImplType = this.defaultImplementation.infer();

		for (ImplContainer c : this.implementations) {
			TypeTuple targs = c.typeSpec;
			Type timpl = c.implementation.infer();

			if (!Type.unify(targs, argsType)) {
				throw new Exception(
						"Bad extended lambda specialized arguments of type "
								+ targs.getRep().toString()
								+ " do not unify with default arguments "
								+ argsType.getRep().toString());
			}

			if (!Type.unify(defImplType, timpl)) {
				throw new Exception(
						"Bad extended lambda specialized implementation "
								+ c.implementation.toString() + "of type "
								+ timpl.toString()
								+ "do not unify with default implementation "
								+ this.defaultImplementation.toString()
								+ " of type " + defImplType.toString());
			}
		}
		
		Type t;

		if(this.implementations.isEmpty()){
			t = new TypeArrow(argsType.getRep(), defImplType.getRep());
		}
		else{
			//Need to know comparator at compile time!
			ImplContainer impl = this.getSortedImplementations().peek();
			//Types should be unified from previous code
			t = new TypeArrow(impl.typeSpec.getRep(), impl.implementation.getType().getRep()); 
		}

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
	public PriorityQueue<ImplContainer> getSortedImplementations() {
		PriorityQueue<ImplContainer> q = new PriorityQueue<ImplContainer>();
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
	public PriorityQueue<ImplContainer> getSortedImplementations(
			Comparator<? super ImplContainer> c) {
		PriorityQueue<ImplContainer> q = new PriorityQueue<ImplContainer>(c);
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
	public Lambda transformStaticDispatch(ImplContainer impl) throws Exception {
		if (impl.implementation == this.defaultImplementation) {
			return new Lambda(this.args, this.defaultImplementation);
		}
		if (this.implementations.contains(impl)) {
			return new Lambda(this.args, impl.implementation);
		}
		throw new Exception("Implementation " + impl.toString()
				+ " do not belong to extended lambda " + this.toString());
	}

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
	public Lambda transformStaticDispatch(Comparator<? super ImplContainer> c)
			throws Exception {
		PriorityQueue<ImplContainer> queue = this.getSortedImplementations(c);
		return this.transformStaticDispatch(queue.peek());
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
		ImplContainer impl = null;

		// Maybe better search in Java 8?
		for (ImplContainer i : this.implementations) {
			if (i.typeSpec == argsType) {
				impl = i;
				break;
			}
		}

		if (impl == null) {
			throw new Exception("Extended lambda " + this.toString()
					+ " has no implementation for argument types "
					+ argsType.toString());
		}

		return this.transformStaticDispatch(impl);
	}
	
	@Override
	public String toString(){
		StringBuilder sb = new StringBuilder();
		
		sb.append("ExtendedLabmbda ");
		sb.append(this.args.toString());
		sb.append(" ");
		sb.append(this.defaultImplementation);
		sb.append("[");
		
		Iterator<ImplContainer> i = this.implementations.iterator();
		
		while(i.hasNext()){
			ImplContainer c = i.next();
			sb.append(c.toString());
			if(i.hasNext()){
				sb.append(", ");
			}
		}
		
		sb.append("]");
		
		return sb.toString();
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) {
		Environment e = new Environment(topLevel);
		//Mask locally redefined variables
		for(Expression expr : this.args){
			e.put((Variable) expr, expr);
		}
		
		Set<ImplContainer> s = new TreeSet<ImplContainer>();
		for(ImplContainer i : this.implementations){
			s.add(new ImplContainer(i.typeSpec, i.implementation.substituteTopLevelVariables(e)));
		}
		
		return new ExtendedLambda(this.args, this.defaultImplementation.substituteTopLevelVariables(e), s);
	}

	@Override
	public String toClojureCode() throws Exception {
		if(this.implementations.isEmpty()){
			return (new Lambda(this.args, this.defaultImplementation)).toClojureCode();
		}
		ImplContainer impl = this.getSortedImplementations().peek();//Comparator here?
		Lambda l = new Lambda(this.args, impl.implementation);
		l.infer();
		return l.toClojureCode();
	}
}
