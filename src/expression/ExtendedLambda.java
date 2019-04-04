package expression;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Optional;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeVariable;
import util.AppendableException;

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
		Set<Function> s = new TreeSet<Function>();
		
		for(Lambda l : this.implementations){
			Function f = (Function)l.interpret(env);
			s.add(f);
		}
		
		ExtendedFunction ef = new ExtendedFunction(s, env);
		ef.infer(env);
		return ef;
	}
	
	/**
	 * Cannot do this through functional lambdas because throws Exception
	 * @param env environment in which inference is carried out
	 * @param exprs set of expressions for which types are unfered
	 * @return set of infered types
	 * @throws Exception
	 */
	private static Set<Type> inferSet(Environment env, Set<Expression> exprs) throws Exception{
		Set<Type> s = new TreeSet<Type>();
		
		for(Expression e : exprs) {
			s.add(e.infer(env));
		}
		return s;
	}

	@Override
	public Type infer(Environment env) throws AppendableException {
		Set<Type> ltypes = this.implementations.stream().map(x -> x.argsType).collect(Collectors.toSet());
		ltypes = ltypes.stream().filter(x -> x != null).collect(Collectors.toSet());
		Set<Type> rtypes;
		try {
			rtypes = ExtendedLambda.inferSet(env, this.implementations.stream().map(x -> x.body).collect(Collectors.toSet()));
		} catch (Exception e) {
			throw new AppendableException("Not implemented");
		}
		
		if(!Type.unifyMany(ltypes)){
			//throw new Exception("Argument types " + ltypes.toString() +  " of extended lambda " + this.toString() + " does not unify!");
			//TODO
		}
		if(!Type.unifyMany(rtypes)) {
			//throw new Exception("Body types " + rtypes.toString() + " of extended lambda " + this.toString() + " does not unify!");
			//TODO
		}
		
		Optional<Type> ol = ltypes.stream().findAny();
		Optional<Type> or = rtypes.stream().findAny();
		
		if(!ol.isPresent() || !or.isPresent()) {
			//throw new Exception("Mallformed extended lambda? " + this.toString());
			//TODO
		}
		
		Type t = new TypeArrow(ol.get(), or.get());
		
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

	@Override
	public Lambda getLambda(Comparator<? super Lambda> c) {
		PriorityQueue<Lambda> queue = this.getSortedImplementations(c);
		return queue.peek();
	}

	@Override
	public Lambda getLambda() {
		return this.defaultImplementation();
	}
}
