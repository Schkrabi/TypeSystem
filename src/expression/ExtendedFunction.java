package expression;

import interpretation.Environment;

import java.util.Comparator;
import java.util.Optional;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.function.Predicate;

import types.ForallType;
import types.Type;
import types.TypeVariable;

/**
 * Expression for interpreted function with various implementations
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExtendedFunction extends MetaFunction {
	
	/**
	 * Implementations of the function
	 */
	public final Set<Function> implementations;
	
	public ExtendedFunction(Set<Function> implementations, Environment createdEnvironment){
		super(createdEnvironment);
		this.implementations = implementations;
	}

	@Override
	public Function getFunction() {
		return this.defaultImplementation();
	}

	@Override
	public Function getFunction(Comparator<? super Function> c) {
		return this.getSortedImplementations(c).peek();
	}

	@Override
	public Type infer(Environment env) throws Exception {
		Type lastType = null;

		for (Function f : this.implementations) {
			Type currentType = f.infer(env);
			
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
	
	public PriorityQueue<Function> getSortedImplementations(
			Comparator<? super Function> c) {
		PriorityQueue<Function> q = new PriorityQueue<Function>(c);
		q.addAll(this.implementations);
		return q;
	}
	
	public Function defaultImplementation() {
		Optional<Function> o = this.implementations.stream().filter(new Predicate<Function>() {

			@Override
			public boolean test(Function arg0) {
				return arg0.argsType == null;
			}}).findAny();
		
		return o.get();
	}

}
