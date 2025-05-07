package velka.core.abstraction;

import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeVariable;
import velka.types.typeSystem.VelkaAbstraction;

import java.util.ArrayList;
import java.util.Collection;
import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.util.AppendableException;
import velka.util.IImplementationRanker;
import velka.util.Pair;

/**
 * Expression for interpreted function with various implementations
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExtendedFunction extends Expression implements VelkaAbstraction {
	private final Collection<Pair<? extends VelkaAbstraction, ? extends IImplementationRanker>> implementations;
	private final Environment env;
	
	public ExtendedFunction(Environment env) {
		this.implementations = new ArrayList<Pair<? extends VelkaAbstraction, ? extends IImplementationRanker>>();
		this.env = env;
	}
	
	public ExtendedFunction extend(Function function, IImplementationRanker cost) {
		var ef = new ExtendedFunction(this.env);
		ef.implementations.addAll(this.implementations);
		ef.implementations.add(Pair.of(function, cost));
		return ef;
	}

	@Override
	public Type getType() {
		if(this.implementations.isEmpty()) {
			return new TypeArrow(TypeVariable.generate(), TypeVariable.generate());
		}
		
		return RepresentationOr.factory(this.implementations.stream().map(x -> x.first.getType()).toList());
	}

	@Override
	public Object apply(Collection<? extends Object> arg) {
		var selector = this.env.getTypeSystem().getImplementationSelector();
		selector.setEnvironment(this.env);
		var impl = selector.selectImplementation(
				this.implementations, 
				arg);
		
		return impl.evaluate(arg, null);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return this;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return Pair.of(this.getType(), Substitution.EMPTY);
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env) throws AppendableException {
		throw new RuntimeException("doConvert not implemented");
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		throw new RuntimeException("toClojureCode not implemented");
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof ExtendedFunction ef) {
			return this.env.equals(ef.env)
					&& this.implementations.equals(ef.implementations);
		}
		return false;
	}
}
