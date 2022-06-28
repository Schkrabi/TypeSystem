package velka.types;

import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;

import velka.util.AppendableException;

public abstract class TerminalType extends Type {

	@Override
	public Type map(Function<Type, Type> fun) throws AppendableException {
		return fun.apply(this);
	}

	@Override
	public <R> R reduce(Function<TerminalType, R> mapFun, BinaryOperator<R> combiner, R terminator)
			throws AppendableException {
		return mapFun.apply(this);
	}
	
	@Override
	protected <R> R doMap2AndReduce(
			Type other,
			BiFunction<Type, Type, R> mapFun,
			BinaryOperator<R> combiner,
			R terminator) throws AppendableException {
		return mapFun.apply(this, other);
	}

}
