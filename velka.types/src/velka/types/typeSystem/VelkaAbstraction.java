package velka.types.typeSystem;

import java.util.Collection;

import velka.types.Type;
import velka.util.IEvalueable;

public interface VelkaAbstraction extends IEvalueable{

	Type getType();
	Object apply(Collection<? extends Object> arg);
	
	public default Object evaluate(Collection<? extends Object> args, Object env) {
		return this.apply(args);
	}
}
