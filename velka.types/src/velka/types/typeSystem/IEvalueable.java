package velka.types.typeSystem;

import java.util.Collection;

public interface IEvalueable {

	public Object evaluate(Collection<? extends Object> args, Object env);
}
