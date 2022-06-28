package velka.core.application;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Type;
import velka.util.AppendableException;

public abstract class SpecialFormApplication extends Application {

	public SpecialFormApplication(Tuple tuple) {
		super(tuple);
	}
	
	@Override
	public int compareTo(Expression other) {
		if (other instanceof ExceptionExpr) {
			return this.args.compareTo(((ExceptionExpr) other).args);
		}
		return super.compareTo(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode() * this.getClass().getName().hashCode();
	}
	
	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression e = this.interpret(env, typeEnv);
		return e.convert(to, env, typeEnv);
	}
}
