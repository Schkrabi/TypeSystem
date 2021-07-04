package velka.lang.application;

import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;

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
}
