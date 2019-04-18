package expression;

import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

import interpretation.Environment;
import semantic.UserException;
import types.Type;
import types.TypeConcrete;
import types.TypeVariable;
import types.TypesDoesNotUnifyException;
import util.AppendableException;
import util.NameGenerator;

/**
 * Expression for user defined exception
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExceptionExpr extends Expression {
	
	/**
	 * Message of the exception
	 */
	public final Expression message;
	
	public ExceptionExpr(Expression message) {
		this.message = message;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		throw new UserException(this.message.interpret(env).toString());
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if(this.typeHypothesis == null) {
				Map<Expression, Type> tmp = this.message.infer(env);
				Optional<Type> o = Type.unify(TypeConcrete.TypeString, tmp.get(this.message));
				if(!o.isPresent()) {
					throw new TypesDoesNotUnifyException(TypeConcrete.TypeString, tmp.get(this.message));
				}
				tmp.put(this,  new TypeVariable(NameGenerator.next()).quantifyUnconstrainedVariables());
				this.typeHypothesis = tmp;
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		//No variables
		return this;
	}

	@Override
	public String toClojureCode() throws Exception {
		return "(throw (Throwable. \"" + this.message + "\"))";
	}
	
	@Override
	public String toString() {
		return "error " + this.message.toString();
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof ExceptionExpr) {
			return this.message.compareTo(((ExceptionExpr) other).message);
		}
		return super.compareTo(other);
	}

}
