package expression;

import java.util.Optional;

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
	public Type infer(Environment env) throws AppendableException {
		if(this.getType() == null)
		{
			//Creates a new mock type to unify with anything
			this.setType(new TypeVariable(NameGenerator.next()));
		}
		
		Type argType = this.message.infer(env);
		
		try {
			Optional<Type> o = Type.unify(TypeConcrete.TypeString, argType);
			if(!o.isPresent()) {
				throw new TypesDoesNotUnifyException(TypeConcrete.TypeString, argType);
			}
		}catch(AppendableException e) {
			e.appendMessage("in " + this.toString());
			throw e;
		}
		
		return this.getType();
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

}
