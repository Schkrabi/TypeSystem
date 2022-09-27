package velka.core.application;

import java.util.Arrays;
import java.util.Optional;

import velka.core.exceptions.UserException;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitString;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Expression for user defined exception
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExceptionExpr extends SpecialFormApplication {
	
	/**
	 * Symbol for error special form
	 */
	public static final String ERROR = "error";

	public ExceptionExpr(Expression message) {
		super(new Tuple(Arrays.asList(message)));
	}
	
	public Expression getMessage() {
		return ((Tuple)this.args).get(0);
	}
	
	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Tuple iArgs = (Tuple)this.args.interpret(env, typeEnv);
		Expression iArg = iArgs.get(0);
		if(!(iArg instanceof LitString)) {
			iArg = iArg.convert(TypeAtom.TypeStringNative, env, typeEnv);
			iArg = iArg.interpret(env, typeEnv);
		}
		
		String message = ((LitString)iArg).value;
		
		throw new UserException(message);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Pair<Type, Substitution> infered = this.getMessage().infer(env, typeEnv);
			Optional<Substitution> s = Type.unifyTypes(infered.first, TypeAtom.TypeStringNative);
			if(s.isEmpty()) {
				throw new TypesDoesNotUnifyException(infered.first, TypeAtom.TypeStringNative);
			}
			
			return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), s.get().compose(infered.second));
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}
	
	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder sb = new StringBuilder();
		sb.append("(first (");
		sb.append(ClojureCoreSymbols.convertClojureSymbol_full);
		sb.append(" ");
		sb.append(this.getMessage().toClojureCode(env, typeEnv));
		sb.append("))");		
		
		return ClojureHelper.errorHelper(sb.toString());
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof ExceptionExpr) {
			return this.args.equals(((ExceptionExpr) other).args);
		}
		return false;
	}

	@Override
	protected String applicatedToString() {
		return ERROR;
	}
}
