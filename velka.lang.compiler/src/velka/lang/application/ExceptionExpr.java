package velka.lang.application;

import java.util.Arrays;
import java.util.Optional;

import velka.lang.conversions.Conversions;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.interpretation.VelkaClojureCore;
import velka.lang.literal.LitString;
import velka.lang.exceptions.UserException;
import velka.lang.types.Substitution;
import velka.lang.types.SubstitutionsCannotBeMergedException;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeVariable;
import velka.lang.types.TypesDoesNotUnifyException;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;

/**
 * Expression for user defined exception
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExceptionExpr extends SpecialFormApplication {

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
			Pair<Type, Substitution> inf = iArg.infer(env, typeEnv);
			iArg = Conversions.convert(inf.first, iArg, TypeAtom.TypeStringNative, typeEnv);
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
			
			Optional<Substitution> opt = s.get().union(infered.second);
			if(opt.isEmpty()) {
				throw new SubstitutionsCannotBeMergedException(s.get(), infered.second);
			}
			
			return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), opt.get());
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}
	
	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder sb = new StringBuilder();
		sb.append("(throw (Throwable.");
		sb.append("(first (");
		sb.append(VelkaClojureCore.convertClojureSymbol_full);
		sb.append(" ");
		sb.append(this.getMessage().toClojureCode(env, typeEnv));
		sb.append("))))");
		return sb.toString();
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
		return "error";
	}
}
