package velka.core.application;

import java.util.Iterator;
import java.util.Optional;

import velka.core.conversions.Conversions;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.ClojureCoreSymbols;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitBoolean;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Expression for or special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class OrExpression extends SpecialFormApplication {
	
	public static final String OR = "or";

	public OrExpression(Tuple args) {
		super(args);
	}
	
	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		for(Expression e : (Tuple)args) {
			Expression ie = e.interpret(env, typeEnv);
			if(!(ie instanceof LitBoolean)) {
				Pair<Type, Substitution> inf = ie.infer(env, typeEnv);
				ie = Conversions.convert(inf.first, ie, TypeAtom.TypeBoolNative, typeEnv);
				ie = ie.interpret(env, typeEnv);
			}
			
			if(ie.equals(LitBoolean.TRUE)) {
				return LitBoolean.TRUE;
			}
		}
		return LitBoolean.FALSE;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Substitution agg = Substitution.EMPTY;
		for (Expression e : ((Tuple)this.args)) {
			Pair<Type, Substitution> p = e.infer(env, typeEnv);
			Optional<Substitution> s = Type.unifyTypes(p.first, TypeAtom.TypeBoolNative);
			if(s.isEmpty()) {
				throw new TypesDoesNotUnifyException(p.first, TypeAtom.TypeBoolNative);
			}
			
			Optional<Substitution> opt = agg.union(p.second);
			if(opt.isEmpty()) {
				throw new SubstitutionsCannotBeMergedException(agg, p.second);
			}
			
			Optional<Substitution> tmp = opt.get().union(s.get());
			if(tmp.isEmpty()) {
				throw new SubstitutionsCannotBeMergedException(opt.get(), s.get());
			}
			
			agg = tmp.get();
		}

		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, agg);
	}
	
	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder sb = new StringBuilder();
		sb.append("(or ");
		
		Iterator<Expression> i = ((Tuple)this.args).iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			sb.append("(first (");
			sb.append(ClojureCoreSymbols.convertClojureSymbol_full);
			sb.append(" ");
			sb.append(TypeAtom.TypeBoolNative.clojureTypeRepresentation());
			sb.append(" \n");
			sb.append(e.toClojureCode(env, typeEnv));
			sb.append("))");
			if (i.hasNext()) {
				sb.append(" \n");
			}
		}
		
		sb.append(")");
		
		
		return LitBoolean.clojureBooleanToClojureLitBoolean(sb.toString());
	}

	@Override
	protected String applicatedToString() {
		return OR;
	}
}
