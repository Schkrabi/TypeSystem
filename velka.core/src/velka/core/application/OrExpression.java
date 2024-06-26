package velka.core.application;

import java.util.Optional;
import java.util.stream.Collectors;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
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
	public Expression interpret(Environment env) throws AppendableException {
		for(Expression e : (Tuple)args) {
			Expression ie = e.interpret(env);
			if(!(ie instanceof LitBoolean)) {
				ie = ie.convert(TypeAtom.TypeBoolNative, env);
				ie = ie.interpret(env);
			}
			
			if(ie.equals(LitBoolean.TRUE)) {
				return LitBoolean.TRUE;
			}
		}
		return LitBoolean.FALSE;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Substitution agg = Substitution.EMPTY;
		for (Expression e : ((Tuple)this.args)) {
			Pair<Type, Substitution> p = e.infer(env);
			Optional<Substitution> s = Type.unifyTypes(p.first, TypeAtom.TypeBoolNative);
			if(s.isEmpty()) {
				throw new TypesDoesNotUnifyException(p.first, TypeAtom.TypeBoolNative);
			}
			
			//Optional<Substitution> opt = agg.union(p.second);
			//TODO check if union necessary
			Optional<Substitution> opt = Optional.of(agg.compose(p.second));
			if(opt.isEmpty()) {
				throw new SubstitutionsCannotBeMergedException(agg, p.second);
			}
			
			//Optional<Substitution> tmp = opt.get().union(s.get());
			Optional<Substitution> tmp = Optional.of(opt.get().compose(s.get()));
			if(tmp.isEmpty()) {
				throw new SubstitutionsCannotBeMergedException(opt.get(), s.get());
			}
			
			agg = tmp.get();
		}

		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, agg);
	}
	
	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		var as = (Tuple)this.args;
		var al = as.stream().map(e -> {
			try {
			var t = e.infer(env).first;
			if(t.equals(TypeAtom.TypeBoolNative)) {
				return e.toClojureCode(env);
			}
			return ClojureHelper.applyClojureFunction(
					ClojureCoreSymbols.convertClojureSymbol_full, 
					TypeAtom.TypeBoolNative.clojureTypeRepresentation(),
					e.toClojureCode(env));
			} catch(AppendableException ex) {
				throw new RuntimeException(ex);
			}
		}).collect(Collectors.toList());
		
		return ClojureHelper.applyClojureFunction("or", al);
	}

	@Override
	protected String applicatedToString() {
		return OR;
	}
}
