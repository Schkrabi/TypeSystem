package velka.core.application;

import java.util.stream.Collectors;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Expression for And special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class AndExpression extends SpecialFormApplication {
	
	/**
	 * Velka symbol for and special form
	 */
	public static final String AND = "and";
	
	
	public AndExpression(Tuple args) {
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
			
			if(ie.equals(LitBoolean.FALSE)) {
				return LitBoolean.FALSE;
			}
		}
		return LitBoolean.TRUE;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Substitution s = this.args.infer(env).second;
		
		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, s);
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
		
		return ClojureHelper.applyClojureFunction("and", al);
	}

	@Override
	protected String applicatedToString() {
		return AND;
	}
}
