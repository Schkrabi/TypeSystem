package velka.lang.application;

import java.util.Iterator;

import velka.lang.conversions.Conversions;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.ClojureCoreSymbols;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.literal.LitBoolean;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

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
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		for(Expression e : (Tuple)args) {
			Expression ie = e.interpret(env, typeEnv);
			if(!(ie instanceof LitBoolean)) {
				Pair<Type, Substitution> inf = ie.infer(env, typeEnv);
				ie = Conversions.convert(inf.first, ie, TypeAtom.TypeBoolNative, typeEnv);
				ie = ie.interpret(env, typeEnv);
			}
			
			if(ie.equals(LitBoolean.FALSE)) {
				return LitBoolean.FALSE;
			}
		}
		return LitBoolean.TRUE;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Substitution s = this.args.infer(env, typeEnv).second;
		
		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, s);
	}
	
	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder sb = new StringBuilder();
		sb.append("(and ");
		
		Iterator<Expression> i = ((Tuple)this.args).iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			sb.append("(first (");
			sb.append(ClojureCoreSymbols.convertClojureSymbol_full);
			sb.append(" \n");
			sb.append(TypeAtom.TypeBoolNative.clojureTypeRepresentation());
			sb.append(" ");
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
		return AND;
	}
}
