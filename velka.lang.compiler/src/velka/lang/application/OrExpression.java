package velka.lang.application;

import java.util.Iterator;

import velka.lang.conversions.Conversions;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.ClojureCodeGenerator;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.literal.LitBoolean;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.util.AppendableException;
import velka.lang.util.Pair;

/**
 * Expression for or special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class OrExpression extends SpecialFormApplication {

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
			Substitution s = Type.unifyTypes(p.first, TypeAtom.TypeBoolNative);
			agg = agg.union(p.second).union(s);
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
			sb.append(ClojureCodeGenerator.convertClojureSymbol);
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
		return "or";
	}
}
