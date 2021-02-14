package velka.lang.application;

import java.util.Iterator;

import velka.lang.conversions.Conversions;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
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
		for(Expression e : args) {
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
		for (Expression e : this.args) {
			Pair<Type, Substitution> p = e.infer(env, typeEnv);
			Substitution s = Type.unifyTypes(p.first, TypeAtom.TypeBoolNative);
			agg = agg.union(p.second).union(s);
		}

		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, agg);
	}
	
	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected String applicationToClojure(Tuple convertedArgs, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder s = new StringBuilder();
		s.append("(with-meta ");
		s.append("[(or");
		s.append(' ');

		Iterator<Expression> i = convertedArgs.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			s.append("(get ");
			s.append(e.toClojureCode(env, typeEnv));
			s.append(" 0)");
			if (i.hasNext()) {
				s.append(' ');
			}
		}
		s.append(")]");
		
		s.append("{:lang-type ");
		s.append(TypeAtom.TypeBoolNative.clojureTypeRepresentation());
		s.append("})");
		
		return s.toString();
	}

	@Override
	protected String applicatedToString() {
		return "or";
	}
}
