package application;

import java.util.Arrays;
import java.util.Iterator;
import expression.Expression;
import expression.Tuple;
import interpretation.Environment;
import literal.LitBoolean;
import types.Substitution;
import types.Type;
import types.TypeAtom;
import types.TypeTuple;
import util.AppendableException;
import util.Pair;

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
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Substitution agg = Substitution.EMPTY;
		for (Expression e : this.args) {
			Pair<Type, Substitution> p = e.infer(env);
			Substitution s = Type.unify(p.first, TypeAtom.TypeBoolNative);
			agg = agg.union(p.second).union(s);
		}

		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, agg);
	}

	@Override
	protected String applicationToClojure(Tuple convertedArgs, Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder();
		s.append("(with-meta ");
		s.append("[(or");
		s.append(' ');

		Iterator<Expression> i = convertedArgs.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			s.append("(get ");
			s.append(e.toClojureCode(env));
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

	@Override
	protected Expression apply(Tuple convertedArgs, Environment evaluationEnvironment) throws AppendableException {
		Expression arg0 = convertedArgs.get(0);
		LitBoolean b0 = (LitBoolean)arg0.interpret(evaluationEnvironment);
		if(b0.value) {
			return LitBoolean.TRUE;
		}
		Expression arg1 = convertedArgs.get(1);
		LitBoolean b1 = (LitBoolean)arg1.interpret(evaluationEnvironment);
		if(b1.value) {
			return LitBoolean.TRUE;
		}
		return LitBoolean.FALSE;
	}

	@Override
	protected TypeTuple getFunArgsType(TypeTuple argsType, Environment env) throws AppendableException {
		return new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, TypeAtom.TypeBoolNative));
	}
}
