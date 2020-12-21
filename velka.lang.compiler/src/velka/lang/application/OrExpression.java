package velka.lang.application;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import velka.lang.conversions.Conversions;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;
import velka.lang.literal.LitBoolean;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeAtom;
import velka.lang.types.TypeTuple;
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

	@Override
	protected Expression apply(Tuple convertedArgs, Environment evaluationEnvironment, TypeEnvironment typeEnv) throws AppendableException {
		Expression arg0 = convertedArgs.get(0);
		LitBoolean b0 = (LitBoolean)arg0.interpret(evaluationEnvironment, typeEnv);
		if(b0.value) {
			return LitBoolean.TRUE;
		}
		Expression arg1 = convertedArgs.get(1);
		LitBoolean b1 = (LitBoolean)arg1.interpret(evaluationEnvironment, typeEnv);
		if(b1.value) {
			return LitBoolean.TRUE;
		}
		return LitBoolean.FALSE;
	}

	@Override
	protected TypeTuple getFunArgsType(TypeTuple argsType, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		return new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, TypeAtom.TypeBoolNative));
	}
	
	@Override
	protected Tuple convertArgs(Tuple args, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		List<Expression> l = new LinkedList<Expression>();
		for(Expression e : args) {
			Pair<Type, Substitution> p = e.infer(env, typeEnv);
			Expression c = Conversions.convert(p.first, e, TypeAtom.TypeBoolNative, typeEnv);
			l.add(c);
		}
		
		Tuple t = new Tuple(l);
		return t;
	}
}
