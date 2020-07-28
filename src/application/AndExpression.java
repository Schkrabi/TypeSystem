package application;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

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
 * Expression for And special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class AndExpression extends SpecialFormApplication {
	
	
	public AndExpression(Tuple args) {
		super(args);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Substitution s = this.args.infer(env).second;
		
		return new Pair<Type, Substitution>(TypeAtom.TypeBoolNative, s);
	}

	@Override
	protected String applicationToClojure(Tuple convertedArgs, Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder();
		s.append("(with-meta ");
		s.append("[(and");
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
		return "and";
	}

	@Override
	protected Expression apply(Tuple convertedArgs, Environment evaluationEnvironment) throws AppendableException {
		Expression arg0 = convertedArgs.get(0);
		LitBoolean b0 = (LitBoolean)arg0.interpret(evaluationEnvironment);
		if(!b0.value) {
			return LitBoolean.FALSE;
		}
		Expression arg1 = convertedArgs.get(1);
		LitBoolean b1 = (LitBoolean)arg1.interpret(evaluationEnvironment);
		if(!b1.value) {
			return LitBoolean.FALSE;
		}
		return LitBoolean.TRUE;
	}

	@Override
	protected TypeTuple getFunArgsType(TypeTuple argsType, Environment env) throws AppendableException {
		return new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, TypeAtom.TypeBoolNative));
	}
	
	@Override
	protected Tuple convertArgs(Tuple args, Environment env) throws AppendableException {
		List<Expression> l = new LinkedList<Expression>();
		for(Expression e : args) {
			Pair<Type, Substitution> p = e.infer(env);
			Expression c = p.first.convertTo(e, TypeAtom.TypeBoolNative);
			l.add(c);
		}
		
		Tuple t = new Tuple(l);
		return t;
	}
}
