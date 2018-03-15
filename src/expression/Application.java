package expression;

import java.util.Iterator;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import interpretation.Environment;

/**
 * Expression for function application
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Application extends Expression {

	/**
	 * Expression that should yield function
	 */
	public final Expression fun;
	/**
	 * Arguments of the function
	 */
	public final Tuple args;

	public Application(Expression fun, Tuple args) {
		this.fun = fun;
		this.args = args;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression ifun = fun.interpret(env);
		Lambda lambda;
		
		if(ifun instanceof Lambda) {
			lambda = (Lambda)ifun;
		}
		else if(ifun instanceof ExtendedLambda) {
			ExtendedLambda elambda = (ExtendedLambda) ifun;
			lambda = elambda.getSortedImplementations().peek(); //Might want to add comparator here
		}
		else {
			throw new Exception(fun + " is not a fucntion");
		}		
		
		if(lambda.args.values.length != this.args.values.length) {
			throw new Exception("In aplication of " + fun + "number of arguments mismatch, expected "
					+ lambda.args.values.length + " got " + this.args.values.length);
		}
		
		Environment childEnv = new Environment(env);
		for (int i = 0; i < lambda.args.values.length; i++) {
			childEnv.put((Variable) lambda.args.values[i], this.args.values[i]);
		}
		
		return lambda.body.interpret(childEnv);
	}

	@Override
	public String toString() {
		return this.fun.toString() + " " + this.args.toString();
	}

	@Override
	public Type infer() throws Exception {
		Type funType;
		
		if(this.fun instanceof Constructor) {
			Constructor constr = (Constructor)this.fun;
			funType = constr.infer();
		}else {
			funType = this.fun.infer();
		}
		
		Type argsType = this.args.infer();

		if (!(funType.isApplicableType())) {
			throw new Exception(fun + " is not a function");
		}
		TypeArrow funArrType;

		if (funType instanceof TypeArrow) {
			funArrType = (TypeArrow) funType;
		} else if (funType instanceof ForallType) {
			funArrType = (TypeArrow) ((ForallType) funType).getBoundType();
		} else {
			throw new Exception("Instance of " + funType + " was evaluated as applicable");
		}

		if (!Type.unify(funArrType.ltype, argsType)) {
			throw new Exception("Arguments of " + this.fun + " does not unify with args " + this.args + " expected "
					+ funArrType.ltype + " got " + argsType);
		}

		this.setType(funArrType.rtype);

		return funArrType.rtype;
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		Expression f = this.fun.substituteTopLevelVariables(topLevel);
		Expression a = this.args.substituteTopLevelVariables(topLevel);
		
		if(a instanceof Tuple) {
			Tuple t = (Tuple)a;
			return new Application(f, t);
		}else {
		//	return new Application(f, a);
			throw new Exception("Argument is always tuple");
		}
	}

	@Override
	public String toClojureCode() throws Exception {
		StringBuilder s = new StringBuilder();
		s.append('(');
		s.append(this.fun.toClojureCode());
		s.append(' ');

		Type rawFunType = this.fun.getType().getRep();
		if(rawFunType instanceof ForallType) {
			rawFunType = ((ForallType)rawFunType).getBoundType();
		}
		if(!(rawFunType instanceof TypeArrow)) {
			throw new Exception("Invalid elambda type " + this.fun.getType().getRep() + " in " + this);
		}
		TypeArrow funType = (TypeArrow)rawFunType;
	
		TypeTuple argsType = (TypeTuple) funType.ltype;

		Iterator<Expression> i = this.args.iterator();
		Iterator<Type> j = argsType.iterator();
		while (i.hasNext()) {
			Expression e = i.next();
			Type argType = j.next().getRep();

			if (!argType.equals(e.getType())) {
				e = e.getType().convertTo(e, argType);
			}

			s.append(e.toClojureCode());

			if (i.hasNext()) {
				s.append(" ");
			}
		}
		s.append(')');
		return s.toString();
	}
}
