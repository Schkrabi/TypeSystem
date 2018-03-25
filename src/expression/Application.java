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
		
		if(!MetaLambda.isApplicableExpression(ifun)){
			throw new Exception(ifun.toString() + "is not a function");
		}
		
		Lambda lambda = ((MetaLambda)ifun).getLambda(); //Might want to add comparator here
				
		if(lambda.args.values.length != this.args.values.length) {
			throw new Exception("In aplication of " + fun + "number of arguments mismatch, expected "
					+ lambda.args.values.length + " got " + this.args.values.length);
		}
		
		Environment childEnv = new Environment(env);
		for (int i = 0; i < lambda.args.values.length; i++) {
			childEnv.put((Variable) lambda.args.values[i], this.args.values[i]);
		}
		
		TypeArrow lambdaType = TypeArrow.getFunctionType(lambda.getType());
		
		childEnv = Application.autoConvertArgs(childEnv, lambda.args, (TypeTuple)lambdaType.ltype);
		
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
		
		if(!MetaLambda.isApplicableExpression(this.fun)){
			throw new Exception(this.fun.toString() + " is not a function");
		}
		
		Lambda lambda = ((MetaLambda)this.fun).getLambda(); //Maybe comparator here?
		
		s.append(lambda.toClojureCode());
		s.append(' ');

		TypeArrow funType = TypeArrow.getFunctionType(lambda.getType());
		
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
	
	/**
	 * Lazily converts all the arguments to given representation
	 * 
	 * @param e
	 *            environment containing the arguments associated with their names
	 * @param args
	 *            argument names in the environment
	 * @param argTypes
	 *            formal inferred types of the arguments
	 * @return new environment where all the arguments will be converted
	 * @throws Exception
	 */
	private static Environment autoConvertArgs(Environment e, Tuple args, TypeTuple argTypes) throws Exception {
		Environment ret = new Environment(e.parent);

		for (int i = 0; i < args.values.length; i++) {
			ret.put((Variable) args.values[i], e.get((Variable) args.values[i]).getType()
					.convertTo(e.get((Variable) args.values[i]), argTypes.values[i]));
		}

		return ret;
}
}
