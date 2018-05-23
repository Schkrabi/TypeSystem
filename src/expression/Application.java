package expression;

import java.util.Iterator;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
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
		
		if(!MetaFunction.isFunction(ifun)){
			throw new Exception(ifun.toString() + "is not a function");
		}
		
		Function f = ((MetaFunction)ifun).getFunction(); //Might want to add comparator here
				
		if(f.args.values.length != this.args.values.length) {
			throw new Exception("In aplication of " + fun + "number of arguments mismatch, expected "
					+ f.args.values.length + " got " + this.args.values.length);
		}
		
		Environment childEnv = new Environment(f.creationEnvironment); //Lexical clojure!!!
		for (int i = 0; i < f.args.values.length; i++) {
			Expression e = this.args.values[i].interpret(env);
			Type t = e.getType();
			Variable v = new Variable(((Variable) f.args.values[i]).name);
			v.setType(t);
			childEnv.put(v, e);
		}
		
		TypeArrow lambdaType = TypeArrow.getFunctionType(f.getType());
		
		childEnv = Application.autoConvertArgs(childEnv, f.args, (TypeTuple)lambdaType.ltype);
		
		Expression rslt = f.body.interpret(childEnv); 
		
		if(ifun instanceof Constructor){
			rslt.setType(lambdaType.rtype);
		}
		
		return rslt;
	}

	@Override
	public String toString() {
		return this.fun.toString() + " " + this.args.toString();
	}

	@Override
	public Type infer(Environment env) throws Exception {
		Type funType;
		
		/*if(this.fun instanceof Constructor) { 
			Constructor constr = (Constructor)this.fun;
			funType = constr.infer(env);
		}else {*/
			funType = this.fun.infer(env);
		//}
		
		if(funType instanceof ForallType) {
			funType = ((ForallType) funType).getBoundType();
		}
		
		Type argsType = this.args.infer(env);

		if (!(funType.isApplicableType())
				&& !(funType instanceof TypeVariable)) {
			throw new Exception(fun + " is not a function");
		}
		TypeArrow funArrType;

		if(funType instanceof TypeArrow) {
			funArrType = (TypeArrow) funType;
		}else if(funType instanceof TypeVariable) {
			funArrType = new TypeArrow(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()));
		}else {
			throw new AppendableException(funType.toString() + "is not an applicable type");
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
		
		/*if(!MetaLambda.isApplicableExpression(this.fun)){
			throw new Exception(this.fun.toString() + " is not a function");
		}*/
		
		s.append(this.fun.toClojureCode());
		s.append(' ');

		TypeArrow funType = TypeArrow.getFunctionType(this.fun.getType());
		
		TypeTuple argsType;
		
		if(funType.ltype instanceof TypeTuple) {
			argsType = (TypeTuple) funType.ltype;
		}
		else if(funType.ltype instanceof TypeVariable) {
			Type[] ts = new Type[this.args.values.length];
			
			for(int i = 0; i < this.args.values.length; i++) {
				ts[i] = new TypeVariable(NameGenerator.next());
			}
			
			argsType = new TypeTuple(ts);
		} else {
			throw new AppendableException("Problem with functional type of " + this.fun);
		}

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
			Variable name = (Variable) args.values[i];
			Type fromType = e.getVariableValue((Variable) args.values[i]).getType();
			Expression arg = e.getVariableValue((Variable) args.values[i]);
			Type toType = argTypes.values[i];
			
			ret.put(name, fromType.convertTo(arg, toType));
		}

		return ret;
}
}
