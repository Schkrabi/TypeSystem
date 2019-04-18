package expression;

import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import types.TypesDoesNotUnifyException;
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

		if (!MetaFunction.isFunction(ifun)) {
			throw new Exception(ifun.toString() + "is not a function");
		}

		Function f = ((MetaFunction) ifun).getFunction(); // Might want to add comparator here

		if (f.args.values.length != this.args.values.length) {
			throw new Exception("In aplication of " + fun + "number of arguments mismatch, expected "
					+ f.args.values.length + " got " + this.args.values.length);
		}

		Environment childEnv = new Environment(f.creationEnvironment); // Lexical clojure!!!
		for (int i = 0; i < f.args.values.length; i++) {
			Expression e = this.args.values[i].interpret(env);
			// Type t = e.getType();
			Variable v = new Variable(((Variable) f.args.values[i]).name);
			// v.setType(t);
			childEnv.put(v, e);
		}

		TypeArrow lambdaType = TypeArrow.getFunctionType(f.getType());

		childEnv = Application.autoConvertArgs(childEnv, f.args, lambdaType.ltype);

		Expression rslt = f.body.interpret(childEnv);

		/*
		 * if(ifun instanceof Constructor){ rslt.setType(lambdaType.rtype); }
		 */

		return rslt;
	}

	@Override
	public String toString() {
		return this.fun.toString() + " " + this.args.toString();
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();

			if (this.typeHypothesis == null) {
				Map<Expression, Type> funHyp = this.fun.infer(env);
				Map<Expression, Type> argsHyp = this.args.infer(env);
				
				Type funType = funHyp.get(this.fun);
				if (!funType.isApplicableType()) {
					throw new AppendableException("Type " + funType + " of " + this.fun + " is not TypeArrow!");
				}
				if(funType instanceof ForallType) {
					funType = ((ForallType) funType).getBoundType();
				}
				
				TypeArrow funArrType = (TypeArrow)funType;

				Optional<Type> unifiedArgsType = Type.unify(funArrType.ltype, argsHyp.get(this.args));
				if (!unifiedArgsType.isPresent()) {
					throw new TypesDoesNotUnifyException(funHyp.get(this.args), argsHyp.get(this.args));
				}

				Map<Expression, Type> tmp = new TreeMap<Expression, Type>();
				tmp.putAll(funHyp);
				tmp.putAll(argsHyp);
				tmp.put(this.args, unifiedArgsType.get());

				tmp.put(this, ((TypeArrow) funType).rtype);

				this.typeHypothesis = tmp;
			}
			hyp.putAll(this.typeHypothesis);

			return hyp;
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		Expression f = this.fun.substituteTopLevelVariables(topLevel);
		Expression a = this.args.substituteTopLevelVariables(topLevel);

		if (a instanceof Tuple) {
			Tuple t = (Tuple) a;
			return new Application(f, t);
		} else {
			// return new Application(f, a);
			throw new Exception("Argument is always tuple");
		}
	}

	@Override
	public String toClojureCode() throws Exception {
		StringBuilder s = new StringBuilder();
		s.append('(');

		/*
		 * if(!MetaLambda.isApplicableExpression(this.fun)){ throw new
		 * Exception(this.fun.toString() + " is not a function"); }
		 */

		s.append(this.fun.toClojureCode());
		s.append(' ');

		TypeArrow funType = TypeArrow.getFunctionType(this.fun.getType());

		TypeTuple argsType;

		if (funType.ltype instanceof TypeTuple) {
			argsType = (TypeTuple) funType.ltype;
		} else if (funType.ltype instanceof TypeVariable) {
			Type[] ts = new Type[this.args.values.length];

			for (int i = 0; i < this.args.values.length; i++) {
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
			Type argType = j.next();

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
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Application) {
			Application o = (Application)other;
			int c = this.fun.compareTo(o.fun);
			if(c != 0)
				return c;
			return this.args.compareTo(o.args);
		}
		return super.compareTo(other);
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
	private static Environment autoConvertArgs(Environment e, Tuple args, Type argType) throws Exception {
		if (argType instanceof TypeVariable) {
			return e;
		}
		if (!(argType instanceof TypeTuple)) {
			throw new AppendableException("Args type of function must be single vartiable or typetuple got " + argType);
		}
		TypeTuple argTypes = (TypeTuple) argType;
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
