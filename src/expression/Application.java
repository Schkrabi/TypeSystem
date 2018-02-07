package expression;

import java.util.Iterator;
import java.util.Map;
import java.util.PriorityQueue;

import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import util.ImplContainer;
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

	public Application(Expression fun, Expression arg) {
		this.fun = fun;
		this.args = new Tuple(new Expression[] { arg });
	}

	public Application(Expression fun, Tuple args) {
		this.fun = fun;
		this.args = args;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression ifun = fun.interpret(env);

		if (!(ifun instanceof ExtendedLambda)) {
			throw new Exception(fun + " is not a fucntion");
		}

		ExtendedLambda elambda = (ExtendedLambda) ifun;

		if (elambda.args.values.length != this.args.values.length) {
			throw new Exception("In aplication of " + fun + "number of arguments mismatch, expected "
					+ elambda.args.values.length + " got " + this.args.values.length);
		}

		Environment childEnv = new Environment(env);
		for (int i = 0; i < elambda.args.values.length; i++) {
			childEnv.put((Variable) elambda.args.values[i], this.args.values[i]);
		}

		// Ready for comparator
		PriorityQueue<ImplContainer> queue = elambda.getSortedImplementations();
		ImplContainer implContainer = queue.peek();

		// Std lambda
		if (elambda instanceof Lambda || implContainer == null) {
			// Do I want to do this?
			childEnv = Application.autoConvertForDefaultRepresentation(childEnv);
			return elambda.defaultImplementation.interpret(childEnv);
		}

		Expression impl = implContainer.implementation;
		TypeTuple argsType = queue.peek().typeSpec;
		childEnv = Application.autoConvertArgs(childEnv, elambda.args, argsType);

		return impl.interpret(childEnv);
	}

	/**
	 * Lazily converts all the arguments to their default representation
	 * 
	 * @param e
	 *            environment containing the arguments associated with their names
	 * @return new environment where all the arguments will be in their default
	 *         representation when interpreted
	 * @throws Exception 
	 */
	private static Environment autoConvertForDefaultRepresentation(Environment e) throws Exception {
		Environment ret = new Environment(e.parent);

		for (Map.Entry<Variable, Expression> entry : e.entrySet()) {
			Type t = entry.getValue().getType(); //Requires to run infer before interpret!			
			ret.put(entry.getKey(), t.convertToDefaultRepresentation(entry.getValue()));
		}

		return ret;
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

	@Override
	public String toString() {
		return this.fun.toString() + " " + this.args.toString();
	}

	@Override
	public Type infer() throws Exception {
		Type funType = this.fun.infer();
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
	public Expression substituteTopLevelVariables(Environment topLevel) {
		return new Application(this.fun.substituteTopLevelVariables(topLevel),
				this.args.substituteTopLevelVariables(topLevel));
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
