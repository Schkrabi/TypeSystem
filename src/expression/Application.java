package expression;

import java.util.Iterator;
import java.util.Optional;

import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.InvalidNumberOfArgumentsException;
import util.NameGenerator;
import util.Pair;
import interpretation.Environment;

/**
 * Expression for function application in form (fun arg1 arg2 ...)
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
	public Expression interpret(Environment env) throws AppendableException {
		Expression ifun = fun.interpret(env);

		if (!MetaFunction.isFunction(ifun)) {
			throw new AppendableException(ifun.toString() + "is not a function");
		}

		Pair<Type, Substitution> argInfered = this.args.infer(env);
		final TypeTuple argsType = (TypeTuple) argInfered.first;

		// Implementation chosen on vector distance of argument types
		Function f = ((MetaFunction) ifun).getFunction(argsType);

		if (f.args.size() != this.args.size()) {
			throw new InvalidNumberOfArgumentsException(f.args.size(), this.args, this);
		}

		Environment childEnv = new Environment(f.creationEnvironment); // Lexical clojure!!!
		Iterator<Expression> i = f.args.iterator();
		Iterator<Expression> j = this.args.iterator();
		while (i.hasNext()) {
			Expression e = j.next();
			Variable v = (Variable) i.next();
			childEnv.put(v, new PostponeInterpretation(e, env));
		}

		TypeArrow funType = (TypeArrow) f.infer(env).first;

		childEnv = Application.autoConvertArgs(childEnv, f.args, argsType, (TypeTuple) funType.ltype);

		Expression rslt = f.body.interpret(childEnv);

		return rslt;
	}

	@Override
	public String toString() {
		return "(" + this.fun.toString() + " " + this.args.toString() + ")";
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			// Infer function type
			TypeArrow funType = new TypeArrow(new TypeVariable(NameGenerator.next()),
					new TypeVariable(NameGenerator.next()));
			Pair<Type, Substitution> funInfered = this.fun.infer(env);
			Substitution substFun = Type.unify(funType, funInfered.first);

			funType = (TypeArrow) funType.apply(substFun);

			// Infer arguments type
			Pair<Type, Substitution> argsInfered = this.args.infer(env);

			// Unify arguments and formal argument types
			Substitution substArgs = Type.unify(argsInfered.first, funType.ltype);

			// Compose all substitutions (and check if they are compatible)
			Substitution s = Substitution.EMPTY;
			s = s.union(funInfered.second);
			s = s.union(substFun);
			s = s.union(argsInfered.second);
			s = s.union(substArgs);

			return new Pair<Type, Substitution>(funType.rtype.apply(s), s);
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode() throws AppendableException {
		// TODO
		return "";
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof Application) {
			Application o = (Application) other;
			int c = this.fun.compareTo(o.fun);
			if (c != 0)
				return c;
			return this.args.compareTo(o.args);
		}
		return super.compareTo(other);
	}

	/**
	 * Lazily converts all the arguments to given representation
	 * 
	 * @param e        environment containing the arguments associated with their
	 *                 names
	 * @param args     argument names in the environment
	 * @param argTypes formal inferred types of the arguments
	 * @return new environment where all the arguments will be converted
	 * @throws Exception
	 */
	private static Environment autoConvertArgs(Environment e, Tuple args, TypeTuple fromType, TypeTuple toType)
			throws AppendableException {
		Environment ret = new Environment(e.parent);

		Iterator<Expression> i = args.iterator();
		Iterator<Type> j = fromType.iterator();
		Iterator<Type> k = toType.iterator();
		while (i.hasNext()) {
			Variable name = (Variable) i.next();
			Expression arg = e.getVariableValue(name);

			ret.put(name, j.next().convertTo(arg, k.next()));
		}

		return ret;
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof Application) {
			return this.fun.equals(((Application) other).fun) && this.args.equals(((Application) other).args);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.fun.hashCode() * this.args.hashCode();
	}
}
