package expression;

import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;
import types.TypesDoesNotUnifyException;
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

		//Implementation chosen on vector distance of argument types
		Function f = ((MetaFunction) ifun).getFunction(new Comparator<Function>() {

			@Override
			public int compare(Function arg0, Function arg1) {
				int sum = 0;
				Iterator<Type> i = arg0.argsType.iterator();
				Iterator<Type> j = arg1.argsType.iterator();
				while(i.hasNext()) {
					if(!i.equals(j)) {
						sum++;
					}
				}
				return sum;
			}});

		if (f.args.size() != this.args.size()) {
			throw new InvalidNumberOfArgumentsException(f.args.size(), this.args, this);
		}

		Environment childEnv = new Environment(f.creationEnvironment); // Lexical clojure!!!
		Iterator<Expression> i = f.args.iterator();
		Iterator<Expression> j = this.args.iterator();
		while(i.hasNext()) {
			Expression e = j.next();
			Variable v = (Variable)i.next();
			childEnv.put(v, e);
		}

		TypeArrow lambdaType = (TypeArrow)f.infer(env).first;

		childEnv = Application.autoConvertArgs(childEnv, f.args, lambdaType.ltype);

		Expression rslt = f.body.interpret(childEnv);

		return rslt;
	}

	@Override
	public String toString() {
		return this.fun.toString() + " " + this.args.toString();
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException { 
		try {
			//Infer function type
			TypeArrow funType = new TypeArrow(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()));
			Pair<Type, Substitution> funInfered = this.fun.infer(env);
			Optional<Substitution> substFun = Type.unify(funType, funInfered.first);
			
			if(!substFun.isPresent()) {
				throw new TypesDoesNotUnifyException(funType, funInfered.first);
			}
			
			funType = (TypeArrow)funType.apply(substFun.get());
			
			//Infer arguments type
			Pair<Type, Substitution> argsInfered = this.args.infer(env);
			
			//Unify arguments and formal argument types
			Optional<Substitution> substArgs = Type.unify(argsInfered.first, funType.ltype);
			if(!substArgs.isPresent()) {
				throw new TypesDoesNotUnifyException(argsInfered.first, funType.ltype);
			}
			
			//Compose all substitutions (and check if they are compatible)
			Substitution s = new Substitution();
			s = s.compose(funInfered.second);
			s = s.compose(substFun.get());
			s = s.compose(argsInfered.second);
			s = s.compose(substArgs.get());
			
			return new Pair<Type, Substitution>(funType.rtype.apply(s), s);
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode() throws AppendableException {
		//TODO
		return "";
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
	private static Environment autoConvertArgs(Environment e, Tuple args, Type argType) throws AppendableException {
		TypeTuple argTypes = (TypeTuple) argType;
		Environment ret = new Environment(e.parent);

		Iterator<Expression> i = args.iterator();
		Iterator<Type> j = argTypes.iterator();
		while(i.hasNext()){
			Variable name = (Variable) i.next();
			Type fromType = e.getVariableValue(name).infer(e).first;
			Expression arg = e.getVariableValue(name);
			Type toType = j.next();

			ret.put(name, fromType.convertTo(arg, toType));
		}

		return ret;
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof Application) {
			return this.fun.equals(((Application) other).fun) 
					&& this.args.equals(((Application) other).args);
		}
		return false;
	}
	
	@Override 
	public int hashCode() {
		return this.fun.hashCode() * this.args.hashCode();
	}
}
