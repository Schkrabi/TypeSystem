package application;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import abstraction.Abstraction;
import expression.Expression;
import expression.Tuple;
import types.RepresentationOr;
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
	
	/**
	 * Gets type of arguments of abstraction in this application
	 * @param argsType
	 * @param env
	 * @return
	 * @throws AppendableException
	 */
	protected TypeTuple getFunArgsType(TypeTuple argsType, Environment env) throws AppendableException {
		Type funInfered = this.fun.infer(env).first;
		if (funInfered instanceof RepresentationOr) {
			return (TypeTuple)Application.getBestImplementationType(argsType, (RepresentationOr) funInfered).ltype;
		} else if (funInfered instanceof TypeArrow) {
			return ((TypeTuple)((TypeArrow) funInfered).ltype);
		} else if (funInfered instanceof TypeVariable) {
			return new TypeTuple(argsType.stream().map(x -> new TypeVariable(NameGenerator.next())).collect(Collectors.toList()));
		}
		throw new AppendableException("Expecting expression yielding function at first place in application, got "
					+ funInfered.toString());
	}
	
	/**
	 * Converts representations of real arguments into types of function arguments
	 * @param args real arguments
	 * @param env environment
	 * @return tuple with converted arguments
	 * @throws AppendableException 
	 */
	protected Tuple convertArgs(Tuple args, Environment env) throws AppendableException {		
		TypeTuple argsType = (TypeTuple)args.infer(env).first;
		TypeTuple expectedType = this.getFunArgsType(argsType, env);
		
		if(args.size() != expectedType.size()) {
			throw new InvalidNumberOfArgumentsException(expectedType.size(), args, this);
		}
		
		Iterator<Expression> i = args.iterator();
		Iterator<Type> j = argsType.iterator();
		Iterator<Type> k = expectedType.iterator();
		
		List<Expression> l = new LinkedList<Expression>();
		
		while(i.hasNext()) {
			Expression arg = i.next();
			Type fromType = j.next();
			Type toType = k.next();
			
			l.add(fromType.convertTo(arg, toType));
		}
		
		Tuple ret = new Tuple(l);
		
		return ret;
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression ifun = fun.interpret(env);

		if (!(ifun instanceof Abstraction)) {
			throw new AppendableException(ifun.toString() + "is not an abstration");
		}
		Abstraction abst = (Abstraction)ifun;
		
		Tuple interpretedAndConvertedArgs = (Tuple)this.convertArgs(this.args, env).interpret(env);
		
		return abst.substituteAndEvaluate(interpretedAndConvertedArgs, env);
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
			// Infer arguments type
			final Pair<Type, Substitution> argsInfered = this.args.infer(env);

			// Unify arguments and formal argument types
			Substitution substArgs = Type.unify(argsInfered.first, funType.ltype);

			if (funInfered.first instanceof RepresentationOr) {
				Type best = Application.getBestImplementationType((TypeTuple) argsInfered.first,
						(RepresentationOr) funInfered.first);
				funInfered = new Pair<Type, Substitution>(best, funInfered.second);
			}
			Substitution substFun = Type.unify(funType, funInfered.first);

			funType = (TypeArrow) funType.apply(substFun);

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
	public String toClojureCode(Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder("(");
		s.append(Application.clojureEapply);
		s.append(" ");

		TypeTuple argsType = (TypeTuple) this.args.infer(env).first;

		s.append(this.fun.toClojureCode(env));

		s.append(" [");
		Iterator<Type> argsTypeIterator = argsType.iterator();
		while (argsTypeIterator.hasNext()) {
			Type t = argsTypeIterator.next();
			s.append(t.toClojure());
			if (argsTypeIterator.hasNext()) {
				s.append(" ");
			}
		}
		s.append("] ");
		
		s.append(this.convertArgs(this.args, env).toClojureCode(env));
		s.append(")");

		return s.toString();
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

	/**
	 * Finds type closest to argsType in RepresentationOr of elambda
	 * 
	 * @param argsType type of function argument
	 * @param funType  infered representationOr type
	 * @return a single TypeArrow which left is closest to argsType
	 */
	private static TypeArrow getBestImplementationType(TypeTuple argsType, RepresentationOr funType) {
		return funType.getRepresentations().stream()
				.map(t -> new Pair<Integer, TypeArrow>(((TypeTuple) ((TypeArrow) t).ltype).tupleDistance(argsType),
						(TypeArrow) t))
				.reduce(new Pair<Integer, TypeArrow>(Integer.MAX_VALUE, null), (p1, p2) -> {
					if (p1.first < p2.first)
						return p1;
					else
						return p2;
				}).second;
	}

	/**
	 * code of eapply functionn for clojure
	 */
	public static final String clojureEapply = "eapply";/*
														 * "(fn [elambda type args]\n" +
														 * "    (letfn [(vectorDist [v1 v2] (reduce + (map (fn [x y] (if (= x y) 0 1)) v1 v2)))\n"
														 * +
														 * "            (rankImpls [v impls] (map (fn [u] [(vectorDist (get u 0) v) (get u 1)]) impls))\n"
														 * +
														 * "            (getImpl [type elambda] (get (reduce (fn [x y] (if (< (get x 0) (get y 0)) x y)) (rankImpls type elambda)) 1))]\n"
														 * + "        (apply (getImpl type elambda) args)))";
														 */
}
