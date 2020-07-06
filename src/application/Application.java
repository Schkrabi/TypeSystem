package application;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import expression.Expression;
import expression.Tuple;
import interpretation.Environment;
import types.RepresentationOr;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import util.AppendableException;
import util.InvalidNumberOfArgumentsException;
import util.Pair;

/**
 * Expression for various kind of application (application of abstractions or special forms)
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Application extends Expression {
	
	/**
	 * Arguments of the function
	 */
	public final Tuple args;
	
	public Application(Tuple args) {
		this.args = args;
	}
	
	/**
	 * Produces clojure code for this application with already converted arguments
	 * @param convertedArgs
	 * @param env
	 * @return Clojure code representing this application
	 * @throws AppendableException
	 */
	protected abstract String applicationToClojure(Tuple convertedArgs, Environment env) throws AppendableException;
	
	/**
	 * Returns name of fucntion or special form of this application
	 * @return
	 */
	protected abstract String applicatedToString();
	
	/**
	 * Interprets the application with already converted and interpreted arguments
	 * @param convertedArgs
	 * @param evaluationEnvironment
	 * @return expression
	 * @throws AppendableException
	 */
	protected abstract Expression apply(Tuple convertedArgs, Environment evaluationEnvironment) throws AppendableException;
	
	/**
	 * Gets type of arguments that this application is expecting
	 * @param argsType
	 * @param env
	 * @return
	 * @throws AppendableException
	 */
	protected abstract TypeTuple getFunArgsType(TypeTuple argsType, Environment env) throws AppendableException;
	
	/**
	 * Finds type closest to argsType in RepresentationOr of elambda
	 * 
	 * @param argsType type of function argument
	 * @param funType  infered representationOr type
	 * @return a single TypeArrow which left is closest to argsType
	 */
	protected static TypeArrow getBestImplementationType(TypeTuple argsType, RepresentationOr funType) {
		Stream<Pair<Integer, TypeArrow>> costs = funType.getRepresentations().stream()
				.map(t -> new Pair<Integer, TypeArrow>(((TypeTuple) ((TypeArrow) t).ltype).tupleDistance(argsType),
						(TypeArrow) t));
		List<Pair<Integer, TypeArrow>> costsList = costs.collect(Collectors.toList());
		Pair<Integer, TypeArrow> best = costsList.stream().reduce(new Pair<Integer, TypeArrow>(Integer.MAX_VALUE, null), (p1, p2) -> {
			if (p1.first < p2.first)
				return p1;
			else
				return p2;
		});
		
		return best.second;
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
		Tuple interpretedAndConvertedArgs = (Tuple)this.convertArgs(this.args, env);
		
		return this.apply(interpretedAndConvertedArgs, env);
	}
	
	@Override
	public String toString() {
		return "(" + this.applicatedToString() + " " + this.args.toString() + ")";
	}
	
	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		Tuple convertedArgs = this.convertArgs(this.args, env);
		return this.applicationToClojure(convertedArgs, env);
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof Application) {
			return this.args.equals(((Application) other).args);
		}
		return false;
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Application) {
			return this.args.compareTo(((Application) other).args);
		}
		return super.compareTo(other);
	}
	
	@Override
	public int hashCode() {
		return this.args.hashCode();
	}
}
