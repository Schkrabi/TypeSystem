package application;

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
import util.NameGenerator;
import util.Pair;
import interpretation.Environment;

/**
 * Expression for function application in form (fun arg1 arg2 ...)
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class AbstractionApplication extends Application {

	/**
	 * Expression that should yield function
	 */
	public final Expression fun;

	public AbstractionApplication(Expression fun, Tuple args) {
		super(args);
		this.fun = fun;
	}

	/**
	 * Gets type of arguments of abstraction in this application
	 * 
	 * @param argsType
	 * @param env
	 * @return
	 * @throws AppendableException
	 */
	protected TypeTuple getFunArgsType(TypeTuple argsType, Environment env) throws AppendableException {
		Type funInfered = this.fun.infer(env).first;
		if (funInfered instanceof RepresentationOr) {
			return (TypeTuple) AbstractionApplication.getBestImplementationType(argsType,
					(RepresentationOr) funInfered).ltype;
		} else if (funInfered instanceof TypeArrow) {
			return ((TypeTuple) ((TypeArrow) funInfered).ltype);
		} else if (funInfered instanceof TypeVariable) {
			return new TypeTuple(
					argsType.stream().map(x -> new TypeVariable(NameGenerator.next())).collect(Collectors.toList()));
		}
		throw new AppendableException(
				"Expecting expression yielding function at first place in application, got " + funInfered.toString());
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		try {
			Pair<Type, Substitution> funInfered = this.fun.infer(env);
			
			Pair<Type, Substitution> argsInfered = this.args.infer(env);

			if(!(funInfered.first instanceof TypeArrow)) {
				// Find best implementation if applicable
				if (funInfered.first instanceof RepresentationOr) {
					Type best = AbstractionApplication.getBestImplementationType((TypeTuple) argsInfered.first,
							(RepresentationOr) funInfered.first);
					funInfered = new Pair<Type, Substitution>(best, funInfered.second);
				}
				// If type of function is unknown (unresolved variable) assume type arrow
				else if (funInfered.first instanceof TypeVariable) {
					funInfered = new Pair<Type, Substitution>(
							new TypeArrow(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next())),
							Substitution.EMPTY);
				}
				else {
					throw new AppendableException(funInfered.first.toString() + " is not applicable type! In:" + this.toString());
				}
			}

			// Unify arguments and formal argument types
			Substitution substArgs = Type.unify(argsInfered.first, ((TypeArrow) funInfered.first).ltype);
			substArgs = substArgs.union(argsInfered.second);
			substArgs = substArgs.union(funInfered.second);

			TypeArrow funType = (TypeArrow) funInfered.first.apply(substArgs);

			return new Pair<Type, Substitution>(funType.rtype, substArgs);
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof AbstractionApplication) {
			AbstractionApplication o = (AbstractionApplication) other;
			int c = this.fun.compareTo(o.fun);
			if (c != 0)
				return c;
			return this.args.compareTo(o.args);
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof AbstractionApplication) {
			return this.fun.equals(((AbstractionApplication) other).fun)
					&& this.args.equals(((AbstractionApplication) other).args);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.fun.hashCode() * this.args.hashCode();
	}

	@Override
	protected String applicationToClojure(Tuple convertedArgs, Environment env) throws AppendableException {
		StringBuilder s = new StringBuilder("(");
		s.append(AbstractionApplication.clojureEapply);
		s.append(" ");

		s.append(this.fun.toClojureCode(env));

		s.append(" ");
		s.append(convertedArgs.toClojureCode(env));
		
		s.append(" ");
		s.append(AbstractionApplication.clojureRankingFunction);
		
		s.append(")");

		return s.toString();
	}

	@Override
	protected String applicatedToString() {
		return this.fun.toString();
	}

	@Override
	protected Expression apply(Tuple convertedArgs, Environment evaluationEnvironment) throws AppendableException {
		Expression ifun = fun.interpret(evaluationEnvironment);

		if (!(ifun instanceof Abstraction)) {
			throw new AppendableException(ifun.toString() + "is not an abstration");
		}
		Abstraction abst = (Abstraction) ifun;

		Tuple interpretedArgs = (Tuple) convertedArgs.interpret(evaluationEnvironment);

		return abst.substituteAndEvaluate(interpretedArgs, evaluationEnvironment);
	}
	
	public static final String clojureRankingFunction = /*"ranking-function";*/
			"(fn [v1 v2] (reduce + (map (fn [x y] (if (= x y) 0 1)) v1 v2)))";

	/**
	 * code of eapply functionn for clojure
	 */
	public static final String clojureEapply = /* "eapply";*/ 
			"(fn [abstraction arguments ranking-function]"
				+ "(letfn [ "
						+ "(implementation-arg-type "
							+ "[implementation] "
							+ "(:arg-type (:lang-type (meta implementation)))) "
						+ "(rank-implementations "
							+ "[v implementations ranking-function] "
							+ "(map "
								+ "(fn [u] [(ranking-function (implementation-arg-type u) v) u]) "
								+ "implementations)) "
						+ "(select-implementation "
								+ "[type abstraction ranking-function] "
								+ "(get (reduce "
										+ "(fn [x y] (if (< (get x 0) (get y 0)) x y)) "
										+ "(rank-implementations type abstraction ranking-function)) 1))] "
						+ "(apply "
							+ "(select-implementation (:lang-type (meta arguments)) abstraction ranking-function) "
							+ "arguments)))";							

}
