package velka.lang.application;

import java.util.Arrays;
import java.util.stream.Collectors;

import velka.lang.abstraction.Abstraction;
import velka.lang.expression.Expression;
import velka.lang.expression.Tuple;
import velka.lang.types.RepresentationOr;
import velka.lang.types.Substitution;
import velka.lang.types.Type;
import velka.lang.types.TypeArrow;
import velka.lang.types.TypeTuple;
import velka.lang.types.TypeVariable;
import velka.lang.util.AppendableException;
import velka.lang.util.NameGenerator;
import velka.lang.util.Pair;
import velka.lang.interpretation.Environment;
import velka.lang.interpretation.TypeEnvironment;

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
	protected TypeTuple getFunArgsType(TypeTuple argsType, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Type funInfered = this.fun.infer(env, typeEnv).first;
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
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Pair<Type, Substitution> funInfered = this.fun.infer(env, typeEnv);
			
			Pair<Type, Substitution> argsInfered = this.args.infer(env, typeEnv);

			if(!(funInfered.first instanceof TypeArrow)) {
				// Find best implementation if applicable
				if (funInfered.first instanceof RepresentationOr) {
					Type best = AbstractionApplication.getBestImplementationType((TypeTuple) argsInfered.first,
							(RepresentationOr) funInfered.first);
					funInfered = new Pair<Type, Substitution>(best, funInfered.second);
				}
				// If type of function is unknown (unresolved variable) assume type arrow
				else if (funInfered.first instanceof TypeVariable) {
					TypeArrow ta = new TypeArrow(new TypeVariable(NameGenerator.next()), new TypeVariable(NameGenerator.next()));
					Substitution sta = new Substitution(Arrays.asList(new Pair<TypeVariable, Type>((TypeVariable)funInfered.first, ta)));
					funInfered = new Pair<Type, Substitution>(ta, sta);
				}
				else {
					throw new AppendableException(funInfered.first.toString() + " is not applicable type! In:" + this.toString());
				}
			}

			// Unify arguments and formal argument types
			Substitution substArgs = Type.unifyTypes(argsInfered.first, ((TypeArrow) funInfered.first).ltype);
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
	protected String applicationToClojure(Tuple convertedArgs, Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder s = new StringBuilder("(");
		s.append(AbstractionApplication.clojureEapply);
		s.append(" ");

		s.append(this.fun.toClojureCode(env, typeEnv));

		s.append(" ");
		s.append(convertedArgs.toClojureCode(env, typeEnv));
		
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
	protected Expression apply(Tuple convertedArgs, Environment evaluationEnvironment, TypeEnvironment typeEnv) throws AppendableException {
		Expression ifun = fun.interpret(evaluationEnvironment, typeEnv);

		if (!(ifun instanceof Abstraction)) {
			throw new AppendableException(ifun.toString() + "is not an abstration");
		}
		Abstraction abst = (Abstraction) ifun;

		Tuple interpretedArgs = (Tuple) convertedArgs.interpret(evaluationEnvironment, typeEnv);

		return abst.substituteAndEvaluate(interpretedArgs, evaluationEnvironment, typeEnv);
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
							+ "(.ltype (:lang-type (meta implementation)))) "
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
