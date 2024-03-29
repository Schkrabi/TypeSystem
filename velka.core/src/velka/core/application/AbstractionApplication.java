package velka.core.application;

import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import velka.core.abstraction.Abstraction;
import velka.core.exceptions.InvalidNumberOfArgumentsException;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.ThrowingFunction;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;

/**
 * Expression for function application in form (fun arg1 arg2 ...)
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class AbstractionApplication extends Application {
	
	/**
	 * Symbol for eapply special form
	 */
	public static final String EAPPLY = "eapply";

	/**
	 * Expression that should yield function
	 */
	public final Expression fun;

	/**
	 * Cost function for this abstraction application
	 */
	public final Optional<Expression> costFunction;

	public AbstractionApplication(Expression fun, Expression args) {
		super(args);
		this.fun = fun;
		this.costFunction = Optional.empty();
	}

	public AbstractionApplication(Expression fun, Expression args, Expression costFunction) {
		super(args);
		this.fun = fun;
		this.costFunction = Optional.of(costFunction);
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		// Interpret the abstraction
		Expression ifun = this.fun.interpret(env, typeEnv);

		if (!(ifun instanceof Abstraction)) {
			throw new AppendableException(ifun.toString() + " is not an abstration");
		}
		Abstraction abst = (Abstraction) ifun;

		// Interpret arguments
		Expression intArgs = this.args.interpret(env, typeEnv);
		if(!(intArgs instanceof Tuple)) {
			throw new AppendableException("Invalid argument expression " + intArgs.toString() + " in " + this.toString());
		}
		
		Tuple iArgs = (Tuple)intArgs; 
		Pair<Type, Substitution> iArgsInfered = iArgs.infer(env, typeEnv);

		// Select implementation to use (if applicable)
		abst = abst.selectImplementation(iArgs, this.costFunction, env, typeEnv);

		// Convert arguments to specific representations
		Pair<Type, Substitution> abstInfered = abst.inferWithArgs(iArgs, env, typeEnv);
		Type abstArgsType = ((TypeArrow) abstInfered.first).ltype;

		int expectedNumberOfArgs = ((TypeTuple) abstArgsType).size();
		int providedNumberOfArgs = ((TypeTuple) iArgsInfered.first).size();

		if (expectedNumberOfArgs != providedNumberOfArgs) {
			throw new InvalidNumberOfArgumentsException(expectedNumberOfArgs, iArgs, this);
		}

		Expression cArgs = iArgs.convert(abstArgsType, env, typeEnv);
		Tuple cArgsTuple = (Tuple) cArgs.interpret(env, typeEnv);

		// Finally evaluate application
		return abst.substituteAndEvaluate(cArgsTuple, env, typeEnv, this.costFunction);
	}

	/**
	 * For given types and substitutions infers result of the abstraction
	 * application
	 * 
	 * @param argsType  application arguments type
	 * @param argsSubst application arguments inference substitution
	 * @param abstType  application abstraction type
	 * @param abstSubst application abstraction substitution
	 * @return Pair of type and substitution
	 * @throws AppendableException
	 */
	private static Pair<Type, Substitution> inferResultType(Type argsType, Substitution argsSubst,
			TypeArrow abstType, Substitution abstSubst) throws AppendableException {
		Optional<Substitution> s = Type.unifyTypes(argsType, abstType.ltype);
		if(s.isEmpty()) {
			throw new TypesDoesNotUnifyException(argsType, abstType.ltype);
		}
		
		//Remove unbound abstType substitutions
		Set<TypeVariable> unbound = abstType.getVariables();
		Substitution r = s.get().stream()
				.filter(p -> !unbound.contains(p.first))
				.collect(Substitution.toSubstitution);
		
		Optional<Substitution> composed = r.merge(argsSubst);
		if(composed.isEmpty()) {
			throw new SubstitutionsCannotBeMergedException(s.get(), argsSubst);
		}
		composed = composed.get().merge(abstSubst);
		if(composed.isEmpty()) {
			throw new SubstitutionsCannotBeMergedException(composed.get(), abstSubst);
		}
		
		TypeArrow finalFnType = (TypeArrow) abstType
				.apply(s.get())
				.apply(argsSubst)
				.apply(abstSubst);
		
		return new Pair<Type, Substitution>(finalFnType.rtype, composed.get());
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Pair<Type, Substitution> funInfered = this.fun.infer(env, typeEnv);
			Pair<Type, Substitution> argsInfered = this.args.infer(env, typeEnv);

			if (funInfered.first instanceof TypeArrow) {				
				return AbstractionApplication.inferResultType(argsInfered.first, argsInfered.second,
						(TypeArrow) funInfered.first, funInfered.second);
			}
			if (funInfered.first instanceof TypeVariable) {
				TypeArrow abstArrowType = new TypeArrow(new TypeVariable(NameGenerator.next()),
						new TypeVariable(NameGenerator.next()));
				Substitution s = new Substitution(
						new Pair<TypeVariable, Type>((TypeVariable) funInfered.first, abstArrowType));
				/*Optional<Substitution> tmp = s.union(funInfered.second);
				if(tmp.isEmpty()) {
					throw new SubstitutionsCannotBeMergedException(s, funInfered.second);
				}*/
				Optional<Substitution> tmp = Optional.of(s.compose(funInfered.second));
				
				return AbstractionApplication.inferResultType(argsInfered.first, argsInfered.second,
						abstArrowType, tmp.get());
			}
			if (funInfered.first instanceof RepresentationOr && ((RepresentationOr) funInfered.first)
					.getRepresentations().stream().allMatch(x -> x instanceof TypeArrow)) {
				RepresentationOr abstRepOr = (RepresentationOr) funInfered.first;
				List<Pair<Type, Substitution>> l = null;
				try {
					l = abstRepOr.getRepresentations().stream().map(ThrowingFunction.wrapper(x -> {
						return AbstractionApplication.inferResultType(argsInfered.first, argsInfered.second,
								(TypeArrow) x, funInfered.second);
					})).collect(Collectors.toList());
				} catch (RuntimeException re) {
					if (re.getCause() instanceof AppendableException) {
						throw (AppendableException) re.getCause();
					}
					throw re;
				}

				List<Substitution> substs = l.stream().map(x -> x.second).collect(Collectors.toList()); 
				
				Substitution finalSubst = substs.stream().reduce(Substitution.EMPTY, (agg, s) -> agg.compose(s));
				
				Type finalType = RepresentationOr
						.makeRepresentationOr(l.stream().map(x -> x.first).collect(Collectors.toList()));
				return new Pair<Type, Substitution>(finalType, finalSubst);
			}

			throw new AppendableException(funInfered.first.toString() + " is not applicable type!");
		} catch (AppendableException e) {
			e.appendMessage(" in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		String code = ClojureHelper.applyVelkaFunction_argsTuple(
				this.fun.toClojureCode(env, typeEnv),
				this.args.toClojureCode(env, typeEnv),
				this.costFunction.isPresent() ?
						this.costFunction.get().toClojureCode(env, typeEnv) :
						"nil");
		
		return code;
	}

	@Override
	public int compareTo(Expression other) {
		if (other instanceof AbstractionApplication) {
			AbstractionApplication o = (AbstractionApplication) other;
			int c = this.fun.compareTo(o.fun);
			if (c != 0)
				return c;
			c = this.args.compareTo(o.args);
			if (c != 0)
				return c;
			if (this.costFunction.isEmpty() && o.costFunction.isEmpty())
				return 0;
			if (this.costFunction.isEmpty() && o.costFunction.isPresent())
				return -1;
			if (this.costFunction.isPresent() && o.costFunction.isEmpty())
				return 1;
			c = this.costFunction.get().compareTo(o.costFunction.get());
			return c;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof AbstractionApplication) {
			return this.fun.equals(((AbstractionApplication) other).fun)
					&& this.args.equals(((AbstractionApplication) other).args)
					&& this.costFunction.equals(((AbstractionApplication) other).costFunction);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.fun.hashCode() * this.args.hashCode() * this.costFunction.hashCode();
	}

	@Override
	protected String applicatedToString() {
		return this.fun.toString();
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env, TypeEnvironment typeEnv)
			throws AppendableException {
		Expression intprt = this.interpret(env, typeEnv);
		return intprt.convert(to, env, typeEnv);
	}
}
