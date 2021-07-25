package velka.core.application;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import velka.core.abstraction.Abstraction;
import velka.core.abstraction.Operator;
import velka.core.abstraction.Operators;
import velka.core.conversions.Conversions;
import velka.core.exceptions.InvalidNumberOfArgumentsException;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.interpretation.ClojureCoreSymbols;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitComposite;
import velka.core.literal.LitInteger;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;
import velka.util.ThrowingFunction;
import velka.types.RepresentationOr;
import velka.types.Substitution;
import velka.types.SubstitutionCollectionCannotBeMerged;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
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
	 * Ranking function for this abstraction application
	 */
	public final Optional<Expression> rankingFunction;

	public AbstractionApplication(Expression fun, Expression args) {
		super(args);
		this.fun = fun;
		this.rankingFunction = Optional.empty();
	}

	public AbstractionApplication(Expression fun, Expression args, Expression rankingFunction) {
		super(args);
		this.fun = fun;
		this.rankingFunction = Optional.of(rankingFunction);
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		// Interpret the abstraction
		Expression ifun = this.fun.interpret(env, typeEnv);

		if (!(ifun instanceof Abstraction)) {
			throw new AppendableException(ifun.toString() + "is not an abstration");
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
		abst = abst.selectImplementation(iArgs, this.rankingFunction, env, typeEnv);

		// Convert arguments to specific representations
		Pair<Type, Substitution> abstInfered = abst.infer(env, typeEnv);
		Type abstArgsType = ((TypeArrow) abstInfered.first).ltype;

		int expectedNumberOfArgs = ((TypeTuple) abstArgsType).size();
		int providedNumberOfArgs = ((TypeTuple) iArgsInfered.first).size();

		if (expectedNumberOfArgs != providedNumberOfArgs) {
			throw new InvalidNumberOfArgumentsException(expectedNumberOfArgs, iArgs, this);
		}

		Expression cArgs = Conversions.convert(iArgsInfered.first, iArgs, abstArgsType, typeEnv);
		Tuple cArgsTuple = (Tuple) cArgs.interpret(env, typeEnv);

		// Finally evaluate application
		return abst.substituteAndEvaluate(cArgsTuple, env, typeEnv, this.rankingFunction);
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
	private static Pair<Type, Substitution> inferResultType(TypeTuple argsType, Substitution argsSubst,
			TypeArrow abstType, Substitution abstSubst) throws AppendableException {
		Optional<Substitution> s = Type.unifyTypes(argsType, abstType.ltype);
		if(s.isEmpty()) {
			throw new TypesDoesNotUnifyException(argsType, abstType.ltype);
		}
		
		//Why this was not assigned?, check during testing
		Optional<Substitution> tmp = s.get().union(argsSubst);
		if(tmp.isEmpty()) {
			throw new SubstitutionsCannotBeMergedException(s.get(), argsSubst);
		}
		s = tmp;
		
		tmp = s.get().union(abstSubst);
		if(tmp.isEmpty()) {
			throw new SubstitutionsCannotBeMergedException(s.get(), abstSubst);
		}
		s = tmp;

		TypeArrow finalFnType = (TypeArrow) abstType.apply(s.get());
		return new Pair<Type, Substitution>(finalFnType.rtype, s.get());
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		try {
			Pair<Type, Substitution> funInfered = this.fun.infer(env, typeEnv);
			Pair<Type, Substitution> argsInfered = this.args.infer(env, typeEnv);

			if (funInfered.first instanceof TypeArrow) {
				return AbstractionApplication.inferResultType((TypeTuple) argsInfered.first, argsInfered.second,
						(TypeArrow) funInfered.first, funInfered.second);
			}
			if (funInfered.first instanceof TypeVariable) {
				TypeArrow abstArrowType = new TypeArrow(new TypeVariable(NameGenerator.next()),
						new TypeVariable(NameGenerator.next()));
				Substitution s = new Substitution(
						new Pair<TypeVariable, Type>((TypeVariable) funInfered.first, abstArrowType));
				Optional<Substitution> tmp = s.union(funInfered.second);
				if(tmp.isEmpty()) {
					throw new SubstitutionsCannotBeMergedException(s, funInfered.second);
				}
				
				return AbstractionApplication.inferResultType((TypeTuple) argsInfered.first, argsInfered.second,
						abstArrowType, tmp.get());
			}
			if (funInfered.first instanceof RepresentationOr && ((RepresentationOr) funInfered.first)
					.getRepresentations().stream().allMatch(x -> x instanceof TypeArrow)) {
				RepresentationOr abstRepOr = (RepresentationOr) funInfered.first;
				List<Pair<Type, Substitution>> l = null;
				try {
					l = abstRepOr.getRepresentations().stream().map(ThrowingFunction.wrapper(x -> {
						return AbstractionApplication.inferResultType((TypeTuple) argsInfered.first, argsInfered.second,
								(TypeArrow) x, funInfered.second);
					})).collect(Collectors.toList());
				} catch (RuntimeException re) {
					if (re.getCause() instanceof AppendableException) {
						throw (AppendableException) re.getCause();
					}
					throw re;
				}

				List<Substitution> substs = l.stream().map(x -> x.second).collect(Collectors.toList()); 
				Optional<Substitution> finalSubst = Substitution
						.unionMany(substs);
				
				if(finalSubst.isEmpty()) {
					throw new SubstitutionCollectionCannotBeMerged(substs);
				}
				
				Type finalType = RepresentationOr
						.makeRepresentationOr(l.stream().map(x -> x.first).collect(Collectors.toList()));
				return new Pair<Type, Substitution>(finalType, finalSubst.get());
			}

			throw new AppendableException(funInfered.first.toString() + " is not applicable type!");
		} catch (AppendableException e) {
			e.appendMessage(" in " + this);
			throw e;
		}
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		StringBuilder sb = new StringBuilder();
		
		sb.append("(");
		sb.append(ClojureCoreSymbols.eapplyClojureSymbol_full);
		sb.append(" \n");
		sb.append(this.fun.toClojureCode(env, typeEnv));
		sb.append(" \n");
		sb.append(this.args.toClojureCode(env, typeEnv));
		
		if(this.rankingFunction.isPresent()) {
			sb.append(" \n");
			sb.append(this.rankingFunction.get().toClojureCode(env, typeEnv));
		}
		
		sb.append(")");
		
		return sb.toString();
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
			if (this.rankingFunction.isEmpty() && o.rankingFunction.isEmpty())
				return 0;
			if (this.rankingFunction.isEmpty() && o.rankingFunction.isPresent())
				return -1;
			if (this.rankingFunction.isPresent() && o.rankingFunction.isEmpty())
				return 1;
			c = this.rankingFunction.get().compareTo(o.rankingFunction.get());
			return c;
		}
		return super.compareTo(other);
	}

	@Override
	public boolean equals(Object other) {
		if (other instanceof AbstractionApplication) {
			return this.fun.equals(((AbstractionApplication) other).fun)
					&& this.args.equals(((AbstractionApplication) other).args)
					&& this.rankingFunction.equals(((AbstractionApplication) other).rankingFunction);
		}
		return false;
	}

	@Override
	public int hashCode() {
		return this.fun.hashCode() * this.args.hashCode() * this.rankingFunction.hashCode();
	}

	@Override
	protected String applicatedToString() {
		return this.fun.toString();
	}

	/**
	 * default ranking function
	 */
	public static final Operator defaultRanking = new Operator() {

		@Override
		protected Expression doSubstituteAndEvaluate(Tuple args, Environment env, TypeEnvironment typeEnv,
				Optional<Expression> rankingFunction) throws AppendableException {
			LitComposite formalArgList = (LitComposite) args.get(0);
			LitComposite realArgList = (LitComposite) args.get(1);

			int acc = 0;
			Tuple formalArgCons = (Tuple) formalArgList.value;
			Tuple realArgCons = (Tuple) realArgList.value;

			while (!formalArgCons.equals(Tuple.EMPTY_TUPLE) && !realArgCons.equals(Tuple.EMPTY_TUPLE)) {
				Expression formalArg = formalArgCons.get(0);
				Expression realArg = realArgCons.get(0);
				Type formalArgType = formalArg.infer(env, typeEnv).first;
				Type realArgType = realArg.infer(env, typeEnv).first;
				
				if(Type.unifyRepresentation(formalArgType, realArgType).isEmpty()) {
					acc++;
				}

				formalArgCons = (Tuple) ((LitComposite) formalArgCons.get(1)).value;
				realArgCons = (Tuple) ((LitComposite) realArgCons.get(1)).value;
			}

			return new LitInteger(acc);
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			Type t = new TypeArrow(new TypeTuple(TypeAtom.TypeListNative, TypeAtom.TypeListNative),
					TypeAtom.TypeIntNative);
			return new Pair<Type, Substitution>(t, Substitution.EMPTY);
		}

		@Override
		public String toString() {
			return "defaultRankingFunction";
		}

		@Override
		protected String toClojureOperator(Environment env, TypeEnvironment typeEnv) throws AppendableException {
			return 
					"(fn [formalArgList realArgList]\n" + 
					"    (letfn [\n" + 
					"        (is-list-empty [l] (= [] (first l)))\n" + 
					"        (list-head [l] (first (first l)))\n" + 
					"        (list-tail [l] (second (first l)))\n" + 
					"        (equal-heads [formalArgList realArgList]\n" + 
					"            (if (.isPresent \n" +  
					"                        (velka.types.Type/unifyRepresentation\n" + 
					"                            (" + ClojureCoreSymbols.getTypeClojureSymbol_full + " (list-head formalArgList))\n" + 
					"                            (" + ClojureCoreSymbols.getTypeClojureSymbol_full + " (list-head realArgList)))) 0 1))\n" +  
					"        (aggregate [formalArgList realArgList]\n" + 
					"            (if (or (is-list-empty formalArgList) (is-list-empty realArgList))\n" + 
					"                0\n" + 
					"                (+\n"+ 
					"                    (equal-heads formalArgList realArgList)\n" + 
					"                    (aggregate (list-tail formalArgList) (list-tail realArgList)))))]\n" + 
					LitInteger.clojureIntToClojureLitInteger("(aggregate formalArgList realArgList)") + "))";	
		}

		@Override
		public Symbol getClojureSymbol() {
			return new Symbol("velka-default-ranking", Operators.NAMESPACE);
		}

	};
}
