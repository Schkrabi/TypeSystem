package velka.core.abstraction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.core.application.AbstractionApplication;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.util.DeclarableInTypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Class for constructors
 * 
 * @author r.skrabal
 *
 */
public abstract class Constructor extends Operator implements DeclarableInTypeEnvironment {

	/**
	 * Declares this constructor in TypeEnvironment
	 * 
	 * @remark Most operators use trivial infer function, returing a constant type
	 *         and empty substituion. This is exploited by default implementation of
	 *         Constructor, counting on fact that env and typeEnv will never be used
	 *         on infer call. If inference of constructor is not trivial, this
	 *         method should be overriden.
	 * @param typeEnv where is delcared
	 * @throws AppendableException
	 */
	public void declareInTypeEnvironment(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env);
		TypeArrow ta = (TypeArrow)p.first;
		TypeAtom constructed = (TypeAtom)ta.rtype;
		final var me = this;
		
		env.getTypeSystem().addConstructor(
				constructed, 
				(TypeTuple)ta.ltype,
				new velka.types.typeSystem.IEvalueable() {

					@Override
					public Object evaluate(Collection<? extends Object> args, Object env) {
						var eargs = new ArrayList<Expression>();
						args.stream().forEach(o -> eargs.add((Expression)o));
						
						var apl = new AbstractionApplication(me, new Tuple(eargs));
						try {
							var eenv = (Environment)env;
							return apl.interpret(eenv);
						} catch (AppendableException e) {
							throw new RuntimeException(e);
						}
					}
					
		});
	}

	@Override
	public String toClojureCode(Environment env) {
		Pair<Type, Substitution> p;
		try {
			p = this.infer(env);
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		TypeArrow ta = (TypeArrow)p.first;
		TypeAtom constructed = (TypeAtom)ta.rtype;
		
		final var arg = "_arg";
		final var rhis = "_this";
		final var cenv = "_env";
		String code;
		try {
			code = ClojureHelper.applyClojureFunction(".addConstructor", 
					ClojureCoreSymbols.typeSystem_full,
					constructed.clojureTypeRepresentation(),
					ta.ltype.clojureTypeRepresentation(),
					ClojureHelper.reify(velka.types.typeSystem.IEvalueable.class, 
							Pair.of("evaluate", Pair.of(List.of(rhis, arg, cenv), 
									ClojureHelper.applyVelkaFunction_argsTuple(super.toClojureCode(env), 
											arg)))));
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}				
		
		return code;
	}
}
