package velka.core.abstraction;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.core.application.AbstractionApplication;
import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.literal.LitDouble;
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
 * Class for Velka conversion operators
 * @author Mgr. Radomir Skrabal
 *
 */
public abstract class Conversion extends Operator implements DeclarableInTypeEnvironment {

	/** Returns the cost of the conversion */
	public abstract Expression cost();
	
	/**
	 * Declares this conversion in TypeEnvironment
	 * 
	 * @remark Most operators use trivial infer function, returing a constant type
	 *         and empty substitution. This is exploited by default implementation of
	 *         Conversion, counting on fact that env and typeEnv will never be used
	 *         on infer call. If inference of constructor is not trivial, this
	 *         method should be overridden.
	 * @param typeEnv where is declared
	 * @throws AppendableException
	 */
	public void declareInTypeEnvironment(Environment env) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env);
		TypeArrow ta = (TypeArrow)p.first;
		TypeAtom from = (TypeAtom)((TypeTuple)ta.ltype).get(0);
		TypeAtom to = (TypeAtom)ta.rtype;
		final var me = this;
		
		env.getTypeSystem().addConversion(from, to, 
				new velka.types.typeSystem.IEvalueable() {

					@Override
					public Object evaluate(Collection<? extends Object> args, Object env) {
						var eargs = new ArrayList<Expression>(args.size());
						args.stream().forEach(o -> eargs.add((Expression)o));
						
						var appl = new AbstractionApplication(me, new Tuple(eargs));
						
						try {
							var eenv = (Environment)env;
							return appl.interpret(eenv);
						} catch (AppendableException e) {
							throw new RuntimeException(e);
						}
					}
			
				}, 
				new velka.types.typeSystem.IEvalueable() {

					@Override
					public Object evaluate(Collection<? extends Object> args, Object env) {
						var eargs = new ArrayList<Expression>(args.size());
						args.stream().forEach(o -> eargs.add((Expression)o));
						
						var appl = new AbstractionApplication(me.cost(), new Tuple(eargs));
						
						try {
							var eenv = (Environment)env;
							var r = appl.interpret(eenv);
							if(r instanceof LitDouble ld) {
								return Double.valueOf(ld.value);
							}
							throw new RuntimeException("Invalid cost for conversion operator " + me);
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
		TypeAtom from = (TypeAtom)((TypeTuple)ta.ltype).get(0);
		TypeAtom to = (TypeAtom)ta.rtype;
		String arg = "_arg", rhis = "_rhis", cenv = "_env";
		
		String code;
		try {
			code = ClojureHelper.applyClojureFunction(".addConversion", 
					ClojureCoreSymbols.typeSystem_full,
					from.clojureTypeRepresentation(),
					to.clojureTypeRepresentation(),
					ClojureHelper.reify(velka.types.typeSystem.IEvalueable.class, 
							Pair.of("evaluate", Pair.of(List.of(rhis, arg, cenv), ClojureHelper.applyVelkaFunction_argsTuple(
									super.toClojureCode(env), 
									arg)))),
					ClojureHelper.reify(velka.types.typeSystem.IEvalueable.class, 
							Pair.of("evaluate", Pair.of(List.of(rhis, arg, cenv), ClojureHelper.applyVelkaFunction_argsTuple(
									this.cost().toClojureCode(env), 
									arg)))));
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
		return code;
	}

}
