package velka.core.abstraction;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.util.DeclarableInTypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.util.AppendableException;
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
	public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		TypeArrow ta = (TypeArrow)p.first;
		TypeAtom from = (TypeAtom)((TypeTuple)ta.ltype).get(0);
		TypeAtom to = (TypeAtom)ta.rtype;
		typeEnv.addConversion(from, to, this, this.cost());
	}

}
