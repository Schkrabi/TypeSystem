package velka.core.abstraction;

import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.util.DeclarableInTypeEnvironment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.util.AppendableException;
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
	public void declareInTypeEnvironment(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> p = this.infer(env, typeEnv);
		TypeArrow ta = (TypeArrow)p.first;
		TypeAtom constructed = (TypeAtom)ta.rtype;
		typeEnv.addConstructor(constructed, this, env);
	}

}
