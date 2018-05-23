package expression;

import java.util.Comparator;

import interpretation.Environment;
import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypeVariable;

/**
 * Expression for representation of interpreted function
 * @author Mgr. Radomir Skrabal
 *
 */
public class Function extends MetaFunction implements Comparable<Function>{
	
	/**
	 * Type of the function arguments
	 */
	public final TypeTuple argsType;
	/**
	 * Function arguments
	 */
	public final Tuple args;
	/**
	 * Body of the fucntion
	 */
	public final Expression body;
	
	public Function(TypeTuple argsType, Tuple args, Expression body, Environment createdEnvironment){
		super(createdEnvironment);
		this.argsType = argsType;
		this.args = args;
		this.body = body;
	}

	@Override
	public Type infer(Environment env) throws Exception {
		Type inferedArgsType = this.args.infer(new Environment());
		Type bodyType = this.body.infer(this.creationEnvironment);

		if (this.argsType != null && !Type.unify(this.argsType, inferedArgsType)) {
			throw new Exception("Infered arguments type " + inferedArgsType + " do not unify with specified args type "
					+ this.argsType + " in " + this);
		}

		Type t = new TypeArrow(this.argsType == null ? inferedArgsType : this.argsType, bodyType.getRep());

		for (TypeVariable v : t.getUnconstrainedVariables()) {
			t = new ForallType(v, t);
		}

		this.setType(t);

		return t;
	}

	@Override
	public Function getFunction() {
		return this;
	}

	@Override
	public Function getFunction(Comparator<? super Function> c) {
		return this;
	}

	@Override
	public int compareTo(Function o) {
		if(this.argsType == o.argsType) {
			return 0; 
		}
		if(this.argsType == null) {
			return 1;
		}
		if(o.argsType == null) {
			return -1;
		}
		
		return this.argsType.compareTo(o.argsType);
	}

	@Override
	public String toString() {
		return "(func " + this.args.toString() + " " + this.body.toString() + ")";
	}
}
