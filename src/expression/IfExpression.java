package expression;

import types.Substitution;
import types.Type;
import types.TypeArrow;
import types.TypeConcrete;
import types.TypeTuple;
import types.TypeVariable;
import util.AppendableException;
import util.NameGenerator;
import util.Pair;

import interpretation.Environment;

/**
 * Expression for special form if
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class IfExpression extends Application {
	
	public IfExpression(Tuple args) {
		super(IfWrapper.singleton, args);
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression condition = this.args.values[0];
		Expression trueBranch = this.args.values[1];
		Expression falseBranch = this.args.values[2];
		
		LitBoolean b = (LitBoolean) condition.interpret(env);
		if (b.value) {
			return trueBranch.interpret(env);
		} else {
			return falseBranch.interpret(env);
		}
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof IfExpression) {			
			return this.args.compareTo(((IfExpression) other).args);
		}
		return super.compareTo(other);
	}
	
	/**
	 * Wrapper for if
	 * @author Mgr. Radomir Skrabal
	 *
	 */
	private static final class IfWrapper extends Expression{
		
		public static final IfWrapper singleton = new IfWrapper();

		@Override
		public Expression interpret(Environment env) throws Exception {
			return this;
		}

		@Override
		public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
			TypeVariable branchType = new TypeVariable(NameGenerator.next());
			TypeTuple argsType = new TypeTuple(new Type[] {TypeConcrete.TypeBool, branchType, branchType});
			
			return new Pair<Type, Substitution>(new TypeArrow(argsType, branchType), new Substitution());
		}

		@Override
		public String toClojureCode() throws Exception {
			return "if";
		}
		
	}
}
