package velka.core.application;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.core.literal.LitInteger;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Special form get for getting values from tuples
 * @author Mgr. Radomir Skrabal
 *
 */
public class Get extends SpecialFormApplication {
	
	/**
	 * Symbol for special form get
	 */
	public static final String GET = "get";

	private Get(Tuple tuple) {
		super(tuple);
	}
	
	public Expression getTuple() {
		return ((Tuple)this.args).get(0);
	}
	
	public Expression getIndex() {
		return ((Tuple)this.args).get(1);
	}

	@Override
	protected String applicatedToString() {
		return GET;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Expression expressionTuple = this.getTuple().interpret(env, typeEnv);
		Expression expressionIndex = this.getIndex().interpret(env, typeEnv);
		
		if(!(expressionTuple instanceof Tuple)) {
			throw new AppendableException("First argument of get must interpret to tuple. Got " + expressionTuple.toString());
		}
		LitInteger index;
		if(expressionIndex instanceof LitInteger) {
			index = (LitInteger)expressionIndex;
		}else {
			Expression c = expressionIndex.convert(TypeAtom.TypeIntNative, env, typeEnv);
			c = c.interpret(env, typeEnv);
			
			if(!(c instanceof LitInteger)) {
				throw new AppendableException("Second argument of get must interpret to integer. Got " + expressionIndex.toString());
			}
			
			index = (LitInteger)c;
		}
		
		Tuple tuple = (Tuple)expressionTuple;
		
		Expression e = tuple.get((int) index.value);
		
		return e;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> tuplePair = this.getTuple().infer(env, typeEnv);
		Pair<Type, Substitution> indexPair = this.getIndex().infer(env, typeEnv);
		
		Type.unifyTypes(indexPair.first, TypeAtom.TypeIntNative);
		if(!(tuplePair.first instanceof TypeTuple)) {
			throw new AppendableException("First argument of get must infer to TypeTuple. Got " + tuplePair.first);
		}
		
		return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		String idx = null;
		var t = this.getIndex().infer(env, typeEnv).first;
		if(t.equals(TypeAtom.TypeIntNative)) {
			idx = this.getIndex().toClojureCode(env, typeEnv);
		}
		else {
			idx = ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol_full, 
					this.getIndex().toClojureCode(env, typeEnv));
		}
		
		String code = ClojureHelper.applyClojureFunction("get", 
				this.getTuple().toClojureCode(env, typeEnv),
				idx);
		
		return code;
	}
	
	@Override
	public boolean equals(Object other) {
		if(other instanceof Get) {
			return this.args.equals(((Get) other).args);
		}
		return false;
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Get) {
			return this.args.compareTo(((Get) other).args);
		}
		return super.compareTo(other);
	}
	
	@Override
	public int hashCode() {
		return this.args.hashCode();
	}
	
	/**
	 * Constructs Get 
	 * @param tuple expression that will evaluate to tuple
	 * @param index expression that will evaluate to LitInteger
	 * @return new Get instance
	 */
	public static Get makeGet(Expression tuple, Expression index) {
		return new Get(new Tuple(tuple, index));
	}

}
