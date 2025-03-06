package velka.core.application;

import java.util.Collection;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.core.literal.LitInteger;
import velka.java.CodeModelInstance;
import velka.java.runtime.VelkaTuple;
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
public class Get extends SpecialFormApplication implements CompileableToJava {
	
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
	public Expression interpret(Environment env) throws AppendableException {
		Expression expressionTuple = this.getTuple().interpret(env);
		Expression expressionIndex = this.getIndex().interpret(env);
		
		if(!(expressionTuple instanceof Tuple)) {
			throw new AppendableException("First argument of get must interpret to tuple. Got " + expressionTuple.toString());
		}
		@SuppressWarnings("unchecked")
		var index = (LitInteger)env.getTypeSystem().convert(
				env.getTypeSystem().getType(expressionIndex), 
				TypeAtom.TypeIntNative, 
				expressionIndex, 
				env);
		
		Tuple tuple = (Tuple)expressionTuple;
		
		Expression e = tuple.get((int) index.value);
		
		return e;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> tuplePair = this.getTuple().infer(env);
		Pair<Type, Substitution> indexPair = this.getIndex().infer(env);
		
		Type.unifyTypes(indexPair.first, TypeAtom.TypeIntNative);
		if(!((tuplePair.first instanceof TypeTuple) || (tuplePair.first instanceof TypeVariable))) {
			throw new AppendableException("First argument of get must infer to TypeTuple. Got " + tuplePair.first);
		}
		
		return new Pair<Type, Substitution>(new TypeVariable(NameGenerator.next()), Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		String idx = null;
		var t = this.getIndex().infer(env).first;
		if(t.equals(TypeAtom.TypeIntNative)) {
			idx = this.getIndex().toClojureCode(env);
		}
		else {
			idx = ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol_full, 
					this.getIndex().toClojureCode(env));
		}
		
		String code = ClojureHelper.applyClojureFunction("get", 
				this.getTuple().toClojureCode(env),
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

	@Override
	public JExpression toJavaExpr(Environment env) {
		var tCl = CodeModelInstance.instance().ref(VelkaTuple.class);
		var ctjT = (CompileableToJava)this.getTuple();
		var ctjI = (CompileableToJava)this.getIndex();
		
		return JExpr.cast(tCl, ctjT.toJavaExpr(env)).invoke("get").arg(ctjI.toJavaExpr(env));
	}

}
