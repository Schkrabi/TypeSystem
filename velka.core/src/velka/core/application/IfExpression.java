package velka.core.application;

import velka.util.AppendableException;
import velka.util.ClojureCoreSymbols;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;

import java.util.Arrays;
import java.util.Optional;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;
import com.sun.codemodel.JMod;

import velka.core.expression.Expression;
import velka.core.expression.Tuple;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.core.literal.LitBoolean;
import velka.java.CodeModelInstance;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;

/**
 * Expression for special form if
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class IfExpression extends SpecialFormApplication implements CompileableToJava {
	
	/**
	 * Symbol for special form if
	 */
	public static final String IF = "if";
	
	public IfExpression(Expression condition, Expression trueBranch, Expression falseBranch) {
		super(new Tuple(Arrays.asList(condition, trueBranch, falseBranch)));
	}

	/**
	 * Gets condition of this ifExpression
	 * 
	 * @return expression
	 */
	protected Expression getCondition() {
		return ((Tuple)this.args).get(0);
	}

	/**
	 * Gets true branch of this ifExpression
	 * 
	 * @return expression
	 */
	protected Expression getTrueBranch() {
		return ((Tuple)this.args).get(1);
	}

	/**
	 * Gets false branch of this ifExpression
	 * 
	 * @return
	 */
	protected Expression getFalseBranch() {
		return ((Tuple)this.args).get(2);
	}
	
	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Expression iCond = this.getCondition().interpret(env);
		if(!(iCond instanceof LitBoolean)) {
			iCond = (Expression)env.getTypeSystem().convert(
						env.getTypeSystem().getType(iCond),
						TypeAtom.TypeBoolNative,
						iCond,
						env);
		}
		
		if(!(iCond instanceof LitBoolean)) {
			throw new AppendableException(iCond + " not a LitBoolean");
		}
		LitBoolean cond = (LitBoolean)iCond;
		if(cond.value) {
			return this.getTrueBranch().interpret(env);
		}
		
		Expression iFalse = this.getFalseBranch().interpret(env);
		Pair<Type, Substitution> inf = this.infer(env);
		
		iFalse = (Expression)env.getTypeSystem().convert(
									env.getTypeSystem().getType(iFalse),
									inf.first,
									iFalse,
									env);
		return iFalse;
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Pair<Type, Substitution> argsInfered = this.args.infer(env);
		TypeVariable tv = new TypeVariable(NameGenerator.next());
		Type argsExpected = new TypeTuple(Arrays.asList(TypeAtom.TypeBoolNative, tv, tv));
		
		Optional<Substitution> s = Type.unifyTypes(argsInfered.first, argsExpected);
		if(s.isEmpty()) {
			throw new TypesDoesNotUnifyException(argsInfered.first, argsExpected);
		}

		Substitution composed = s.get().compose(argsInfered.second);
		
		return new Pair<Type, Substitution>(tv.apply(composed), composed);
	}
	
	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		String cond = null;
		var ct = this.getCondition().infer(env).first;
		if(ct.equals(TypeAtom.TypeBoolNative)) {
			cond = this.getCondition().toClojureCode(env);
		}
		else {
			cond = ClojureHelper.applyClojureFunction(ClojureCoreSymbols.convertClojureSymbol_full, 
					this.getCondition().toClojureCode(env));
		}
		
		var code = ClojureHelper.clojureIfHelper(
				cond, 
				this.getTrueBranch().toClojureCode(env), 
				this.getFalseBranch().toClojureCode(env));
		
		return code;
	}

	@Override
	protected String applicatedToString() {
		return IF;
	}
	
	@Override
	public boolean equals(Object other) {
		if (other instanceof IfExpression) {
			return super.equals(other);
		}
		return false;
	}

	@Override
	public JExpression toJavaExpr(Environment env) {
		var cond = (CompileableToJava)this.getCondition();
		var then = (CompileableToJava)this.getTrueBranch();
		var _else = (CompileableToJava)this.getFalseBranch();
		
		var jcond = cond.toJavaExpr(env);
		var jthen = then.toJavaExpr(env);
		var jelse = _else.toJavaExpr(env);
		
		var spcl = CodeModelInstance.instance().anonymousClass(java.util.function.Supplier.class);
		var _get = spcl.method(JMod.PUBLIC, CodeModelInstance.instance()._ref(Object.class), "get");
		
		var _if = _get.body()._if(JExpr.cast(CodeModelInstance.instance().ref(Boolean.class), jcond));
		_if._then()._return(jthen);
		_if._else()._return(jelse);
		
		var e = JExpr._new(spcl).invoke(_get);
		return e;
	}
}
