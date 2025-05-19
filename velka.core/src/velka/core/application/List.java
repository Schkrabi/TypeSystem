package velka.core.application;

import java.util.stream.Collectors;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;

import velka.core.expression.Expression;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.core.literal.LitInteropObject;
import velka.core.literal.Literal;
import velka.java.CodeModelInstance;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Special form to construct static lists
 */
public class List extends Expression implements CompileableToJava {

	private final java.util.List<Expression> args;
	
	private List(java.util.Collection<Expression> args) {
		this.args = new java.util.ArrayList<Expression>(args);
	}
	
	public static List of(java.util.Collection<Expression> args) {
		return new List(args);
	}
	
	public static List of(Expression ...args) {
		return new List(java.util.List.of(args));
	}
	
	@Override
	public Expression interpret(Environment env) throws AppendableException {
		var s = io.vavr.collection.Stream.ofAll(this.args.stream())
			.map(e -> {
			try {
				var exp = e.interpret(env);
				if(exp instanceof Literal lit) {
					return Literal.literalToObject(lit);
				}
				return exp;
			}catch(AppendableException ex) {
				throw new RuntimeException(ex);
			}
		});
		
		return new LitInteropObject(s, TypeAtom.TypeListNative);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		 this.args.stream().forEach(e -> {
			try {
				e.infer(env);
			} catch (AppendableException e1) {
				throw new RuntimeException(e1);
			}
		});
		return Pair.of(TypeAtom.TypeListNative, Substitution.EMPTY);
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		var argCodes = this.args.stream().map(e -> {
			try {
				return e.toClojureCode(env);
			}catch(Exception ex) {
				throw new RuntimeException(ex);
			}		
			}).collect(Collectors.toList());
		
		var code = ClojureHelper.applyClojureFunction("list", argCodes); 
		
		return code;
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env) throws AppendableException {
		throw new RuntimeException("doConvert not implemented");
	}

	@Override
	public String toString() {
		var argsStr = this.args.stream().map(e -> e.toString())
				.reduce((s1, s2) -> new StringBuilder().append(s1).append(" ").append(s2).toString());
		
		return new StringBuilder()
				.append("(list ")
				.append(argsStr)
				.append(")")
				.toString();
	}

	@Override
	public JExpression toJavaExpr(Environment env) {
		var inv = CodeModelInstance.instance().ref(io.vavr.collection.Stream.class)
				.staticInvoke("of");
		
		for(var a : this.args) {
			var ctj = (CompileableToJava)a;
			var je = ctj.toJavaExpr(env);
			inv.arg(je);
		}
		
		return inv;
	}
}
