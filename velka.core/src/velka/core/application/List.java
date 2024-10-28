package velka.core.application;

import java.util.stream.Collectors;

import velka.core.expression.Expression;
import velka.core.interpretation.Environment;
import velka.core.literal.LitInteropObject;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeAtom;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;

/**
 * Special form to construct static lists
 */
public class List extends Expression {

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
		var l = this.args.stream().map(e -> {
			try {
				return e.interpret(env);
			}catch(AppendableException ex) {
				throw new RuntimeException(ex);
			}
		}).collect(Collectors.toList());
		
		return new LitInteropObject(l, TypeAtom.TypeListNative);
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
		
		var code = Type.addTypeMetaInfo(ClojureHelper.applyClojureFunction("list", argCodes), TypeAtom.TypeListNative);
		return code;
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env) throws AppendableException {
		Expression e = this.interpret(env);
		return e.convert(to, env);
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
}
