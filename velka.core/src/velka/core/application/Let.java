package velka.core.application;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.TypeHolder;
import velka.core.interpretation.Environment;
import velka.types.Substitution;
import velka.types.Type;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.Pair;

/** Let special form */
public class Let extends Expression {
	
	private final List<Pair<Symbol, Expression>> bindings;
	public final Expression body;
	
	public Let(Expression body, Collection<Pair<Symbol, Expression>> bindings) {
		this.body = body;
		this.bindings = new ArrayList<Pair<Symbol, Expression>>(bindings);
	}
	
	@SafeVarargs
	public Let(Expression body, Pair<Symbol, Expression> ...bindings) {
		this.body = body;
		this.bindings = List.of(bindings);
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		var e = env;
		
		for(var p : this.bindings) {
			var expr = p.second.interpret(e);
			e = Environment.create(e);
			e.put(p.first, expr);
		}
		
		return this.body.interpret(e);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		var e = Environment.create(env);
		var s = Substitution.EMPTY;
		
		for(var p : this.bindings) {
			var t = p.second.infer(e);
			e = Environment.create(e);
			e.put(p.first, new TypeHolder(t.first));
			s = s.compose(t.second);
		}
		
		var t = this.body.infer(e);
		return Pair.of(t.first, t.second.compose(s));
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		String code = ClojureHelper.letHelper(
				this.body.toClojureCode(env), 
				this.bindings.stream().map(p -> {
					try {
						return Pair.of(p.first.toClojureCode(env), p.second.toClojureCode(env));
					} catch (AppendableException e) {
						throw new RuntimeException(e);
					}
				}).collect(Collectors.toList()));
		
		return code;
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		var e = this.interpret(env);
		return e.convert(to, env);
	}
	
	@Override
	public String toString() {
		return new StringBuilder()
				.append("(let (")
				.append(this.bindings.stream().map(p -> new StringBuilder()
						.append("(")
						.append(p.first)
						.append(" ")
						.append(p.second)
						.append(")")
						.toString()
						).reduce((s1, s2) -> new StringBuilder(s1).append(" ").append(s2).toString()).get())
				.append(") ")
				.append(this.body.toString())
				.append(")")
				.toString();
	}
	
	@Override
	public boolean equals(Object o) {
		if(o == null || !(o instanceof Let)) return false;
		var other = (Let)o;
		return this.body.equals(other.body)
				&& this.bindings.equals(other.bindings);
	}

	
	@Override
	public int compareTo(Expression other) {
		if (other instanceof Let) {
			var o = (Let)other;
			var cmp = this.body.compareTo(o.body);
			if(cmp != 0) return cmp;
			cmp = Integer.compare(this.bindings.size(), o.bindings.size());
			if(cmp != 0) return cmp;
			
			var it = this.bindings.iterator();
			var ot = o.bindings.iterator();
			while(cmp == 0) {
				var p = it.next();
				var op = ot.next();
				cmp = p.first.compareTo(op.first);
				if(cmp != 0) break;
				cmp = p.second.compareTo(op.second);
			}
			return cmp;
		}
		return super.compareTo(other);
	}
}
