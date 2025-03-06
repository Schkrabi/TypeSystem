	package velka.core.application;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import com.sun.codemodel.JExpression;

import velka.core.abstraction.Lambda;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.interfaces.CompileableToJava;
import velka.core.interpretation.Environment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.ClojureHelper;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Expression for loop special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Loop extends Expression implements CompileableToJava {
	
	public static final String LOOP = "loop";
	
	/**
	 * Symbol for marking recurrence point
	 */
	public static final Symbol RECUR_MARK_SYMBOL = new Symbol(NameGenerator.next());
	
	private final List<Pair<Symbol, Expression>> bindings;
	
	public final Expression body;
	
	public Loop(Expression body, Collection<Pair<Symbol, Expression>> bindings) {
		this.bindings = new ArrayList<Pair<Symbol, Expression>>(bindings);
		this.body = body;
	}
	
	private Lambda createLoopLambda(Environment env) throws AppendableException {
		Lambda l = new Lambda(this.body,
				this.bindings.stream().map(p -> {
					try {
						return Pair.of(p.first, p.second.infer(env).first);
					} catch (AppendableException e) {
						throw new RuntimeException(e);
					}
				}).toList());
		return l;
	}
	
	private Tuple getInitArgs()
	{
		return new Tuple(this.bindings.stream().map(p -> p.second).toList());
	}

	@Override
	public Expression interpret(Environment env) throws AppendableException {
		Environment recurEnvironment = Environment.create(env);
		Lambda loopExpression = this.createLoopLambda(env);		
		recurEnvironment.put(RECUR_MARK_SYMBOL, loopExpression);
		
		AbstractionApplication appl = new AbstractionApplication(loopExpression, this.getInitArgs());
		return appl.interpret(recurEnvironment);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		Environment recurEnvironment = Environment.create(env);
		TypeVariable tv = new TypeVariable(NameGenerator.next());
		recurEnvironment.put(RECUR_MARK_SYMBOL, new TypeHolder(tv));
		Lambda loopExpression = this.createLoopLambda(env);
		
		AbstractionApplication appl = new AbstractionApplication(loopExpression, this.getInitArgs());
		Pair<Type, Substitution> infered = appl.infer(recurEnvironment);		
		
		Substitution s = infered.second;
		Substitution tmp;

		if (!s.containsVariable(tv)) {
			tmp = new Substitution(Arrays.asList(new Pair<TypeVariable, Type>(tv, infered.first)));
		} else {
			Optional<Substitution> opt = Type.unifyTypes(s.get(tv).get(), infered.first);
			if(opt.isEmpty()) {
				throw new TypesDoesNotUnifyException(s.get(tv).get(), infered.first);
			}
			
			tmp = opt.get();
		}

		return new Pair<Type, Substitution>(infered.first, s.compose(tmp));
	}

	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		List<String> bds = new LinkedList<String>();
		
		for(var p : this.bindings) {
			bds.add(p.first.toClojureCode(env));
			bds.add(p.second.toClojureCode(env));
		}
		
		String code = ClojureHelper.applyClojureFunction("loop", 
				ClojureHelper.clojureVectorHelper(bds),
				this.body.toClojureCode(env));
		
		return code;
	}
	
	@Override
	public int hashCode() {
		return new StringBuilder()
				.append(this.body.hashCode())
				.append(this.bindings.hashCode())
				.toString().hashCode();
	}

	@Override
	public boolean equals(Object other) {
		if(other instanceof Loop l) {
			return this.bindings.equals(l.bindings)
					&& this.body.equals(((Loop) other).body);
		}
		return false;
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder()
				.append("(")
				.append(LOOP)
				.append(" ");
		
		
		var it = this.bindings.iterator();
		while(it.hasNext()){
			var p = it.next();
			
			sb.append("(")
				.append(p.first.toString())
				.append(" ")
				.append(p.second.toString())
				.append(")");
			
			if(it.hasNext()) {
				sb.append(" ");
			}
		}
		
		sb.append(this.body.toString())
			.append(")");
		return sb.toString();
	}

	@Override
	protected Expression doConvert(Type from, Type to, Environment env)
			throws AppendableException {
		throw new RuntimeException("Loop does not have implemented doCOnvert");
	}

	@Override
	public JExpression toJavaExpr(Environment env) {
		throw new RuntimeException("Loop is not supported for java compilation.");
	}
}
