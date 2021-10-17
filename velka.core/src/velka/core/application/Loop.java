package velka.core.application;

import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

import velka.core.abstraction.Lambda;
import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.interpretation.ClojureHelper;
import velka.core.interpretation.Environment;
import velka.core.interpretation.TypeEnvironment;
import velka.types.Substitution;
import velka.types.SubstitutionsCannotBeMergedException;
import velka.types.Type;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.TypesDoesNotUnifyException;
import velka.util.AppendableException;
import velka.util.NameGenerator;
import velka.util.Pair;

/**
 * Expression for loop special form
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Loop extends Expression {
	
	public static final String LOOP = "loop";
	
	/**
	 * Symbol for marking recurrence point
	 */
	public static final Symbol RECUR_MARK_SYMBOL = new Symbol(NameGenerator.next());
	
	public final Tuple bindedSymbols;
	
	public final Expression body;
	
	/**
	 * Holds values for initialization
	 */
	public final Tuple initArgs;
	
	public Loop(Tuple bindedSymbols, Expression body, Tuple initArgs) {
		this.bindedSymbols = bindedSymbols;
		this.body = body;
		this.initArgs = initArgs;
	}
	
	private Lambda createLoopLambda(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Pair<Type, Substitution> argsInfered = this.initArgs.infer(env, typeEnv);
		
		TypeTuple argTypes = (TypeTuple)argsInfered.first;
		
		Lambda l = new Lambda(this.bindedSymbols, argTypes, this.body);
		return l;
	}

	@Override
	public Expression interpret(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Environment recurEnvironment = Environment.create(env);
		Lambda loopExpression = this.createLoopLambda(env, typeEnv);		
		recurEnvironment.put(RECUR_MARK_SYMBOL, loopExpression);
		
		AbstractionApplication appl = new AbstractionApplication(loopExpression, initArgs);
		return appl.interpret(recurEnvironment, typeEnv);
	}

	@Override
	public Pair<Type, Substitution> infer(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		Environment recurEnvironment = Environment.create(env);
		TypeVariable tv = new TypeVariable(NameGenerator.next());
		recurEnvironment.put(RECUR_MARK_SYMBOL, new TypeHolder(tv));
		Lambda loopExpression = this.createLoopLambda(env, typeEnv);
		
		AbstractionApplication appl = new AbstractionApplication(loopExpression, initArgs);
		Pair<Type, Substitution> infered = appl.infer(recurEnvironment, typeEnv);		
		
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
		Optional<Substitution> opt = s.union(tmp);
		if(opt.isEmpty()) {
			throw new SubstitutionsCannotBeMergedException(s, tmp);
		}

		return new Pair<Type, Substitution>(infered.first, opt.get());
	}

	@Override
	public String toClojureCode(Environment env, TypeEnvironment typeEnv) throws AppendableException {
		List<String> bidings = new LinkedList<String>();
		Iterator<Expression> i = this.bindedSymbols.iterator();
		Iterator<Expression> j = this.initArgs.iterator();
		
		while(i.hasNext() && j.hasNext()) {
			Expression currentSymbol = i.next();
			Expression currentValue = j.next();
			
			bidings.add(currentSymbol.toClojureCode(env, typeEnv));
			bidings.add(currentValue.toClojureCode(env, typeEnv));
		}
		
		String code = ClojureHelper.applyClojureFunction("loop", 
				ClojureHelper.clojureVectorHelper(bidings),
				this.body.toClojureCode(env, typeEnv));
		
		return code;
	}
	
	@Override
	public int hashCode() {
		return this.bindedSymbols.hashCode() * this.initArgs.hashCode() * this.body.hashCode();
	}

	@Override
	public boolean equals(Object other) {
		if(other instanceof Loop) {
			return this.bindedSymbols.equals(((Loop) other).bindedSymbols)
					&& this.initArgs.equals(((Loop) other).initArgs)
					&& this.body.equals(((Loop) other).body);
		}
		return false;
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof Loop) {
			int cmp = this.bindedSymbols.compareTo(((Loop) other).bindedSymbols);
			if(cmp != 0) {
				return cmp;
			}
			cmp = this.initArgs.compareTo(((Loop) other).initArgs);
			if(cmp != 0) {
				return cmp;
			}
			
			cmp = this.body.compareTo(((Loop) other).body);
			return cmp;
		}
		return super.compareTo(other);
	}
	
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		
		sb.append("(");
		sb.append(LOOP);
		sb.append(" ");
		
		Iterator<Expression> i = this.bindedSymbols.iterator();
		Iterator<Expression> j = this.initArgs.iterator();
		
		while(i.hasNext() && j.hasNext()) {
			Expression symbol = i.next();
			Expression value = j.next();
			
			sb.append("(");
			sb.append(symbol.toString());
			sb.append(" ");
			sb.append(value.toString());
			sb.append(")");
			
			if(i.hasNext() && j.hasNext()) {
				sb.append(" ");
			}
		}
		
		sb.append(this.body.toString());
		sb.append(")");
		return sb.toString();
	}
}
