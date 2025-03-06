package velka.core.abstraction;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import velka.core.expression.Expression;
import velka.core.expression.Symbol;
import velka.core.expression.Tuple;
import velka.core.expression.TypeHolder;
import velka.core.interpretation.Environment;
import velka.types.Substitution;
import velka.types.Type;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.AppendableException;
import velka.util.Pair;

/**
 * Expression for representation of interpreted function
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class Function extends Expression implements VelkaAbstraction {

	public final Environment env;
	public final Expression body;
	public final List<Pair<Symbol, Type>> parms;
	private final Type type;
	

	public Function(Environment env, Expression body, Collection<Pair<Symbol, Type>> parms) {
		this.env = env;
		this.body = body;
		this.parms = new java.util.ArrayList<Pair<Symbol, Type>>(parms);
		
		Type t = null;
		try
		{
			//Simulate the lexical clojure for the body inference
			var ienv = Environment.create(env);
			parms.stream().forEach(p -> ienv.put(p.first, new TypeHolder(p.second)));
			
			var p = this.body.infer(ienv);
			var tt = new TypeTuple(parms.stream().map(x -> x.second).toList());
			t = new velka.types.TypeArrow(tt.apply(p.second), p.first);
		}
		catch(AppendableException ae) {
			throw new RuntimeException(ae);
		}
		this.type = t;
	}


	@Override
	public Type getType() {
		return this.type;
	}


	@Override
	public Object apply(Collection<? extends Object> arg) {
		try {
			var clj = Environment.create(this.env);
			
			var itParm = this.parms.iterator();
			var itArg = arg.iterator();
			
			while(itParm.hasNext()
					&& itArg.hasNext()) {
				var parm = itParm.next();
				var sym = parm.first;
				var t = parm.second;
				var a = itArg.next();
				
				var bind = this.env.getTypeSystem().convert(
						this.env.getTypeSystem().getType(a), 
						t, 
						a, 
						this.env);
				
				clj.put(sym, (Expression)bind);
			}
			
			var ret = this.body.interpret(clj);
			return ret;
			
		} catch (AppendableException e) {
			throw new RuntimeException(e);
		}
	}


	@Override
	public Expression interpret(Environment env) throws AppendableException {
		return this;
	}


	@Override
	public Pair<Type, Substitution> infer(Environment env) throws AppendableException {
		return Pair.of(this.getType(), Substitution.EMPTY);
	}


	@Override
	public String toClojureCode(Environment env) throws AppendableException {
		throw new RuntimeException("Function is an interpretation runtime construct and cannot be compiled to clojure!");
	}


	@Override
	protected Expression doConvert(Type from, Type to, Environment env) throws AppendableException {
		throw new RuntimeException("doConvert is not implemented in function.");
	}
	
	@Override
	public boolean equals(Object o) {
		if(this == o) return true;
		if(o instanceof Function f) {
			return this.env.equals(f.env)
					&& this.parms.equals(f.parms)
					&& this.body.equals(f.body);
		}
		return false;
	}
}
