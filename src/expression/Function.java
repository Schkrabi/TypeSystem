package expression;

import java.util.Comparator;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import types.TypesDoesNotUnifyException;
import util.AppendableException;

/**
 * Expression for representation of interpreted function
 * @author Mgr. Radomir Skrabal
 *
 */
public class Function extends MetaFunction implements Comparable<Expression>{
	
	/**
	 * Type of the function arguments
	 */
	public final TypeTuple argsType;
	/**
	 * Function arguments
	 */
	public final Tuple args;
	/**
	 * Body of the fucntion
	 */
	public final Expression body;
	
	public Function(TypeTuple argsType, Tuple args, Expression body, Environment createdEnvironment){
		super(createdEnvironment);
		this.argsType = argsType;
		this.args = args;
		this.body = body;
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			
			if(this.typeHypothesis == null) {
				Map<Expression, Type> tmp = new TreeMap<Expression, Type>();
				
				Map<Expression, Type> inferedArgs = this.args.infer(new Environment());
				Map<Expression, Type> inferedBody = this.body.infer(this.creationEnvironment);
				
				tmp.putAll(inferedBody);
				
				for(Map.Entry<Expression, Type> e : inferedArgs.entrySet()) {
					if(inferedBody.containsKey(e.getKey())) {
						Optional<Type> t = Type.unify(e.getValue(), inferedBody.get(e.getKey()));
						
						if(!t.isPresent()) {
							throw new TypesDoesNotUnifyException(e.getValue(), inferedBody.get(e.getKey()));
						}
						
						tmp.put(e.getKey(), t.get());
					}
					else{
						tmp.put(e.getKey(), e.getValue());
					}
				}
				
				if(this.argsType != null) {
					Optional<Type> o = Type.unify(this.argsType, tmp.get(this.args));
					
					if(!o.isPresent()) {
						throw new TypesDoesNotUnifyException(this.argsType, inferedBody.get(this.args));
					}
					
					tmp.put(this.args, o.get());
				}
				
				tmp.put(this, new TypeArrow(tmp.get(this.args), tmp.get(this.body)).quantifyUnconstrainedVariables());
				this.typeHypothesis = tmp;
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}

	@Override
	public Function getFunction() {
		return this;
	}

	@Override
	public Function getFunction(Comparator<? super Function> c) {
		return this;
	}

	@Override
	public int compareTo(Expression other) {
		if(other instanceof Function) {
			Function o = (Function)other;
			if(this.argsType == o.argsType) {
				return 0; 
			}
			if(this.argsType == null) {
				return 1;
			}
			if(o.argsType == null) {
				return -1;
			}
			
			return this.argsType.compareTo(o.argsType);
		}
		return super.compareTo(other);
	}

	@Override
	public String toString() {
		return "(func " + this.args.toString() + " " + this.body.toString() + ")";
	}
}
