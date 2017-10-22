package expression;

import java.util.Map;

import types.ForallType;
import types.Type;
import types.TypeArrow;
import types.TypeTuple;
import interpretation.Environment;

public class Application extends Expression {
	
	public final Expression fun;
	public final Tuple args;
	
	public Application(Expression fun, Expression arg) {
		this.fun = fun;
		this.args = new Tuple(new Expression[]{arg});
	}
	
	public Application(Expression fun, Tuple args){
		this.fun = fun;
		this.args = args;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression ifun = fun.interpret(env);
		
		if(!(ifun instanceof ExtendedLambda)) {
			throw new Exception(fun + " is not a fucntion");
		}
		
		ExtendedLambda elambda = (ExtendedLambda)ifun;
		
		if(elambda.args.values.length != this.args.values.length){
			throw new Exception("In aplication of " + fun + "number of arguments mismatch, expected " + elambda.args.values.length + " got " + this.args.values.length);
		}
		
		Environment childEnv = new Environment(env);
		for(int i = 0; i < elambda.args.values.length; i++){
			childEnv.put((Variable) elambda.args.values[i], this.args.values[i]);
		}
		
		//Std lambda
		if(elambda instanceof Lambda){	
			childEnv = Application.autoConvertForDefaultImplementation(childEnv);
			return ((Lambda)elambda).getBody().interpret(childEnv);
		}
		
		Expression impl = elambda.getImplementation((TypeTuple)args.getType().getRep()); 
		//Optimized implementation not found
		if(impl == null){
			childEnv = Application.autoConvertForDefaultImplementation(childEnv);
			return elambda.getDefaultUmplementation().interpret(childEnv);
		}
		
		return impl.interpret(childEnv);
	}
	
	private static Environment autoConvertForDefaultImplementation(Environment e){
		Environment ret = new Environment(e.parent);
		
		for(Map.Entry<Variable, Expression> entry : e.entrySet()){
			ret.put(entry.getKey(), Literal.defaultImplementationLazy(entry.getValue()));
		}
		
		return ret;
	}

	@Override
	public String toString() {
		return this.fun.toString() + " " + this.args.toString();
	}

	@Override
	public Type infer() throws Exception {
		Type funType = this.fun.infer();
		Type argsType = this.args.infer();
		
		if(!(funType.isApplicableType())){
			throw new Exception(fun + " is not a fucntion");
		}
		TypeArrow funArrType;
		
		if(funType instanceof TypeArrow){
			funArrType = (TypeArrow)funType;
		}
		else if(funType instanceof ForallType){
			funArrType = (TypeArrow)((ForallType)funType).getBoundType();
		}
		else {
			throw new Exception("Instance of " + funType + " was evaluated as applicable");
		}
		
		if(!Type.unify(funArrType.ltype, argsType)){
			throw new Exception("Arguments of " + this.fun + " does not unify with args " + this.args + " expected " + funArrType.ltype + " got " + argsType);
		}
		
		this.setType(funArrType.rtype);
		
		return funArrType.rtype;
	}
}
