package expression;

import types.ForallType;
import types.Type;
import types.TypeArrow;
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
		
		//Std lambda
		if(ifun instanceof Lambda){
			Lambda lambda = (Lambda)ifun;
			
			if(lambda.args.values.length != this.args.values.length){
				throw new Exception("In aplication of " + fun + "number of arguments mismatch, expected " + lambda.args.values.length + " got " + this.args.values.length);
			}
			
			Environment childEnv = new Environment(env);
			for(int i = 0; i < lambda.args.values.length; i++){
				childEnv.put((Variable) lambda.args.values[i], this.args.values[i]);
			}
			
			return ((Lambda)ifun).getBody().interpret(childEnv);
		}
		//Extended lambda
		throw new Exception("Not Implemented");
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
		
		return funArrType.rtype;
	}
}
