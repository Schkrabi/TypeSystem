package expression;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import interpretation.Environment;
import types.Type;
import types.TypeArrow;

/**
 * Class for parsed Scheme sequence. Allows to transform into Application Expression based on type inference.
 * @author Mgr. Radomir Skrabal
 *
 */
public class Sequence extends Expression {
	
	public final List<Expression> members;
	
	public Sequence(List<Expression> members){
		this.members = members;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression head = this.head().interpret(env);
		
		if(head instanceof ExtendedLambda){
			//Will interpret head again, but doesnt matter, lambda interprets to itself and no side effects here
			Application tmp = new Application(head, this.tail()); 
			return tmp.interpret(env);
		}
		
		return this.asTuple().interpret(env);
	}

	@Override
	public Type infer() throws Exception {
		Type headType = this.head().infer();
		
		if(headType instanceof TypeArrow){
			Application tmp = new Application(this.head(), this.tail());
			return tmp.infer();
		}
		
		return this.asTuple().infer();
	}
	
	@Override
	public String toString(){
		StringBuilder s = new StringBuilder();
		s.append('(');
		Iterator<Expression> i = this.members.iterator();
		
		while(i.hasNext()){
			Expression e = i.next();
			s.append(e.toString());
			if(i.hasNext()){
				s.append(" ");
			}
		}
		s.append(")");
		return s.toString();
	}

	protected Expression head(){
		return this.members.get(0);
	}
	
	protected Tuple tail(){
		List<Expression> tmp = new LinkedList<Expression>();
		tmp.addAll(this.members);
		tmp.remove(0);
		return new Tuple((Expression[])tmp.toArray());
	}
	
	public Expression transform(){
		if(this.getType() == null){
			return this;
		}
		if(this.head().getType() instanceof TypeArrow){
			return new Application(this.head(), this.tail());
		}
		
		//Not sure
		return this.asTuple();
	}
	
	public Tuple asTuple(){
		Expression[] exprs = new Expression[this.members.size()];
		exprs = this.members.toArray(exprs);
		return new Tuple(exprs);
	}
}
