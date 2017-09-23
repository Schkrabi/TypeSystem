package types;

public abstract class Type {
	
	private Type rep;
	
	public Type(){
		this.rep = this;
	}

	public static boolean unify(Type m, Type n){
		Type s = m.getRep();
		Type t = n.getRep();
		
		if(s == t){ //Also covers if the nodes represents same basic type
			return true;
		}
		else if(	s instanceof TypeArrow
				&&	t instanceof TypeArrow){
			Type.union(s, t);
			TypeArrow as = (TypeArrow)s;
			TypeArrow at = (TypeArrow)t;
			return 		Type.unify(as.ltype, at.ltype)
					&&	Type.unify(as.rtype, at.rtype);
		}
		else if(	s instanceof TypeVariable
				||	t instanceof TypeVariable){
			Type.union(s, t);
			return true;
		}
		else if(	s instanceof TypeTuple
				&&	t instanceof TypeTuple){
			TypeTuple ts = (TypeTuple)s;
			TypeTuple tt = (TypeTuple)t;
			
			if(ts.values.length != tt.values.length){
				return false;
			}
			
			for(int i = 0; i < ts.values.length; i++){
				if(!Type.unify(ts.values[i], tt.values[i])){
					return false;
				}
			}
			return true;
		}
		return false;
	}
	
	public Type getRep(){
		if(this.rep == this){
			return this.rep;
		}
		return this.rep.getRep();
	}
	
	private static void union(Type t1, Type t2){
		if(t1 instanceof TypeConcrete){
			t2.rep = t1.rep;
			return;
		}
		if(t2 instanceof TypeConcrete){
			t1.rep = t2.rep;
		}
		t2.rep = t1.rep;
		return;
	}
}
