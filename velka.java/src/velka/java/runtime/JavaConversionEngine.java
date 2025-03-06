package velka.java.runtime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.typeSystem.IConversionEngine;
import velka.types.typeSystem.TypeSystem;
import velka.types.typeSystem.VelkaAbstraction;

public class JavaConversionEngine implements IConversionEngine {

	@Override
	public Object convertTuple(TypeSystem typeSystem, TypeTuple from, TypeTuple to, Object o, Object env) {
		if(o instanceof VelkaTuple tuple) {
			if(tuple.size() == to.size()
					&& tuple.size() == from.size()){
				var it = tuple.iterator();
				var ft = from.iterator();
				var tt = to.iterator();
				var l = new ArrayList<Object>(tuple.size());
				
				while(it.hasNext()) {
					var current = it.next();
					var currentTo = tt.next();
					var currentFrom = ft.next();
					var conv = typeSystem.convert(currentFrom, currentTo, 
							current, 
							env);
					l.add(conv);
				}
				
				var ret = new VelkaTuple(l, to);
				return ret;
			}
		}
		
		throw new RuntimeException("Unexpected object " + o.toString() + " converted to tuple");
	}

	@Override
	public Object convertFunction(TypeSystem typeSystem, TypeArrow from, TypeArrow to, Object o, Object env) {
		if(o instanceof VelkaAbstraction abst) {			
			var ret = new VelkaAbstraction() {
				private final VelkaAbstraction vabst = abst;
				
				@Override
				public Type getType() {
					return to;
				}

				@Override
				public Object apply(Collection<? extends Object> arg) {
					var carg = (VelkaTuple)typeSystem.convert(from.ltype, to.ltype, 
							arg, 
							env);
					var rslt = this.vabst.apply(carg);
					var crslt = typeSystem.convert(to.rtype, from.rtype, 
							rslt, 
							env);
					
					return crslt;
				}
				
			};
			
			return ret;
		}
		throw new RuntimeException("Unexpected object" + o.toString() + " converted to tuple");
	}

	@Override
	public Collection<? extends Object> instantiateCollection(Object o) {
		return VelkaTuple.of(o);
	}

}
