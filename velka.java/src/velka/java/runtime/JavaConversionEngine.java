package velka.java.runtime;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.typeSystem.IConversionEngine;
import velka.types.typeSystem.TypeSystem;
import velka.types.typeSystem.VelkaAbstraction;

public class JavaConversionEngine implements IConversionEngine {

	@Override
	public Object convertTuple(TypeSystem typeSystem, TypeTuple from, TypeTuple to, Object o, Object env) {
		if (o instanceof VelkaTuple tuple) {
	        int size = tuple.size();
	        if (size == to.size() && size == from.size()) {
	            var l = new Object[size];
	            var lt = new Type[size];
	            boolean changed = false;

	            for (int i = 0; i < size; i++) {
	                Object current = tuple.get(i);
	                Type currentFrom = from.get(i);
	                Type currentTo = to.get(i);

	                Object conv = current;
	                Type targetType = currentFrom;

	                if (!(currentFrom instanceof TypeVariable)) {
	                    conv = typeSystem.convert(currentFrom, currentTo, current, env);
	                    if (conv != current) {
	                        targetType = currentTo;
	                        changed = true;
	                    }
	                }

	                l[i] = conv;
	                lt[i] = targetType;
	            }

	            return changed 
	                ? new VelkaTuple(l, new TypeTuple(lt)) 
	                : tuple;
	        }
	    }

	    throw new RuntimeException(
	        "Unexpected object for tuple conversion: " + 
	        o + " (type: " + (o != null ? o.getClass().getSimpleName() : "null") + ")");
	}

	@Override
	public Object convertFunction(TypeSystem typeSystem, TypeArrow from, TypeArrow to, Object o, Object env) {
		if(o instanceof VelkaAbstraction abst) {
			VelkaAbstraction ret = abst;
			if(Type.unifyRepresentation(from.ltype, to.ltype)
					.isEmpty()) {
				if(Type.unifyRepresentation(from.rtype, to.rtype)
						.isEmpty()) {
					ret = new VelkaAbstraction() {
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
							
							Object crslt = null;
							
							crslt = typeSystem.convert(to.rtype, from.rtype, 
								rslt, 
								env);
							
							return crslt;
						}
						
					};
				}
				else {
					ret = new VelkaAbstraction() {
						private final VelkaAbstraction vabst = abst;
						
						@Override
						public Type getType() {
							return new TypeArrow(to.ltype, from.rtype);
						}

						@Override
						public Object apply(Collection<? extends Object> arg) {
							var carg = (VelkaTuple)typeSystem.convert(from.ltype, to.ltype, 
									arg, 
									env);
							var rslt = this.vabst.apply(carg);
							
							return rslt;
						}
						
					};
				}
			}
			else if(Type.unifyRepresentation(from.rtype, to.rtype)
					.isEmpty()) {
				ret = new VelkaAbstraction() {
					private final VelkaAbstraction vabst = abst;
					
					@Override
					public Type getType() {
						return new TypeArrow(from.ltype, to.rtype);
					}

					@Override
					public Object apply(Collection<? extends Object> arg) {
						var rslt = this.vabst.apply(arg);
						
						Object crslt = typeSystem.convert(to.rtype, from.rtype, 
								rslt, 
								env);
						
						return crslt;
					}
					
				};
			}
			
			
			return ret;
		}
		throw new RuntimeException("Unexpected object" + o.toString() + " converted to tuple");
	}

	@Override
	public Collection<? extends Object> instantiateCollection(Object o) {
		return VelkaTuple.of(o);
	}

}
