package velka.types.typeSystem;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import velka.types.RepresentationOr;
import velka.types.Type;
import velka.util.AppendableException;
import velka.util.Pair;

/** Class to represent an extended function in Clojure and Java */
public class VelkaExtendedFunction implements VelkaAbstraction {
	
	private final List<Pair<? extends VelkaAbstraction, ? extends VelkaAbstraction>> impls
		= new ArrayList<Pair<? extends VelkaAbstraction, ? extends VelkaAbstraction>>();
	
	private final TypeSystem ts;
	private final Type type;
	
	public VelkaExtendedFunction(TypeSystem ts, Type type) {
		this.ts = ts;
		this.type = type;
	}
	
	/** Creates new extended function with added implementation and cost */
	public VelkaExtendedFunction extend(VelkaAbstraction impl, VelkaAbstraction cost) {
		var t = RepresentationOr.or(this.type, impl.getType());			
		var ef = new VelkaExtendedFunction(this.ts, t);
		ef.impls.addAll(this.impls);
		ef.impls.add(Pair.of(impl, cost));
		return ef;
	}

	@Override
	public Type getType() {
		return this.type;
	}

	@Override
	public Object apply(Collection<? extends Object> arg) {
		var selector = this.ts.getImplementationSelector();
		var impl = selector.selectImplementation(this.impls, arg);
		
		return impl.apply(arg);
	}

}
