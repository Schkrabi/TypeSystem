package velka.types.typeSystem;

import java.util.Collection;

import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.Type;
import velka.util.RankAggregation;
import velka.util.IEvalueable;
import velka.util.Pair;

public class ImplementationSelector {
	private final TypeSystem typeSystem;
	private Object env = null;
	
	private double implRank(
			VelkaAbstraction impl,
			VelkaAbstraction cost,
			Collection<? extends Object> args) {
		var implArgType = (TypeTuple)((TypeArrow)impl.getType()).ltype;
		
		var agg = RankAggregation.instance().neutralRank();
		
		var itArgType = implArgType.iterator();
		var itArgs = args.iterator();
		
		while(itArgType.hasNext()) {
			var ttype = itArgType.next();
			var arg = itArgs.next();
			var ftype =  this.typeSystem.getType(arg);
			
			var cc = this.typeSystem.conversionCost(ftype, ttype, arg, this.env);
			
			//There is no conversion
			if(cc == null) {
				return RankAggregation.instance().worstRank();
			}
			
			agg = RankAggregation.instance().aggregate(agg, cc);
		}
		
		var ic = cost.apply(args);
		var dic = this.typeSystem.extractRank(ic);
		
		agg = RankAggregation.instance().aggregate(agg, dic);
		
		return agg;
	}
	
	public VelkaAbstraction selectImplementation(
			Collection<velka.util.Pair<? extends VelkaAbstraction, ? extends VelkaAbstraction>> impls,
			Collection<? extends Object> args) {
		
		var bestRank = RankAggregation.instance().worstRank();
		VelkaAbstraction bestImpl = null;
		
		for(var p : impls) {
			var rank = this.implRank(p.first, p.second, args);
			if(bestImpl == null || RankAggregation.instance().isFirstBetterThankSecond(rank, bestRank)) {
				bestRank = rank;
				bestImpl = p.first;
			}
		}
		
		return bestImpl;
	}
	
	public ImplementationSelector(TypeSystem typeSystem) 
	{
		this.typeSystem = typeSystem;
	}
	
	public void setEnvironment(Object env) {
		this.env = env;
	}
}
