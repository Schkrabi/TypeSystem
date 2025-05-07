package velka.types.typeSystem;

import java.util.Collection;

import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.util.RankAggregation;
import velka.util.IImplementationRanker;

public class ImplementationSelector {
	private final TypeSystem typeSystem;
	private Object env = null;
	
	private double bestRank;
	private RankAggregation ragg = RankAggregation.instance();
	
	private double implRank(
			VelkaAbstraction impl,
			IImplementationRanker cost,
			Collection<? extends Object> args,
			TypeTuple argType) {
		var parmType = (TypeTuple)((TypeArrow)impl.getType()).ltype;
		
		var agg = ragg.neutralRank();
		var itArgs = args.iterator();
		
		for(var i = 0; i < parmType.size(); i++) {
			var arg = itArgs.next();
			
			var cc = this.typeSystem.conversionCost(argType.get(i), parmType.get(i), arg, this.env, this.bestRank);
			
			//There is no conversion
			if(cc == ragg.invalidRank()) {
				return ragg.worstRank();
			}
			
			//agg = RankAggregation.instance().aggregate(agg, cc);
			agg = agg * cc;
			
			if(agg <= this.bestRank) {
				return ragg.worstRank();
			}
		}
		
		var ic = cost.eval(args);
		
		//agg = RankAggregation.instance().aggregate(agg, dic);
		agg = agg * ic;
		
		return agg;
	}
	
	public VelkaAbstraction selectImplementation(
			Collection<velka.util.Pair<? extends VelkaAbstraction, ? extends IImplementationRanker>> impls,
			Collection<? extends Object> args) {
		VelkaAbstraction bestImpl = null;
		
		for(var p : impls) {
			var rank = this.implRank(p.first, p.second, args, (TypeTuple)this.typeSystem.getType(args));
			if(bestImpl == null 
				//|| RankAggregation.instance().isFirstBetterThankSecond(rank, bestRank)) {
				|| rank > bestRank) {
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
