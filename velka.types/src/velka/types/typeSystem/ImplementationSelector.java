package velka.types.typeSystem;

import java.util.Collection;
import java.util.List;
import java.util.stream.DoubleStream;

import velka.types.TypeArrow;
import velka.types.TypeTuple;
import velka.types.Type;
import velka.util.RankAggregation;
import velka.util.IEvalueable;
import velka.util.Pair;

public class ImplementationSelector {
	private final TypeSystem typeSystem;
	private Object env = null;
	
	private double bestRank;
	private RankAggregation ragg = RankAggregation.instance();
	
	private double implRank(
			VelkaAbstraction impl,
			VelkaAbstraction cost,
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
		
		var ic = cost.apply(args);
		var dic = this.typeSystem.extractRank(ic);
		
		//agg = RankAggregation.instance().aggregate(agg, dic);
		agg = agg * dic;
		
		return agg;
	}
	
	public VelkaAbstraction selectImplementation(
			Collection<velka.util.Pair<? extends VelkaAbstraction, ? extends VelkaAbstraction>> impls,
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
