package velka.util;

import java.util.function.BinaryOperator;
import java.util.function.BiFunction;

/** Provides aggregation function for costs **/
public abstract class RankAggregation {

	//private BinaryOperator<Double> costAggregation = CompleteLattices.GoguenTNorm;
	private BiFunction<Double, Double, Integer> rankComparator = (x, y) -> x.compareTo(y);
	
	public abstract double aggregate(double val1, double val2);
	
	public abstract boolean isFirstBetterThankSecond(double first, double second);
	
	public abstract double defaultConversionRank();
	
	public abstract double defaultImplementationRank();
	
	public abstract double neutralRank();
	
	public abstract double functionConversionRank();
	
	public abstract double worstRank();
	
	public abstract double invalidRank();
	
	protected RankAggregation() {}
	
	private static RankAggregation singleton = null;
	public static RankAggregation instance() {
		if(singleton == null) {
			singleton = new GoguenRankAggregation();
		}
		return singleton;
	}
}
