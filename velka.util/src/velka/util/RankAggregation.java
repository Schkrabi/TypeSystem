package velka.util;

import java.util.function.BinaryOperator;
import java.util.function.BiFunction;

/** Provides aggregation function for costs **/
public class RankAggregation {

	private BinaryOperator<Double> costAggregation = CompleteLattices.GoguenTNorm;
	private BiFunction<Double, Double, Integer> rankComparator = (x, y) -> x.compareTo(y);
	
	public double aggregate(double val1, double val2) {
		return costAggregation.apply(val1, val2);
	}
	
	public boolean isFirstBetterThankSecond(double first, double second) {
		return this.rankComparator.apply(first, second) > 0;
	}
	
	public void setCostAggregatio(BinaryOperator<Double> f) {
		this.costAggregation = f;
	}
	
	public double defaultConversionRank() {
		return this.DEFAULT_CONVERSION_RANK;
	}
	
	public void setDefaultConversionRank(double rank) {
		this.DEFAULT_CONVERSION_RANK = rank;
	}
	
	public double defaultImplementationRank() {
		return this.DEFAULT_IMPLEMENTATION_RAND;
	}
	
	public void setDefautlImplementationRank(double rank) {
		this.DEFAULT_IMPLEMENTATION_RAND = rank;
	}
	
	public double neutralRank() {
		return this.NEUTRAL_ELEMENT;
	}
	
	public void setNeutralRank(double rank) {
		this.NEUTRAL_ELEMENT = rank;
	}
	
	public double functionConversionRank() {
		return this.FUNCTION_CONVERSION_RANK;
	}
	
	public void setFunctionConversionRank(double rank) {
		this.DEFAULT_CONVERSION_RANK = rank;
	}
	
	public double worstRank() {
		return this.WORST_RANK;
	}
	
	public void setWorstRank(double rank) {
		this.WORST_RANK = rank;
	}
	
	public BiFunction<Double, Double, Integer> rankComparator(){
		return this.rankComparator;
	}
	
	public void setRankComparator(BiFunction<Double, Double, Integer> comparator) {
		this.rankComparator = comparator;
	}
	
	private RankAggregation() {}
	
	private static RankAggregation singleton = null;
	public static RankAggregation instance() {
		if(singleton == null) {
			singleton = new RankAggregation();
		}
		return singleton;
	}
	
	private double DEFAULT_CONVERSION_RANK = 0.999d;
	private double DEFAULT_IMPLEMENTATION_RAND = 0.5d;
	private double FUNCTION_CONVERSION_RANK = 0.001d;
	private double NEUTRAL_ELEMENT = 1.0d;
	private double WORST_RANK = 0.0d;
}
