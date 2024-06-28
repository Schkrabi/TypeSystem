package velka.util;

import java.util.function.BinaryOperator;

/** Provides aggregation function for costs **/
public class CostAggregation {

	private BinaryOperator<Double> costAggregation = CompleteLattices.GoguenTNorm;
	
	public double aggregate(double val1, double val2) {
		return costAggregation.apply(val1, val2);
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
	
	private CostAggregation() {}
	
	private static CostAggregation singleton = null;
	public static CostAggregation instance() {
		if(singleton == null) {
			singleton = new CostAggregation();
		}
		return singleton;
	}
	
	private double DEFAULT_CONVERSION_RANK = 0.999d;
	private double DEFAULT_IMPLEMENTATION_RAND = 0.5d;
	private double FUNCTION_CONVERSION_RANK = 0.001d;
	private double NEUTRAL_ELEMENT = 1.0d;
}
