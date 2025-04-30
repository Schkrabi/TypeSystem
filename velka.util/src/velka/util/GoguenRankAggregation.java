package velka.util;

public class GoguenRankAggregation extends RankAggregation {

	private final static double DEFAULT_CONVERSION_RANK = 0.999d;
	private final static double DEFAULT_IMPLEMENTATION_RAND = 0.5d;
	private final static double FUNCTION_CONVERSION_RANK = 0.001d;
	private final static double NEUTRAL_ELEMENT = 1.0d;
	private final static double WORST_RANK = 0.0d;
	
	@Override
	public double aggregate(double val1, double val2) {
		return val1 * val2;
	}

	@Override
	public boolean isFirstBetterThankSecond(double first, double second) {
		return first > second;
	}

	@Override
	public double defaultConversionRank() {
		return DEFAULT_CONVERSION_RANK;
	}

	@Override
	public double defaultImplementationRank() {
		return DEFAULT_IMPLEMENTATION_RAND;
	}

	@Override
	public double neutralRank() {
		return NEUTRAL_ELEMENT;
	}

	@Override
	public double functionConversionRank() {
		return FUNCTION_CONVERSION_RANK;
	}

	@Override
	public double worstRank() {
		return WORST_RANK;
	}

	@Override
	public double invalidRank() {
		return Double.NaN;
	}

}
