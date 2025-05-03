package velka.util;

public interface IConversionRanker {

	public double eval(Object arg);
	
	public static IConversionRanker constRanker(double cost) {
		return new IConversionRanker() {

			@Override
			public double eval(Object arg) {
				return cost;
			}
			
		};
	}
	
	public static final IConversionRanker DEFAULT = IConversionRanker
			.constRanker(RankAggregation.instance().defaultConversionRank());
}
