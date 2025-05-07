package velka.util;

import java.util.Collection;

public interface IImplementationRanker {

	public double eval(Collection<? extends Object> args);
	
	public static IImplementationRanker constRanker(double value) {
		return new IImplementationRanker() {

			@Override
			public double eval(Collection<? extends Object> args) {
				return value;
			}
			
		};
	}
	
	public static final IImplementationRanker DEFAULT = 
			IImplementationRanker.constRanker(RankAggregation.instance().defaultImplementationRank());
}
