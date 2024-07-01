package velka.util;

import java.util.BitSet;
import java.util.TreeSet;

/** Utility functions for bit sets */
public class BitSetHelper {
	
	/** Converts bit set to tree set */
	public static TreeSet<Long> bitset2treeset(BitSet bs){
		var treeSet = new java.util.TreeSet<Long>((Long e1, Long e2) -> {
			return Long.compare(e1, e2);
		});
		
		bs.stream().forEach(x -> treeSet.add((long) x));
		
		return treeSet;
	}
}
