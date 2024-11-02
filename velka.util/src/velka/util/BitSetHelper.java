package velka.util;

import java.util.BitSet;
import java.util.HashSet;
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
	
	/** Converts hashset with integers or doubles to a treeset */
	public static TreeSet<Object> hashset2treeset(HashSet<Object> hs){
		java.util.Comparator<Object> cmp = null;
		if(hs.stream().allMatch(x -> x instanceof Long || x instanceof Integer)) {
			cmp = new java.util.Comparator<Object>() {

				@Override
				public int compare(Object o1, Object o2) {
					var i1 = (Long)o1;
					var i2 = (Long)o2;
					
					return Long.compare(i1, i2);
				}
				
			};
		}
		else if(hs.stream().allMatch(x -> x instanceof Double)) {
			cmp = new java.util.Comparator<Object>() {

				@Override
				public int compare(Object o1, Object o2) {
					var d1 = (Double)o1;
					var d2 = (Double)o2;
					return Double.compare(d1, d2);
				}
			};
		}
		else {
			throw new RuntimeException("Only Integer or Double sets can be automaticaly converted.");
		}
		
		var treeset = new TreeSet<Object>(cmp);
		treeset.addAll(hs);
		return treeset;
	}
	
	/** Convers integer hashset to bit set */
	public static BitSet hashset2bitset(HashSet<Object> hashset) {
		if(!hashset.stream().allMatch(x -> x instanceof Long || x instanceof Integer)) {
			throw new RuntimeException("Only integer sets can be converted to bit sets");
		}
		var bitset = new BitSet();
		hashset.stream().forEach(x -> {
			if(x instanceof Integer) {
				var i = (Integer)x;
				bitset.set(i.intValue());
			}
			if(x instanceof Long) {
				var l = (Long)x;
				bitset.set(l.intValue());
			}
		});
		return bitset;
	}
	
	/** Converts bitset to hashset */
	public static HashSet<Object> bitset2hashset(BitSet bitset){
		var hs = new HashSet<Object>();
		bitset.stream().forEach(i -> hs.add(Long.valueOf((long)i)));
		return hs;
	}
}
