package velka.util;

import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Class for the generation of unique variable names
 * 
 * @author Mgr. Radomir Skrabal
 *
 */
public class NameGenerator {
	/**
	 * Seed of the generator
	 */
	private static String seed = "a";
	
	private static final String prefix = "SYSGENNAME"; 

	/**
	 * Returns next unique string
	 * 
	 * @return String object
	 */
	public static String next() {
		String s = seed;

		char[] chs = s.toCharArray();
		int i = chs.length - 1;
		boolean updated = false;
		boolean append = false;
		while (!updated) {
			chs[i]++;
			if (chs[i] > 'z') {
				chs[i] = 'a';
				i--;
			} else {
				updated = true;
			}
			if (i < 0) {
				append = true;
				break;
			}
		}
		seed = new String(chs);
		if (append) {
			seed = seed + 'a';
		}

		return prefix + s;
	}
	
	/** Creates collection of n unique names */
	public static Collection<String> uniqueNameCollection(int n) {
		return Stream.iterate(next(), x -> next()).limit(n).collect(Collectors.toList());
	}
}
