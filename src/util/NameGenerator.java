package util;

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

		return s;
	}
}
