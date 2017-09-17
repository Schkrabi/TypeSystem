package util;

public class NameGenerator {
	private static String seed = "a";

	public static String next() {
		String s = seed;
		
		char[] chs = s.toCharArray();
		int i = chs.length - 1;
		boolean updated = false;
		boolean append = false;
		while(!updated) {
			chs[i]++;
			if(chs[i] > 'z') {
				chs[i] = 'a';
				i--;
			}
			else {
				updated = true;
			}
			if(i < 0) {
				append = true;
				break;
			}
		}
		seed = new String(chs);
		if(append) {
			seed = seed + 'a';
		}
		
		return s;
	}
}
