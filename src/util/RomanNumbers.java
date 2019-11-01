package util;

public class RomanNumbers {
	/**
	 * Regular expression for roman number validity checking
	 */
	private static final String romanRegExp = "(^(?=[MDCLXVI])M*(C[MD]|D?C{0,3})(X[CL]|L?X{0,3})(I[XV]|V?I{0,3})$)";
	
	/**
	 * Returns true if given string contains a roman number
	 * @param s inspected string
	 * @return true or false
	 */
	public static boolean check(String s){
		return s.matches(romanRegExp);
	}
	
	/**
	 * Converts roman number in string to integer
	 * 
	 * @param str
	 *            String containing the roman integer
	 * @return int value of the integer
	 * @throws Exception
	 *             thrown is str is not a roman number
	 */
	public static int roman2int(String str) throws AppendableException {
		if(! RomanNumbers.check(str)) {
			throw new AppendableException("Invalid roman number " + str);
		}

		int res = 0;

		for (int i = 0; i < str.length(); i++) {
			int s1 = value(str.charAt(i));

			if (i + 1 < str.length()) {
				int s2 = value(str.charAt(i + 1));

				if (s1 >= s2) {
					res = res + s1;
				} else {
					res = res + s2 - s1;
					i++;
				}
			} else {
				res = res + s1;
				i++;
			}
		}

		return res;
	}
	
	/**
	 * Used for int2roman conversion
	 */
	private static final String[] hundreds = { "", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM" };
	/**
	 * Used for int2roman conversion
	 */
	private static final String[] tens = { "", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC" };
	/**
	 * Used for int2roman conversion
	 */
	private static final String[] ones = { "", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX" };

	/**
	 * Converts integer to roman number string
	 * 
	 * @param val
	 *            value to be converted
	 * @return String object containing roman number
	 */
	public static String int2roman(int val) {
		int tmpVal = val;
		StringBuilder s = new StringBuilder();

		while (tmpVal >= 1000) {
			s.append("M");
			tmpVal -= 1000;
		}

		s.append(hundreds[tmpVal / 100]);
		tmpVal = tmpVal % 100;
		s.append(tens[tmpVal / 10]);
		tmpVal = tmpVal % 10;
		s.append(ones[tmpVal]);

		return s.toString();
	}

	/**
	 * This function returns value of a Roman symbol
	 * 
	 * @param r
	 *            character to convert
	 * @return int value
	 * @throws AppendableException 
	 */
	public static int value(char r) throws AppendableException {
		switch(r) {
		case 'I':
			return 1;
		case 'V':
			return 5;
		case  'X':
			return 10;
		case 'L':
			return 50;
		case 'C':
			return 100;
		case 'D':
			return 500;
		case 'M':
			return 1000;
		default:
			throw new AppendableException( r + " is not a roman number symbol");
		}
	}
	
	/**
	 * Clojure code of roman to int function
	 */
	public static final String roman2intClojure = "(fn [arg]\n" + 
			"                    (letfn [(romanCheck [arg] (re-matches #\"(^(?=[MDCLXVI])M*(C[MD]|D?C{0,3})(X[CL]|L?X{0,3})(I[XV]|V?I{0,3})$)\" arg))]\n" + 
			"                        (when-not (romanCheck arg) (throw (Exception. (str \"Invalid roman number \" arg))))\n" + 
			"                        (let [  values {\\I 1\n" + 
			"                                        \\V 5\n" + 
			"                                        \\X 10\n" + 
			"                                        \\L 50\n" + 
			"                                        \\C 100\n" + 
			"                                        \\D 500\n" + 
			"                                        \\M 1000}\n" + 
			"                                numbered (map (fn [x] (get values x)) arg)\n" + 
			"                                first   (reverse (cons 0 numbered))\n" + 
			"                                second  (cons 0 (reverse numbered))]\n" + 
			"                            (reduce +(map (fn [cur prev]\n" + 
			"                                        (if (< cur prev)\n" + 
			"                                                (- cur)\n" + 
			"                                                cur)) first second)))))";
	
	/**
	 * Clojure code if int to roman function
	 */
	public static final String int2RomanClojure = "(fn [n]\n" + 
			"                (let [  hundreds    [\"\" \"C\" \"CC\" \"CCC\" \"CD\" \"D\" \"DC\" \"DCC\" \"DCCC\" \"CM\"]\n" + 
			"                        tens        [\"\" \"X\" \"XX\" \"XXX\" \"XL\" \"L\" \"LX\" \"LXX\" \"LXXX\" \"XC\"]\n" + 
			"                        ones        [\"\" \"I\" \"II\" \"III\" \"IV\" \"V\" \"VU\" \"VII\" \"VIII\" \"IX\"]]\n" + 
			"                        (letfn [(rec [n] \n" + 
			"                                    (if (>= n 1000)\n" + 
			"                                        (str \"M\" (rec (- n 1000)))\n" + 
			"                                        (str (get hundreds (quot n 100)) (get tens (quot (mod n 100) 10)) (get ones  (mod n 10)))))]\n" + 
			"                            (rec n))))";
}
