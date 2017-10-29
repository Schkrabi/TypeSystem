package expression;

import types.Type;
import types.TypeConcrete;
import interpretation.Environment;

public class IntRoman extends LitInteger {
	public final String value;
	
	public IntRoman(String value){
		if(!value.matches("(^(?=[MDCLXVI])M*(C[MD]|D?C{0,3})(X[CL]|L?X{0,3})(I[XV]|V?I{0,3})$)")){
			this.value = "";
			return;
		}		
		this.value = value;
	}
	
	@Override
	public Expression interpret(Environment env) {
		return this;
	}
	
	@Override
	public String toString(){
		return this.value;
	}
	
	public static int roman2int(String str){
        int res = 0;
 
        for (int i=0; i<str.length(); i++)
        {
            int s1 = value(str.charAt(i));
 
            if (i+1 <str.length())
            {
                int s2 = value(str.charAt(i+1));
 
                if (s1 >= s2)
                {
                    res = res + s1;
                }
                else
                {
                    res = res + s2 - s1;
                    i++;
                }
            }
            else
            {
                res = res + s1;
                i++;
            }
        }
 
        return res;
	}
	
	private static final String[] hundreds = {"", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"};
	private static final String[] tens = {"", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"};
	private static final String[] ones = {"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"};
	
	public static String int2roman(int val){
		int tmpVal = val;
		StringBuilder s = new StringBuilder();
		
		while(tmpVal > 1000){
			s.append("M");
			tmpVal -= 1000;
		}
		
		s.append(hundreds[tmpVal/100]);
		tmpVal = tmpVal % 100;
		s.append(tens[tmpVal/10]);
		tmpVal = tmpVal % 10;
		s.append(ones[tmpVal]);
		
		return s.toString();
	}
	
	/** This function returns value of a Roman symbol */
    private static int value(char r)
    {
        if (r == 'I')
            return 1;
        if (r == 'V')
            return 5;
        if (r == 'X')
            return 10;
        if (r == 'L')
            return 50;
        if (r == 'C')
            return 100;
        if (r == 'D')
            return 500;
        if (r == 'M')
            return 1000;
        return -1;
    }
    
    @Override
	public Type infer() throws Exception {
    	this.setType(TypeConcrete.TypeIntRoman);
		return TypeConcrete.TypeIntRoman;
	}

	@Override
	public Literal fromDefaultRepresentation(Literal l) {
		IntBinary def = (IntBinary)l;
		return new IntRoman(IntRoman.int2roman(def.value));
	}

	@Override
	public Literal toDefaultRepresentation() {
		return new IntBinary(IntRoman.roman2int(this.value));
	}

	@Override
	public Literal convertRepresentation(Class<? extends Literal> c) throws Exception {
		if(c == IntRoman.class){
			return this;
		}
		if(c == IntBinary.class){
			return this.toDefaultRepresentation();
		}
		if(c == IntString.class){
			return new IntString(Integer.toString(IntRoman.roman2int(this.value)));
		}
		return super.convertRepresentation(c);
	}
}
