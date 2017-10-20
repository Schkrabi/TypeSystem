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
	
	public static int roman2int(IntRoman value){
		String str = value.value;
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
}
