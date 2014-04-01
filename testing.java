public class testing {
  public int y = 23452;
  protected int x;
  
  public testing(){}

  public String foo() {
   // String result = ""+(x = x+1);
   // if ((x % 3) == 0) result = null;
   // return result;
    String result = ""+(x = x+1);
    return result;
  }

    public int concat() {
	int n = 10;
	int sum = 0;
	//while (n > 0) {
	    String s = foo();
            //String x = s + foo();
	//    sum = sum + s.hashCode();
	//    n = n-1;
	//}
	return s.length();
    }
    
    public int hashCode(char[] chars) {
        int h = 0;
        for (int i = 0; i < chars.length; i = i+1) {
            h = h + 1;
        }

        return h;
    }

  public static int test() {
    return new testing().concat();
  }
}
