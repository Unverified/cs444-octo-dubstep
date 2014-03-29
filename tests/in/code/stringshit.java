public class stringshit {
  public stringshit(){}

  public stringshit(char[] c){
    c[2] = 'd';
  }

  public static int test() {
    String x = "abc";

    if(x == new String("abc")) return 14657;
    else return 14658;
  }
}
