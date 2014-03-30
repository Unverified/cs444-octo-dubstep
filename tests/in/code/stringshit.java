public class stringshit {
  public stringshit(){}

  public static int x = 0;

  public stringshit(char[] c){
    c[2] = 'd';
  }

  public static int test() {
    stringshit shit = new stringshit();
    String x = "abc";

    if(x == new String("abc")) return 14657;
    else return 14658;
  }
}
