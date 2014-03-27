public class first {
  public first(){}

  public static void m1(int x){
    x = 1;
  }

  public static int test() {
    int x = 0;

    x = x + 1 * (x = 2);

    first.m1(x);

    return x;
  }
}
