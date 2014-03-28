public class methodcall {
  public methodcall(){}

  public static int m1(int x, int y) {
    int z = 10000;
    z = x + y;
    return z;
  }

  public static int test() {
    int x = 4000;
    int y = 658;
    return methodcall.m1(x, y);
  }
}
