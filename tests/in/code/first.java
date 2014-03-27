public class first {
  public first(){}

  public static void m1() {
    int x = 4655;
    x = x + 1;          // 14675, when passed into nativeWrite will display 'A', dont ask me why
    int y = 9999;
    y = y + x + 1 + 1;
  }

  public static int test() {
    first.m1();
    return 0;
  }
}
