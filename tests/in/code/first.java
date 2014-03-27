public class first {
  public first(){}

  public static int test() {
    int x = 4656;
    x = x + 1;		// 14675, when passed into nativeWrite will display 'A', dont ask me why
    int y = 10000;
    y = y + x;

    return x;
  }
}
