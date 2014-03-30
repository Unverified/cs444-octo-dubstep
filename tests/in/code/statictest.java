public class statictest {

  public static int s = 0;
  public int x = 14657;

  public statictest() {
  }

  public int m1() {
    x = 14657;
    int y = 0;
    y = x;
    return s;
  }

  public static int test() {
    return 0;
  }
}
