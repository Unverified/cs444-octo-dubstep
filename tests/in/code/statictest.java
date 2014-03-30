public class statictest {

  public static int s = 0;
  public int x = 0;

  public statictest() {
  }

  public static int test() {
    statictest t = new statictest();
    t.x = 14657;
    return t.x;
  }
}
