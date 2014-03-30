public class statictest {

  public static int s = 0;
  public int x = 0;

  public statictest() {
  }

  public int m1() {
    
    return 0;
  }

  public static int test() {
    statictest s = new statictest();
    s.m1();
    return 0;
  }
}
