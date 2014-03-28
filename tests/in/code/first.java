public class first {
  public first(){}

  public static int test() {
    int y = 0;
    boolean x = true;

    x = ((y == 1) || x) && x;

    if(x == true) {
      y = 14657;
    } else if (x == false) {
      y = 14658;
    } else {
      y = 14659;
    }

    return y;
  }
}
