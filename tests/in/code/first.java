public class first {
  public first(){}

  public static int test() {
    int y = 0;
    boolean x = true;

    x = x || false;

    if(x == true) {
      y = 14657;
    } else if (x == false) {
      y = 14658;
    }

    return y;
  }
}
