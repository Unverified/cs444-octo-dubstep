public class first {
  public first(){}

  public static int m1(int x){
    int test = 243;

    if(test == 0) {
      x = x + 3;
    } else if (test == 1) {
      x = x + 2;
    } else {
      x = x + 1;
    }

    return x;
  }

  public static int test() {
    int x = first.m1(14656);
    return x;
  }
}
