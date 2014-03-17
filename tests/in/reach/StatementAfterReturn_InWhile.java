public class StatementAfterReturn_InWhile {
  public StatementAfterReturn_InWhile(){}

  public int m1() {
    int x = 0;
    while(x == 0) {
      return 0;
      x = 0;
    }

    return 0;
  }
}
