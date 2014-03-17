public class StatementAfterInfWhile {
  public StatementAfterInfWhile(){}

  public int m1() {
    int x = 0;
    while(true) {
      x = x + 1;
    }

    x = 100;
    return x;
  }
}
