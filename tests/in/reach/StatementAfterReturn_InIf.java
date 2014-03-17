public class StatementAfterReturn_InIf {
  public StatementAfterReturn_InIf(){}

  public int m1() {
    int x = 0;
    if(x == 0) {
      return 0;
      x = 0;
    }

    return 0;
  }
}
