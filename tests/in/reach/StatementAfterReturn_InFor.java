public class StatementAfterReturn_InFor {
  public StatementAfterReturn_InFor(){}

  public int m1() {
    int x = 0;
    for(;x == 0;) {
      return 0;
      x = 0;
    }

    return 0;
  }
}
