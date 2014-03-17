public class AllReachable {
  public AllReachable(){}

  public int m1() {
    boolean tru = true;

    if(tru) {
      int x = 0;
      while(x < 100) {
        x = x + 1;
      }
      return x;
    }
      
    return 0;
  }
}
