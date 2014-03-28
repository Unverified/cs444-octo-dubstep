public class nesting {
  public nesting(){}

  public static int test() {
    int x = 0;
    
    if(x == 0) {
      for(;x < 1000; x = x + 1); // x == 1000
      if(x == 1000) {
        int y = 10;
        {
          int z = 10;
          x = x + y + z; // x == 1020
        }
        int z = 13637;
        x = x + z;       
      }
    }

    return x;
  }
}
