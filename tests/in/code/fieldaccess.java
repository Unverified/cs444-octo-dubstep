public class fieldaccess {
  public int x = 1000;
  public static int y = 1000;

  public fieldaccess() {}

  public static int test() {
    fieldaccess fa = new fieldaccess();
    fa.x = fa.x + 1000;				// fa.x == 2000
    //fa.y = fa.y + 1000;			// apperently your not allowed to do this? (in joos that is)
    fieldaccess.y = fieldaccess.y + 1000;	// fieldaccess.y == 2000
    return fa.x + fieldaccess.y + 10657;
  }
}
