public class stringplus {
  public stringplus() {}

  public String something() {
    return new String("a");
  }

  public static int test() {
    String a = "a";
    String ab = a + 1;

    return ab.charAt(1);
  }
}
