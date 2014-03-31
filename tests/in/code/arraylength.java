public class arraylength {
  public arraylength() {}

  public static int test() {
    int length = 5000;
    int[] array = new int[length];

    for(int i = 0; i < length; i = i + 1) {
      array[i] = 1;
    }

    int x = 0;
    for(int i = 0; i < length; i = i + 1) {
      x = x + array[i];
    }

    return x + array.length + 4657;
  }
}
