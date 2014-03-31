public class stringshit {

  public static int x = 0;

  public stringshit(){}

  public int m1(String strarg) {
    int result = 0;
    boolean pass1 = false;
    boolean pass2 = false;
    boolean pass3 = false;
    boolean pass4 = false;
    boolean pass5 = false;

    String shit = "a";

    if(shit == "a") pass1 = true;

    shit = "ab";
    if(shit == "ab") pass2 = true;

    shit = new String("ab");
    if(shit != "ab") pass3 = true;

    shit = "abc";
    if(shit != new String("abc")) pass4 = true;

    if(strarg != "strarg") pass5 = true;

    if(!pass1) return 14658;
    if(!pass2) return 14659;
    if(!pass3) return 14660;
    if(!pass4) return 14661;
    if(!pass5) return 14662;
    else return 14657;		//passed
  }

  public static int test() {
    stringshit sh = new stringshit();
    return sh.m1("strarg");
  }
}
