public class J1_commentsInExp4 {

    public J1_commentsInExp4 () {}

    public int 
		  foo() {
	return 123;
    }

    public static int test() {
	J1_commentsInExp4 j = /* comments */ new /* are */ J1_commentsInExp4();
        return j./* not enough! */foo();
    }

}
