// TYPE_CHECKING
public class J1_typecheck_instanceof {

    public J1_typecheck_instanceof () {}

    public static int test() {
	if(!(new Object() instanceof Object)) return 0;		// 0
	if(!(new Object[0] instanceof Object[])) return 1;	// 1
	if(!(new Object[0] instanceof Object)) return 2;	// 2
	if(null instanceof Object) return 3;			// 3
	if(!(new Integer(17) instanceof Number)) return 4;	// 5
	if(new Object() instanceof Number) return 5;		// 5

        /* Nicks super special awesome tests of endurance */

        int[] a = new int[0];
        Object o = new Object();				// o == Object
        Object ao = a;						// ao == int[]

        if(!(a instanceof int[])) return 6;			// 6  (a instanceof int[]) == true
        if(!(a instanceof Object)) return 7;			// 7  (a instanceof Object) == true
        if(!(ao instanceof int[])) return 8;			// 8  (ao instanceof int[]) == true
        if(null instanceof int[]) return 9;			// 9
        if(null instanceof Object[]) return 10;			// 10
        if(o instanceof int[]) return 11;			// 11 (o instanceof int[]) == false
        if(new Object() instanceof int[]) return 12;		// 12 (new Object() instanceof int[]) == false

	return 123;
    }
}
