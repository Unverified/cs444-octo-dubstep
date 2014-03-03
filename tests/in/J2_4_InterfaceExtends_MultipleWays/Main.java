// HIERARCHY
// JOOS1: PARSER_WEEDER,JOOS1_INTERFACE,PARSER_EXCEPTION
/* Hierarchy:
 * Joos 2: An interface must not be mentioned more than once in the same extends
 *  clause of an interface (missing from the JLS but enforced by javac).
 *  
 * Main implements D
 * D implements B, C, A
 * C implements A
 * B implements A
 */

public abstract class Main implements D {
    
    public abstract int a1();

    public Main() {}
    
    public static int test() {
	return 123;
    }
}
