package foo;

import foo.bar.InterfaceA;

public class ClassA extends Object implements InterfaceA {
  public Object x;
  public Object m1() {
    int a = m2().m3();
    Object obj = new Object();
    Object[] objs = new Object[3];
    Object obj2 = (Object)obj;
  }

  public ClassA() {
    
  }
}
