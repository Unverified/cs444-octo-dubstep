package tlinker_test;

import tlinker_test.A.*;

public class classA extends classB {
  public classA() {}
  public classA m1() {
    classA var = classA.m1();
    return var;
  }
}
