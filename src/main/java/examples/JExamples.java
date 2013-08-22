package examples;

public class JExamples {

  public int simpleIf(int arg) {
    boolean cond = arg > 1;
    System.out.print(cond);
    int res = cond ? arg : 1;
    return res;
  }

}