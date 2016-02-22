package ADPC;

import ADPC.Output;
import static ADPC.ADP.*;
import ADPC.DoubleTupel;
import ADPC.IntTupel;


public class DefaultOutput implements Output {

  public void prelude(String s)
  {
    System.out.println("Algebra: " + s);
  }

  public void optimal(int score)
  {
    print_optimal(score);
  }

  public void optimal(double score)
  {
    print_optimal(score);
  }

  public void optimal(DoubleTupel score)
  {
    optimal(score.getMainValue());
  }

  public void optimal(IntTupel score)
  {
    optimal(score.getMainValue());
  }

  public void suboptimal(int score, StringBuilder s)
  {
    print_suboptimal(score, s.toString());
  }

  public void start(int diff)
  {
    print_start();
  }

  public void end()
  {
    print_end();
  }

}
