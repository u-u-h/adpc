package ADPC;

import ADPC.DoubleTupel;
import ADPC.IntTupel;

public interface Output {

  public void prelude(String s);

  public void optimal(int score);

  public void optimal(double score);

  public void optimal(DoubleTupel score);

  public void optimal(IntTupel score);

//  public void optimal(double score);
//  Variante fuer tupel etc.

  public void suboptimal(int score, StringBuilder s);


  public void start(int diff);

  public void end();




}
