package ADPC;

import static ADPC.ADP.convert_input;
import static ADPC.RNA.init_rna;

public class Sequence {
  public Sequence(String s) {
    byte[] seq = convert_input(s);
    init_rna(seq);

    sequence = seq;
    backup = seq.clone();
    unconverted = s;
    original_length = s.length();
  }

  public int original_length;
  public String unconverted;
  public byte[] sequence;
  public byte[] backup;
}
