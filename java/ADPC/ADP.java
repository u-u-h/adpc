package ADPC;

import ADPC.Options;
import ADPC.Sequence;
import ADPC.DoubleTupel;
import ADPC.IntTupel;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ADP {

  public static int decode(char c)
  {
    return (int) c - (int) '0';
  }

  public static void print_start()
  {
    System.out.println("============================== Suboptimal Output ===============================");
  }

  public static void print_end()
  {
    System.out.println("================================================================================");
  }

  public static void print_optimal(int score)
  {
    System.out.println("Optimal Score: " + score);
  }


  public static void print_optimal(double score)
  {
    System.out.println("Optimal Score: " + score);
  }

  public static void print_suboptimal(int score, String s)
  {
    System.out.println(s + " (Score: " + score + ")");
  }


  static String d = new String("....................................................................................................");

  public static String dots(int i, int j)
  {
    int l = j-i;
    fill_string(d, l, '.');
    return d.substring(d.length()-l);
  }

  public static String dots(int l)
  {
    fill_string(d, l, '.');
    return d.substring(d.length()-l);
  }

  static void fill_string(String s, int l, char c)
  {
    for (int k=s.length(); k<l; k++)
      d += c;
  }

  static final int A = 0;
  static final int C = 1;
  static final int G = 2;
  static final int U = 3;
  static final int N = 4;
  
  // assumes returned sequence starts at 1 like adpc needs it
  public static byte[] convert_input(String s)
  {
    byte[] r = new byte[s.length()+1];

    for (int i=0; i < s.length(); i++)
      switch (s.charAt(i)) {
        case 'a' : r[i+1] = A; break;
        case 'A' : r[i+1] = A; break;
        case 'c' : r[i+1] = C; break;
        case 'C' : r[i+1] = C; break;
        case 'g' : r[i+1] = G; break;
        case 'G' : r[i+1] = G; break;
        case 'u' : r[i+1] = U; break;
        case 'U' : r[i+1] = U; break;
        case 't' : r[i+1] = U; break;
        case 'T' : r[i+1] = U; break;
        default : r[i+1] = N; break;
      }

    return r;
  }

  public static void shift_input(Options opts, Sequence seq, int c)
  {
    System.arraycopy(seq.backup, opts.window_pos+1, seq.sequence, 1,
        opts.window_size);

    if (c != 0)
      print_window(seq, opts);
  }

  static String spaces = new String("                                                                                                    ");
  public static void print_window(Sequence seq, Options opts)
  {
    String a = Integer.toString(opts.window_pos+1);
    String b = Integer.toString(opts.window_pos + opts.window_size);
    int c = opts.window_size - a.length() - b.length();
    fill_string(spaces, c, ' ');
    String r = a + spaces.substring(0, c) + b;    

    System.out.println(r);
    System.out.println(seq.unconverted.substring(opts.window_pos,
          opts.window_pos + opts.window_size));
  }

  public static String read_string(String filename) throws IOException
  {
    BufferedReader br = null;
    String r = null;
    br = new BufferedReader(new FileReader(filename));
    r = br.readLine();
    return r;
  }

  public static boolean is_suboptimal(int result_score, int score, int traceback_diff)
  {
    if (Math.abs(result_score - score) <= traceback_diff)
      return true;
    return false;
  }

  public static boolean is_suboptimal(DoubleTupel result_score, int score, int traceback_diff)
  {
    return is_suboptimal((int) result_score.getMainValue(), score, traceback_diff);
  }

  public static boolean is_suboptimal(IntTupel result_score, int score, int traceback_diff)
  {
    return is_suboptimal(result_score.getMainValue(), score, traceback_diff);
  }

  public static int get_result_score(int result_score)
  {
    return result_score;
  }

  public static int get_result_score(DoubleTupel result_score)
  {
    return (int) result_score.getMainValue();
  }

  public static int get_result_score(IntTupel result_score)
  {
    return result_score.getMainValue();
  }

  public static void main(String[] argv)
  {
    try {
      System.out.print(read_string(argv[0]));
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(1);
    }
  }
}
