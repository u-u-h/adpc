package optparse;

import java.io.IOException;
import java.util.List;

import optparse.OptionParser;
import optparse.ParseException;
import optparse.HelpException;

import optparse.MyOptions;
import optparse.MoreOptions;

/**
 * Simple stupid test class.
 *
 * @author Georg Sauthoff, gsauthof<at>(techfak.uni.bielefeld.de|sdf.lonestar.org), 2007/05
 */
class Main {
  
  /** Creates a new instance of Main */
  public Main() {
  }
  
  /**
   * @param args the command line arguments
   */
  public static void main(String[] args) {
    MyOptions my = new MyOptions();
    MoreOptions mr = new MoreOptions();
    try {
      OptionParser op = new OptionParser(my);
      op.setVerbose(true);
      op.init(mr);
      List<String> files = op.parse(args);
      System.out.println(my.prefix + " " + my.window_mode + " " +
          my.window_size + " " + my.enable + " " + mr.enable);
      for (String s : files)
        System.out.println(s);
    } catch (HelpException e) {
      System.exit(0);
    }
    catch (ParseException e) {
      System.err.println(e.getMessage());
      System.exit(1);
    }
    catch (Exception e) {
      System.err.println("Write a bug report!");
      e.printStackTrace();
      System.exit(1);
    }
  }
  
}
