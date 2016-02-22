package ADPC;

import optparse.Option;

public class Options {
  // FIXME deprecated, not needed anymore -> nullop
  @Option(opt = 'x', gnu = "window-mode", desc = "enables the Window Mode")
    public boolean window_mode = false;
  @Option(opt = 'w', gnu = "size", desc = "size of the window")
    public int window_size = 0;
  @Option(opt = 'W', gnu = "step", desc = "step size for the moving window" )
    public int window_step = 1;
  public int window_pos;
  @Option (opt = 'c', gnu = "traceback percent", desc = "suboptimal candidate range in percent")
    public int traceback_percent = 0;
  @Option (opt = 'e', gnu = "traceback diff", desc = "suboptimal candidate range")
    public int traceback_diff = -1;

  @Option (opt = 'f', gnu = "file", desc = "read sequence from file instead as non-option argument")
    public String filename;

}


