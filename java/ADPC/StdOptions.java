package ADPC;

import optparse.Option;

public class StdOptions {
  @Option (opt = 'd', gnu = "diff", desc = "traceback diff")
    public int traceback_diff = 5;
  @Option (opt = 'f', gnu = "file", desc = "read from filename instead of argv")
    public String filename;
}

