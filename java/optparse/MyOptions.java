package optparse;

import optparse.Option;

/**
 * Part of simple stupid test case.
 *
 * @author Georg Sauthoff, gsauthof<at>(techfak.uni.bielefeld.de|sdf.lonestar.org), 2007/05
 */
class MyOptions {
    @Option(opt = 'w' , desc = "enables window mode")
      public boolean window_mode;
    @Option(opt = 'p', gnu = "prefix") public String prefix;
    @Option(opt = 'l', gnu = "window-size") public int window_size;


  @Option(opt = 'a', gnu = "algebras", arg = "buyer", multiple = true)
    public boolean enable;
}

/**
 * Part of simple stupid test case.
 *
 * @author Georg Sauthoff, gsauthof<at>(techfak.uni.bielefeld.de|sdf.lonestar.org), 2007/05
 */
class MoreOptions {
  @Option(opt = 'a', arg = "seller", multiple = true) public boolean enable;
  @Option(gnu = "special", desc = "magic value") public int special;
}
