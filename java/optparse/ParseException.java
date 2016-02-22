package optparse;

/**
 * Thrown in case of an user error.
 *
 * @author Georg Sauthoff, gsauthof<at>(techfak.uni.bielefeld.de|sdf.lonestar.org), 2007/05
 */
public  class ParseException extends Exception {
  ParseException(String name) {
  super(name);
  }
}

