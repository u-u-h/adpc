package optparse;

import java.lang.Exception;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.io.PrintStream;
import java.io.IOException;

import optparse.Option;
import optparse.MyOptions;
import optparse.ParseException;
import optparse.HelpException;


/**
 * The OptionParser API.
 *
 * See optparse.Main.java for an example how to use this nice API.
 *
 * Implemenatation notes:
 *
 *   Uses at least three > Java 5.0 Features:
 *     Enums, Runtime Annotations, new for loops, Generics, StringBuilder,
 *     PrintStream.printf
 *
 * @author Georg Sauthoff, gsauthof<at>(techfak.uni.bielefeld.de|sdf.lonestar.org), 2007/05
 */
public class OptionParser {
  
  HashMap<Character,Opt> s_table = new HashMap<Character,Opt>();
  HashMap<String,Opt> l_table = new HashMap<String, Opt>();
  LinkedList<Opt> list = new LinkedList<Opt>();
  
  enum Type { BOOL, INT, STRING, MULTIPLE };
  
  enum State { START, VALUE, FILES };
  
  class Opt {

    Opt()
    {
    }

    void set(Option a, Field f, Object parent)
    {
      this.parent = parent;
      this.a = a;
      this.f = f;
    }
    
    public Opt(Option a, Field f, Object parent) throws IllegalAccessException {
      Class c = f.getType();
      
      if (c == boolean.class) {
        if (a.multiple()) {
          if (a.arg() == null)
            throw new IllegalAccessException(
                "You have to specify arg if you use multiples");
          type = Type.MULTIPLE;
          hasArg = true;
          multiples = new HashMap<String, Opt>();
          addMultiple(a, f, parent);
          this.a = a;
          return;
        } else
          type = Type.BOOL;
      }
      else
        if (c == String.class)
          type = Type.STRING;
        else
          if (c == int.class)
            type = Type.INT;
          else
            throw new IllegalAccessException("Datatype not supported");
      
      hasArg = c != boolean.class;
      set(a, f, parent);
    }

    String opt()
    {
      String s = "";
      if (a.opt() != ' ') {
        s = "-" + a.opt();
        if (!a.gnu().equals(""))
          return s + ", --" + a.gnu();
        return s;
      }
      return "--" + a.gnu();
    }

    void printLine(String args)
    {
      writer.printf("%-20s%-20s %s\n" , opt(), args, a.desc());
    }

    public void print()
    {
      String args;
      if (hasArg) {
        if (type == Type.MULTIPLE) {
          StringBuilder sb = new StringBuilder();
          for (Opt o : multiples.values()) {
            sb.append(o.a.arg());
            sb.append('|');
          }
          sb.deleteCharAt(sb.length()-1);
          args = sb.toString();
        }
        else
          args = a.arg();
        printLine(args);
      } else
        printLine("");
    }

    void addMultiple(Option a, Field f, Object parent)
    {
          Opt o = new Opt();
          o.type = Type.BOOL;
          o.set(a, f, parent);
          multiples.put(a.arg(), o);
    }
    
    public void setValue(String s) throws IllegalAccessException,
         ParseException, IOException  {
      
      switch (type) {
        case STRING : f.set(parent, s);
        break;
        case INT : f.setInt(parent, Integer.parseInt(s));
        break;
        case MULTIPLE :
          if (!multiples.containsKey(s))
            throw new ParseException("Unknown value: " + s);
          Opt t = multiples.get(s);
          t.toggle();
          break;
        default : throw new IllegalAccessException();
      }
    }
    
    public boolean hasArgument() {
      return hasArg;
    }
    
    public void toggle() throws IllegalAccessException, IOException  {
      boolean b = ! f.getBoolean(parent);
      f.setBoolean(parent, b);

      if (verbose) {
        if (b)
          writer.print("Enabled: ");
        else
          writer.print("Disabled: ");
        writer.print(a.arg());
        writer.print("\n");
      }
    }
    
    boolean hasArg;
    Field f;
    Option a;
    Object parent;
    Type type;

    HashMap<String, Opt> multiples;
  }
  
  public OptionParser()
  {
  }
  
  public OptionParser(Object o) throws IllegalAccessException {
    init(o);
  }
  
  public void init(Object o) throws IllegalAccessException {
    for (Field field: o.getClass().getFields()) {
      Option a = field.getAnnotation(Option.class);
      if (a == null)
        continue;
      Opt t = null;

      if (a.opt() == ' ' && a.gnu().equals(""))
        throw new IllegalAccessException("Annotate at least with opt or gnu");
      if (a.multiple()) {
        if (a.opt() != ' ' && s_table.containsKey(a.opt()))
          t = s_table.get(a.opt());
        else
          if (!a.gnu().equals("") && l_table.containsKey(a.gnu()))
            t = l_table.get(a.gnu());
        if (t != null) {
          t.addMultiple(a, field, o);
          continue;
        }
      } 
      if (a.opt() != ' ' && s_table.containsKey(a.opt()))
        throw new IllegalAccessException("Option already present: " + a.opt()
            + " (" + s_table.get(a.opt()).opt() + ")" );
      if (!a.gnu().equals("") && l_table.containsKey(a.gnu()))
        throw new IllegalAccessException("Option already present: " + a.gnu());
      t = new Opt(a, field, o);
      s_table.put(a.opt(), t);
      l_table.put(a.gnu(), t);
      list.add(t);
    }
  }
  
  public List<String> parse(String[] argv)
    throws ParseException, IOException, HelpException {
    Opt current = null;
    List<String> files = new LinkedList<String>();
    State state = State.START;
    String s = null;
    
    try {
      for (int i = 0; i<argv.length; i++) {
        s = argv[i];
        String k;
        int r;
        String a = "-";
        String b = "--";
        switch (state) {
          case START :
            if (s.equals(""))
                continue;
            if (s.startsWith(b)) {
              if (s.length() == 2)
                state = State.FILES;
              else {
                r = s.indexOf('=');
                if (r==-1)
                  k = s.substring(2);
                else
                  k = s.substring(2,r);
                if (k.equals("help")) {
                  showHelp();
                  throw new HelpException();
                }
                if (l_table.containsKey(k)) {
                  current = l_table.get(k);
                  if (r==-1)
                    if (current.hasArgument()) {
                      state = State.VALUE; // --foo=bar vs --foo bar
                      break;
                      //throw new ParseException("gnu style argument is missing");
                    } else
                      current.toggle();
                    current.setValue(s.substring(r+1));
                } else
                  throw new ParseException("unknown long option: " + k);
              }
            } else
              if (s.startsWith(a))
                if (s.length() == 2) {
                  if (s.charAt(1) == 'h') {
                    showHelp();
                    throw new HelpException();
                  }
                  if (s_table.containsKey(s.charAt(1))) {
                    current = s_table.get(s.charAt(1));
                    if (current.hasArgument())
                      state = State.VALUE;
                    else
                      current.toggle();
                  } else
                    throw new ParseException("unknown option: " + s);
                } else
                  throw new ParseException("short option is too long: " + s);
              else
                files.add(s);
            break;
          case VALUE:
            current.setValue(s);
            state = State.START;
            break;
          case FILES:
            files.add(s);
            break;
            
        }
        
      }
    } catch (IllegalAccessException e) {
      e.printStackTrace();
      System.out.println("Please report a bug!");
      System.exit(1);
    }
    if (current != null && current.hasArgument() && state != State.START)
      throw new ParseException("Argument is missing: " + s);
    
    return files;
  }

  public void showHelp() throws IOException
  {
    for (Opt o : list)
      o.print();
    writer.printf("%-40s shows this nice help screen\n", "-h, --help");
  }

  PrintStream writer = System.out;


  public void setPrintStream(PrintStream w)
  {
    writer = w;
  }

  boolean verbose;

  public void setVerbose(boolean b)
  {
    verbose = b;
  }

}

