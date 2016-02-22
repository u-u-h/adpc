

package optparse;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Use this Annotation for your class-Fields.
 *
 * Implementation notes:
 *
 *   see http://java.sun.com/docs/books/tutorial/java/javaOO/annotations.html
 *   and http://java.sun.com/j2se/1.5.0/docs/guide/language/annotations.html
 *
 *   without this annotation of the annotation (i.e. RetentionPolicy.RUNTIME)
 *   aint no reflection stuff will work ...
 *
 * @author Georg Sauthoff, gsauthof<at>(techfak.uni.bielefeld.de|sdf.lonestar.org), 2007/05
 */
@Retention(RetentionPolicy.RUNTIME)
 public @interface Option {
    char opt() default ' ';
    String gnu() default  "";
    String desc() default  "";
    String arg() default  "FOO";

    boolean multiple() default false;

}
