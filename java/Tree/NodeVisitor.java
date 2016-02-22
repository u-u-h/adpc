/*
 * NodeVisit.java
 *
 * Created on June 18, 2007, 11:43 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package Tree;

/**
 *
 * @author georg
 */
public interface NodeVisitor {

	void visit(Node node, int level);
	
}
