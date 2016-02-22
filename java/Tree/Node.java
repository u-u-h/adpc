/*
 * Node.java
 *
 * Created on June 18, 2007, 10:51 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package Tree;

import java.util.LinkedList;
import java.util.List;

/**
 *
 * @author georg
 */
public class Node {

	private List<Node> children = new LinkedList<Node>();

	private String label;

	private Node parent = null;
	private float x;
	private float y;

        public Node(String label) {
          this.label = label;
        }
	
	/** Creates a new instance of Node */
	public Node() {
	}

	public List<Node> getChildren() {
		return children;
	}

	public void setChildren(List<Node> children) {
		this.children = children;
	}

	public void addChild(Node node) {
		children.add(node);
		node.setParent(this);
	}

	public String getLabel() {
		return label;
	}

	public void setLabel(String label) {
		this.label = label;
	}

	public Node getParent() {
		return parent;
	}

	public void setParent(Node parent) {
		this.parent = parent;
	}

	public boolean hasParent() {
		return parent != null;
	}

	public float getX() {
		return x;
	}

	public void setX(float x) {
		this.x = x;
	}

	public float getY() {
		return y;
	}

	public void setY(float y) {
		this.y = y;
	}
	
}
