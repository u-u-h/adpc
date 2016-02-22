/*
 * Tree.java
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
public class Tree {
	
	private Node root = new Node();
	
	public Tree() {
		
	}
	
	public Tree(Node root) {
		setRoot(root);
		init();
	}
	
	public Node getRoot() {
		return root;
	}
	
	public void setRoot(Node root) {
		this.root = root;
	}
	
	
	public void depthFirst(NodeVisitor visitor) {
		dFirst(visitor, root, 0);
	}
	
	private void dFirst(NodeVisitor visitor, Node node, int level) {
		visitor.visit(node, level);
		for (Node c : (List<Node>) node.getChildren())
			dFirst(visitor, c, level+1);
	}
	
	class Pair {
		public Pair(Node n, int level) {
			this.n = n;
			this.level = level;
		}
		Node n; int level;
	}
	
	public void breadthFirst(NodeVisitor visitor) {
		LinkedList<Pair> queue = new LinkedList<Pair>();
		queue.add(new Pair(root, 0));
		while (queue.size() > 0) {
			Pair current = queue.poll();
			visitor.visit(current.n, current.level);
			for (Node n : current.n.getChildren())
				queue.add(new Pair(n, current.level+1));
		}
	}
	
	int depth;
	
	private int initDepth() {
		depth = 0;
		depthFirst(new NodeVisitor() {
			public void visit(Node node, int level) {
				if (level+1 > depth)
					depth = level+1;
			}
		});
		return depth;
	}
	
	int[] widths;
	
	private void initWidths() {
		widths = new int[depth];
		depthFirst(new NodeVisitor() {
			public void visit(Node node, int level) {
				widths[level]++;
			}
		});
	}
	
	public void init() {
		initDepth();
		initWidths();
	}
	
	public int getDepth() {
		return depth;
	}
	
	public int getWidth(int level) {
		return widths[level];
	}
	
}
