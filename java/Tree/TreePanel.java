/*
 * TreePanel.java
 *
 * Created on June 19, 2007, 3:21 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package Tree;

import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import javax.swing.JComponent;

/**
 *
 * @author gsauthof
 */
public class TreePanel extends JComponent {

	private Tree tree;

	public TreePanel(Tree tree) {
		this();
		this.setTree(tree);
	}
	
	/** Creates a new instance of TreePanel */
	public TreePanel() {
		super();
	}

	float y_unit;

	int[] levels;
	Graphics2D g2;
	
	
protected void paintComponent(Graphics g) {
        if (isOpaque()) { //paint background
            g.setColor(getBackground());
            g.fillRect(0, 0, getWidth(), getHeight());
        }
	if (tree.getDepth() == 0)
		return;
	g2 = (Graphics2D) g.create();
	g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
	    RenderingHints.VALUE_ANTIALIAS_ON);
	levels = new int[tree.getDepth()];
	y_unit = getHeight() / tree.getDepth();
	tree.depthFirst(new NodeVisitor() {
		public void visit(Node node, int level) {
			float x_unit = getWidth() / tree.getWidth(level);
			FontMetrics fm = g2.getFontMetrics();
			Rectangle2D b = fm.getStringBounds(node.getLabel(), g2);
			if (level > 0) {
				float new_x = x_unit /2 + x_unit * levels[level];
				float new_y = y_unit * level + y_unit / 2
					- (float) b.getHeight() ;
				g2.draw(new Line2D.Float(
				    node.getParent().getX(), 
				    node.getParent().getY(), 
				    new_x, new_y));
			}
			g2.drawString(node.getLabel(), x_unit * levels[level]
				+ x_unit / 2 - (float) b.getWidth()/2 ,
				y_unit * level + y_unit / 2);
			node.setX(x_unit /2 + x_unit * levels[level]);
			node.setY(y_unit * level + y_unit / 2 
				+ (float) b.getHeight() / 3 );
			levels[level]++;
		}
	});
	g2.dispose();
}

public Dimension getPreferredSize() {
	return new Dimension(800, 100);
}

	public void setTree(Tree tree) {
		this.tree = tree;
		this.repaint();
	}

}