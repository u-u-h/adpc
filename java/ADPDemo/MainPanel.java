/*
 * MainPanel.java
 *
 * Created on June 18, 2007, 10:45 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package ADPDemo;

import Tree.Tree;
import Tree.TreePanel;
import de.unibi.bibiserv.rnamovies.RNAMovies;
import java.awt.Dimension;
import java.util.List;
import java.util.Vector;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.JSplitPane;

/**
 *
 * @author georg
 */
public class MainPanel extends JPanel implements ResultListener, InputListener,
	SelectionListener {
	
	Vector<Candidate> list;
	String seq;
	RNAMovies rnaM;
	TreePanel treeP;
	
	/** Creates a new instance of MainPanel */
	public MainPanel() {
		this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		treeP = new TreePanel(new Tree());
		treeP.setMinimumSize(new Dimension(200,100));
		rnaM = new RNAMovies();
		JSplitPane sp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
			treeP, rnaM);
		add(sp);
		//r.setData(">foo\nacgaucgacuagcuaggc\n....((....))......\n");
	}
	
	public void newResult(List<Candidate> list) {
		this.list = new Vector(list);
		StringBuilder b = new StringBuilder();
		b.append(">foo\n");
		b.append(seq);
		b.append('\n');
		for (Candidate c : list) {
			b.append(c.getPprint());
			b.append('\n');
		}
		rnaM.setData(b.toString());
		treeP.setTree(list.get(0).getTree());
	}
	
	public void newInput(String s, int p) {
		seq = s;
	}

	public void newSelection(int i) {
		rnaM.getMovie().gotoFrame(i);
		treeP.setTree(list.get(i).getTree());
	}
	
}
