/*
 * SortOutput.java
 *
 * Created on June 20, 2007, 3:32 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package ADPDemo;

import ADPC.Algebra;
import ADPC.DoubleTupel;
import ADPC.IntTupel;
import ADPC.Output;
import Tree.Tree;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

/**
 *
 * @author gsauthof
 */
public class SortOutput implements Output {

	List<Candidate> list;
	Algebra algebra;
	
	/** Creates a new instance of SortOutput */
	public SortOutput(Algebra algebra) {
		this.algebra = algebra;
	}

	public void prelude(String s) {
	}

	public void optimal(int score) {
	}

	public void optimal(double score) {
	}

	public void optimal(DoubleTupel score) {
	}

	public void suboptimal(int score, StringBuilder s) {
		Tree tree = new Tree();
		algebra.getTree(tree.getRoot());
		tree.init();
		list.add(new Candidate(s.toString(), score, tree));
	}

	public void start(int diff) {
		list = new LinkedList<Candidate>();
	}

	public void end() {
		Collections.sort(list);
		for (ResultListener l : listener)
			l.newResult(list);
	}

	List<ResultListener> listener = new LinkedList<ResultListener>();

	void addResultListener(ResultListener l) {
		listener.add(l);
	}

	public void optimal(IntTupel score) {
	}

	
}
