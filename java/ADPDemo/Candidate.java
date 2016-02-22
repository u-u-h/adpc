/*
 * Candidate.java
 *
 * Created on June 20, 2007, 3:37 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package ADPDemo;

import Tree.Tree;

/**
 *
 * @author gsauthof
 */
public class Candidate implements Comparable {

	private Tree tree = new Tree();
	private String pprint;
	private int score;

	public Candidate(String pprint, int score, Tree tree) {
		this.pprint = pprint;
		this.score = score;
		this.tree = tree;
	}
	
	/** Creates a new instance of Candidate */
	public Candidate() {
	}

	public Tree getTree() {
		return tree;
	}

	public void setTree(Tree tree) {
		this.tree = tree;
	}

	public String getPprint() {
		return pprint;
	}

	public void setPprint(String pprint) {
		this.pprint = pprint;
	}

	public int getScore() {
		return score;
	}

	public void setScore(int score) {
		this.score = score;
	}

	public int compareTo(Object o) {
		return score - ((Candidate)o).getScore();
	}

	public String toString() {
		return pprint + " (" + score + ")";
	}
	
}
