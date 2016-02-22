/*
 * CandidatePanel.java
 *
 * Created on June 20, 2007, 3:26 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package ADPDemo;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

/**
 *
 * @author gsauthof
 */
public class CandidatePanel extends JPanel implements ResultListener,
	ListSelectionListener {
	
	JList list;
	JScrollPane listScroller;
	List<SelectionListener> listener = new LinkedList<SelectionListener>();
	
	/** Creates a new instance of CandidatePanel */
	public CandidatePanel() {
		setLayout(new BorderLayout());
		list = new JList();
		list.setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		list.setLayoutOrientation(JList.HORIZONTAL_WRAP);
		list.setVisibleRowCount(-1);
		list.addListSelectionListener(this);
		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		list.setLayoutOrientation(JList.VERTICAL);
		//JScrollPane listScroller = new JScrollPane(list);
		listScroller = new JScrollPane();
		listScroller.getViewport().setView(list);
		listScroller.setPreferredSize(new Dimension(getWidth(), 80));
		add(listScroller);
	}

	public void newResult(List<Candidate> l) {
		Vector v = new Vector(l);
		list.setListData(v);
		listScroller.setPreferredSize(new Dimension(getWidth(), 80));
		revalidate();
	}

	public void valueChanged(ListSelectionEvent e) {
		if (list.getSelectedIndex() == -1)
			return;
		for (SelectionListener l : listener)
			l.newSelection(list.getSelectedIndex());
	}

	public void addSelectionListener(SelectionListener l) {
		listener.add(l);
	}

	
}
