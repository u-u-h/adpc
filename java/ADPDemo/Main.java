/*
 * Main.java
 *
 * Created on June 15, 2007, 6:45 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package ADPDemo;

import java.awt.BorderLayout;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

/**
 *
 * @author georg
 */
public class Main {
    
    /** Creates a new instance of Main */
    public Main() {
    }

    public static void main(String[] argv) {
	    JFrame main = new JFrame();
	    main.addWindowListener(new WindowAdapter() 
	     { public void windowClosing(WindowEvent e) { System.exit(0);} });
	    InputPanel ipanel = new InputPanel();
	    Compute compute = new Compute();
	    SortOutput sortOutput = new SortOutput(compute.getAlgebra());
	    compute.getAlgebra().setOutput(sortOutput);
	    MainPanel mainPanel = new MainPanel();
	    ipanel.addInputListener(mainPanel);
	    ipanel.addInputListener(compute);
	    CandidatePanel candidatePanel = new CandidatePanel();
	    candidatePanel.addSelectionListener(mainPanel);
	    sortOutput.addResultListener(mainPanel);
	    sortOutput.addResultListener(candidatePanel);
	    main.add(ipanel, BorderLayout.NORTH);
	    main.add(mainPanel);
	    main.add(candidatePanel, BorderLayout.SOUTH);
	    main.setSize(400, 400);
	    JMenu menu = new JMenu("File");
	    JMenuItem item = new JMenuItem("Exit");
	    JMenuBar bar = new JMenuBar();
	    menu.add(item);
	    bar.add(menu);
	    main.setJMenuBar(bar);
	    main.setVisible(true);
    }
    
}
