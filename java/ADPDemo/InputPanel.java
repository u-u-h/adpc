/*
 * InputPanel.java
 *
 * Created on June 20, 2007, 3:03 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package ADPDemo;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.List;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

/**
 *
 * @author gsauthof
 */
public class InputPanel extends JPanel implements ActionListener {
	
	private List<InputListener> listener = new LinkedList();
	
	JTextField field = new JTextField("agcaugcaucgaucgacguac");
	JTextField percent = new JTextField("10");
	
	/** Creates a new instance of InputPanel */
	public InputPanel() {
		super();
		setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
		add(field);
		add(percent);
		JButton button = new JButton("OK");
		button.addActionListener(this);
		add(button);
	}
	
	public void actionPerformed(ActionEvent e) {
		int p = 0;
		p = Integer.parseInt(percent.getText());
		for (InputListener il : listener)
			il.newInput(field.getText(), p);
	}
	
	public void addInputListener(InputListener il) {
		listener.add(il);
	}
	
}
