/*
 * Compute.java
 *
 * Created on June 22, 2007, 10:20 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package ADPDemo;

import ADPC.Options;
import ADPC.RnaAlgebra;
import ADPC.Sequence;
import RNAfold.Algebra_mfe;

/**
 *
 * @author gsauthof
 */
public class Compute implements InputListener {

	private RnaAlgebra algebra;
	
	/** Creates a new instance of Compute */
	public Compute() {
		algebra = new Algebra_mfe();
	}

	public void newInput(String s, int p) {
		Sequence seq = new Sequence(s);
		Options opts = new Options();
		opts.traceback_percent = p;
		getAlgebra().init(seq, opts);
		getAlgebra().start();
	}

	public RnaAlgebra getAlgebra() {
		return algebra;
	}

	
}
