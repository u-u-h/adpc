/*
 * ResultListener.java
 *
 * Created on June 22, 2007, 10:25 AM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package ADPDemo;

import java.util.List;

/**
 *
 * @author gsauthof
 */
public interface ResultListener {

	void newResult(List<Candidate> list);
	
}
