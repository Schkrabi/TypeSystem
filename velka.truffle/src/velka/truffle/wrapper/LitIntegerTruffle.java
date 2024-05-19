/**
 * 
 */
package velka.truffle.wrapper;

import velka.core.literal.LitInteger;
import velka.truffle.node.LongLiteralNode;
import velka.truffle.node.VelkaNode;

/**
 * @author Mgr. Radomir Skrabal
 */
@ClassExtension(velka.core.literal.LitInteger.class)
public class LitIntegerTruffle implements IToVelkaNode {

	public final LitInteger wrapped;
	
	public LitIntegerTruffle(LitInteger wrapped) {
		this.wrapped = wrapped;
	}
	
	@Override
	public VelkaNode toVelkaNode() {
		return new LongLiteralNode(wrapped.value);
	}
}
