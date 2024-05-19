package velka.truffle.node;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;

/** Base class for Velka graal nodes **/
public abstract class VelkaNode extends Node {
	/** Generic execution entrypoint*/
	public abstract Object executeGeneric(VirtualFrame virtualFrame);
}
