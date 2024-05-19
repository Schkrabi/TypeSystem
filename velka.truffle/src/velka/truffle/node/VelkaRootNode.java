/**
 * 
 */
package velka.truffle.node;

import java.util.Collection;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;

import velka.truffle.language.VelkaTruffleLanguage;

import com.oracle.truffle.api.nodes.ExplodeLoop;

/**
 * Root node for Velka AST programs
 */
public class VelkaRootNode extends RootNode {

	public VelkaRootNode(VelkaTruffleLanguage language, FrameDescriptor frameDescriptor, Collection<VelkaNode> nodes) {
		super(language, frameDescriptor);
		this.nodes = nodes.toArray(VelkaNode[]::new);
	}

	@Children
	public final VelkaNode[] nodes;
	
	@Override
	@ExplodeLoop
	public Object execute(VirtualFrame frame) {
		for(int i = 0; i < nodes.length - 1; i++) {
			nodes[i].executeGeneric(frame);
		}
		return nodes[nodes.length - 1].executeGeneric(frame);
	}

}
