package velka.truffle.language;

import java.io.IOException;
import java.util.Collection;
import java.util.List;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.nodes.Node;

import velka.core.expression.Expression;
import velka.truffle.compiler.TruffleCompiler;
import velka.truffle.node.LongLiteralNode;
import velka.truffle.node.VelkaRootNode;
import velka.util.AppendableException;

@TruffleLanguage.Registration(id = "vlk", name = "Velka")
public class VelkaTruffleLanguage extends TruffleLanguage<VelkaLanguageContext> {

	@Override
	protected VelkaLanguageContext createContext(Env env) {
		return new VelkaLanguageContext(env);
	}

	private static final LanguageReference<VelkaTruffleLanguage> REFERENCE =
            LanguageReference.create(VelkaTruffleLanguage.class);
	
	public static VelkaTruffleLanguage get(Node node) {
        return REFERENCE.get(node);
    }
	
	@Override
    protected CallTarget parse(ParsingRequest request) throws IOException {
//		var source = request.getSource();
//        List<Expression> exprs;
//		try {
//			exprs = velka.parser.Parser.read(source.getReader());
//		} catch (IOException | AppendableException e) {
//			throw new RuntimeException(e);
//		}
        Collection<velka.truffle.node.VelkaNode> ast = List.of(new LongLiteralNode(42)); 
        		
        		//TruffleCompiler.instance().compile(exprs);
        //TODO FrameDescriptor?
        var root = new VelkaRootNode(this, null, ast);
        return root.getCallTarget();
	}
}
