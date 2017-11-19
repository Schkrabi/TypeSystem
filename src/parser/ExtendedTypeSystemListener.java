package parser;

import java.util.Deque;
import java.util.LinkedList;

import expression.Expression;

public class ExtendedTypeSystemListener extends SchemeBaseListener {
	Deque<Expression> stack = new LinkedList<Expression>();
	
	@Override 
	public void exitExprs(SchemeParser.ExprsContext ctx) {
		System.out.println(ctx.getText());
	}
	
	@Override 
	public void exitExpr(SchemeParser.ExprContext ctx) { 
		System.out.println(ctx.getText());
	}
	
	@Override 
	public void exitSeq(SchemeParser.SeqContext ctx) { 
		System.out.println(ctx.getText());
	}
	
	@Override 
	public void exitAtom(SchemeParser.AtomContext ctx) {
		//String s = ctx.getToken(0, 0).toString();
		System.out.println(ctx.getText());
	}
	
	@Override 
	public void exitQuote(SchemeParser.QuoteContext ctx) {
		System.out.println(ctx.getText());
	}
}
