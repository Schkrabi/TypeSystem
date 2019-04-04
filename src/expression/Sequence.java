package expression;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import interpretation.Environment;
import types.Type;
import util.AppendableException;

/**
 * Class for parsed Scheme sequence. Allows to transform into Application
 * Expression based on type inference.
 * 
 * @author Mgr. Radomir Skrabal
 * 
 */
public class Sequence extends Expression implements Iterable<Expression> {

	public final List<Expression> members;

	public Sequence(List<Expression> members) {
		this.members = members;
	}

	@Override
	public Expression interpret(Environment env) throws Exception {
		Expression head = this.head().interpret(env);

		if (MetaLambda.isApplicableExpression(head)) {
			// Will interpret head again, but doesn't matter, lambda interprets
			// to itself and no side effects here
			Application tmp = new Application(head, this.tail());
			tmp.infer(env);
			return tmp.interpret(env);
		}

		Tuple t = this.asTuple();
		t.infer(env);
		
		return t.interpret(env);
	}

	@Override
	public Type infer(Environment env) throws AppendableException {
		Type headType = this.head().infer(env);
		Type ret;

		if (headType.isApplicableType()) {
			Application tmp = new Application(this.head(), this.tail());
			ret = tmp.infer(env);
		} else {
			ret = this.asTuple().infer(env);
		}

		this.setType(ret);
		return ret;
	}

	@Override
	public String toString() {
		StringBuilder s = new StringBuilder();
		s.append('(');
		Iterator<Expression> i = this.members.iterator();

		while (i.hasNext()) {
			Expression e = i.next();
			s.append(e.toString());
			if (i.hasNext()) {
				s.append(" ");
			}
		}
		s.append(")");
		return s.toString();
	}

	protected Expression head() {
		return this.members.get(0);
	}

	protected Tuple tail() {
		List<Expression> tmp = new LinkedList<Expression>();
		tmp.addAll(this.members);
		tmp.remove(0);

		Expression[] exprs = new Expression[tmp.size()];
		exprs = tmp.toArray(exprs);
		return new Tuple(exprs);
	}

	public Expression transform() {
		if (this.getType() == null) {
			return this;
		}
		if (this.head().getType().isApplicableType()) {
			return new Application(this.head(), this.tail());
		}

		// Not sure
		return this.asTuple();
	}

	public Tuple asTuple() {
		Expression[] exprs = new Expression[this.members.size()];
		exprs = this.members.toArray(exprs);
		return new Tuple(exprs);
	}

	@Override
	public Expression substituteTopLevelVariables(Environment topLevel) throws Exception {
		List<Expression> l = new ArrayList<Expression>();
		for (Expression e : this) {
			l.add(e.substituteTopLevelVariables(topLevel));
		}
		return new Sequence(l);
	}

	@Override
	public Iterator<Expression> iterator() {
		return new SequenceIterator();
	}

	private class SequenceIterator implements Iterator<Expression> {

		Iterator<Expression> listIterator = Sequence.this.members.iterator();

		@Override
		public boolean hasNext() {
			return this.listIterator.hasNext();
		}

		@Override
		public Expression next() {
			return this.listIterator.next();
		}

		@Override
		public void remove() {
			this.listIterator.remove();
			;
		}

	}

	@Override
	public String toClojureCode() throws Exception {
		return this.transform().toClojureCode();
	}
}
