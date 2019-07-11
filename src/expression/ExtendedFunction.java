package expression;

import interpretation.Environment;

import java.util.AbstractMap;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.PriorityQueue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import types.Type;
import types.TypeArrow;
import types.TypeSetDoesNotUnifyException;
import util.AppendableException;
import util.ThrowingFunction;

/**
 * Expression for interpreted function with various implementations
 * @author Mgr. Radomir Skrabal
 *
 */
public class ExtendedFunction extends MetaFunction {
	
	/**
	 * Implementations of the function
	 */
	public final Set<Function> implementations;
	
	public ExtendedFunction(Set<Function> implementations, Environment createdEnvironment){
		super(createdEnvironment);
		this.implementations = implementations;
	}

	@Override
	public Function getFunction() {
		return this.defaultImplementation();
	}

	@Override
	public Function getFunction(Comparator<? super Function> c) {
		return this.getSortedImplementations(c).peek();
	}

	@Override
	public Map<Expression, Type> infer(Environment env) throws AppendableException {
		try {
			Map<Expression, Type> hyp = new TreeMap<Expression, Type>();
			if (this.typeHypothesis == null) {
				final Map<Expression, Map<Expression, Type>> infered = this.implementations.stream().map(
						(ThrowingFunction<Expression, AbstractMap.SimpleEntry<Expression, Map<Expression, Type>>>) (x -> new AbstractMap.SimpleEntry<Expression, Map<Expression, Type>>(
								x, x.infer(this.creationEnvironment))))
						.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

				Set<Type> implTypes = infered.entrySet().stream().map(x -> x.getValue().get(x.getKey()))
						.collect(Collectors.toSet());

				Optional<Type> ltype = Type
						.unifyMany(implTypes.stream().map(x -> ((TypeArrow) x).ltype).collect(Collectors.toSet()));
				if (!ltype.isPresent()) {
					throw new TypeSetDoesNotUnifyException(
							implTypes.stream().map(x -> ((TypeArrow) x).ltype).collect(Collectors.toSet()));
				}

				Optional<Type> rtype = Type
						.unifyMany(implTypes.stream().map(x -> ((TypeArrow) x).rtype).collect(Collectors.toSet()));
				if (!rtype.isPresent()) {
					throw new TypeSetDoesNotUnifyException(
							implTypes.stream().map(x -> ((TypeArrow) x).rtype).collect(Collectors.toSet()));
				}

				this.typeHypothesis = infered.entrySet().stream().flatMap(x -> x.getValue().entrySet().stream())
						.collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

				Type t = new TypeArrow(ltype.get(), rtype.get());

				this.typeHypothesis.put(this, t);
			}
			hyp.putAll(this.typeHypothesis);
			return hyp;
		} catch (AppendableException e) {
			e.appendMessage("in " + this);
			throw e;
		}
	}
	
	public PriorityQueue<Function> getSortedImplementations(
			Comparator<? super Function> c) {
		PriorityQueue<Function> q = new PriorityQueue<Function>(c);
		q.addAll(this.implementations);
		return q;
	}
	
	public Function defaultImplementation() {
		Optional<Function> o = this.implementations.stream().filter(new Predicate<Function>() {

			@Override
			public boolean test(Function arg0) {
				return arg0.argsType == null;
			}}).findAny();
		
		return o.get();
	}
	
	@Override
	public int compareTo(Expression other) {
		if(other instanceof ExtendedFunction) {
			ExtendedFunction o = (ExtendedFunction)other;
			int c = this.creationEnvironment.compareTo(o.creationEnvironment);
			if(c != 0)
				return c;
			
			c = (int)Math.signum(this.implementations.size() - o.implementations.size());
			if(c != 0)
				return c;
			
			Set<Function> tmp = new TreeSet<Function>();
			tmp.addAll(this.implementations);
			tmp.addAll(o.implementations);
			
			if(tmp.size() == this.implementations.size())
				return 0;
			
			Set<Function> thisSubOther = new TreeSet<Function>();
			thisSubOther.addAll(tmp);
			thisSubOther.removeAll(o.implementations);
			
			Set<Function> otherSubThis = tmp;
			otherSubThis.removeAll(this.implementations);
			
			Iterator<Function> i = thisSubOther.iterator();
			Iterator<Function> j = otherSubThis.iterator();
			
			while(i.hasNext() && j.hasNext()) {
				Function f = i.next();
				Function g = j.next();
				c = f.compareTo(g);
				if(c != 0)
					return c;
			}
			
			return 0;
		}
		return super.compareTo(other);
	}
}
