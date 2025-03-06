package velka.java.runtime;

import velka.types.TypeTuple;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Stream;

import com.sun.codemodel.JExpr;

import velka.java.CodeModelInstance;
import velka.types.Type;
import velka.types.TypeAtom;

public class VelkaTuple implements Iterable<Object>, Collection<Object> {
	private final Object[] data;
	public final TypeTuple type;
	
	public VelkaTuple(Object[] data, TypeTuple type) {
		this.data = data;
		this.type = type;
	}
	
	public VelkaTuple(Collection<? extends Object> data, TypeTuple type) {
		this.data = data.toArray();
		this.type = type;
	}
	
	public Object get(int index) {
		return data[index];
	}
	
	public Type getType(int index) {
		return type.get(index);
	}
	
	public int size() {
		return this.data.length;
	}
	
	public Stream<Object> stream(){
		var sb = Stream.builder();
		
		for(Object o : this) {
			sb.add(o);
		}
		
		return sb.build();
	}
	
	@Override
	public String toString() {
		return Arrays.deepToString(data).replace(",", "");
	}
	
	@Override
	public int hashCode() {
		return Arrays.deepHashCode(data);
	}
	
	@Override
	public boolean equals(Object o) {
		if(o instanceof VelkaTuple other) {
			if(this.type.equals(other.type)
				&& this.data.length == other.data.length)
			{
				for(int i = 0; i < this.data.length; i++) {
					if(!this.data[i].equals(other.data[i])) {
						return false;
					}
				}
				return true;
			}
		}
		return false;
	}

	@Override
	public Iterator<Object> iterator() {
		return new VelkaTupleIterator(this);
	}
	
	static class VelkaTupleIterator implements Iterator<Object>{
		
		private int pos;
		private final VelkaTuple tuple;
		
		public VelkaTupleIterator(VelkaTuple tuple) {
			this.tuple = tuple;
			this.pos = -1;
		}

		@Override
		public boolean hasNext() {
			return (this.pos + 1) < tuple.size(); 
		}

		@Override
		public Object next() {
			if(this.hasNext()) {
				this.pos += 1;
				return this.tuple.get(this.pos);
			}
			throw new NoSuchElementException();
		}
		
	}

	@Override
	public boolean isEmpty() {
		return this.data.length == 0;
	}

	@Override
	public boolean contains(Object o) {
		for(Object obj : this.data) {
			if(o.equals(obj)) return true;
		}
		return false;
	}

	@Override
	public Object[] toArray() {
		return this.data.clone();
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T> T[] toArray(T[] a) {
		return (T[])this.data.clone();
	}

	@Override
	public boolean add(Object e) {
		return false;
	}

	@Override
	public boolean remove(Object o) {
		return false;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return c.stream().allMatch(o -> this.contains(o));
	}

	@Override
	public boolean addAll(Collection<? extends Object> c) {
		return false;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		return false;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		return false;
	}

	@Override
	public void clear() {		
	}
	
	/** Converts std args to VelkaTuple **/
	public static VelkaTuple fromArgs(String[] args) {		
		var type = new TypeTuple(Stream.generate(() -> TypeAtom.TypeStringNative).limit(args.length).toList());
		var tuple = new VelkaTuple(args, type);
		return new VelkaTuple(List.of(tuple), new TypeTuple(List.of(type)));
	}
	
	/** Creates a VelkaTuple from elements */
	public static VelkaTuple of(Object ...elements) {
		var ts = new ArrayList<Type>();
		for(var e : elements) {
			var t = JavaTypeSystem.instance().getType(e);
			ts.add(t);
		}
		
		return new VelkaTuple(elements, new TypeTuple(ts));
	}
	
	/** Creates a code that creates the velka tuple in code */
	public static com.sun.codemodel.JInvocation _velkaTuple(com.sun.codemodel.JExpression ...elements){
		var _new = CodeModelInstance.instance().ref(VelkaTuple.class).staticInvoke("of");
		
		for(var e : elements) {
			_new.arg(e);
		}
		
		return _new;
	}
}
