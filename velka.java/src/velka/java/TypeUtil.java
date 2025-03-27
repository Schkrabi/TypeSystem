package velka.java;

import java.util.BitSet;
import java.util.HashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.sun.codemodel.JExpr;
import com.sun.codemodel.JExpression;
import com.sun.codemodel.JType;

import velka.java.runtime.TypedObject;
import velka.java.runtime.VelkaTuple;
import velka.types.RepresentationOr;
import velka.types.Type;
import velka.types.TypeArrow;
import velka.types.TypeAtom;
import velka.types.TypeName;
import velka.types.TypeRepresentation;
import velka.types.TypeTuple;
import velka.types.TypeVariable;
import velka.types.UnexpectedTypeException;
import velka.types.typeSystem.VelkaAbstraction;
import velka.util.annotations.LangbaseType;

import java.lang.reflect.Modifier;

public class TypeUtil {

	/** Creates java code to represent a type*/
	public JExpression type2java(Type type) {
		if(type instanceof TypeAtom ta) {
			return this.typeatom2java(ta);
		}
		else if(type instanceof TypeTuple tt) {
			return this.typetuple2java(tt);
		}
		else if(type instanceof TypeArrow ta) {
			return this.typearrow2java(ta);
		}
		else if(type instanceof TypeVariable tv) {
			return this.typevariable2java(tv);
		}
		else if(type instanceof RepresentationOr ts) {
			return this.typeset2java(ts);
		}
		
		throw new RuntimeException(new UnexpectedTypeException(type, type.getClass()));
	}
	
	/** Generates code to create a type atom*/
	private JExpression typeatom2java(TypeAtom type) {
		var _typeAtom = CodeModelInstance.instance().ref(TypeAtom.class);
		
		var fieldOpt = Stream.of(TypeAtom.class.getFields()).filter(f -> {
			try {
				return Modifier.isStatic(f.getModifiers()) && f.getAnnotation(LangbaseType.class) != null && f.get(null).equals(type);
			} catch (IllegalArgumentException | IllegalAccessException e) {
				throw new RuntimeException(e);
			}
		}).findAny();
		
		if(fieldOpt.isPresent()) {
			return _typeAtom.staticRef(fieldOpt.get().getName());
		}
		
		return JExpr._new(_typeAtom)
				.arg(JExpr._new(CodeModelInstance.instance().ref(TypeName.class)).arg(type.name.name))
				.arg(JExpr._new(CodeModelInstance.instance().ref(TypeRepresentation.class)).arg(type.representation.name));
	}
	
	/** Generates code to create a type tuple*/
	private JExpression typetuple2java(TypeTuple type) {
		var expr = JExpr._new(CodeModelInstance.instance().ref(TypeTuple.class));
		
		for(Type t : type) {
			expr = expr.arg(this.type2java(t));
		}
		
		return expr;
	}
	
	/** Generates code to create a type arrow*/
	private JExpression typearrow2java(TypeArrow type) {
		var expr = JExpr._new(CodeModelInstance.instance().ref(TypeArrow.class))
				.arg(this.type2java(type.ltype))
				.arg(this.type2java(type.rtype));
		return expr;
	}
	
	/** Generates code to create a type variable*/
	private JExpression typevariable2java(TypeVariable type) {
		var expr = JExpr._new(CodeModelInstance.instance().ref(TypeVariable.class))
				.arg(JExpr.lit(type.name));
		return expr;
	}
	
	/** Generates code to create a type set (represetnation or)*/
	private JExpression typeset2java(RepresentationOr type) {
		var expr = CodeModelInstance.instance().ref(RepresentationOr.class)
				.staticInvoke("factory");
		
		for(Type t : type.getRepresentations()) {
			expr = expr.arg(this.type2java(t));
		}
		
		return expr;
	}
	
	/** Wrapped class mapping */
	private Map<TypeAtom, Class<?>> velkaTypeMap =  
			Stream.of(
				Map.of(
					TypeAtom.TypeIntNative, Integer.class,
					TypeAtom.TypeInt, Integer.class,
					TypeAtom.TypeDoubleNative, Double.class,
					TypeAtom.TypeDouble, Double.class,
					TypeAtom.TypeBool, Boolean.class,
					TypeAtom.TypeBoolNative, Boolean.class,
					TypeAtom.TypeStringNative, String.class,
					TypeAtom.TypeString, String.class,
					TypeAtom.TypeListNative, List.class),
				Map.of(
					TypeAtom.TypeListJavaLinked, List.class,
					TypeAtom.TypeList, List.class,
					TypeAtom.TypeListIterator, ListIterator.class,
					TypeAtom.TypeMapTree, TreeMap.class,
					TypeAtom.TypeMap, Map.class,
					TypeAtom.TypeSetBitSet, BitSet.class,
					TypeAtom.TypeScannerNative, Scanner.class,
					TypeAtom.TypeSetTree, TreeSet.class,
					TypeAtom.TypeSetHash, HashSet.class,
					TypeAtom.TypeSet, Object.class))
			.flatMap(map -> map.entrySet().stream())
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
	
	/** Gets the JType for given velka type */
	public JType velkaTypeToJType(Type vtype) {
		Class<?> clazz = null;
		if(vtype instanceof TypeArrow
			|| (vtype instanceof RepresentationOr set
					&& set.isApplicableType())) {
			clazz = VelkaAbstraction.class;
		}
		else if(vtype instanceof TypeTuple) {
			clazz = VelkaTuple.class;
		}
		else if(vtype instanceof TypeVariable) {
			clazz = Object.class;
		}
		else {
			clazz = velkaTypeMap.get(vtype);;
			
			if(clazz == null) {
				clazz = Object.class;
			}
		}
		
		return CodeModelInstance.instance().ref(clazz);
	}
	
	/** Gets the JType of the Type class*/
	public JType typeJType() {
		return CodeModelInstance.instance()._ref(Type.class);
	}
	
	public com.sun.codemodel.JClass typeJClass() {
		return CodeModelInstance.instance().ref(Type.class);
	}
	
	public JType typeArrowJType() {
		return CodeModelInstance.instance()._ref(TypeArrow.class);
	}
	
	public com.sun.codemodel.JClass typeArrowJClass() {
		return CodeModelInstance.instance().ref(TypeArrow.class);
	}
	
	public JType typeTupleJType() {
		return CodeModelInstance.instance()._ref(TypeTuple.class);
	}
	
	public com.sun.codemodel.JClass typeTupleJClass() {
		return CodeModelInstance.instance().ref(TypeTuple.class);
	}
	
	public JType typeAtomJType() {
		return CodeModelInstance.instance()._ref(TypeAtom.class);
	}
	
	public com.sun.codemodel.JClass typeAtomJClass() {
		return CodeModelInstance.instance().ref(TypeAtom.class);
	}
	
	private TypeUtil() {}
	private static TypeUtil singleton = null;
	
	public static TypeUtil instance() {
		if(singleton == null) {
			singleton = new TypeUtil();
		}
		return singleton;
	}
}
