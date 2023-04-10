package language.typing;

import language.typing.types.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class FunIncATypeUtil {
    public static Boolean subtype(Type subType, Type superType) {
        return (meet(subType, superType)).equals(subType);
    }

    public static Type meet(List<Type> types) {
        return types.stream().reduce(new AnyType(), FunIncATypeUtil::meet);
    }

    public static Type meet(Type type1, Type type2){
        if (type1.equals(type2)) {
            return type1;
        } else if(type1 instanceof AnyType){
            return type2;
        } else if (type2 instanceof AnyType) {
            return type1;
        } else if (type1 instanceof TypeRef && type2 instanceof ConstructorType) {
            if(((TypeRef) type1).getName().equals(((ConstructorType) type2).getName()))
                return type1;
        } else if (type1 instanceof ConstructorType && type2 instanceof TypeRef) {
            if(((ConstructorType) type1).getName().equals(((TypeRef) type2).getName()))
                return type1;
        } else if (type1 instanceof TupleType && type2 instanceof TupleType) {
            List<Type> tupleTypes1 = ((TupleType) type1).types;
            List<Type> tupleTypes2 = ((TupleType) type2).types;
            if(tupleTypes1.size() == tupleTypes2.size()){
                int n = tupleTypes1.size();
                List<Type> tupTys = new ArrayList<>(n);
                for (int i = 0; i < n; i++)
                    tupTys.add(meet(tupleTypes1.get(i), tupleTypes2.get(i)));
                return new TupleType(tupTys);
            }
        } else if (type1 instanceof SetType && type2 instanceof SetType) {
            Type setType1 = ((SetType) type1).getSetType();
            Type setType2 = ((SetType) type2).getSetType();
            if (setType1 == null || setType2 == null)
                return new SetType(new NothingType());
            return new SetType(meet(setType1, setType2));
        } else if (type1 instanceof BooleanType) { // meet of primitive types
            // case of type2 is Any or Boolean already covered, Nothing Type is returned further down
        } else if (type1 instanceof DoubleType) {
            if (type2 instanceof LongType || type2 instanceof IntType) {
                return type2;
            }
        } else if (type1 instanceof IntType) {
            if (type2 instanceof DoubleType || type2 instanceof LongType) {
                return new IntType();
            }
        } else if (type1 instanceof LongType) {
            if (type2 instanceof DoubleType) {
                return new LongType();
            } else if (type2 instanceof IntType) {
                return new IntType();
            }
        } else if (type1 instanceof StringType) {

        }
        return new NothingType();
    }

    public static Type join(List<Type> types) {
        return types.stream().reduce(new NothingType(), FunIncATypeUtil::join);
    }

    public static Type join(Type type1, Type type2) {
        if(type1.equals(type2)){
            return type1;
        } else if (type1 instanceof NothingType){
            return type2;
        } else if (type2 instanceof NothingType) {
            return type1;
        } else if (type1 instanceof TupleType && type2 instanceof TupleType) {
            List<Type> tupleTypes1 = ((TupleType) type1).types;
            List<Type> tupleTypes2 = ((TupleType) type2).types;
            if(tupleTypes1.size() == tupleTypes2.size()){
                int n = tupleTypes1.size();
                List<Type> tupTys = new ArrayList<>(n);
                for (int i = 0; i < n; i++)
                    tupTys.add(join(tupleTypes1.get(i), tupleTypes2.get(i)));
                return new TupleType(tupTys);
            }
        } else if (type1 instanceof SetType && type2 instanceof SetType) {
            Type setType1 = ((SetType) type1).getSetType();
            Type setType2 = ((SetType) type2).getSetType();
            return new SetType(join(setType1, setType2));
        } else if (type1 instanceof BooleanType) { // join of primitive scalatypes
            // case type2 is Nothing or Boolean is already covered, return of Any is down below
        } else if (type1 instanceof DoubleType) {
            if (type2 instanceof LongType || type2 instanceof IntType) {
                return new DoubleType();
            }
        } else if (type1 instanceof IntType) {
            if (type2 instanceof LongType || type2 instanceof DoubleType) {
                return type2;
            }
        } else if (type1 instanceof LongType) {
            if (type2 instanceof IntType) {
                return type1;
            } else if (type2 instanceof DoubleType) {
                return type2;
            }
        } else if (type1 instanceof StringType) {
            return new AnyType();
        }
        return new AnyType();
    }

    public static Type substitute(Type type, Map<String, Type> subst) { // TODO
        if (type instanceof FunType) {
            FunType funType = (FunType)  type;
            funType.paramTypes = funType.paramTypes.stream().map(t -> substitute(t, subst)).collect(Collectors.toList());
            funType.returnType = substitute(funType.returnType, subst);
        } else if (type instanceof TupleType) {
            TupleType tupleType = (TupleType) type;
            tupleType.types = tupleType.types.stream().map(t -> substitute(t, subst)).collect(Collectors.toList());
        } else if (type instanceof TypeRef){
            String name = ((TypeRef) type).getName();
            if (subst.containsKey(name))
                type = subst.get(name);
        } else if (type instanceof ConstructorType) {
            ConstructorType constructorType = (ConstructorType) type;
            constructorType.setTypes(
                    constructorType.getTypes().stream().map(t -> substitute(t, subst)).collect(Collectors.toList()));
        } else if (type instanceof SetType) {
            SetType setType = (SetType) type;
            setType.setSetType(substitute(setType.getSetType(), subst));
        }
        return type;
    }
}
