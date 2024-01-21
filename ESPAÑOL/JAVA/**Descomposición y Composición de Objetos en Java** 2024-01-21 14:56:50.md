```java
import java.util.*;
import java.lang.reflect.*;

public class Complejo {

    private static final int MAX_PROFUNDIDAD = 10;

    private Object valor;
    private Class<?> tipo;
    private int profundidad;

    public Complejo(Object valor) {
        this.valor = valor;
        this.tipo = valor.getClass();
        this.profundidad = 0;
    }

    public Object getValor() {
        return valor;
    }

    public Class<?> getTipo() {
        return tipo;
    }

    public int getProfundidad() {
        return profundidad;
    }

    public static Complejo de(Object valor) {
        return new Complejo(valor);
    }

    public static List<Complejo> descomponer(Object valor) {
        List<Complejo> resultado = new ArrayList<>();
        descomponer(valor, resultado, 0);
        return resultado;
    }

    private static void descomponer(Object valor, List<Complejo> resultado, int profundidad) {
        if (profundidad > MAX_PROFUNDIDAD) {
            return;
        }

        if (valor == null) {
            resultado.add(new Complejo(null));
            return;
        }

        Class<?> tipo = valor.getClass();
        if (tipo.isArray()) {
            Object[] array = (Object[]) valor;
            for (Object elemento : array) {
                descomponer(elemento, resultado, profundidad + 1);
            }
        } else if (tipo.isPrimitive() || tipo.isEnum() || tipo.isSealed() || tipo.isFinal() || tipo.isAnnotation()) {
            resultado.add(new Complejo(valor));
        } else {
            for (Field field : tipo.getDeclaredFields()) {
                field.setAccessible(true);
                try {
                    Object fieldValue = field.get(valor);
                    descomponer(fieldValue, resultado, profundidad + 1);
                } catch (IllegalAccessException e) {
                    // Ignorar el campo
                }
            }
        }
    }

    public static Object componer(List<Complejo> componentes) {
        if (componentes == null || componentes.isEmpty()) {
            return null;
        }

        Complejo primerComponente = componentes.get(0);
        Object valor = primerComponente.getValor();
        Class<?> tipo = primerComponente.getTipo();

        if (tipo.isArray()) {
            Class<?> componenteTipo = tipo.getComponentType();
            int longitud = componentes.size();
            Object array = Array.newInstance(componenteTipo, longitud);
            for (int i = 0; i < longitud; i++) {
                Array.set(array, i, componer(componentes.get(i + 1)));
            }
            return array;
        } else if (tipo.isPrimitive() || tipo.isEnum() || tipo.isSealed() || tipo.isFinal() || tipo.isAnnotation()) {
            return valor;
        } else {
            try {
                Object objeto = tipo.newInstance();
                for (Field field : tipo.getDeclaredFields()) {
                    field.setAccessible(true);
                    Object fieldValue = componer(componentes.get(field.getMetadata().getBoundMembers().length + 1));
                    field.set(objeto, fieldValue);
                }
                return objeto;
            } catch (InstantiationException | IllegalAccessException e) {
                // Ignorar el campo
                return null;
            }
        }
    }

    public static void main(String[] args) {
        Object[] array = {1, 2, 3, 4, 5};
        List<Complejo> componentes = descomponer(array);
        Object arrayCom