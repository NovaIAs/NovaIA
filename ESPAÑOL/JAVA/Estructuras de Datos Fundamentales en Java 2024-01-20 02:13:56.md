```java
// **********************************************************************
// *                            CLASE Nodo                              *
// **********************************************************************

public class Nodo {
    private int valor;
    private Nodo siguiente;

    public Nodo(int valor) {
        this.valor = valor;
        this.siguiente = null;
    }

    public int getValor() {
        return valor;
    }

    public void setValor(int valor) {
        this.valor = valor;
    }

    public Nodo getSiguiente() {
        return siguiente;
    }

    public void setSiguiente(Nodo siguiente) {
        this.siguiente = siguiente;
    }
}

// **********************************************************************
// *                           CLASE Lista                               *
// **********************************************************************

public class Lista {
    private Nodo cabeza;
    private Nodo cola;
    private int tamaño;

    public Lista() {
        this.cabeza = null;
        this.cola = null;
        this.tamaño = 0;
    }

    public void agregarAlFinal(int valor) {
        Nodo nuevoNodo = new Nodo(valor);

        if (estaVacia()) {
            cabeza = cola = nuevoNodo;
        } else {
            cola.setSiguiente(nuevoNodo);
            cola = nuevoNodo;
        }

        tamaño++;
    }

    public void agregarAlInicio(int valor) {
        Nodo nuevoNodo = new Nodo(valor);

        if (estaVacia()) {
            cabeza = cola = nuevoNodo;
        } else {
            nuevoNodo.setSiguiente(cabeza);
            cabeza = nuevoNodo;
        }

        tamaño++;
    }

    public int eliminarAlFinal() {
        if (estaVacia()) {
            throw new RuntimeException("La lista está vacía");
        }

        int valorEliminado = cola.getValor();

        if (cabeza == cola) {
            cabeza = cola = null;
        } else {
            Nodo nodoAnterior = cabeza;

            while (nodoAnterior.getSiguiente() != cola) {
                nodoAnterior = nodoAnterior.getSiguiente();
            }

            cola = nodoAnterior;
            cola.setSiguiente(null);
        }

        tamaño--;

        return valorEliminado;
    }

    public int eliminarAlInicio() {
        if (estaVacia()) {
            throw new RuntimeException("La lista está vacía");
        }

        int valorEliminado = cabeza.getValor();

        if (cabeza == cola) {
            cabeza = cola = null;
        } else {
            cabeza = cabeza.getSiguiente();
        }

        tamaño--;

        return valorEliminado;
    }

    public int eliminarPorValor(int valor) {
        if (estaVacia()) {
            throw new RuntimeException("La lista está vacía");
        }

        Nodo nodoAnterior = null;
        Nodo nodoActual = cabeza;

        while (nodoActual != null && nodoActual.getValor() != valor) {
            nodoAnterior = nodoActual;
            nodoActual = nodoActual.getSiguiente();
        }

        if (nodoActual == null) {
            throw new RuntimeException("El valor no se encontró en la lista");
        }

        if (nodoAnterior == null) {
            return eliminarAlInicio();
        } else if (nodoActual == cola) {
            return eliminarAlFinal();
        } else {
            nodoAnterior.setSiguiente(nodoActual.getSiguiente());
            tamaño--;
            return valor;
        }
    }

    public boolean buscar(int valor) {
        Nodo nodoActual = cabeza;

        while (nodoActual != null) {
            if (nodoActual.getValor() == valor) {
                return true;
            }
            nodoActual = nodoActual.getSiguiente();
        }

        return false;
    }

    public int getTamaño() {
        return tamaño;
    }

    public boolean estaVacia() {
        return tamaño == 0;
    }

    public void imprimir() {
        Nodo nodoActual = cabeza;

        while (nodoActual != null) {
            System.out.print(nodoActual.getValor() + " ");
            nodoActual = nodoActual.getSiguiente();
        }

        System.out.println();
    }
}

// **********************************************************************
// *                          CLASE Pila                                *
// **********************************************************************

public class Pila {
    private Lista lista = new Lista();

    public void push(int valor) {
        lista.agregarAlInicio(valor);
    }

    public int pop() {
        return lista.eliminarAlInicio();
    }

    public int peek() {
        return lista.getCabeza().getValor();
    }

    public boolean estaVacia() {
        return lista.estaVacia();
    }

    public int getTamaño() {
        return lista.getTamaño();
    }

    public void imprimir() {
        lista.imprimir();
    }
}

// **********************************************************************
// *                          CLASE Cola                                *
// **********************************************************************

public class Cola {
    private Lista lista = new Lista();

    public void enqueue(int valor) {
        lista.agregarAlFinal(valor);
    }

    public int dequeue() {
        return lista.eliminarAlInicio();
    }

    public int peek() {
        return lista.getCabeza().getValor();
    }

    public boolean estaVacia() {
        return lista.estaVacia();
    }

    public int getTamaño() {
        return lista.getTamaño();
    }

    public void imprimir() {
        lista.imprimir();
    }
}

// **********************************************************************
// *                         CLASE Árbol Binario                         *
// **********************************************************************

public class ÁrbolBinario {
    private Nodo raíz;

    public ÁrbolBinario(int valor) {
        raíz = new Nodo(valor);
    }

    public void insertar(int valor) {
        insertarRecursivo(valor, raíz);
    }

    private void insertarRecursivo(int valor, Nodo nodoActual) {
        if (valor <= nodoActual.getValor()) {
            if (nodoActual.getIzquierdo() == null) {
                nodoActual.setIzquierdo(new Nodo(valor));
            } else {
                insertarRecursivo(valor, nodoActual.getIzquierdo());
            }
        } else {
            if (nodoActual.getDerecho() == null) {
                nodoActual.setDerecho(new Nodo(valor));
            } else {
                insertarRecursivo(valor, nodoActual.getDerecho());
            }
        }
    }

    public boolean buscar(int valor)