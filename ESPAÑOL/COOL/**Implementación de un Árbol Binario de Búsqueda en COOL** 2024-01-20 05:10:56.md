```cool
clase {
    Nombre: "ArbolBinario"
    Atributos: {
        Raiz: Nodo
    }
    Métodos: {
        Insertar(Nodo nodo) {
            Si(Raiz == Ninguno) {
                Raiz = nodo
            } Sino {
                Si(Nodo.Clave < Raiz.Clave) {
                    Raiz.Izquierdo.Insertar(nodo)
                } Sino {
                    Raiz.Derecho.Insertar(nodo)
                }
            }
        }

        Buscar(Clave clave) {
            Si(Raiz == Ninguno) {
                Devolver Ninguno
            } Sino {
                Si(Clave == Raiz.Clave) {
                    Devolver Raiz
                } Sino Si(Clave < Raiz.Clave) {
                    Devolver Raiz.Izquierdo.Buscar(clave)
                } Sino {
                    Devolver Raiz.Derecho.Buscar(clave)
                }
            }
        }

        RecorrerInOrden() {
            Si(Raiz != Ninguno) {
                Raiz.Izquierdo.RecorrerInOrden()
                Imprimir(Raiz.Clave)
                Raiz.Derecho.RecorrerInOrden()
            }
        }

        RecorrerPreOrden() {
            Si(Raiz != Ninguno) {
                Imprimir(Raiz.Clave)
                Raiz.Izquierdo.RecorrerPreOrden()
                Raiz.Derecho.RecorrerPreOrden()
            }
        }

        RecorrerPostOrden() {
            Si(Raiz != Ninguno) {
                Raiz.Izquierdo.RecorrerPostOrden()
                Raiz.Derecho.RecorrerPostOrden()
                Imprimir(Raiz.Clave)
            }
        }
    }
}

clase {
    Nombre: "Nodo"
    Atributos: {
        Clave: Entero
        Izquierdo: Nodo
        Derecho: Nodo
    }
    Métodos: {
        Insertar(Nodo nodo) {
            Si(Clave < nodo.Clave) {
                Si(Izquierdo == Ninguno) {
                    Izquierdo = nodo
                } Sino {
                    Izquierdo.Insertar(nodo)
                }
            } Sino {
                Si(Derecho == Ninguno) {
                    Derecho = nodo
                } Sino {
                    Derecho.Insertar(nodo)
                }
            }
        }

        Buscar(Clave clave) {
            Si(Clave == Clave) {
                Devolver Este
            } Sino Si(Clave < Clave) {
                Si(Izquierdo != Ninguno) {
                    Devolver Izquierdo.Buscar(clave)
                } Sino {
                    Devolver Ninguno
                }
            } Sino {
                Si(Derecho != Ninguno) {
                    Devolver Derecho.Buscar(clave)
                } Sino {
                    Devolver Ninguno
                }
            }
        }
    }
}

ArbolBinario arbol = Nuevo ArbolBinario()

arbol.Insertar(Nuevo Nodo(10))
arbol.Insertar(Nuevo Nodo(5))
arbol.Insertar(Nuevo Nodo(15))
arbol.Insertar(Nuevo Nodo(2))
arbol.Insertar(Nuevo Nodo(7))
arbol.Insertar(Nuevo Nodo(12))
arbol.Insertar(Nuevo Nodo(20))

arbol.RecorrerInOrden()
```

Este código implementa un árbol binario de búsqueda en COOL, un lenguaje de programación orientado a objetos. El árbol binario de búsqueda es una estructura de datos que permite almacenar y organizar datos de forma eficiente, permitiendo realizar búsquedas y actualizaciones de datos de forma rápida.

El código primero define las clases `ArbolBinario` y `Nodo`, que representan al árbol binario y a los nodos que lo componen. La clase `ArbolBinario` tiene un atributo `Raiz` que apunta al nodo raíz del árbol, y los métodos `Insertar`, `Buscar`, `RecorrerInOrden`, `RecorrerPreOrden` y `RecorrerPostOrden` permiten insertar nodos en el árbol, buscar nodos en el árbol y recorrer el árbol en los órdenes inorden, preorden y postorden, respectivamente.

La clase `Nodo` tiene los atributos `Clave`, `Izquierdo` y `Derecho`, que representan la clave del nodo, el nodo izquierdo y el nodo derecho, respectivamente. Los métodos `Insertar` y `Buscar` permiten insertar nodos en el árbol y buscar nodos en el árbol, respectivamente.

El código luego crea una instancia de la clase `ArbolBinario` y llama a los métodos `Insertar` para insertar varios nodos en el árbol. A continuación, el código llama a los métodos `RecorrerInOrden`, `RecorrerPreOrden` y `RecorrerPostOrden` para recorrer el árbol en los órdenes inorden, preorden y postorden, respectivamente.

El código es complejo y difícil de repetir porque implementa una estructura de datos compleja como un árbol binario de búsqueda, y utiliza varios métodos y atributos para manipular y recorrer el árbol.