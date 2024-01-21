```c++
// Programa para resolver el problema de la mochila con algoritmo voraz
// El problema de la mochila es un problema clásico de programación dinámica
// en el que se nos da una mochila con una capacidad limitada y una lista de objetos
// con diferentes valores y pesos. El objetivo es elegir el subconjunto de objetos
// que maximiza el valor total de los objetos en la mochila sin exceder su capacidad.

// Definimos las estructuras necesarias para representar el problema
struct Objeto {
    int valor;
    int peso;
};

struct Mochila {
    int capacidad;
    vector<Objeto> objetos;
};

// Función para comparar objetos por su valor por unidad de peso
bool compararObjetos(const Objeto& a, const Objeto& b) {
    return (a.valor / a.peso) > (b.valor / b.peso);
}

// Función para resolver el problema de la mochila con algoritmo voraz
vector<Objeto> resolverMochila(Mochila mochila) {
    // Ordenamos los objetos por su valor por unidad de peso en orden descendente
    sort(mochila.objetos.begin(), mochila.objetos.end(), compararObjetos);

    // Creamos una lista para almacenar los objetos seleccionados
    vector<Objeto> objetosSeleccionados;

    // Iteramos sobre los objetos en orden descendente de valor por unidad de peso
    for (auto objeto : mochila.objetos) {
        // Si el objeto cabe en la mochila, lo añadimos a la lista de objetos seleccionados
        if (objeto.peso <= mochila.capacidad) {
            objetosSeleccionados.push_back(objeto);
            mochila.capacidad -= objeto.peso;
        }
    }

    // Devolvemos la lista de objetos seleccionados
    return objetosSeleccionados;
}

// Función principal
int main() {
    // Creamos una mochila con una capacidad de 10
    Mochila mochila;
    mochila.capacidad = 10;

    // Creamos una lista de objetos
    vector<Objeto> objetos;
    objetos.push_back({5, 3});
    objetos.push_back({10, 5});
    objetos.push_back({15, 8});
    objetos.push_back({20, 10});

    // Añadimos los objetos a la mochila
    mochila.objetos = objetos;

    // Resolvemos el problema de la mochila
    vector<Objeto> objetosSeleccionados = resolverMochila(mochila);

    // Mostramos los objetos seleccionados
    cout << "Objetos seleccionados:" << endl;
    for (auto objeto : objetosSeleccionados) {
        cout << "Valor: " << objeto.valor << ", Peso: " << objeto.peso << endl;
    }

    // Calculamos el valor total de los objetos seleccionados
    int valorTotal = 0;
    for (auto objeto : objetosSeleccionados) {
        valorTotal += objeto.valor;
    }

    // Mostramos el valor total de los objetos seleccionados
    cout << "Valor total: " << valorTotal << endl;

    return 0;
}
```

Explicación del código:

* La estructura `Objeto` representa un objeto con un valor y un peso.
* La estructura `Mochila` representa una mochila con una capacidad y una lista de objetos.
* La función `compararObjetos` compara dos objetos por su valor por unidad de peso.
* La función `resolverMochila` resuelve el problema de la mochila con algoritmo voraz.
* La función `main` crea una mochila, una lista de objetos y añade los objetos a la mochila.
* La función `main` llama a la función `resolverMochila` para resolver el problema de la mochila.
* La función `main` muestra los objetos seleccionados y el valor total de los objetos seleccionados.