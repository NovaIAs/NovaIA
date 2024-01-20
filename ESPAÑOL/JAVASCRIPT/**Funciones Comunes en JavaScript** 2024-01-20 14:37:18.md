```javascript
// Función recursiva para calcular el factorial de un número
function factorial(numero) {
    if (numero === 0) {
        return 1;
    } else {
        return numero * factorial(numero - 1);
    }
}

// Función para generar números aleatorios entre un rango
function generarAleatorio(minimo, maximo) {
    return Math.floor(Math.random() * (maximo - minimo + 1)) + minimo;
}

// Función para ordenar un array de números
function ordenarArray(array) {
    return array.sort(function(a, b) {
        return a - b;
    });
}

// Función para eliminar elementos duplicados de un array
function eliminarDuplicados(array) {
    return [...new Set(array)];
}

// Función para buscar un elemento en un array usando búsqueda binaria
function busquedaBinaria(array, elemento) {
    let inicio = 0;
    let fin = array.length - 1;

    while (inicio <= fin) {
        let medio = Math.floor((inicio + fin) / 2);

        if (array[medio] === elemento) {
            return medio;
        } else if (array[medio] < elemento) {
            inicio = medio + 1;
        } else {
            fin = medio - 1;
        }
    }

    return -1;
}

// Función para crear un objeto a partir de un array de pares clave-valor
function crearObjeto(array) {
    const objeto = {};

    for (const [clave, valor] of array) {
        objeto[clave] = valor;
    }

    return objeto;
}

// Función para convertir un objeto a un array de pares clave-valor
function objetoAArray(objeto) {
    const array = [];

    for (const clave in objeto) {
        array.push([clave, objeto[clave]]);
    }

    return array;
}

// Función para crear una función que retorne el doble de un número
function crearFuncionDoblarNumero() {
    return function(numero) {
        return numero * 2;
    };
}

// Función para crear una función que retorne la suma de dos números
function crearFuncionSumarNumeros() {
    return function(numero1, numero2) {
        return numero1 + numero2;
    };
}

// Función para aplicar una función a cada elemento de un array
function aplicarFuncion(array, funcion) {
    const resultado = [];

    for (const elemento of array) {
        resultado.push(funcion(elemento));
    }

    return resultado;
}

// Función para filtrar un array según una condición
function filtrarArray(array, condicion) {
    const resultado = [];

    for (const elemento of array) {
        if (condicion(elemento)) {
            resultado.push(elemento);
        }
    }

    return resultado;
}

// Función para crear un array de objetos a partir de un array de datos
function crearArrayObjetos(arrayDatos) {
    const arrayObjetos = [];

    for (const dato of arrayDatos) {
        const objeto = {};
        objeto.nombre = dato;
        objeto.longitud = dato.length;
        arrayObjetos.push(objeto);
    }

    return arrayObjetos;
}

// Función para crear un array de números primos hasta un número dado
function crearArrayPrimos(hasta) {
    const arrayPrimos = [];

    for (let i = 2; i <= hasta; i++) {
        let esPrimo = true;

        for (let j = 2; j < i; j++) {
            if (i % j === 0) {
                esPrimo = false;
                break;
            }
        }

        if (esPrimo) {
            arrayPrimos.push(i);
        }
    }

    return arrayPrimos;
}

// Función para crear un objeto con las propiedades de un objeto pasado como argumento
function crearObjetoPropiedades(objeto) {
    const objetoPropiedades = {};

    for (const clave in objeto) {
        objetoPropiedades[clave] = typeof objeto[clave];
    }

    return objetoPropiedades;
}

// Función para crear un array de objetos con las propiedades de un objeto pasado como argumento
function crearArrayObjetosPropiedades(objeto) {
    const arrayObjetosPropiedades = [];

    for (const clave in objeto) {
        const objetoPropiedad = {};
        objetoPropiedad.nombre = clave;
        objetoPropiedad.tipo = typeof objeto[clave];
        arrayObjetosPropiedades.push(objetoPropiedad);
    }

    return arrayObjetosPropiedades;
}
```

Explicación del código:

1. **Función factorial:** Esta función calcula el factorial de un número usando recursividad.

2. **Función generarAleatorio:** Esta función genera un número aleatorio entre un rango dado.

3. **Función ordenarArray:** Esta función ordena un array de números en orden ascendente.

4. **Función eliminarDuplicados:** Esta función elimina los elementos duplicados de un array.

5. **Función busquedaBinaria:** Esta función busca un elemento en un array usando búsqueda binaria.

6. **Función crearObjeto:** Esta función crea un objeto a partir de un array de pares clave-valor.

7. **Función objetoAArray:** Esta función convierte un objeto a un array de pares clave-valor.

8. **Función crearFuncionDoblarNumero:** Esta función crea una función que retorna el doble de un número.

9. **Función crearFuncionSumarNumeros:** Esta función crea una función que retorna la suma de dos números.

10. **Función aplicarFuncion:** Esta función aplica una función a cada elemento de un array.

11. **Función filtrarArray:** Esta función filtra un array según una condición.

12. **Función crearArrayObjetos:** Esta función crea un array de objetos a partir de un array de datos.

13. **Función crearArrayPrimos:** Esta función crea un array de números primos hasta un número dado.

14. **Función crearObjetoPropiedades:** Esta función crea un objeto con las propiedades de un objeto pasado como argumento.

15. **Función crearArrayObjetosPropiedades:** Esta función crea un array de objetos con las propiedades de un objeto pasado como argumento.