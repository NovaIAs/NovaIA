```
// Definir una función para crear un objeto de coche
function crearCoche(marca, modelo, año) {
  // Crear un objeto de coche vacío
  const coche = {};

  // Añadir propiedades al objeto de coche
  coche.marca = marca;
  coche.modelo = modelo;
  coche.año = año;

  // Devolver el objeto de coche
  return coche;
}

// Crear un array de coches
const coches = [];

// Añadir coches al array
coches.push(crearCoche('Toyota', 'Camry', 2020));
coches.push(crearCoche('Honda', 'Accord', 2021));
coches.push(crearCoche('Ford', 'Mustang', 2022));

// Iterar sobre el array de coches
for (const coche of coches) {
  // Imprimir la información de cada coche
  console.log(`Marca: ${coche.marca}`);
  console.log(`Modelo: ${coche.modelo}`);
  console.log(`Año: ${coche.año}`);
  console.log('');
}
```

Explicación:

1. La función `crearCoche` crea un objeto de coche con las propiedades `marca`, `modelo` y `año`.
2. El array `coches` se crea para almacenar objetos de coche.
3. Los objetos de coche se añaden al array `coches` utilizando el método `push`.
4. El bucle `for of` itera sobre el array `coches` e imprime la información de cada coche en la consola.