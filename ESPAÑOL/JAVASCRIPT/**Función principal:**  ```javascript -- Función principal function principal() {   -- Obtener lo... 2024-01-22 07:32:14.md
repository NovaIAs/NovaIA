**Función principal:**

```javascript
function principal() {
  // Obtener los elementos necesarios
  const listaElementos = document.getElementById("lista-elementos");
  const botonAgregar = document.getElementById("boton-agregar");
  const inputElemento = document.getElementById("input-elemento");

  // Añadir evento "click" al botón para agregar elementos
  botonAgregar.addEventListener("click", () => {
    agregarElemento(inputElemento.value);
  });

  // Función para agregar un elemento a la lista
  function agregarElemento(elemento) {
    // Crear un nuevo elemento de lista
    const nuevoElemento = document.createElement("li");
    nuevoElemento.textContent = elemento;

    // Añadir el nuevo elemento a la lista
    listaElementos.appendChild(nuevoElemento);
  }
}
```

**Explicación:**

* La función `principal()` se encarga de obtener los elementos necesarios (la lista, el botón y el input), añadir el evento "click" al botón para agregar elementos y definir la función `agregarElemento()`.
* La función `agregarElemento()` crea un nuevo elemento de lista, le asigna el valor del input y lo añade a la lista.