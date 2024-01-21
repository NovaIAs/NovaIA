```typescript
// Objetivo: Crear un programa en TypeScript que permita gestionar una lista de compras.

// Definición de la clase ListaCompras
class ListaCompras {
  // Propiedades
  private items: string[] = [];  // Array de artículos en la lista de compras

  // Métodos
  agregarItem(item: string): void {
    this.items.push(item);  // Añadir un artículo a la lista
  }

  quitarItem(item: string): void {
    const index = this.items.indexOf(item);  // Buscar el índice del artículo
    if (index >= 0) {
      this.items.splice(index, 1);  // Eliminar el artículo de la lista
    }
  }

  obtenerItems(): string[] {
    return this.items;  // Obtener la lista de artículos
  }

  imprimirLista(): void {
    console.log("Lista de compras:");
    this.items.forEach((item) => {
      console.log(`- ${item}`);
    });
  }
}

// Ejemplo de uso de la clase ListaCompras
const miLista = new ListaCompras();
miLista.agregarItem("Manzanas");
miLista.agregarItem("Peras");
miLista.agregarItem("Uvas");
miLista.quitarItem("Peras");
miLista.imprimirLista();

// Salida:
// Lista de compras:
// - Manzanas
// - Uvas
```

Explicación del código:

1. Definición de la clase `ListaCompras`: Esta clase representa una lista de compras. Tiene una propiedad privada `items` que es un array de cadenas de texto que representa los artículos en la lista de compras.

2. Métodos de la clase `ListaCompras`:

   - `agregarItem(item: string)`: Este método añade un artículo a la lista de compras.

   - `quitarItem(item: string)`: Este método quita un artículo de la lista de compras.

   - `obtenerItems(): string[]`: Este método devuelve la lista de artículos en la lista de compras.

   - `imprimirLista()`: Este método imprime la lista de artículos en la lista de compras en la consola.

3. Ejemplo de uso de la clase `ListaCompras`:

   - Creamos una instancia de la clase `ListaCompras` llamada `miLista`.

   - Añadimos algunos artículos a la lista de compras usando el método `agregarItem`.

   - Quitamos un artículo de la lista de compras usando el método `quitarItem`.

   - Imprimimos la lista de compras usando el método `imprimirLista`.

4. Salida: La salida del programa es la lista de compras impresa en la consola. En este ejemplo, la lista de compras contiene dos artículos: "Manzanas" y "Uvas".