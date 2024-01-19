```typescript
// Prompt: Crea un código complejo en TypeScript que sea amplio y diferenciado, explicando su funcionamiento.

// **Objetivo:** Demostrar características avanzadas de TypeScript, incluyendo conceptos como tipos genéricos, clases, funciones de orden superior, promesas y async/await.

// **Descripción del Código:**

1. **Construyendo una Clase Abstracta para Formas Geométricas:**

   ```typescript
   abstract class Forma {
       constructor(public ancho: number, public alto: number) {}
       abstract area(): number;
   }
   ```

   * Esta clase abstracta define una base común para diferentes formas geométricas.

2. **Creando Clases Concretas para Rectángulo y Triángulo:**

   ```typescript
   class Rectangulo extends Forma {
       area(): number {
           return this.ancho * this.alto;
       }
   }

   class Triangulo extends Forma {
       area(): number {
           return (this.ancho * this.alto) / 2;
       }
   }
   ```

   * Estas clases concretas amplían la clase base Forma y proporcionan implementaciones para calcular el área de un rectángulo y un triángulo.

3. **Utilizando Tipos Genéricos para crear una Lista:**

   ```typescript
   interface ILista<T> {
       agregar(elemento: T): void;
       obtener(indice: number): T;
       longitud(): number;
   }

   class Lista<T> implements ILista<T> {
       private elementos: T[] = [];

       agregar(elemento: T): void {
           this.elementos.push(elemento);
       }

       obtener(indice: number): T {
           return this.elementos[indice];
       }

       longitud(): number {
           return this.elementos.length;
       }
   }
   ```

   * Esta interfaz y su clase de implementación definen una lista genérica que puede contener cualquier tipo de elemento.

4. **Usando Funciones de Orden Superior para Filtrar y Ordenar:**

   ```typescript
   // Filtrar Elementos
   const filtrado = lista.filtrar((elemento: T) => elemento > 10);

   // Ordenar Elementos
   const ordenado = lista.ordenar((a: T, b: T) => a - b);
   ```

   * Las funciones de orden superior permiten manipular listas de manera concisa y eficiente.

5. **Prometiendo un Resultado Asíncrono:**

   ```typescript
   function obtenerDatosAsincronos(): Promise<any> {
       return new Promise((resolve, reject) => {
           // Simulando una demora asíncrona
           setTimeout(() => {
               resolve({ datos: "Obtenidos" });
           }, 2000);
       });
   }
   ```

   * La función `obtenerDatosAsincronos` retorna una promesa que se resolverá con los datos después de un retraso simulado.

6. **Utilizando async/await para Esperar a la Promesa Asíncrona:**

   ```typescript
   async function consumirDatosAsincronos() {
       const datos = await obtenerDatosAsincronos();
       console.log(datos.datos);
   }

   consumirDatosAsincronos();
   ```

   * Las palabras clave `async` y `await` permiten manejar promesas de manera síncrona, haciendo que el código sea más fácil de leer y escribir.

Este código demuestra varias características avanzadas de TypeScript y proporciona una base sólida para crear aplicaciones complejas y escalables.