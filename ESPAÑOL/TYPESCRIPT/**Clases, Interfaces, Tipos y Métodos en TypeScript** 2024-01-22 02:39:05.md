```typescript
// Definición de una interfaz
interface Perro {
  nombre: string;
  raza: string;
  edad: number;
}

// Definición de un tipo
type RazaPerro = "Pastor alemán" | "Caniche" | "Golden retriever";

// Definición de una clase
class Animal {
  constructor(public nombre: string, public especie: string) {}

  hablar(): void {
    console.log(`Hola, soy ${this.nombre} y soy un ${this.especie}`);
  }
}

// Definición de una clase hija
class Perro extends Animal {
  constructor(nombre: string, raza: RazaPerro, edad: number) {
    super(nombre, "perro");
    this.raza = raza;
    this.edad = edad;
  }

  ladrar(): void {
    console.log(`Guau, guau! Me llamo ${this.nombre}`);
  }
}

// Definición de una función
function saludar(animal: Animal): void {
  animal.hablar();
}

// Creación de un objeto de tipo Perro
const perrito = new Perro("Firulais", "Pastor alemán", 5);

// Llamada a la función saludar con el objeto perrito
saludar(perrito);

// Llamada al método ladrar del objeto perrito
perrito.ladrar();
```

Explicación del código:

* Se define una interfaz `Perro` que describe las propiedades de un perro: `nombre`, `raza` y `edad`.

* Se define un tipo `RazaPerro` que contiene las razas de perro válidas: "Pastor alemán", "Caniche" y "Golden retriever".

* Se define una clase abstracta `Animal` que tiene un constructor que recibe el nombre y la especie del animal. También tiene un método `hablar()` que imprime un mensaje.

* Se define una clase hija `Perro` que extiende de la clase `Animal`. El constructor de la clase `Perro` recibe el nombre, la raza y la edad del perro. También tiene un método `ladrar()` que imprime un mensaje.

* Se define una función `saludar()` que recibe un objeto de tipo `Animal` y llama al método `hablar()` del objeto.

* Se crea un objeto de tipo `Perro` llamado `perrito` con los valores: "Firulais", "Pastor alemán" y 5.

* Se llama a la función `saludar()` con el objeto `perrito`.

* Se llama al método `ladrar()` del objeto `perrito`.