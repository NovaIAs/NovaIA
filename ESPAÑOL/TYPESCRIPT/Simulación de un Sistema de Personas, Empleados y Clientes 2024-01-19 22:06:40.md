```typescript
// Definición de una función que genera un número aleatorio entre dos valores dados
const generarNumeroAleatorio = (min: number, max: number): number => {
  return Math.floor(Math.random() * (max - min + 1)) + min;
};

// Definición de una clase que representa una persona
class Persona {
  nombre: string;
  edad: number;

  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  hablar(): void {
    console.log(`Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años.`);
  }
}

// Definición de una clase que representa un empleado
class Empleado extends Persona {
  salario: number;

  constructor(nombre: string, edad: number, salario: number) {
    super(nombre, edad);
    this.salario = salario;
  }

  trabajar(): void {
    console.log(`${this.nombre} está trabajando.`);
  }

  cobrarSalario(): void {
    console.log(`${this.nombre} cobra un salario de ${this.salario} euros.`);
  }
}

// Definición de una clase que representa un cliente
class Cliente extends Persona {
  saldo: number;

  constructor(nombre: string, edad: number, saldo: number) {
    super(nombre, edad);
    this.saldo = saldo;
  }

  comprarProducto(precio: number): void {
    if (this.saldo >= precio) {
      this.saldo -= precio;
      console.log(`${this.nombre} ha comprado un producto por ${precio} euros.`);
    } else {
      console.log(`${this.nombre} no tiene suficiente saldo para comprar el producto.`);
    }
  }
}

// Creación de un array de personas
const personas: Persona[] = [];

// Añadir 10 personas al array de personas
for (let i = 0; i < 10; i++) {
  const nombre = `Persona ${i + 1}`;
  const edad = generarNumeroAleatorio(18, 65);
  const persona = new Persona(nombre, edad);
  personas.push(persona);
}

// Mostrar las personas del array de personas
console.log('Personas:');
for (const persona of personas) {
  persona.hablar();
}

// Creación de un array de empleados
const empleados: Empleado[] = [];

// Añadir 5 empleados al array de empleados
for (let i = 0; i < 5; i++) {
  const nombre = `Empleado ${i + 1}`;
  const edad = generarNumeroAleatorio(25, 50);
  const salario = generarNumeroAleatorio(1000, 3000);
  const empleado = new Empleado(nombre, edad, salario);
  empleados.push(empleado);
}

// Mostrar los empleados del array de empleados
console.log('Empleados:');
for (const empleado of empleados) {
  empleado.hablar();
  empleado.trabajar();
  empleado.cobrarSalario();
}

// Creación de un array de clientes
const clientes: Cliente[] = [];

// Añadir 5 clientes al array de clientes
for (let i = 0; i < 5; i++) {
  const nombre = `Cliente ${i + 1}`;
  const edad = generarNumeroAleatorio(18, 65);
  const saldo = generarNumeroAleatorio(100, 1000);
  const cliente = new Cliente(nombre, edad, saldo);
  clientes.push(cliente);
}

// Mostrar los clientes del array de clientes
console.log('Clientes:');
for (const cliente of clientes) {
  cliente.hablar();
  cliente.comprarProducto(generarNumeroAleatorio(10, 100));
}
```

Explicación del código:

* Se define una función `generarNumeroAleatorio` que genera un número aleatorio entre dos valores dados.
* Se definen tres clases: `Persona`, `Empleado` y `Cliente`. La clase `Persona` representa a una persona, la clase `Empleado` representa a un empleado y la clase `Cliente` representa a un cliente.
* Se crean arrays de personas, empleados y clientes.
* Se añaden personas, empleados y clientes a sus respectivos arrays.
* Se muestran las personas, empleados y clientes de sus respectivos arrays.

Este código es complejo y diferenciado porque utiliza varias características del lenguaje TypeScript, como clases, herencia, polimorfismo y arrays. También utiliza la función `generarNumeroAleatorio` para generar números aleatorios.