```typescript
// Módulo para la generación de números aleatorios
import { randomBytes } from 'crypto';

// Función para generar un número entero aleatorio entre dos valores
function generarNumeroAleatorio(min: number, max: number): number {
  const buffer = randomBytes(4);
  const numeroBuffer = buffer.readUInt32BE(0);
  const numeroAleatorio = (numeroBuffer % (max - min + 1)) + min;
  return numeroAleatorio;
}

// Clase para representar a un enemigo
class Enemigo {
  // Propiedades públicas
  nombre: string;
  vida: number;
  ataque: number;
  defensa: number;

  // Constructor
  constructor(nombre: string, vida: number, ataque: number, defensa: number) {
    this.nombre = nombre;
    this.vida = vida;
    this.ataque = ataque;
    this.defensa = defensa;
  }

  // Método para atacar a otro enemigo
  atacar(enemigo: Enemigo): void {
    const daño = Math.max(0, this.ataque - enemigo.defensa);
    enemigo.vida -= daño;
    console.log(`${this.nombre} atacó a ${enemigo.nombre} por ${daño} puntos de daño.`);
  }

  // Método para comprobar si el enemigo está muerto
  estaMuerto(): boolean {
    return this.vida <= 0;
  }
}

// Clase para representar a un jugador
class Jugador extends Enemigo {
  // Propiedades públicas
  experiencia: number;
  nivel: number;

  // Constructor
  constructor(nombre: string, vida: number, ataque: number, defensa: number) {
    super(nombre, vida, ataque, defensa);
    this.experiencia = 0;
    this.nivel = 1;
  }

  // Método para ganar experiencia
  ganarExperiencia(experiencia: number): void {
    this.experiencia += experiencia;
    console.log(`${this.nombre} ganó ${experiencia} puntos de experiencia.`);

    // Comprobar si el jugador ha subido de nivel
    while (this.experiencia >= this.nivel * 100) {
      this.nivel++;
      this.vida += 10;
      this.ataque += 5;
      this.defensa += 3;
      console.log(`${this.nombre} subió al nivel ${this.nivel}.`);
    }
  }
}

// Función principal del juego
async function main() {
  // Crear un jugador y un enemigo
  const jugador = new Jugador('Héroe', 100, 10, 5);
  const enemigo = new Enemigo('Monstruo', 150, 8, 3);

  // Bucle principal del juego
  while (!jugador.estaMuerto() && !enemigo.estaMuerto()) {
    // El jugador ataca al enemigo
    jugador.atacar(enemigo);

    // Comprobar si el enemigo está muerto
    if (enemigo.estaMuerto()) {
      console.log(`${jugador.nombre} derrotó a ${enemigo.nombre}.`);
      jugador.ganarExperiencia(100);
      continue;
    }

    // El enemigo ataca al jugador
    enemigo.atacar(jugador);

    // Comprobar si el jugador está muerto
    if (jugador.estaMuerto()) {
      console.log(`${enemigo.nombre} derrotó a ${jugador.nombre}.`);
      break;
    }
  }
}

// Iniciar el juego
main();
```

Explicación del código:

* El módulo `crypto` se importa para generar números aleatorios.
* La función `generarNumeroAleatorio` se utiliza para generar un número entero aleatorio entre dos valores.
* La clase `Enemigo` representa a un enemigo en el juego. Tiene propiedades para el nombre, la vida, el ataque y la defensa. También tiene métodos para atacar a otro enemigo y comprobar si está muerto.
* La clase `Jugador` es una subclase de `Enemigo` que representa al jugador en el juego. Tiene propiedades adicionales para la experiencia y el nivel. También tiene un método para ganar experiencia.
* La función `main` es la función principal del juego. Crea un jugador y un enemigo, y luego entra en un bucle principal del juego. En cada iteración del bucle, el jugador ataca al enemigo, el enemigo ataca al jugador y se comprueba si alguno de los dos está muerto.
* El juego termina cuando el jugador o el enemigo muere.