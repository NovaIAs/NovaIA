```typescript
// Clase abstracta para definir un elemento del juego
abstract class Elemento {
  protected nombre: string;
  protected descripcion: string;

  constructor(nombre: string, descripcion: string) {
    this.nombre = nombre;
    this.descripcion = descripcion;
  }

  getNombre(): string {
    return this.nombre;
  }

  getDescripcion(): string {
    return this.descripcion;
  }

  abstract usar(jugador: Jugador): void;
}

// Clase que representa un jugador
class Jugador {
  private nombre: string;
  private inventario: Elemento[];

  constructor(nombre: string) {
    this.nombre = nombre;
    this.inventario = [];
  }

  getNombre(): string {
    return this.nombre;
  }

  getInventario(): Elemento[] {
    return this.inventario;
  }

  añadirAlInventario(elemento: Elemento): void {
    this.inventario.push(elemento);
  }

  usarElemento(elemento: Elemento): void {
    elemento.usar(this);
  }
}

// Clase que representa una habitación
class Habitacion {
  private nombre: string;
  private descripcion: string;
  private elementos: Elemento[];

  constructor(nombre: string, descripcion: string, elementos: Elemento[]) {
    this.nombre = nombre;
    this.descripcion = descripcion;
    this.elementos = elementos;
  }

  getNombre(): string {
    return this.nombre;
  }

  getDescripcion(): string {
    return this.descripcion;
  }

  getElementos(): Elemento[] {
    return this.elementos;
  }
}

// Clase que representa una puerta
class Puerta extends Elemento {
  private habitacion1: Habitacion;
  private habitacion2: Habitacion;

  constructor(nombre: string, descripcion: string, habitacion1: Habitacion, habitacion2: Habitacion) {
    super(nombre, descripcion);
    this.habitacion1 = habitacion1;
    this.habitacion2 = habitacion2;
  }

  usar(jugador: Jugador): void {
    // Si la puerta está cerrada, se abre
    if (this.getDescripcion() === "Puerta cerrada") {
      this.descripcion = "Puerta abierta";
    }
    // Si la puerta está abierta, se cierra
    else {
      this.descripcion = "Puerta cerrada";
    }

    // Si la puerta está abierta, el jugador puede pasar a la otra habitación
    if (this.getDescripcion() === "Puerta abierta") {
      jugador.añadirAlInventario(this);
      jugador.usarElemento(this);
    }
  }
}

// Clase que representa una llave
class Llave extends Elemento {
  private puerta: Puerta;

  constructor(nombre: string, descripcion: string, puerta: Puerta) {
    super(nombre, descripcion);
    this.puerta = puerta;
  }

  usar(jugador: Jugador): void {
    // Si el jugador tiene la llave, se abre la puerta
    if (jugador.getInventario().includes(this)) {
      this.puerta.usar(jugador);
    }
    // Si el jugador no tiene la llave, no se abre la puerta
    else {
      console.log("No tienes la llave para abrir la puerta.");
    }
  }
}

// Clase que representa una poción
class Pocion extends Elemento {
  private efecto: string;

  constructor(nombre: string, descripcion: string, efecto: string) {
    super(nombre, descripcion);
    this.efecto = efecto;
  }

  usar(jugador: Jugador): void {
    // Se aplica el efecto de la poción al jugador
    console.log(`El jugador ${jugador.