```typescript
// Módulo principal
import { Component, OnInit } from '@angular/core';

// Decorador del componente Angular
@Component({
  selector: 'app-componente-complejo',
  templateUrl: './componente-complejo.component.html',
  styleUrls: ['./componente-complejo.component.css']
})

// Clase del componente Angular
export class ComponenteComplejoComponent implements OnInit {

  // Propiedades del componente
  public titulo: string = 'Componente Complejo';
  public lista: string[] = ['Elemento 1', 'Elemento 2', 'Elemento 3'];
  public objeto: any = {
    propiedad1: 'Valor 1',
    propiedad2: 'Valor 2',
    propiedad3: 'Valor 3'
  };

  // Constructor del componente
  constructor() { }

  // Método que se ejecuta al inicializar el componente
  ngOnInit(): void {
    // Lógica para inicializar el componente
  }

  // Método que se ejecuta al hacer clic en un botón
  public onButtonClick(): void {
    // Lógica para manejar el evento de clic del botón
  }
}
```

// Explicación del código:

1. **Módulo Principal:**

   - Importamos los módulos necesarios de Angular, como `Component` y `OnInit`.

2. **Decorador del Componente Angular:**

   - El decorador `@Component` define las propiedades meta del componente, como su selector, plantilla y estilos.

3. **Clase del Componente Angular:**

   - Definimos la clase del componente Angular, que extiende de `OnInit`.

4. **Propiedades del Componente:**

   - Definimos las propiedades del componente, como `titulo`, `lista` y `objeto`.

5. **Constructor del Componente:**

   - El constructor del componente se ejecuta cuando se crea una instancia del componente.

6. **Método `ngOnInit`:**

   - El método `ngOnInit` se ejecuta cuando se inicializa el componente.

7. **Método `onButtonClick`:**

   - Definimos un método que se ejecuta cuando se hace clic en un botón en la plantilla del componente.

Este código crea un componente Angular complejo con múltiples propiedades, métodos y eventos. Es un ejemplo de cómo crear componentes en Angular y cómo manejar eventos y lógicas complejas dentro de ellos.