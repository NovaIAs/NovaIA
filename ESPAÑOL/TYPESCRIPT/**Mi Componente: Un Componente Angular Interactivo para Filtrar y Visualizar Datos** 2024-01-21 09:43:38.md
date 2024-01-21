```typescript
import { Component, OnInit, Input, Output, EventEmitter } from '@angular/core';

@Component({
  selector: 'app-mi-componente',
  templateUrl: './mi-componente.component.html',
  styleUrls: ['./mi-componente.component.css']
})
export class MiComponenteComponent implements OnInit {

  // Propiedades de entrada
  @Input() titulo: string; // Título del componente
  @Input() datos: any[]; // Datos a mostrar en el componente

  // Propiedades de salida
  @Output() cambioDeDatos = new EventEmitter<any[]>(); // Emite un evento cuando los datos cambian

  // Propiedades internas
  datosFiltrados: any[]; // Datos filtrados según el filtro actual

  // Filtro actual
  filtro: string = '';

  constructor() { }

  ngOnInit(): void {
    this.filtrarDatos();
  }

  // Filtra los datos según el filtro actual
  filtrarDatos(): void {
    this.datosFiltrados = this.datos.filter((dato) => {
      return dato.nombre.toLowerCase().includes(this.filtro.toLowerCase());
    });
  }

  // Maneja el evento de cambio del filtro
  onFiltroChange(filtro: string): void {
    this.filtro = filtro;
    this.filtrarDatos();
  }

  // Maneja el evento de clic en un dato
  onDatoClick(dato: any): void {
    this.cambioDeDatos.emit(dato);
  }

}
```

Explicación del código:

* El componente `MiComponenteComponent` es un componente de Angular que se utiliza para mostrar una lista de datos filtrados.

* El componente tiene dos propiedades de entrada: `titulo` y `datos`. La propiedad `titulo` es el título del componente, y la propiedad `datos` es un array de objetos que contienen los datos a mostrar en el componente.

* El componente también tiene dos propiedades de salida: `cambioDeDatos` y `filtro`. La propiedad `cambioDeDatos` es un evento que se emite cuando los datos cambian, y la propiedad `filtro` es un string que contiene el filtro actual.

* El componente también tiene una propiedad interna llamada `datosFiltrados` que contiene los datos filtrados según el filtro actual.

* El método `ngOnInit()` se ejecuta cuando el componente se inicializa. En este método, se llama al método `filtrarDatos()` para filtrar los datos según el filtro actual.

* El método `filtrarDatos()` filtra los datos según el filtro actual. El método devuelve un array de objetos que contienen los datos que coinciden con el filtro.

* El método `onFiltroChange()` se ejecuta cuando el filtro cambia. En este método, se actualiza el valor de la propiedad `filtro` y se llama al método `filtrarDatos()` para filtrar los datos según el nuevo filtro.

* El método `onDatoClick()` se ejecuta cuando se hace clic en un dato. En este método, se emite el evento `cambioDeDatos` con el dato que se ha hecho clic.