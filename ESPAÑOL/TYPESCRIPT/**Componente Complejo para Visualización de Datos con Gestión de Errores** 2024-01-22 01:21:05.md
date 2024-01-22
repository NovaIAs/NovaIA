```typescript
// Importar las librerías necesarias
import { Component, OnInit, ViewChild, ElementRef } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { Observable, of } from 'rxjs';
import { catchError, map, tap } from 'rxjs/operators';

// Definir el componente
@Component({
  selector: 'mi-componente-complejo',
  templateUrl: './mi-componente-complejo.component.html',
  styleUrls: ['./mi-componente-complejo.component.css']
})
export class MiComponenteComplejoComponent implements OnInit {
  // Obtener el elemento HTML desde el template
  @ViewChild('miGrafico') miGrafico: ElementRef;

  // Variables reactivas
  datos$: Observable<any>; // observable para datos
  error$: Observable<any>; // observable para errores

  // Opciones predeterminadas para el gráfico
  opcionesGrafico = {
    title: {
      text: 'Mi gráfico complejo'
    },
    series: [
      {
        data: []
      }
    ]
  };

  // Constructor
  constructor(private http: HttpClient) { }

  // Método que se ejecuta al iniciar el componente
  ngOnInit() {
    // Obtener los datos del servidor
    this.datos$ = this.http.get('api/datos').pipe(
      map(data => data['datos']), // extraer solo los datos
      tap(data => console.log('Datos obtenidos:', data)),
      catchError(error => of([])) // manejar errores
    );

    // Suscribirse al observable de datos
    this.datos$.subscribe(datos => {
      // Actualizar las opciones del gráfico con los datos recibidos
      this.opcionesGrafico.series[0].data = datos;

      // Crear el gráfico
      Highcharts.chart(this.miGrafico.nativeElement, this.opcionesGrafico);
    });

    // Suscribirse al observable de errores
    this.error$ = this.datos$.pipe(
      catchError(error => of(error)) // mapear errores al observable de errores
    );
  }
}
```

**Explicación del código:**

* El componente `MiComponenteComplejoComponent` depende de varias librerías, entre ellas `Component`, `OnInit`, `ViewChild`, `HttpClient` y operadores como `tap`, `map`, `catchError`, `of`.
* El componente utiliza `ViewChild` para obtener una referencia al elemento HTML con el id 'miGrafico' en el template.
* Se definen dos observables reactivos: `datos$` y `error$`.
* `datos$` hace una petición HTTP a la API en '/api/datos' y extrae los datos de la respuesta recibida.
* `error$` se utiliza para manejar errores en la petición HTTP.
* Las opciones predeterminadas del gráfico se definen en `opcionesGrafico`.
* En el método `ngOnInit()`, se subscribe al observable `datos$` para actualizar las opciones del gráfico con los datos recibidos y crear el gráfico utilizando Highcharts.
* También se subscribe al observable `error$` para mostrar errores en caso de que ocurran.

**Este código es complejo debido a:**

* Su uso de múltiples librerías y operadores reactivos.
* La gestión de errores mediante un observable dedicado.
* La creación dinámica de un gráfico utilizando Highcharts.