```typescript
// Importar las bibliotecas necesarias.
import { Component, OnInit, ViewChild } from '@angular/core';
import { MatTableDataSource } from '@angular/material/table';
import { MatPaginator } from '@angular/material/paginator';
import { MatSort } from '@angular/material/sort';
import { ApiService } from '../api.service';

// Definir el componente de angular.
@Component({
  selector: 'app-tabla-datos',
  templateUrl: './tabla-datos.component.html',
  styleUrls: ['./tabla-datos.component.css']
})
export class TablaDatosComponent implements OnInit {
  // Definir las variables del componente.
  displayedColumns: string[] = ['nombre', 'apellido', 'correo', 'telefono'];
  dataSource = new MatTableDataSource();

  // Definir las vistas secundarias del componente.
  @ViewChild(MatPaginator) paginator!: MatPaginator;
  @ViewChild(MatSort) sort!: MatSort;

  // Constructor del componente.
  constructor(private apiService: ApiService) { }

  // Método que se ejecuta al iniciar el componente.
  ngOnInit(): void {
    // Obtener los datos de la API.
    this.apiService.getDatos().subscribe((data: any) => {
      // Asignar los datos a la fuente de datos de la tabla.
      this.dataSource.data = data;
    });
  }

  // Método que se ejecuta al cambiar la página de la tabla.
  onPageChange(event: any) {
    // Actualizar la vista de la tabla.
    this.dataSource.paginator = event;
  }

  // Método que se ejecuta al cambiar el orden de la tabla.
  onSortChange(event: any) {
    // Actualizar la vista de la tabla.
    this.dataSource.sort = event;
  }
}
```

Explicación del código:

* El componente `TablaDatosComponent` implementa la interfaz `OnInit` de Angular, lo que significa que tiene un método `ngOnInit()` que se ejecuta cuando el componente se inicializa.
* El componente utiliza el servicio `ApiService` para obtener los datos de la API.
* El componente utiliza la biblioteca `MatTableDataSource` para crear la fuente de datos de la tabla.
* El componente utiliza las vistas secundarias `MatPaginator` y `MatSort` para agregar paginación y ordenación a la tabla.
* El método `ngOnInit()` obtiene los datos de la API y los asigna a la fuente de datos de la tabla.
* El método `onPageChange()` se ejecuta cuando el usuario cambia la página de la tabla y actualiza la vista de la tabla.
* El método `onSortChange()` se ejecuta cuando el usuario cambia el orden de la tabla y actualiza la vista de la tabla.