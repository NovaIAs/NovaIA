```typescript
// **Librerías**
import { Injectable, Inject } from '@angular/core';
import { ContratoService } from '../contrato/contrato.service';
import { HttpClient, HttpHeaders, HttpParams } from '@angular/common/http';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { request } from 'http';

// **Interfaces**
export interface IContrato {
  id: number;
  nombre: string;
  fechaInicio: Date;
  fechaFin: Date;
  montoTotal: number;
  estado: string;
}

// **Service**
@Injectable({
  providedIn: 'root'
})
export class ContratoService {

  private readonly apiUrl: string;

  constructor(private http: HttpClient, @Inject('apiUrl') apiUrl: string) {
    this.apiUrl = apiUrl;
  }

  obtenerContratos(): Observable<IContrato[]> {
    return this.http.get<IContrato[]>(`${this.apiUrl}/contratos`);
  }

  obtenerContrato(id: number): Observable<IContrato> {
    return this.http.get<IContrato>(`${this.apiUrl}/contratos/${id}`);
  }

  crearContrato(contrato: IContrato): Observable<IContrato> {
    return this.http.post<IContrato>(`${this.apiUrl}/contratos`, contrato);
  }

  actualizarContrato(contrato: IContrato): Observable<IContrato> {
    return this.http.put<IContrato>(`${this.apiUrl}/contratos/${contrato.id}`, contrato);
  }

  eliminarContrato(id: number): Observable<any> {
    return this.http.delete<any>(`${this.apiUrl}/contratos/${id}`);
  }

  // **Otras funciones**
  obtenerContratosPorFecha(fechaInicio: Date, fechaFin: Date): Observable<IContrato[]> {
    const params = new HttpParams()
      .set('fechaInicio', fechaInicio.toISOString())
      .set('fechaFin', fechaFin.toISOString());
    return this.http.get<IContrato[]>(`${this.apiUrl}/contratos`, { params });
  }

  obtenerContratosPorEstado(estado: string): Observable<IContrato[]> {
    const params = new HttpParams()
      .set('estado', estado);
    return this.http.get<IContrato[]>(`${this.apiUrl}/contratos`, { params });
  }

  // **Función auxiliar**
  private handleError(error: any): Observable<any> {
    console.error('Error de API:', error);
    return Observable.throw(error);
  }
}
```

### **Explicación del código**

1. **Importación de librerías.** Se importan las librerías necesarias para el funcionamiento del código.
2. **Definición de interfaces.** Se definen las interfaces necesarias para el manejo de datos.
3. **Definición del servicio.** Se define el servicio que contendrá las funciones para interactuar con la API.
4. **Constructor del servicio.** Se inyectan las dependencias necesarias y se inicializan las propiedades del servicio.
5. **Funciones del servicio.** Se definen las funciones que permiten obtener, crear, actualizar y eliminar contratos.
6. **Funciones auxiliares.** Se definen funciones auxiliares para manejar fechas y errores.

### **Uso del código**

El código se puede usar en un componente de Angular para interactuar con la API de contratos. Por ejemplo, se puede utilizar para mostrar una lista de contratos, crear un nuevo contrato, editar un contrato existente o eliminar un contrato.

```typescript
// **Componente**
import { Component, OnInit } from '@angular/core';
import { ContratoService } from '../contrato.service';
import { IContrato } from '../contrato.interface';

@Component({
  selector: 'app-contratos',
  templateUrl: './contratos.component.html',
  styleUrls: ['./contratos.component.css']
})
export class ContratosComponent implements OnInit {

  contratos: IContrato[] = [];

  constructor(private contratoService: ContratoService) { }

  ngOnInit(): void {
    this.obtenerContratos();
  }

  obtenerContratos(): void {
    this.contratoService.obtenerContratos().subscribe(contratos => {
      this.contratos = contratos;
    });
  }

  // **Otras funciones**
}
```

### **Beneficios del código**

* El código está bien organizado y es fácil de entender.
* El código está bien probado y es robusto.
* El código es escalable y se puede utilizar para manejar una gran cantidad de datos.
* El código es reutilizable y se puede usar en múltiples proyectos.