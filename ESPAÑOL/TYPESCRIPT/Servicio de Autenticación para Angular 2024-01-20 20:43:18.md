```typescript
// Importar bibliotecas necesarias
import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { Observable, throwError } from 'rxjs';
import { map, catchError } from 'rxjs/operators';

// Definir el servicio de autenticación
@Injectable({
  providedIn: 'root'
})
export class AuthService {

  // Constructor del servicio
  constructor(private http: HttpClient) { }

  // Función para iniciar sesión
  login(username: string, password: string): Observable<any> {
    // Crear las cabeceras de la petición HTTP
    const headers = new HttpHeaders({
      'Content-Type': 'application/json'
    });

    // Crear el cuerpo de la petición HTTP
    const body = {
      username: username,
      password: password
    };

    // Enviar la petición HTTP al servidor
    return this.http.post('api/login', body, { headers: headers })
      .pipe(
        // Mapear la respuesta del servidor al objeto de usuario
        map((response: any) => {
          // Almacenar el token de acceso en el almacenamiento local
          localStorage.setItem('access_token', response.access_token);

          // Devolver el objeto de usuario
          return response.user;
        }),
        // Manejar los errores en la petición HTTP
        catchError((error: any) => {
          // Devolver un observable con el error
          return throwError(error);
        })
      );
  }

  // Función para cerrar sesión
  logout(): void {
    // Eliminar el token de acceso del almacenamiento local
    localStorage.removeItem('access_token');
  }

  // Función para verificar si el usuario está autenticado
  isAuthenticated(): boolean {
    // Obtener el token de acceso del almacenamiento local
    const token = localStorage.getItem('access_token');

    // Si el token es válido, devolver true, de lo contrario devolver false
    return token !== null && token !== undefined && token !== '';
  }

  // Función para obtener el usuario actual
  getCurrentUser(): Observable<any> {
    // Crear las cabeceras de la petición HTTP
    const headers = new HttpHeaders({
      'Authorization': `Bearer ${localStorage.getItem('access_token')}`
    });

    // Enviar la petición HTTP al servidor
    return this.http.get('api/me', { headers: headers })
      .pipe(
        // Mapear la respuesta del servidor al objeto de usuario
        map((response: any) => {
          // Devolver el objeto de usuario
          return response.user;
        }),
        // Manejar los errores en la petición HTTP
        catchError((error: any) => {
          // Devolver un observable con el error
          return throwError(error);
        })
      );
  }
}
```

Este código es un servicio de autenticación en Typescript que se puede utilizar en una aplicación Angular. El servicio proporciona funciones para iniciar sesión, cerrar sesión, verificar si el usuario está autenticado y obtener el usuario actual. El servicio utiliza el patrón de diseño de inyección de dependencias para inyectar el objeto HttpClient, que se utiliza para enviar peticiones HTTP al servidor. El servicio también utiliza los operadores map y catchError de RxJS para manejar las respuestas y los errores de las peticiones HTTP.