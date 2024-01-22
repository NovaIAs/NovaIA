```typescript
// **Librerías**
import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';

// **Interfaz**
interface Post {
  id: number;
  title: string;
  body: string;
}

// **Componente Angular**
@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css']
})

// **Clase del Componente**
export class AppComponent implements OnInit {
  // **Propiedades**
  posts: Post[] = [];

  // **Constructor**
  constructor(private http: HttpClient) { }

  // **Ciclo de Vida - Método ejecutado después que Angular ha inicializado completamente un componente**
  ngOnInit(): void {
    this.obtenerDatos();
  }

  // **Métodos**
  obtenerDatos() {
    this.http.get<Post[]>('https://jsonplaceholder.typicode.com/posts')
      .subscribe(
        (data: Post[]) => {
          this.posts = data;
        },
        (error: any) => {
          console.error(error);
        }
      );
  }

  filtrarPosts(busqueda: string) {
    if (busqueda.length === 0) {
      this.obtenerDatos();
      return;
    }

    let postsFiltrados: Post[] = [];

    postsFiltrados = this.posts.filter(function (post) {
      return post.title.toLowerCase().includes(busqueda.toLowerCase());
    });

    this.posts = postsFiltrados;
  }
}
```

**Explicación del Código:**

1. **Librerías:** Importamos las librerías necesarias para el funcionamiento del componente Angular:
   - `Component` y `OnInit` son las interfaces proporcionadas por Angular para definir componentes y definir métodos que se ejecutarán en diferentes momentos del ciclo de vida.
   - `HttpClient` es la librería para consumir servicios web.

2. **Interfaz:** Definimos una interfaz llamada `Post` que representa la estructura de los datos que vamos a obtener del servicio web.

3. **Componente Angular:** Definimos el componente principal de la aplicación con `@Component`. Especificamos el `selector`, el `templateUrl` donde se encuentra el HTML del componente y el `styleUrls` donde se encuentran los estilos CSS del componente.

4. **Clase del Componente:**
   - **Propiedades:** Creamos una propiedad llamada `posts` que será un array de objetos `Post`.
   - **Constructor:** Inyectamos el servicio `HttpClient` en el constructor.
   - **Ciclo de Vida - Método `ngOnInit`:** Angular llama a este método después de que el componente ha sido inicializado completamente. Aquí llamamos al método `obtenerDatos` para recuperar los datos del servicio web.
   - **Métodos:**
     - `obtenerDatos`: Este método utiliza el servicio `HttpClient` para hacer una petición GET a la URL especificada y suscribirse a la respuesta. Si la petición es exitosa, asignamos los datos recuperados a la propiedad `posts`.
     - `filtrarPosts`: Este método filtra los `posts` en función de un término de búsqueda proporcionado. Si el término de búsqueda está vacío, recupera todos los `posts`. Si no, filtra los `posts` cuyo título contenga el término de búsqueda en minúsculas.

5. **HTML del Componente:**
   - Utilizamos la propiedad `posts` para mostrar una lista de `posts`.
   - También tenemos un input de texto donde el usuario puede introducir un término de búsqueda. Cuando el usuario introduce algo en el input, llamamos al método `filtrarPosts` para filtrar los `posts`.