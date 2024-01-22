```typescript
// Importando las librerías necesarias
import { Observable, interval } from 'rxjs';
import { map, filter, take, switchMap } from 'rxjs/operators';

// Creando una función asíncrona que devuelve un Observable
const obtenerDatos = (url: string) => {
  return Observable.create((observador) => {
    fetch(url)
      .then((respuesta) => respuesta.json())
      .then((datos) => {
        observador.next(datos);
        observador.complete();
      })
      .catch((error) => {
        observador.error(error);
      });
  });
};

// Creando un Observable que emite un valor cada segundo
const fuente = interval(1000);

// Utilizando el operador switchMap para obtener los datos cada segundo
const datos = fuente.pipe(
  switchMap((valor) => obtenerDatos('https://jsonplaceholder.typicode.com/todos/' + valor))
);

// Utilizando el operador filter para filtrar los datos por estado
const datosCompletados = datos.pipe(
  filter((dato) => dato.completed)
);

// Utilizando el operador take para limitar el número de emisiones
const primerosDiezDatosCompletados = datosCompletados.pipe(
  take(10)
);

// Suscribiéndonos al Observable para imprimir los datos en la consola
primerosDiezDatosCompletados.subscribe((dato) => {
  console.log(dato);
});
```

Este código realiza lo siguiente:

1. Importa las librerías necesarias de RxJS.
2. Define una función asíncrona `obtenerDatos` que devuelve un Observable que emite los datos obtenidos de una URL.
3. Crea un Observable `fuente` que emite un valor cada segundo.
4. Utiliza el operador `switchMap` para obtener los datos cada segundo utilizando la función `obtenerDatos`.
5. Utiliza el operador `filter` para filtrar los datos por estado, en este caso, los datos completados.
6. Utiliza el operador `take` para limitar el número de emisiones a los primeros 10 datos completados.
7. Finalmente, se suscribe al Observable para imprimir los datos en la consola.