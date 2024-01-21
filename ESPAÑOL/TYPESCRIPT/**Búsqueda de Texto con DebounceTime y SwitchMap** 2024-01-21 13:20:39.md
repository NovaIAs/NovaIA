```typescript
// Importar las bibliotecas necesarias
import { Observable, of, fromEvent } from 'rxjs';
import { map, filter, switchMap, debounceTime } from 'rxjs/operators';

// Crear un observable que emita el valor actual del campo de texto
const input$ = fromEvent(document.querySelector('input'), 'input');

// Filtrar los valores del observable que no sean vacíos
const nonEmptyInput$ = input$.pipe(
  filter(event => (event.target as HTMLInputElement).value !== '')
);

// DebounceTime: esperar 300 milisegundos antes de emitir el valor
const debouncedInput$ = nonEmptyInput$.pipe(
  debounceTime(300)
);

// SwitchMap: cuando se emita un valor, realizar una petición HTTP
// El resultado de la petición se emite como un nuevo observable
const searchResults$ = debouncedInput$.pipe(
  switchMap(event => {
    const term = (event.target as HTMLInputElement).value;
    return Observable.ajax(`https://api.example.com/search?q=${term}`);
  })
);

// Subscribirse al observable searchResults$ para recibir los resultados de la búsqueda
searchResults$.subscribe(response => {
  console.log('Resultados de la búsqueda:', response);
});
```

Este código crea un observable que emite el valor actual del campo de texto cada vez que el usuario introduce un carácter.

Luego, filtra los valores del observable que no sean vacíos y aplica un debounceTime de 300 milisegundos, lo que significa que espera 300 milisegundos antes de emitir el valor actual.

A continuación, utiliza el operador switchMap para realizar una petición HTTP a una API de búsqueda cada vez que se emite un valor. El resultado de la petición se emite como un nuevo observable.

Finalmente, se suscribe al observable searchResults$ para recibir los resultados de la búsqueda y los imprime en la consola.

Este código es complejo porque utiliza una combinación de operadores de RxJS para crear un observable que solo emite valores cuando el usuario ha dejado de escribir durante 300 milisegundos. También utiliza el operador switchMap para realizar una petición HTTP cada vez que se emite un valor, y se suscribe al observable resultante para recibir los resultados de la búsqueda.