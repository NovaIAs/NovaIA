```typescript
// Simulación de un sistema de archivos en memoria

// Definición de la interfaz de un archivo
interface Archivo {
  nombre: string;
  contenido: string;
}

// Definición de la interfaz de un directorio
interface Directorio {
  nombre: string;
  archivos: Archivo[];
  directorios: Directorio[];
}

// Función para crear un archivo
function crearArchivo(nombre: string, contenido: string): Archivo {
  return {
    nombre,
    contenido
  };
}

// Función para crear un directorio
function crearDirectorio(nombre: string): Directorio {
  return {
    nombre,
    archivos: [],
    directorios: [],
  };
}

// Función para agregar un archivo a un directorio
function agregarArchivoADirectorio(directorio: Directorio, archivo: Archivo) {
  directorio.archivos.push(archivo);
}

// Función para agregar un directorio a un directorio
function agregarDirectorioADirectorio(directorio: Directorio, subdirectorio: Directorio) {
  directorio.directorios.push(subdirectorio);
}

// Creación del sistema de archivos en memoria
const raiz = crearDirectorio('raiz');

// Agregar archivos y directorios al sistema de archivos
const archivo1 = crearArchivo('archivo1.txt', 'Contenido del archivo 1');
const archivo2 = crearArchivo('archivo2.txt', 'Contenido del archivo 2');
const subdirectorio1 = crearDirectorio('subdirectorio1');

agregarArchivoADirectorio(raiz, archivo1);
agregarArchivoADirectorio(raiz, archivo2);
agregarDirectorioADirectorio(raiz, subdirectorio1);

// Impresión del contenido del sistema de archivos
console.log('Contenido del sistema de archivos:');
console.log(raiz);

/*
Explicación del código:

El código anterior define una interfaz para representar archivos y directorios, y luego define funciones para crear archivos y directorios, y para agregar archivos y directorios a directorios.

A continuación, se crea el sistema de archivos en memoria creando el directorio raíz y agregando algunos archivos y directorios a él.

Por último, se imprime el contenido del sistema de archivos en la consola.
*/
```