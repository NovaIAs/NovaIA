```cool
-- Este código representa un sistema de archivos simple en COOL.

clase Archivo {
    nombre : String;
    contenido : String;

    constructor nuevo(nombre : String) {
        self.nombre := nombre;
        self.contenido := "";
    }

    método escribir(contenido : String) {
        self.contenido := self.contenido + contenido;
    }

    método leer() : String {
        return self.contenido;
    }
}

clase Directorio {
    nombre : String;
    archivos : Array[Archivo];
    directorios : Array[Directorio];

    constructor nuevo(nombre : String) {
        self.nombre := nombre;
        self.archivos := Array[Archivo].nuevo(0);
        self.directorios := Array[Directorio].nuevo(0);
    }

    método agregarArchivo(archivo : Archivo) {
        self.archivos := self.archivos + [archivo];
    }

    método agregarDirectorio(directorio : Directorio) {
        self.directorios := self.directorios + [directorio];
    }

    método buscarArchivo(nombre : String) : Archivo {
        for (i in 0..self.archivos.longitud) {
            if (self.archivos[i].nombre == nombre) {
                return self.archivos[i];
            }
        }

        return null;
    }

    método buscarDirectorio(nombre : String) : Directorio {
        for (i in 0..self.directorios.longitud) {
            if (self.directorios[i].nombre == nombre) {
                return self.directorios[i];
            }
        }

        return null;
    }
}

clase SistemaArchivos {
    raiz : Directorio;

    constructor nuevo() {
        self.raiz := Directorio.nuevo("/");
    }

    método crearArchivo(ruta : String, contenido : String) {
        Directorio directorio = self.obtenerDirectorio(ruta.split("/")[-2]);
        Archivo archivo = Archivo.nuevo(ruta.split("/")[-1]);
        archivo.escribir(contenido);
        directorio.agregarArchivo(archivo);
    }

    método leerArchivo(ruta : String) : String {
        Directorio directorio = self.obtenerDirectorio(ruta.split("/")[-2]);
        Archivo archivo = directorio.buscarArchivo(ruta.split("/")[-1]);

        if (archivo != null) {
            return archivo.leer();
        } else {
            return "";
        }
    }

    método crearDirectorio(ruta : String) {
        Directorio directorio = self.obtenerDirectorio(ruta.split("/")[-2]);
        Directorio nuevoDirectorio = Directorio.nuevo(ruta.split("/")[-1]);
        directorio.agregarDirectorio(nuevoDirectorio);
    }

    método eliminar(ruta : String) {
        Directorio directorio = self.obtenerDirectorio(ruta.split("/")[-2]);

        if (ruta.split("/")[-1].contiene(".")) {
            directorio.archivos.eliminar(directorio.buscarArchivo(ruta.split("/")[-1]));
        } else {
            directorio.directorios.eliminar(directorio.buscarDirectorio(ruta.split("/")[-1]));
        }
    }

    método obtenerDirectorio(ruta : String) : Directorio {
        Directorio directorio = self.raiz;

        for (parte in ruta.split("/")[1..-1]) {
            directorio = directorio.buscarDirectorio(parte);
            if (directorio == null) {
                return null;
            }
        }

        return directorio;
    }
}

clase Principal {
    método corre()