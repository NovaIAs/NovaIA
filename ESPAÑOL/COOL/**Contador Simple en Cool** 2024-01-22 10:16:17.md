```cool
clase Contador {
    entero cuenta;

    método iniciar {
        cuenta := 0;
    }

    método incrementar {
        cuenta := cuenta + 1;
    }

    método decrementar {
        cuenta := cuenta - 1;
    }

    método obtenerValor {
        retorna cuenta;
    }

    método mostrarValor {
        IO.escribirString("El valor del contador es ");
        IO.escribirEntero(cuenta);
        IO.escribirNuevaLínea();
    }
}

clase Principal {
    método main {
        var contador := new Contador;
        contador.iniciar();
        contador.incrementar();
        contador.incrementar();
        contador.decrementar();
        contador.mostrarValor();
    }
}
```

Explicación del código:

* La clase `Contador` define un contador simple con métodos para iniciar el contador, incrementarlo, decrementarlo, obtener su valor actual y mostrarlo en la pantalla.
* La clase `Principal` contiene el método `main`, que es el punto de entrada del programa.
* En el método `main`, se crea una instancia de la clase `Contador`, se llama al método `iniciar` para inicializar el contador a 0, se llaman los métodos `incrementar` y `decrementar` para cambiar el valor del contador, y se llama al método `mostrarValor` para mostrar el valor actual del contador en la pantalla.