```cool
-- Programa en COOL para calcular el factorial de un número

operacion : entero -> entero
operacion(n) es
  si n <= 1 entonces
    1
  si_no
    n * operacion(n-1)
  fi_si
fi_operacion

clase Factorial {
  operacion: entero -> entero
  operacion(n) es
    si n <= 1 entonces
      1
    si_no
      n * operacion(n-1)
    fi_si
  fi_operacion
}

clase Principal {
  principal:Ninguno -> entero
  principal() es
    (new Factorial()).operacion(5)
  fi_principal
}
```

Explicación:

* El programa define una clase llamada `Factorial` que tiene un método llamado `operacion` que calcula el factorial de un número.
* El método `operacion` usa recursión para calcular el factorial de un número.
* La clase `Principal` tiene un método llamado `principal` que crea una instancia de la clase `Factorial` y llama al método `operacion` para calcular el factorial de 5.
* El resultado del cálculo se imprime en la consola.