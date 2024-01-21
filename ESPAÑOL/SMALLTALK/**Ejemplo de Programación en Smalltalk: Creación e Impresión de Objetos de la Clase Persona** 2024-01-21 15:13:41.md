```smalltalk
"Clase Persona"

Objeto clase Persona
  nombre: ""
  edad: 0

  inicializar

"Instancias de la clase Persona"

Objeto juan := Persona nueva.
Objeto maria := Persona nueva.

"Establece los valores de los atributos de los objetos"

juan nombre: 'Juan'.
juan edad: 25.
maria nombre: 'María'.
maria edad: 30.

"Imprime la información de los objetos"

juan imprimir.
maria imprimir.

"Método imprimir"

Objeto clase Persona >> imprimir

  "Imprime la información del objeto"

  self nombre imprimir.
  ' tiene ' imprimir.
  self edad imprimir.
  ' años' imprimir.
  cr imprimir.
```

Explicación del código:

* La clase `Persona` se define con dos atributos: `nombre` y `edad`.
* Se crean dos objetos de la clase `Persona`, llamados `juan` y `maría`.
* Se establecen los valores de los atributos de los objetos `juan` y `maría`.
* Se define el método `imprimir` en la clase `Persona`, que imprime la información del objeto.
* Se llama al método `imprimir` en los objetos `juan` y `maría`.