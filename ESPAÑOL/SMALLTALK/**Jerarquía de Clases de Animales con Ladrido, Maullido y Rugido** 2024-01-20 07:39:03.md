```smalltalk
"Crear una jerarquía de clases que represente una familia de animales."

Clase Animal
 variable: nombre

 método iniciar(unNombre)
  nombre := unNombre

 método getNombre()
  ^nombre

 clase Perro
subclaseDe: Animal

 método ladrar()
  "¡Guau!"

 clase Gato
subclaseDe: Animal

 método maullar()
  "¡Miau!"

 clase Leon
subclaseDe: Gato

 método rugir()
  "¡Roar!"

"Crear un perro, un gato y un león."

perro := Perro iniciar("Rex")
gato := Gato iniciar("Luna")
leon := Leon iniciar("Leo")

"Llamar a los métodos ladrar, maullar y rugir en cada animal."

perro ladrar

gato maullar

leon rugir
```

Explicación del código:

* Clase `Animal`: Esta clase define las características comunes de todos los animales, como su nombre.
* Clase `Perro`: Esta clase es una subclase de `Animal` que representa a los perros. Tiene un método `ladrar()`.
* Clase `Gato`: Esta clase es una subclase de `Animal` que representa a los gatos. Tiene un método `maullar()`.
* Clase `Leon`: Esta clase es una subclase de `Gato` que representa a los leones. Tiene un método `rugir()`.
* Crear instancia de perro, gato y león: Se crean tres objetos: `perro`, `gato` y `leon` de las clases `Perro`, `Gato` y `Leon` respectivamente.
* Llamar a los métodos: Se llaman los métodos `ladrar()`, `maullar()` y `rugir()` en cada objeto para imprimir los sonidos que hacen los animales.