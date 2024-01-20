```smalltalk
(Caballo nuevoCaballo) crearNuevoCaballo
    "Crea un nuevo caballo. El caballo se añade al mundo y se devuelve al usuario."

    | nuevoCaballo |
    nuevoCaballo := Caballo nuevo.
    mundo agregar: nuevoCaballo.
    ^ nuevoCaballo
```

Este código crea un nuevo caballo y lo añade al mundo. El caballo se devuelve al usuario para que pueda interactuar con él.

```smalltalk
(Caballo this posicionarEn: unaPosición) posicionarEn: nuevaPosición
    "Posiciona el caballo en una nueva posición. La nueva posición se pasa al caballo como un argumento."

    this posicion := nuevaPosición
```

Este código posiciona el caballo en una nueva posición. La nueva posición se pasa al caballo como un argumento.

```smalltalk
(Caballo this moverEnDirección: unaDirección) moverEnDirección: nuevaDirección
    "Mueve el caballo en la dirección especificada. La dirección se pasa al caballo como un argumento."

    this dirección := nuevaDirección
    this posicion := this posicion + (this dirección * this velocidad)
```

Este código mueve el caballo en la dirección especificada. La dirección se pasa al caballo como un argumento.

```smalltalk
(Caballo this comer: unaComida) comer: nuevaComida
    "El caballo come la comida especificada. La comida se pasa al caballo como un argumento."

    this energía := this energía + nuevaComida energía
```

Este código hace que el caballo coma la comida especificada. La comida se pasa al caballo como un argumento.

```smalltalk
(Caballo this morir) morir
    "El caballo muere. El caballo se elimina del mundo."

    mundo quitar: this
```

Este código hace que el caballo muera. El caballo se elimina del mundo.

```smalltalk
(Mundo this agregar: unCaballo) agregar: nuevoCaballo
    "Añade un nuevo caballo al mundo. El caballo se añade a la lista de caballos del mundo."

    this caballos agregar: nuevoCaballo
```

Este código añade un nuevo caballo al mundo. El caballo se añade a la lista de caballos del mundo.

```smalltalk
(Mundo this quitar: unCaballo) quitar: caballoMuerto
    "Elimina un caballo del mundo. El caballo se elimina de la lista de caballos del mundo."

    this caballos quitar: caballoMuerto
```

Este código elimina un caballo del mundo. El caballo se elimina de la lista de caballos del mundo.

```smalltalk
(Mundo this actualizar) actualizar
    "Actualiza el mundo. El mundo mueve todos los caballos y comprueba si alguno de ellos ha muerto."

    this caballos hacer: [ :caballo | caballo moverEnDirección: caballo dirección ]
    this caballos hacer: [ :caballo | caballo morir si: [ caballo energía < 0 ] ]
```

Este código actualiza el mundo. El mundo mueve todos los caballos y comprueba si alguno de ellos ha muerto.

```smalltalk
(Mundo this mostrar) mostrar
    "Muestra el mundo. El mundo muestra todos los caballos en la pantalla."

    this caballos hacer: [ :caballo | caballo mostrar ]
```

Este código muestra el mundo. El mundo muestra todos los caballos en la pantalla.