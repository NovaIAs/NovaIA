```smalltalk
[ [El código es un mensaje a la clase String que crea un nuevo método llamado `vocales`.]

String vocals

[El método `vocales`toma una cadena como entrada y devuelve una cadena que contiene sólo las vocales de la entrada.]

^ String with: self select: [:char | char isVowel]

[La siguiente parte del código define qué caracteres se consideran vocales.]

isVowel

^ (self asLowercase = $a) or: (self asLowercase = $e) or: (self asLowercase = $i) or: (self asLowercase = $o) or: (self asLowercase = $u)

] joinWith: ', '
```

Este código crea un nuevo método llamado `vocales` para la clase `String` en Smalltalk. El método `vocales` toma una cadena de caracteres como entrada y devuelve una nueva cadena que contiene sólo las vocales de la cadena de entrada.

El método `vocales` utiliza el método `select:` para seleccionar los caracteres de la cadena de entrada que son vocales. El método `isVowel` se utiliza para determinar si un carácter es una vocal. El método `isVowel` devuelve `true` si el carácter es una vocal y `false` si no lo es.

El código también define qué caracteres se consideran vocales. En este caso, las vocales son las letras "a", "e", "i", "o" y "u".

Por último, el código utiliza el método `joinWith:` para unir las vocales en una sola cadena. La cadena resultante contiene sólo las vocales de la cadena de entrada.

Este código es un ejemplo de cómo se puede utilizar Smalltalk para crear nuevos métodos para las clases existentes. El método `vocales` es una herramienta útil que se puede utilizar para extraer las vocales de una cadena de caracteres.