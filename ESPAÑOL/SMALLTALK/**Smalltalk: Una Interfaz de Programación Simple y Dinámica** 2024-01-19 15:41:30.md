```smalltalk
"Definición de la clase Principal"
Principal := Clase nueva.

"Definición de métodos individuales"
Principal agregar: #bienvenida.
Principal agregar: #despedida.
Principal agregar: #ayudar.
Principal agregar: #opciones.
Principal agregar: #salir.

"Definición de implementación"
Principal clase extendida
    bienvenida: (unTextoOpcional) -> [
        "Mostrar mensaje de bienvenida"
        Transcript show: '¡Bienvenido al programa!'.
        unTextoOpcional siNoVacio: [ Transcript show: unTextoOpcional ]].

Principal clase extendida
    despedida: -> [
        "Mostrar mensaje de despedida"
        Transcript show: '¡Adiós!'].

Principal clase extendida
    ayudar: -> [
        "Mostrar lista de comandos disponibles"
        Transcript show: 'Comandos disponibles:'.
        (Comando todos) hacer: [:comando |
            Transcript show: comando nombre; cr].
        Transcript cr].

Principal clase extendida
    opciones: -> [
        "Mostrar lista de opciones disponibles"
        Transcript show: 'Opciones disponibles:'.
        (Opcion todos) hacer: [:opcion |
            Transcript show: opcion nombre; cr].
        Transcript cr].

Principal clase extendida
    salir: -> [
        "Salir del programa"
        Transcript show: '¡Hasta la próxima!'].

"Definición de la clase Comando"
Comando := Clase nueva.

"Definición de métodos individuales"
Comando agregar: #nombre.
Comando agregar: #acción.
Comando agregar: #validar.

"Definición de implementación"
Comando clase extendida
    nombre: (unNombre) -> [
        "Obtener el nombre del comando"
        ^ unNombre].

Comando clase extendida
    acción: (unBloque) -> [
        "Obtener la acción asociada al comando"
        ^ unBloque].

Comando clase extendida
    validar: (unContexto) -> [
        "Validar si el comando puede ejecutarse en el contexto dado"
        ^ verdadero].

"Definición de la clase Opción"
Opción := Clase nueva.

"Definición de métodos individuales"
Opción agregar: #nombre.
Opción agregar: #valor.
Opción agregar: #actualizar.

"Definición de implementación"
Opción clase extendida
    nombre: (unNombre) -> [
        "Obtener el nombre de la opción"
        ^ unNombre].

Opción clase extendida
    valor: (unValor) -> [
        "Obtener el valor de la opción"
        ^ unValor].

Opción clase extendida
    actualizar: (unNuevoValor) -> [
        "Actualizar el valor de la opción"
        asignarValor: unNuevoValor].

"Definición de comandos"
Comando crear: #verificar que hacer: [
    "Comando para verificar si un número es primo"
    unNúmero := Texto leerNúmero.
    unNúmero siPrimo: [
        Transcript show: unNúmero; show: ' es primo.'.
    ] siNo: [
        Transcript show: unNúmero; show: ' no es primo.'].
].

Comando crear: #factorial que hacer: [
    "Comando para calcular el factorial de un número"
    unNúmero := Texto leerNúmero.
    unFactorial := unNúmero factorial.
    Transcript show: unNúmero; show: ' factorial es '; show: unFactorial.
].

Comando crear: #hipotenusa que hacer: [
    "Comando para calcular la hipotenusa de un triángulo rectángulo"
    unCatetoA := Texto leerNúmero.
    unCatetoB := Texto leerNúmero.
    unaHipotenusa := unCatetoA hipotenusaConCateto: unCatetoB.
    Transcript show: 'La hipotenusa del triángulo rectángulo con catetos ';
    show: unCatetoA;
    show: ' y ';
    show: unCatetoB;
    show: ' es ';
    show: unaHipotenusa.
].

"Definición de opciones"
Opción crear: #precision que hacer: [
    "Opción para establecer la precisión de los cálculos"
    unValor := Texto leerNúmero.
    Máximo enteros precisión: unValor.
    Transcript show: 'La precisión de los cálculos se ha establecido en ';
    show: unValor.