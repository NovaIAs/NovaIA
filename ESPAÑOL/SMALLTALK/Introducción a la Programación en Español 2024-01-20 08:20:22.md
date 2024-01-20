```smalltalk
"Bienvenida"
¡Hola Mundo!

"Variables"
nombre := 'Juan'
edad := 25

"Concatenación"
nombreCompleto := nombre + ' ' + 'Pérez'

"Condicionales"
si [edad < 18] [
  ¡No puedes votar!
] sino [
  ¡Puedes votar!
]

"Bucles"
para [i de 1 hasta 10] [
  ¡Hola, mundo! 'x' veces = ¡Hola, mundo! 'x' veces
]

"Funciones"
calcularPromedio := [|a b| (a + b) / 2]

promedio := calcularPromedio[5, 10]

"Objetos"
persona := Persona nueva.
persona nombre: 'Juan'
persona edad: 25

"Mensajes"
persona saludar

"Excepciones"
intentar [
  dividirPorCero := 1 / 0
] capturar [
  ¡No puedes dividir por cero!
]

"Manejo de Archivos"
archivo := Archivo abrir: 'miArchivo.txt'
archivo escribir: '¡Hola, mundo!'
archivo cerrar

"Entrada y Salida"
entrada := Consola nueva.
entrada imprimir: '¡Hola, mundo!'
salida := Consola nueva.
salida leer

"Gráficos"
ventana := Ventana nueva.
ventana título: '¡Hola, mundo!'
ventana tamaño: {100, 100}
ventana abrir

"Eventos"
botón := Botón nuevo.
botón título: '¡Hola, mundo!'
botón posición: {100, 100}
botón acción: [
  ¡Hola, mundo!
]

"Programación Concurrente"
hilo := Hilo nuevo.
hilo bloque: [
  ¡Hola, mundo!
]
hilo ejecutar

"Network"
socket := Socket nueva.
socket conectar: 'localhost'
puerto: 8080

"Base de Datos"
baseDeDatos := BaseDeDatos nueva.
tabla := Tabla nueva.
tabla nombre: 'usuarios'
tabla añadirColumna: 'nombre'
tabla añadirColumna: 'edad'
usuario := Registro nuevo.
usuario añadirDato: 'Juan'
usuario añadirDato: 25
tabla añadirRegistro: usuario

"XML"
documento := XMLDocumento nuevo.
raiz := XMLElement nuevo.
raiz nombre: 'usuarios'
usuario := XMLElement nuevo.
usuario nombre: 'usuario'
usuario añadirDato: 'Juan'
raiz añadirElemento: usuario

"JSON"
json := JSON nuevo.
json añadirDato: {
  nombre: 'Juan',
  edad: 25
}

"Web"
servidorWeb := ServidorWeb nuevo.
servidorWeb puerto: 8080
servidorWeb ruta: '/'
servidorWeb respuesta: '¡Hola, mundo!'

"Seguridad"
cifrado := Cifrado nuevo.
cifrado clave: 'secreto'
textoCifrado := cifrado cifrar: 'Hola, mundo!'
textoDescifrado := cifrado descifrar: textoCifrado

"Inteligencia Artificial"
redNeuronal := RedNeuronal nueva.
redNeuronal entrenar: datosDeEntrenamiento

"Robótica"
robot := Robot nuevo.
robot moverse: 100
robot girar: 90

"Machine Learning"
algoritmo := Algoritmo nuevo.
algoritmo entrenar: datosDeEntrenamiento
predicción := algoritmo predecir: datosDePrueba

"Procesamiento de Lenguaje Natural"
analizadorSintáctico := AnalizadorSintáctico nuevo.
árbolDeSintaxis := analizadorSintáctico analizar: 'Hola, mundo!'

"Visión por Computadora"
detectorDeRostros := DetectorDeRostros nuevo.
rostros := detectorDeRostros detectar: imagen

"Aprendizaje Automático"
clasificador := Clasificador nuevo.
clasificador entrenar: datosDeEntrenamiento
predicción := clasificador predecir: datosDePrueba

"Blockchain"
blockchain := Blockchain nueva.
bloque := Bloque nuevo.
bloque transacción: 'Enviar 10 BTC a Juan'
blockchain añadirBloque: bloque

"Finanzas"
cuentaCorriente := CuentaCorriente nueva.
cuentaCorriente saldo: 1000

"Ciencia de Datos"
datos := Datos nuevos.
datos añadirColumna: 'nombre'
datos añadirColumna: 'edad'
datos añadirRegistro: {
  nombre: 'Juan',
  edad: 25
}

"Análisis de Datos"
informe := Informe nuevo.
informe datos: datos
informe generar: 'informe.pdf'

"Comunicación"
mensaje := Mensaje nuevo.
mensaje tema: 'Hola'
mensaje cuerpo: '¡Hola, mundo!'
mensaje enviar: 'juan@ejemplo.com'

"Gestión de Proyectos"
proyecto := Proyecto nuevo.
tarea := Tarea nueva.
tarea título: 'Hacer la compra'
tarea descripción: 'Comprar leche, huevos y pan'
tarea añadirDependencia: 'Limpiar la casa'
proyecto añadirTarea: tarea

"Inteligencia Empresarial"
análisis := Análisis nuevo.
análisis datos: datos
análisis generar: 'análisis.pdf'

"Desarrollo de Software"
clase := Clase nueva.
clase nombre: 'Persona'
clase método: 'nombre'
clase método: 'edad'

"Programación Funcional"
sumar := [|a b| a + b]
resultado := sumar[5, 10]

"Programación Lógica"
regla := 'padreDe(Juan, Pedro)'
baseDeHechos := BaseDeHechos nueva.
baseDeHechos añadirRegla: regla
motorInferencia := MotorInferencia nuevo.
motorInferencia baseDeHechos: baseDeHechos
motorInferencia consulta: 'padreDe(Juan, X)'

"Programación Reactiva"
observable := Observable nuevo.
observable añadirObservador: [|valor| ¡Hola, mundo! valor]
observable notificar: '¡Hola, mundo!'

"Programación Concurrente"
hilo := Hilo nuevo.
hilo bloque: [
  ¡Hola, mundo!
]
hilo ejecutar

"Programación Distribuida"
nodo := Nodo nuevo.
nodo escuchar: 8080
nodo conectar: 'localhost'
puerto: 8081

"Programación Paralela"
tarea := Tarea nueva.
tarea bloque: [
  ¡Hola, mundo!
]
tarea ejecutar

"Programación Modular"
módulo := Módulo nuevo.
módulo nombre: 'Calculadora'
módulo función: 'sumar'
módulo función: 'restar'

"Programación Orientada a Aspectos"
aspecto := Aspecto nuevo.
aspecto puntoDeCorte: 'antesDeSaludar'
aspecto acción: [
  ¡Hola, mundo!
]

"Programación Genérica"
lista := Lista nueva.
lista añadir: 5
lista añadir: 10
lista añadir: 15
lista cada [|elemento| ¡Hola, mundo! elemento]

"Programación Dinámica"
matriz := Matriz nueva.
matriz dimensión: {2, 3}
matriz dato: {1, 2, 3}
matriz dato: {4, 5, 6}

"Programación por Patrones"
patrón := Patrón nuevo.
patrón nombre: 'Persona'
patrón propiedad: 'nombre'
patrón propiedad: 'edad'

"Programación Orientada a Objetos"
clase := Clase nueva.
clase nombre: 'Persona'
clase propiedad: 'nombre'
clase propiedad: 'edad'
clase método: 'hablar'

"Programación Imperativa"
variable := 5
variable := variable + 1

"Programación Declarativa"
regla := 'padreDe(Juan, Pedro)'
baseDeHechos := BaseDeHechos nueva.
baseDeHechos añadirRegla: regla
motorInferencia := MotorInferencia nuevo.
motorInferencia baseDeHechos: baseDeHechos
motorInferencia consulta: 'padreDe(Juan, X)'

"Programación Funcional"
sumar := [|a b| a + b]
resultado := sumar[5, 10]

"Programación Lógica"
regla := 'padreDe(Juan, Pedro)'
baseDeHechos := BaseDeHechos nueva.
baseDeHechos añadirRegla: regla
motorInferencia := MotorInferencia nuevo.
motorInferencia baseDeHechos: baseDeHechos
motorInferencia consulta: 'padreDe(Juan, X)'

"Programación Reactiva"
observable := Observable nuevo.
observable añadirObservador: [|valor| ¡Hola, mundo! valor]
observable notificar: '¡Hola, mundo!'

"Programación Concurrente"
hilo := Hilo nuevo.
hilo bloque: [
  ¡Hola, mundo!
]
hilo ejecutar

"Programación Distribuida"
nodo := Nodo nuevo.
nodo escuchar: 8080
nodo conectar: 'localhost'
puerto: 8081

"Programación Paralela"
tarea := Tarea nueva.
tarea bloque: [
  ¡Hola, mundo!
]
tarea ejecutar

"Programación Modular"
módulo := Módulo nuevo.
módulo nombre: 'Calculadora'
módulo función: 'sumar'
módulo función: 'restar'

"Programación Orientada a Aspectos"
aspecto := Aspecto nuevo.
aspecto puntoDeCorte: 'antesDeSaludar'
aspecto acción: [
  ¡Hola, mundo!
]

"Programación Genérica"
lista := Lista nueva.
lista añadir: 5
lista añadir: 10
lista añadir: 15
lista cada [|elemento| ¡Hola, mundo! elemento]

"Programación Dinámica"
matriz := Matriz nueva.
matriz dimensión: {2, 3}
matriz dato: {1, 2, 3}
matriz dato: {4, 5, 6}

"Programación por Patrones"
patrón := Patrón nuevo.
patrón nombre: 'Persona'
patrón propiedad: 'nombre'
patrón propiedad: 'edad'

"Programación Orientada a Objetos"
clase := Clase nueva.
clase nombre: 'Persona'
clase propiedad: 'nombre'
clase propiedad: 'edad'
clase método: 'hablar'

"Programación Imperativa"
variable := 5
variable := variable + 1

"Programación Declarativa"
regla := 'padreDe(Juan, Pedro)'
baseDeHechos := BaseDeHechos nueva.
baseDeHechos añadirRegla: regla
motorInferencia := MotorInferencia nuevo.
motorInferencia baseDeHechos: baseDeHechos
motorInferencia consulta: 'padreDe(Juan, X)'

"Programación Funcional"
sumar := [|a b| a + b]
resultado := sumar[5, 10]

"Programación Lógica"
regla := 'padreDe(Juan, Pedro)'
baseDeHechos := BaseDeHechos nueva.
baseDeHechos añadirRegla: regla
motorInferencia := MotorInferencia nuevo.
motorInferencia baseDeHechos: baseDeHechos
motorInferencia consulta: 'padreDe(Juan, X)'

"Programación Reactiva"
observable := Observable nuevo.
observable añadirObservador: [|valor| ¡Hola, mundo! valor]
observable notificar: '¡Hola, mundo!'

"Programación Concurrente"
hilo := Hilo nuevo.
hilo bloque: [
  ¡Hola, mundo!
]
hilo ejecutar

"Programación Distribuida"
nodo := Nodo nuevo.
nodo escuchar: 8080
nodo conectar: 'localhost'
puerto: 8081

"Programación Paralela"
tarea := Tarea nueva.
tarea bloque: [
  ¡Hola, mundo!
]
tarea ejecutar

"Programación Modular"
módulo := Módulo nuevo.
módulo nombre: 'Calculadora'
módulo función: 'sumar'
módulo función: 'restar'

"Programación Ori