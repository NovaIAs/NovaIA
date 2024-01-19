```javascript
// Crear una función para generar un número aleatorio entre dos valores
function generarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Crear una función para generar una contraseña segura
function generarContrasena() {
  // Definir el conjunto de caracteres que se utilizarán en la contraseña
  var caracteres = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()";

  // Generar una contraseña de 12 caracteres
  var contrasena = "";
  for (var i = 0; i < 12; i++) {
    // Obtener un índice aleatorio dentro del conjunto de caracteres
    var indice = generarNumeroAleatorio(0, caracteres.length - 1);

    // Obtener el carácter correspondiente al índice aleatorio
    var caracter = caracteres.charAt(indice);

    // Añadir el carácter a la contraseña
    contrasena += caracter;
  }

  // Devolver la contraseña
  return contrasena;
}

// Crear una función para encriptar una contraseña
function encriptarContrasena(contrasena) {
  // Utilizar la función btoa() para encriptar la contraseña
  var contrasenaEncriptada = btoa(contrasena);

  // Devolver la contraseña encriptada
  return contrasenaEncriptada;
}

// Crear una función para desencriptar una contraseña
function desencriptarContrasena(contrasenaEncriptada) {
  // Utilizar la función atob() para desencriptar la contraseña
  var contrasenaDesencriptada = atob(contrasenaEncriptada);

  // Devolver la contraseña desencriptada
  return contrasenaDesencriptada;
}

// Crear una función para guardar una contraseña en el almacenamiento local
function guardarContrasena(contrasena) {
  // Encriptar la contraseña
  var contrasenaEncriptada = encriptarContrasena(contrasena);

  // Almacenar la contraseña en el almacenamiento local
  localStorage.setItem("contrasena", contrasenaEncriptada);
}

// Crear una función para obtener una contraseña del almacenamiento local
function obtenerContrasena() {
  // Obtener la contraseña del almacenamiento local
  var contrasenaEncriptada = localStorage.getItem("contrasena");

  // Desencriptar la contraseña
  var contrasena = desencriptarContrasena(contrasenaEncriptada);

  // Devolver la contraseña
  return contrasena;
}

// Crear un formulario para que el usuario introduzca su contraseña
var formulario = document.createElement("form");
var label = document.createElement("label");
label.innerHTML = "Contraseña:";
var input = document.createElement("input");
input.type = "password";
var button = document.createElement("button");
button.innerHTML = "Guardar";

// Añadir el formulario al documento
document.body.appendChild(formulario);

// Añadir la etiqueta al formulario
formulario.appendChild(label);

// Añadir el campo de texto al formulario
formulario.appendChild(input);

// Añadir el botón al formulario
formulario.appendChild(button);

// Añadir un evento de clic al botón
button.addEventListener("click", function() {
  // Obtener la contraseña del campo de texto
  var contrasena = input.value;

  // Guardar la contraseña en el almacenamiento local
  guardarContrasena(contrasena);

  // Mostrar un mensaje al usuario
  alert("Contraseña guardada correctamente.");
});

// Obtener la contraseña del almacenamiento local
var contrasena = obtenerContrasena();

// Mostrar la contraseña al usuario
alert("Tu contraseña es: " + contrasena);
```

Este código es bastante complejo y tiene muchas partes:

* Primero, definimos una función para generar un número aleatorio entre dos valores. Esto se utiliza para generar una contraseña segura.
* A continuación, definimos una función para generar una contraseña segura. Esta función utiliza la función de números aleatorios para generar una contraseña que consiste en una mezcla de letras, números y símbolos.
* Luego, definimos una función para encriptar una contraseña. Esta función utiliza la función btoa() para encriptar la contraseña.
* A continuación, definimos una función para desencriptar una contraseña. Esta función utiliza la función atob() para desencriptar la contraseña.
* Luego, definimos una función para guardar una contraseña en el almacenamiento local. Esta función utiliza la función setItem() para almacenar la contraseña en el almacenamiento local.
* A continuación, definimos una función para obtener una contraseña del almacenamiento local. Esta función utiliza la función getItem() para obtener la contraseña del almacenamiento local.
* Finalmente, creamos un formulario para que el usuario introduzca su contraseña. Este formulario tiene un campo de texto para que el usuario introduzca su contraseña y un botón para guardar la contraseña.
* Añadimos un evento de clic al botón del formulario. Cuando el usuario hace clic en el botón, la contraseña se guarda en el almacenamiento local y se muestra un mensaje al usuario.
* Por último, obtenemos la contraseña del almacenamiento local y la mostramos al usuario.