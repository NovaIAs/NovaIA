```javascript
// Definir una función para generar un código aleatorio
const generarCodigoAleatorio = (longitud) => {
  // Crear un array con todos los caracteres posibles
  const caracteres = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

  // Inicializar el código vacío
  let codigo = '';

  // Iterar sobre la longitud del código deseado
  for (let i = 0; i < longitud; i++) {
    // Obtener un índice aleatorio dentro del array de caracteres
    const indiceAleatorio = Math.floor(Math.random() * caracteres.length);

    // Obtener el carácter correspondiente al índice aleatorio
    const caracter = caracteres[indiceAleatorio];

    // Concatenar el carácter al código
    codigo += caracter;
  }

  // Devolver el código generado
  return codigo;
};

// Definir una función para encriptar un mensaje usando un código secreto
const encriptarMensaje = (mensaje, codigoSecreto) => {
  // Convertir el mensaje y el código secreto en arrays de caracteres
  const mensajeArray = mensaje.split('');
  const codigoSecretoArray = codigoSecreto.split('');

  // Inicializar el mensaje encriptado
  let mensajeEncriptado = '';

  // Iterar sobre el mensaje
  for (let i = 0; i < mensajeArray.length; i++) {
    // Obtener el carácter actual del mensaje
    const caracter = mensajeArray[i];

    // Obtener el índice del carácter en el alfabeto (A = 0, Z = 25)
    const indiceCaracter = caracter.charCodeAt(0) - 'A'.charCodeAt(0);

    // Obtener el índice del código secreto en el alfabeto
    const indiceCodigoSecreto = codigoSecretoArray[i].charCodeAt(0) - 'A'.charCodeAt(0);

    // Sumar los índices para obtener el índice encriptado
    const indiceEncriptado = (indiceCaracter + indiceCodigoSecreto) % 26;

    // Convertir el índice encriptado en un carácter
    const caracterEncriptado = String.fromCharCode(indiceEncriptado + 'A'.charCodeAt(0));

    // Concatenar el carácter encriptado al mensaje encriptado
    mensajeEncriptado += caracterEncriptado;
  }

  // Devolver el mensaje encriptado
  return mensajeEncriptado;
};

// Definir una función para desencriptar un mensaje usando un código secreto
const desencriptarMensaje = (mensajeEncriptado, codigoSecreto) => {
  // Convertir el mensaje encriptado y el código secreto en arrays de caracteres
  const mensajeEncriptadoArray = mensajeEncriptado.split('');
  const codigoSecretoArray = codigoSecreto.split('');

  // Inicializar el mensaje desencriptado
  let mensajeDesencriptado = '';

  // Iterar sobre el mensaje encriptado
  for (let i = 0; i < mensajeEncriptadoArray.length; i++) {
    // Obtener el carácter actual del mensaje encriptado
    const caracterEncriptado = mensajeEncriptadoArray[i];

    // Obtener el índice del carácter encriptado en el alfabeto (A = 0, Z = 25)
    const indiceCaracterEncriptado = caracterEncriptado.charCodeAt(0) - 'A'.charCodeAt(0);

    // Obtener el índice del código secreto en el alfabeto
    const indiceCodigoSecreto = codigoSecretoArray[i].charCodeAt(0) - 'A'.charCodeAt(0);

    // Restar los índices para obtener el índice desencriptado
    const indiceDesencriptado = (indiceCaracterEncriptado - indiceCodigoSecreto + 26) % 26;

    // Convertir el índice desencriptado en un carácter
    const caracterDesencriptado = String.fromCharCode(indiceDesencriptado + 'A'.charCodeAt(0));

    // Concatenar el carácter desencriptado al mensaje desencriptado
    mensajeDesencriptado += caracterDesencriptado;
  }

  // Devolver el mensaje desencriptado
  return mensajeDesencriptado;
};

// Obtener el mensaje a encriptar del usuario
const mensaje = prompt('Ingrese el mensaje a encriptar:');

// Obtener el código secreto del usuario
const codigoSecreto = prompt('Ingrese el código secreto:');

// Generar un código aleatorio para encriptar el mensaje
const codigoAleatorio = generarCodigoAleatorio(10);

// Encriptar el mensaje usando el código aleatorio
const mensajeEncriptado = encriptarMensaje(mensaje, codigoAleatorio);

// Mostrar el mensaje encriptado al usuario
alert(`Mensaje encriptado: ${mensajeEncriptado}`);

// Desencriptar el mensaje encriptado usando el código aleatorio
const mensajeDesencriptado = desencriptarMensaje(mensajeEncriptado, codigoAleatorio);

// Mostrar el mensaje desencriptado al usuario
alert(`Mensaje desencriptado: ${mensajeDesencriptado}`);
```

Explicación del código:

* La función `generarCodigoAleatorio` genera un código aleatorio de una longitud especificada.
* La función `encriptarMensaje` encripta un mensaje usando un código secreto.
* La función `desencriptarMensaje` desencripta un mensaje encriptado usando un código secreto.
* El código principal solicita al usuario que introduzca un mensaje a encriptar y un código secreto.
* Genera un código aleatorio para encriptar el mensaje.
* Encripta el mensaje usando el código aleatorio.
* Muestra el mensaje encriptado al usuario.
* Desencripta el mensaje encriptado usando el código aleatorio.
* Muestra el mensaje desencriptado al usuario.

Este código es un ejemplo de cómo se puede utilizar la criptografía para encriptar y desencriptar mensajes. La criptografía es una disciplina que se ocupa de la protección de la información mediante el uso de técnicas matemáticas.