```
// Función para calcular el factorial de un número
const factorial = (numero) => {
  if (numero === 0) {
    return 1;
  }
  return numero * factorial(numero - 1);
}

// Función para generar números aleatorios entre un rango
const generarNumeroAleatorio = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Función para ordenar un arreglo de números
const ordenarArreglo = (arreglo) => {
  return arreglo.sort((a, b) => a - b);
}

// Función para buscar un elemento en un arreglo
const buscarElemento = (arreglo, elemento) => {
  return arreglo.indexOf(elemento);
}

// Función para crear un objeto con propiedades y valores
const crearObjeto = (propiedades, valores) => {
  return _.zipObject(propiedades, valores);
}

// Función para clonar un objeto
const clonarObjeto = (objeto) => {
  return JSON.parse(JSON.stringify(objeto));
}

// Función para combinar dos objetos
const combinarObjetos = (objeto1, objeto2) => {
  return _.extend(objeto1, objeto2);
}

// Función para obtener las claves de un objeto
const obtenerClaves = (objeto) => {
  return Object.keys(objeto);
}

// Función para obtener los valores de un objeto
const obtenerValores = (objeto) => {
  return Object.values(objeto);
}

// Función para eliminar una propiedad de un objeto
const eliminarPropiedad = (objeto, propiedad) => {
  delete objeto[propiedad];
}

// Función para verificar si un objeto está vacío
const estaVacio = (objeto) => {
  return _.isEmpty(objeto);
}

// Función para convertir un JSON en un objeto
const convertirJSONAObjeto = (json) => {
  return JSON.parse(json);
}

// Función para convertir un objeto en JSON
const convertirObjetoAJSON = (objeto) => {
  return JSON.stringify(objeto);
}

// Función para crear una función con retraso
const crearFuncionConRetraso = (funcion, retraso) => {
  return _.debounce(funcion, retraso);
}

// Función para crear una función que se ejecuta una sola vez
const crearFuncionQueSeEjecutaUnaSolaVez = (funcion) => {
  return _.once(funcion);
}

// Función para generar un UUID
const generarUUID = () => {
  return ([1e7] + -1e3 + -4e3 + -8e3 + -1e11).replace(/[018]/g, c =>
    (c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
  );
}

// Función para obtener la fecha actual en formato ISO
const obtenerFechaActual = () => {
  return new Date().toISOString();
}

// Función para obtener la fecha actual en formato unix
const obtenerFechaActualUnix = () => {
  return new Date().getTime();
}

// Función para convertir una fecha en formato ISO a formato unix
const convertirFechaISOAUnix = (fecha) => {
  return new Date(fecha).getTime();
}

// Función para convertir una fecha en formato unix a formato ISO
const convertirFechaUnixAISO = (fecha) => {
  return new Date(fecha).toISOString();
}

// Función para formatear una fecha en formato ISO
const formatearFechaISO = (fecha, formato) => {
  return moment(fecha).format(formato);
}

// Función para obtener la diferencia entre dos fechas en días
const obtenerDiferenciaEntreFechas = (fecha1, fecha2) => {
  return Math.floor(
    (Date.parse(fecha2) - Date.parse(fecha1)) / (24 * 60 * 60 * 1000)
  );
}

// Función para obtener la diferencia entre dos fechas en horas
const obtenerDiferenciaEntreFechasHoras = (fecha1, fecha2) => {
  return Math.floor(
    (Date.parse(fecha2) - Date.parse(fecha1)) / (60 * 60 * 1000)
  );
}

// Función para obtener la diferencia entre dos fechas en minutos
const obtenerDiferenciaEntreFechasMinutos = (fecha1, fecha2) => {
  return Math.floor(
    (Date.parse(fecha2) - Date.parse(fecha1)) / (60 * 1000)
  );
}

// Función para obtener la diferencia entre dos fechas en segundos
const obtenerDiferenciaEntreFechasSegundos = (fecha1, fecha2) => {
  return Math.floor((Date.parse(fecha2) - Date.parse(fecha1)) / 1000);
}

// Función para obtener la fecha y hora actual en formato ISO
const obtenerFechaYHoraActual = () => {
  return moment().format("YYYY-MM-DD HH:mm:ss");
}

// Función para obtener la fecha y hora actual en formato unix
const obtenerFechaYHoraActualUnix = () => {
  return moment().valueOf