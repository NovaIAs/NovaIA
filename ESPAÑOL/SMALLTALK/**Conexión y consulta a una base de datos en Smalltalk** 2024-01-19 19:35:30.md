```smalltalk
**Enlace a una base de datos**

```smalltalk
| conexión |
conexión := DatabaseConnection nueva
  iniciar('localhost', 'miBaseDeDatos', 'miUsuario', 'miContraseña');
```

**Consulta a la base de datos**

```smalltalk
| resultado |
resultado := conexión ejecutarConsulta(
  'SELECT * FROM usuarios WHERE nombre = "Juan"');
```

**Recorrido de los resultados**

```smalltalk
resultado cadaUno(
  [:fila |
    Transcript cr;
    Transcript show(fila nombre);
    Transcript show(fila edad)]);
```

**Inserción de un registro**

```smalltalk
conexión ejecutarActualización(
  'INSERT INTO usuarios (nombre, edad) VALUES ("María", 30)');
```

**Actualización de un registro**

```smalltalk
conexión ejecutarActualización(
  'UPDATE usuarios SET edad = edad + 1 WHERE nombre = "Juan"');
```

**Eliminación de un registro**

```smalltalk
conexión ejecutarActualización(
  'DELETE FROM usuarios WHERE nombre = "María"');
```

**Explicación del código:**

* `conexión := DatabaseConnection nueva` crea una nueva instancia de la clase `DatabaseConnection`.
* `iniciar('localhost', 'miBaseDeDatos', 'miUsuario', 'miContraseña')` inicia la conexión con la base de datos especificando el servidor, el nombre de la base de datos, el usuario y la contraseña.
* `resultado := conexión ejecutarConsulta('SELECT * FROM usuarios WHERE nombre = "Juan"')` ejecuta la consulta a la base de datos y almacena el resultado en la variable `resultado`.
* `resultado cadaUno( [:fila | ... ] )` recorre cada fila del resultado y ejecuta el bloque especificado. En este caso, el bloque muestra el nombre y la edad del usuario.
* `conexión ejecutarActualización('INSERT INTO usuarios (nombre, edad) VALUES ("María", 30)')` ejecuta la consulta para insertar un nuevo registro en la base de datos.
* `conexión ejecutarActualización('UPDATE usuarios SET edad = edad + 1 WHERE nombre = "Juan"')` ejecuta la consulta para actualizar un registro existente en la base de datos.
* `conexión ejecutarActualización('DELETE FROM usuarios WHERE nombre = "María"')` ejecuta la consulta para eliminar un registro de la base de datos.