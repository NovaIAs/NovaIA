```
#!/bin/bash

# ### Variables globales ### #

# Ruta del archivo de configuración
CONFIG_FILE="/etc/miconfiguracion.conf"

# ### Funciones ### #

# Función para leer el archivo de configuración
leer_config() {
    # Si no existe el archivo de configuración, se crea uno vacío
    if [ ! -f "$CONFIG_FILE" ]; then
        touch "$CONFIG_FILE"
    fi

    # Se leen los valores del archivo de configuración y se guardan en las variables globales
    while IFS='=' read -r key value; do
        case "$key" in
            "nombre_usuario")
                NOMBRE_USUARIO="$value"
                ;;
            "contrasena")
                CONTRASENA="$value"
                ;;
            "servidor_mysql")
                SERVIDOR_MYSQL="$value"
                ;;
            "base_datos")
                BASE_DATOS="$value"
                ;;
        esac
    done < "$CONFIG_FILE"
}

# Función para conectar a la base de datos MySQL
conectar_mysql() {
    # Se crea una variable con el comando para conectar a la base de datos MySQL
    mysql_cmd="mysql -h $SERVIDOR_MYSQL -u $NOMBRE_USUARIO -p$CONTRASENA $BASE_DATOS"

    # Se ejecuta el comando para conectar a la base de datos MySQL
    $mysql_cmd
}

# Función para crear una tabla en la base de datos MySQL
crear_tabla() {
    # Se crea una variable con el comando para crear la tabla en la base de datos MySQL
    create_table_cmd="CREATE TABLE IF NOT EXISTS usuarios (
        id INT NOT NULL AUTO_INCREMENT,
        nombre VARCHAR(255) NOT NULL,
        apellido VARCHAR(255) NOT NULL,
        PRIMARY KEY (id)
    )"

    # Se ejecuta el comando para crear la tabla en la base de datos MySQL
    conectar_mysql <<< "$create_table_cmd"
}

# Función para insertar datos en la tabla de usuarios
insertar_datos() {
    # Se crea una variable con el comando para insertar datos en la tabla de usuarios
    insert_data_cmd="INSERT INTO usuarios (nombre, apellido) VALUES
        ('Juan', 'Perez'),
        ('Maria', 'Garcia'),
        ('Pedro', 'Rodriguez'),
        ('Ana', 'Lopez'),
        ('Luis', 'Gonzalez')"

    # Se ejecuta el comando para insertar datos en la tabla de usuarios
    conectar_mysql <<< "$insert_data_cmd"
}

# Función para mostrar los datos de la tabla de usuarios
mostrar_datos() {
    # Se crea una variable con el comando para mostrar los datos de la tabla de usuarios
    show_data_cmd="SELECT * FROM usuarios"

    # Se ejecuta el comando para mostrar los datos de la tabla de usuarios
    conectar_mysql <<< "$show_data_cmd"
}

# ### Ejecución del código ### #

# Se llama a la función para leer el archivo de configuración
leer_config

# Se llama a la función para crear la tabla de usuarios
crear_tabla

# Se llama a la función para insertar datos en la tabla de usuarios
insertar_datos

# Se llama a la función para mostrar los datos de la tabla de usuarios
mostrar_datos
```

Explicación del código:

* El código comienza definiendo una serie de variables globales, que se utilizan en todo el código.
* A continuación, se definen una serie de funciones, que se utilizan para realizar tareas específicas.
* La función `leer_config()` lee el archivo de configuración y guarda los valores en las variables globales.
* La función `conectar_mysql()` se utiliza para conectar a la base de datos MySQL.
* La función `crear_tabla()` se utiliza para crear la tabla de usuarios en la base de datos MySQL.
* La función `insertar_datos()` se utiliza para insertar datos en la tabla de usuarios.
* La función `mostrar_datos()` se utiliza para mostrar los datos de la tabla de usuarios.
* Finalmente, se llama a la función `leer_config()` para leer el archivo de configuración, y luego se llaman a las funciones `crear_tabla()`, `insertar_datos()` y `mostrar_datos()` para crear la tabla de usuarios, insertar datos en la tabla y mostrar los datos de la tabla, respectivamente.