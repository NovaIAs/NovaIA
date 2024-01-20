```shell
#!/bin/bash

# Definir variables
menu=(
    "1. Instalar Nginx"
    "2. Instalar MySQL"
    "3. Instalar PHP"
    "4. Instalar WordPress"
    "5. Salir"
)

# Función para mostrar el menú
mostrar_menu() {
    clear
    echo "Menú de instalación"
    for i in ${!menu[*]}; do
        echo "$((i+1)). ${menu[$i]}"
    done
    echo
    read -p "Elige una opción: " opcion
}

# Función para instalar Nginx
instalar_nginx() {
    sudo apt update
    sudo apt install nginx
    sudo systemctl start nginx
    sudo systemctl enable nginx
}

# Función para instalar MySQL
instalar_mysql() {
    sudo apt update
    sudo apt install mysql-server
    sudo systemctl start mysql
    sudo systemctl enable mysql

    # Crear usuario y base de datos
    sudo mysql -u root -p -e "CREATE DATABASE wordpress;"
    sudo mysql -u root -p -e "CREATE USER 'wordpress'@'localhost' IDENTIFIED BY 'contraseña';"
    sudo mysql -u root -p -e "GRANT ALL PRIVILEGES ON wordpress.* TO 'wordpress'@'localhost';"
}

# Función para instalar PHP
instalar_php() {
    sudo apt update
    sudo apt install php php-fpm php-mysql
    sudo systemctl start php7.4-fpm
    sudo systemctl enable php7.4-fpm
}

# Función para instalar WordPress
instalar_wordpress() {
    # Descargar WordPress
    wget https://wordpress.org/latest.tar.gz

    # Extraer WordPress
    tar xzvf latest.tar.gz

    # Copiar WordPress a la ruta de instalación
    sudo cp -r wordpress/* /var/www/html/

    # Configurar WordPress
    sudo chown -R www-data:www-data /var/www/html/
    sudo chmod -R 755 /var/www/html/

    # Crear archivo de configuración de WordPress
    sudo touch /var/www/html/wp-config.php

    # Añadir configuración a wp-config.php
    sudo echo "<?php" >> /var/www/html/wp-config.php
    sudo echo "define('DB_NAME', 'wordpress');" >> /var/www/html/wp-config.php
    sudo echo "define('DB_USER', 'wordpress');" >> /var/www/html/wp-config.php
    sudo echo "define('DB_PASSWORD', 'contraseña');" >> /var/www/html/wp-config.php
    sudo echo "define('DB_HOST', 'localhost');" >> /var/www/html/wp-config.php
    sudo echo "define('DB_CHARSET', 'utf8mb4');" >> /var/www/html/wp-config.php
    sudo echo "define('DB_COLLATE', '');" >> /var/www/html/wp-config.php
    sudo echo "define('AUTH_KEY', 'puedes_generar_una_clave_aleatoria_aquí');" >> /var/www/html/wp-config.php
    sudo echo "define('SECURE_AUTH_KEY', 'puedes_generar_una_clave_aleatoria_aquí');" >> /var/www/html/wp-config.php
    sudo echo "define('LOGGED_IN_KEY', 'puedes_generar_una_clave_aleatoria_aquí');" >> /var/www/html/wp-config.php
    sudo echo "define('NONCE_KEY', 'puedes_generar_una_clave_aleatoria_aquí');" >> /var/www/html/wp-config.php
    sudo echo "define('AUTH_SALT', 'puedes_generar_una_clave_aleatoria_aquí');" >> /var/www/html/wp-config.php
    sudo echo "define('SECURE_AUTH_SALT', 'puedes_generar_una_clave_aleatoria_aquí');" >> /var/www/html/wp-config.php
    sudo echo "define('LOGGED_IN_SALT', 'puedes_generar_una_clave_aleatoria_aquí');" >> /var/www/html/wp-config.php
    sudo echo "define('NONCE_SALT', 'puedes_generar_una_clave_aleatoria_aquí');" >> /var/www/html/wp-config.php
    sudo echo "?>\n" >> /var/www/html/wp-config.php
}

# Función para salir del programa
salir() {
    echo "Saliendo del programa..."
    exit 0
}

# Bucle principal del programa
while true; do
    mostrar_menu

    case $opcion in
        1)
            instalar_nginx
            ;;
        2)
            instalar_mysql
            ;;
        3)
            instalar_php
            ;;
        4)
            instalar_wordpress
            ;;
        5)
            salir
            ;;
        *)
            echo "Opción no válida."
            ;;
    esac

    read -p "Pulsa ENTER para continuar..."
done
```

**Explicación del código:**

* El código comienza definiendo un array con las opciones del menú.
* Luego, se definen cuatro funciones, una para cada opción del menú.
* La función `mostrar_menu()` muestra el menú en pantalla y lee la opción elegida por el usuario.
* Las funciones `instalar_nginx()`, `instalar_mysql()`, `instalar_php()` e `instalar_wordpress()` realizan las tareas necesarias para instalar cada uno de los componentes.
* La función `salir()` sale del programa.
* El código principal del programa es un bucle `while` que muestra el menú y llama a la función correspondiente según la opción elegida por el usuario.

El código está diseñado de manera que sea fácil de entender y modificar, y que pueda ser utilizado para instalar un servidor web completo con Nginx, MySQL, PHP y WordPress.