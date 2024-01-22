```shell
#!/bin/bash

# Definir variables

# Archivos de configuración
CONFIG_FILE_PATH=/etc/my-app/config.ini
CONFIG_FILE_BACKUP_PATH=/etc/my-app/config.ini.bak

# Directorios de datos
DATA_DIR_PATH=/var/lib/my-app/data
DATA_DIR_BACKUP_PATH=/var/lib/my-app/data.bak

# Usuarios y grupos
USER_APP_NAME=my-app-user
GROUP_APP_NAME=my-app-group

# Servicios
SERVICE_NAME=my-app-service
SERVICE_DESCRIPTION="Mi aplicación de ejemplo"

# Funciones

# Función para crear el usuario y el grupo de la aplicación
create_user_and_group() {
  # Crear el usuario
  useradd -m -s /bin/bash $USER_APP_NAME

  # Crear el grupo
  groupadd $GROUP_APP_NAME

  # Añadir el usuario al grupo
  usermod -aG $GROUP_APP_NAME $USER_APP_NAME
}

# Función para instalar dependencias
install_dependencies() {
  # Actualizar el índice de paquetes
  apt update

  # Instalar las dependencias
  apt install -y apache2 mysql-server php

  # Habilitar el módulo PHP para Apache
  a2enmod php7.4
}

# Función para configurar la aplicación
configure_application() {
  # Copiar el archivo de configuración
  cp $CONFIG_FILE_PATH $CONFIG_FILE_BACKUP_PATH

  # Editar el archivo de configuración
  sed -i "s/database_host = localhost/database_host = 127.0.0.1/g" $CONFIG_FILE_PATH
  sed -i "s/database_user = root/database_user = my-app-user/g" $CONFIG_FILE_PATH
  sed -i "s/database_password = password/database_password = my-app-password/g" $CONFIG_FILE_PATH
}

# Función para crear el servicio
create_service() {
  # Crear el archivo de servicio
  touch /etc/systemd/system/$SERVICE_NAME.service

  # Editar el archivo de servicio
  echo "[Unit]" > /etc/systemd/system/$SERVICE_NAME.service
  echo "Description=$SERVICE_DESCRIPTION" >> /etc/systemd/system/$SERVICE_NAME.service
  echo "After=network.target" >> /etc/systemd/system/$SERVICE_NAME.service

  echo "[Service]" >> /etc/systemd/system/$SERVICE_NAME.service
  echo "Type=simple" >> /etc/systemd/system/$SERVICE_NAME.service
  echo "ExecStart=/usr/bin/php /var/www/html/index.php" >> /etc/systemd/system/$SERVICE_NAME.service
  echo "User=$USER_APP_NAME" >> /etc/systemd/system/$SERVICE_NAME.service
  echo "Group=$GROUP_APP_NAME" >> /etc/systemd/system/$SERVICE_NAME.service

  echo "[Install]" >> /etc/systemd/system/$SERVICE_NAME.service
  echo "WantedBy=multi-user.target" >> /etc/systemd/system/$SERVICE_NAME.service

  # Habilitar el servicio
  systemctl enable $SERVICE_NAME.service
}

# Función principal

main() {
  # Crear el usuario y el grupo de la aplicación
  create_user_and_group

  # Instalar dependencias
  install_dependencies

  # Configurar la aplicación
  configure_application

  # Crear el servicio
  create_service

  # Reiniciar el servicio
  systemctl restart $SERVICE_NAME.service

  # Imprimir mensaje de éxito
  echo "La aplicación se ha instalado correctamente."
}

# Llamar a la función principal

main
```

Explicación del código:

* El código está escrito en el lenguaje de programación SHELL.
* El código está dividido en funciones, lo que lo hace más fácil de entender y mantener.
* El código utiliza variables para almacenar la configuración, lo que lo hace más fácil de cambiar.
* El código utiliza el comando `sed` para editar el archivo de configuración.
* El código utiliza el comando `systemctl` para crear y habilitar el servicio.
* El código utiliza el comando `echo` para imprimir mensajes de estado.
* El código llama a la función `main` para ejecutar el script.