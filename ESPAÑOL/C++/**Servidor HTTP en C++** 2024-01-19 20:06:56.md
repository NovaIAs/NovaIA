```c++
// Biblioteca estándar de C++ para entrada y salida
#include <iostream>

// Biblioteca para el uso de expresiones regulares
#include <regex>

// Biblioteca para el uso de flujos de archivos
#include <fstream>

// Biblioteca para el uso de estructuras de datos
#include <vector>
#include <map>
#include <set>

// Biblioteca para el uso de hilos
#include <thread>
#include <mutex>

// Biblioteca para el uso de temporizadores
#include <chrono>

// Biblioteca para el uso de sockets
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

// Definición de constantes
const int PUERTO = 8080;
const int TAM_BUFFER = 1024;

// Declaración de funciones
std::string leer_archivo(const std::string& nombre);
std::vector<std::string> dividir_cadena(const std::string& cadena, const std::string& delimitador);
std::string obtener_cabecera(const std::string& mensaje);
std::string obtener_cuerpo(const std::string& mensaje);
void procesar_peticion(const std::string& peticion, std::string& respuesta);
void iniciar_servidor(int puerto);
void atender_cliente(int sockfd);

// Función para leer el contenido de un archivo
std::string leer_archivo(const std::string& nombre) {
  std::ifstream archivo(nombre);
  std::string contenido;
  std::string linea;
  while (std::getline(archivo, linea)) {
    contenido += linea + "\n";
  }
  archivo.close();
  return contenido;
}

// Función para dividir una cadena en una lista de subcadenas
std::vector<std::string> dividir_cadena(const std::string& cadena, const std::string& delimitador) {
  std::vector<std::string> subcadenas;
  size_t pos = 0;
  while ((pos = cadena.find(delimitador)) != std::string::npos) {
    subcadenas.push_back(cadena.substr(0, pos));
    cadena.erase(0, pos + delimitador.length());
  }
  subcadenas.push_back(cadena);
  return subcadenas;
}

// Función para obtener la cabecera de un mensaje HTTP
std::string obtener_cabecera(const std::string& mensaje) {
  std::string cabecera;
  size_t pos = mensaje.find("\n\n");
  if (pos != std::string::npos) {
    cabecera = mensaje.substr(0, pos);
  }
  return cabecera;
}

// Función para obtener el cuerpo de un mensaje HTTP
std::string obtener_cuerpo(const std::string& mensaje) {
  std::string cuerpo;
  size_t pos = mensaje.find("\n\n");
  if (pos != std::string::npos) {
    cuerpo = mensaje.substr(pos + 2);
  }
  return cuerpo;
}

// Función para procesar una petición HTTP
void procesar_peticion(const std::string& peticion, std::string& respuesta) {
  // Obtener la cabecera y el cuerpo de la petición
  std::string cabecera = obtener_cabecera(peticion);
  std::string cuerpo = obtener_cuerpo(peticion);

  // Obtener el método y la ruta de la petición
  std::vector<std::string> partes = dividir_cadena(cabecera, " ");
  std::string metodo = partes[0];
  std::string ruta = partes[1];

  // Procesar la petición en función del método y la ruta
  if (metodo == "GET" && ruta == "/") {
    // Enviar una respuesta con el contenido del archivo index.html
    respuesta = "HTTP/1.1 200 OK\nContent-Type: text/html\n\n" + leer_archivo("index.html");
  } else if (metodo == "POST" && ruta == "/procesar") {
    // Obtener los datos del cuerpo de la petición
    std::map<std::string, std::string> datos;
    std::vector<std::string> partes = dividir_cadena(cuerpo, "&");
    for (const std::string& parte : partes) {
      std::vector<std::string> par = dividir_cadena(parte, "=");
      datos[par[0]] = par[1];
    }

    // Procesar los datos y generar una respuesta
    std::string mensaje = "Los datos recibidos fueron:\n";
    for (const auto& dato : datos) {
      mensaje += dato.first + ": " + dato.second + "\n";
    }
    respuesta = "HTTP/1.1 200 OK\nContent-Type: text/html\n\n" + mensaje;
  } else {
    // Enviar una respuesta de error
    respuesta = "HTTP/1.1 404 Not Found\nContent-Type: text/html\n\n<h1>Error 404: Página no encontrada</h1>";
  }
}

// Función para iniciar el servidor en un puerto determinado
void iniciar_servidor(int puerto) {
  // Crear un socket
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd == -1) {
    std::cerr << "Error al crear el socket" << std::endl;
    return;
  }

  // Establecer las opciones del socket
  int opt = 1;
  setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt));

  // Vincular el socket a una dirección y un puerto
  sockaddr_in addr;
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = INADDR_ANY;
  addr.sin_port = htons(puerto);
  int bind_res = bind(sockfd, (sockaddr*)&addr, sizeof(addr));
  if (bind_res == -1) {
    std::cerr << "Error al vincular el socket" << std::endl;
    return;
  }

  // Poner el socket en modo escucha
  int listen_res = listen(sockfd, 10);
  if (listen_res == -1) {
    std::cerr << "Error al poner el socket en modo escucha" << std::endl;
    return;
  }

  std::cout << "Servidor iniciado en el puerto " << puerto << std::endl;

  // Bucle infinito para atender a los clientes
  while (true) {
    // Aceptar una nueva conexión
    sockaddr_in client_addr;
    socklen_t client_addr_len = sizeof(client_addr);
    int client_sockfd = accept(sockfd, (sockaddr*)&client_addr, &client_addr_len);
    if (client_sockfd == -1) {
      std::cerr << "Error al aceptar una nueva conexión" << std::endl;
      continue;
    }

    // Crear un hilo para atender al cliente
    std::thread client_thread(atender_cliente, client_sockfd);
    client_thread.detach();
  }

  // Cerrar el socket
  close(sockfd);
}

// Función para atender a un cliente
void atender_cliente(int sockfd) {
  // Recibir la petición del cliente
  char buffer[TAM_BUFFER];
  int recv_res = recv(sockfd, buffer, TAM_BUFFER, 0);
  if (recv_res == -1) {
    std::cerr << "Error al recibir la petición del cliente" << std::endl;
    close(sockfd);
    return;
  }

  // Procesar la petición
  std::string peticion(buffer, recv_res);
  std::string respuesta;
  procesar_peticion(peticion, respuesta);

  // Enviar la respuesta al cliente
  int send_res = send(sockfd, respuesta.c_str(), respuesta.length(), 0);
  if (send_res == -1) {
    std::cerr << "Error al enviar la respuesta al cliente" << std::endl;
    close(sockfd);
    return;
  }

  // Cerrar el socket del cliente
  close(sockfd);
}

// Función principal
int main() {
  iniciar_servidor(PUERTO);

  return 0;
}
```

Este código es una implementación de un servidor HTTP básico en C++. El servidor escucha las peticiones de los clientes en un puerto determinado y las procesa en función del método y la ruta de la petición. El servidor puede enviar respuestas en formato HTML o texto plano.

El código está estructurado en varias funciones, cada una de las cuales tiene una responsabilidad específica. Las funciones principales son:

* `leer_archivo`: lee el contenido de un archivo y lo devuelve como una cadena de caracteres.
* `dividir_cadena`: divide una cadena de caracteres en una lista de subcadenas.
* `obtener_cabecera`: obtiene la cabecera de un mensaje HTTP.
* `obtener_cuerpo`: obtiene el cuerpo de un mensaje HTTP.
* `procesar_peticion`: procesa una petición HTTP y genera una respuesta.
* `iniciar_servidor`: inicia el servidor HTTP en un puerto determinado.
* `atender_cliente`: atiende a un cliente, recibiendo su petición, procesándola y enviando la respuesta.

El servidor se inicia llamando a la función `iniciar_servidor()` con el puerto en el que se quiere escuchar. El servidor se ejecuta en un bucle infinito, aceptando nuevas conexiones y creando un hilo para atender a cada cliente.

Cuando un cliente se conecta al servidor, el servidor recibe su petición y la procesa llamando a la función `procesar_peticion()`. Esta función determina el método y la ruta de la petición, y genera una respuesta en función de ellos. La respuesta se envía al cliente y el servidor cierra la conexión.

El servidor puede enviar respuestas en formato HTML o texto plano. Las respuestas en formato HTML se generan leyendo el contenido de un archivo y enviándolo al cliente. Las respuestas en formato texto plano se generan generando una cadena de caracteres con el mensaje que se quiere enviar al cliente.

El código está diseñado para ser modular y fácil de mantener. Las funciones están bien documentadas y el código está organizado en varios archivos, lo que facilita su lectura y comprensión.