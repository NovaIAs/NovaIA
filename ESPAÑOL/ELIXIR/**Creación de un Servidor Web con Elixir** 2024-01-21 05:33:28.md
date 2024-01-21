```elixir
# Importamos la biblioteca "http" que nos permitirá crear un servidor web.
import Plug.Conn

# Se crea la función "hello_world" que se encarga de procesar las peticiones HTTP entrantes.
def hello_world(%Conn{method: "GET"} = conn) do
  # Creamos una respuesta HTTP con el código de estado 200 (OK) y el mensaje "Hola, mundo!".
  conn
  |> Plug.Conn.put_resp_content_type("text/plain")
  |> Plug.Conn.send_resp(200, "Hola, mundo!")
end

# Se crea la función "handle_request" que se utiliza como middleware para procesar todas las peticiones HTTP entrantes.
def handle_request(conn, _opts) do
  # Si la petición HTTP es para la ruta "/", se invoca la función "hello_world".
  if conn.request_path == "/" do
    hello_world(conn)
  # Si la petición HTTP es para cualquier otra ruta, se envía una respuesta HTTP con el código de estado 404 (Not Found).
  else
    conn
    |> Plug.Conn.put_resp_content_type("text/plain")
    |> Plug.Conn.send_resp(404, "Página no encontrada")
  end
end

# Se crea un nuevo servidor web utilizando la función "Plug.Cowboy.start_http" y se le pasa como argumento la función "handle_request".
Plug.Cowboy.start_http(handle_request, [], port: 8080)

# Por último, se muestra un mensaje en la consola para indicar que el servidor web está escuchando en el puerto 8080.
IO.puts "Servidor web escuchando en el puerto 8080"
```

Explicación:

* El código anterior crea un servidor web sencillo en Elixir utilizando la biblioteca "Plug".
* La función "hello_world" es un controlador que se encarga de procesar las peticiones HTTP entrantes y devolver una respuesta HTTP.
* La función "handle_request" es un middleware que se utiliza para procesar todas las peticiones HTTP entrantes y dirigirlas al controlador adecuado.
* La función "Plug.Cowboy.start_http" se utiliza para crear un nuevo servidor web.
* El código "IO.puts" se utiliza para mostrar un mensaje en la consola.