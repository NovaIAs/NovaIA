```elixir
# Importar las bibliotecas necesarias
import Plug, Phoenix.Router, Cowboy

# Crear una función para manejar las peticiones HTTP
def handle_request(conn, _params) do
  # Obtener el path de la petición
  path = conn.request_path

  # Comprobar si el path es "/", si lo es, devolver una respuesta de bienvenida
  if path == "/" do
    conn
    |> put_resp_content_type("text/plain")
    |> send_resp(200, "Bienvenidos a mi API")
  end

  # Comprobar si el path es "/users", si lo es, devolver una lista de usuarios
  if path == "/users" do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode(%{"users" => [%{nombre: "Juan", edad: 20}, %{nombre: "María", edad: 25}]}))
  end

  # Si el path no coincide con ninguno de los anteriores, devolver un error 404
  conn
  |> put_resp_content_type("text/plain")
  |> send_resp(404, "Página no encontrada")
end

# Crear un router que mapee los paths a las funciones correspondientes
router = Phoenix.Router.new do
  pipe_through :api

  get "/", handler: &handle_request
  get "/users", handler: &handle_request
end

# Crear un servidor Cowboy que escuche en el puerto 8080
Cowboy.start_http(:http, 8080, [], [{:router, router}])
```

**Explicación:**

* El código importa las bibliotecas necesarias, como Plug, Phoenix.Router y Cowboy.
* La función `handle_request` se utiliza para manejar las peticiones HTTP. Esta función comprueba el path de la petición y devuelve una respuesta en consecuencia.
* El router mapea los paths a las funciones correspondientes. En este caso, el path "/" se mapea a la función `handle_request` y el path "/users" se mapea también a la función `handle_request`.
* El servidor Cowboy escucha en el puerto 8080 y utiliza el router para manejar las peticiones HTTP.

Este código es un ejemplo de un servidor web muy básico escrito en Elixir. El servidor escucha en el puerto 8080 y responde a las peticiones HTTP con diferentes mensajes.