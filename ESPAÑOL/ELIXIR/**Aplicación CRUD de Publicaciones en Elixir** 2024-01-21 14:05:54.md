```elixir
# Importamos las librerías necesarias
import Ecto, Ecto.Query
import Phoenix.HTML, Phoenix.LiveView.Socket

# Definimos el esquema de la base de datos
defmodule Post do
  use Ecto.Schema
  import Ecto.Changeset

  schema "posts" do
    field :title, :string
    field :body, :string
    field :author, :string
    timestamps()
  end

  def changeset(post, attrs) do
    post
    |> cast(attrs, [:title, :body, :author])
    |> validate_required([:title, :body, :author])
    |> unique_constraint(:title, message: "Ese título ya ha sido utilizado")
  end
end

# Definimos el contexto de la base de datos
defmodule PostRepository do
  alias Post

  def all() do
    Post
    |> Ecto.Query.order_by(desc: :inserted_at)
    |> Repo.all()
  end

  def get_by_id(id) do
    Post
    |> Ecto.Query.get(id)
    |> Repo.one()
  end

  def create(attrs) do
    %Post{}
    |> Post.changeset(attrs)
    |> Repo.insert()
  end

  def update(post, attrs) do
    post
    |> Post.changeset(attrs)
    |> Repo.update()
  end

  def delete(post) do
    Repo.delete(post)
  end
end

# Definimos el controlador de la aplicación
defmodule PostController do
  use Phoenix.Controller

  def index(conn, _params, socket) do
    posts = PostRepository.all()
    render(conn, socket, "index.html", posts: posts)
  end

  def new(conn, _params, socket) do
    changeset = Post.changeset(%Post{})
    render(conn, socket, "new.html", changeset: changeset)
  end

  def create(conn, %{"post" => post_params}, socket) do
    changeset = Post.changeset(%Post{}, post_params)

    case Repo.insert(changeset) do
      {:ok, _post} ->
        conn
        |> redirect(to: Routes.post_path(socket, :index))
        |> put_flash(:info, "Post creado con éxito")

      {:error, changeset} ->
        render(conn, socket, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}, socket) do
    post = PostRepository.get_by_id(id)
    render(conn, socket, "show.html", post: post)
  end

  def edit(conn, %{"id" => id}, socket) do
    post = PostRepository.get_by_id(id)
    changeset = Post.changeset(post)
    render(conn, socket, "edit.html", post: post, changeset: changeset)
  end

  def update(conn, %{"id" => id, "post" => post_params}, socket) do
    post = PostRepository.get_by_id(id)
    changeset = Post.changeset(post, post_params)

    case Repo.update(changeset) do
      {:ok, _post} ->
        conn
        |> redirect(to: Routes.post_path(socket, :index))
        |> put_flash(:info, "Post actualizado con éxito")

      {:error, changeset} ->
        render(conn, socket, "edit.html", post: post, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}, socket) do
    post = PostRepository.get_by_id(id)
    Repo.delete(post)

    conn
    |> redirect(to: Routes.post_path(socket, :index))
    |> put_flash(:info, "Post eliminado con éxito")
  end
end

# Definimos las rutas de la aplicación
defmodule PostRoutes do
  use Phoenix.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_flash
    plug :protect_from_forgery
  end

  scope "/", PostController do
    pipe_through :browser

    get "/", :index
    get "/new", :new
    post "/create", :create
    get "/:id", :show
    get "/:id/edit", :edit
    put "/:id/update", :update
    delete "/:id/delete", :delete
  end
end

# Definimos el módulo principal de la aplicación
defmodule App do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      {Plug.Cowboy, scheme: :http, plug: PostRoutes, options: [port: 8080]},
      {Ecto.Repo, PostRepository}
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
```

Este código define un sencillo CRUD (Crear, Leer, Actualizar y Eliminar) para una aplicación web en Elixir. Se utiliza el framework Phoenix para la gestión de las rutas y los controladores, y Ecto para el acceso a la base de datos.

El esquema de la base de datos define una tabla de publicaciones (posts) con los campos título, cuerpo, autor y marcas de tiempo. El contexto de la base de datos proporciona funciones para crear, leer, actualizar y eliminar publicaciones de la base de datos.

El controlador de la aplicación define las acciones para gestionar las publicaciones, incluyendo la creación, lectura, actualización y eliminación. Las rutas de la aplicación definen la forma en que se accede a estas acciones a través de las solicitudes HTTP.

El módulo principal de la aplicación inicia la aplicación y supervisa los procesos secundarios, incluyendo el servidor web y el repositorio de la base de datos.

Para utilizar esta aplicación, primero debemos instalarla con el comando `mix phoenix.new app_name`, donde `app_name` es el nombre de la aplicación. Luego debemos agregar las dependencias necesarias al archivo `mix.exs` y ejecutar el comando `mix deps.get`.

Finalmente, podemos iniciar la aplicación con el comando `mix run`. La aplicación estará accesible en la dirección `http://localhost:8080`.