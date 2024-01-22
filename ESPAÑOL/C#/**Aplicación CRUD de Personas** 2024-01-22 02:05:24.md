```c#
// Modelo de datos
public class Persona
{
    public int Id { get; set; }
    public string Nombre { get; set; }
    public string Apellido { get; set; }
    public int Edad { get; set; }
}

// Repositorio de datos
public interface IRepositorioPersona
{
    Persona ObtenerPorId(int id);
    List<Persona> ObtenerTodos();
    void Crear(Persona persona);
    void Actualizar(Persona persona);
    void Eliminar(int id);
}

// Repositorio de datos en memoria
public class RepositorioPersonaEnMemoria : IRepositorioPersona
{
    private List<Persona> personas = new List<Persona>();

    public Persona ObtenerPorId(int id)
    {
        return personas.FirstOrDefault(p => p.Id == id);
    }

    public List<Persona> ObtenerTodos()
    {
        return personas;
    }

    public void Crear(Persona persona)
    {
        personas.Add(persona);
    }

    public void Actualizar(Persona persona)
    {
        var personaExistente = personas.FirstOrDefault(p => p.Id == persona.Id);
        if (personaExistente != null)
        {
            personaExistente.Nombre = persona.Nombre;
            personaExistente.Apellido = persona.Apellido;
            personaExistente.Edad = persona.Edad;
        }
    }

    public void Eliminar(int id)
    {
        var personaExistente = personas.FirstOrDefault(p => p.Id == id);
        if (personaExistente != null)
        {
            personas.Remove(personaExistente);
        }
    }
}

// Servicio de lógica de negocio
public class ServicioPersona
{
    private IRepositorioPersona repositorioPersona;

    public ServicioPersona(IRepositorioPersona repositorioPersona)
    {
        this.repositorioPersona = repositorioPersona;
    }

    public Persona ObtenerPorId(int id)
    {
        return repositorioPersona.ObtenerPorId(id);
    }

    public List<Persona> ObtenerTodos()
    {
        return repositorioPersona.ObtenerTodos();
    }

    public void Crear(Persona persona)
    {
        repositorioPersona.Crear(persona);
    }

    public void Actualizar(Persona persona)
    {
        repositorioPersona.Actualizar(persona);
    }

    public void Eliminar(int id)
    {
        repositorioPersona.Eliminar(id);
    }
}

// Controlador web (MVC)
public class PersonaController : Controller
{
    private ServicioPersona servicioPersona;

    public PersonaController(ServicioPersona servicioPersona)
    {
        this.servicioPersona = servicioPersona;
    }

    public ActionResult Index()
    {
        var personas = servicioPersona.ObtenerTodos();
        return View(personas);
    }

    public ActionResult Details(int id)
    {
        var persona = servicioPersona.ObtenerPorId(id);
        return View(persona);
    }

    public ActionResult Create()
    {
        return View();
    }

    [HttpPost]
    public ActionResult Create(Persona persona)
    {
        if (ModelState.IsValid)
        {
            servicioPersona.Crear(persona);
            return RedirectToAction("Index");
        }

        return View(persona);
    }

    public ActionResult Edit(int id)
    {
        var persona = servicioPersona.ObtenerPorId(id);
        return View(persona);
    }

    [HttpPost]
    public ActionResult Edit(Persona persona)
    {
        if (ModelState.IsValid)
        {
            servicioPersona.Actualizar(persona);
            return RedirectToAction("Index");
        }

        return View(persona);
    }

    public ActionResult Delete(int id)
    {
        var persona = servicioPersona.ObtenerPorId(id);
        return View(persona);
    }

    [HttpPost]
    public ActionResult Delete(Persona persona)
    {
        servicioPersona.Eliminar(persona.Id);
        return RedirectToAction("Index");
    }
}

// Vistas (MVC)
@model List<Persona>
@{
    ViewBag.Title = "Personas";
}

<h1>Personas</h1>

<table class="table table-striped">
    <thead>
        <tr>
            <th>Id</th>
            <th>Nombre</th>
            <th>Apellido</th>
            <th>Edad</th>
        </tr>
    </thead>
    <tbody>
        @foreach (var persona in Model)
        {
            <tr>
                <td>@persona.Id</td>
                <td>@persona.Nombre</td>
                <td>@persona.Apellido</td>
                <td>@persona.Edad</td>
            </tr>
        }
    </tbody>
</table>

@model Persona
@{
    ViewBag.Title = "Detalles de la Persona";
}

<h1>Detalles de la Persona</h1>

<dl class="dl-horizontal">
    <dt>Id</dt>
    <dd>@Model.Id</dd>
    <dt>Nombre</dt>
    <dd>@Model.Nombre</dd>
    <dt>Apellido</dt>
    <dd>@Model.Apellido</dd>
    <dt>Edad</dt>
    <dd>@Model.Edad</dd>
</dl>

@model Persona
@{
    ViewBag.Title = "Crear Persona";
}

<h1>Crear Persona</h1>

@using (Html.BeginForm())
{
    <div class="form-group">
        @Html.LabelFor(m => m.Nombre)
        @Html.TextBoxFor(m => m.Nombre, new { @class = "form-control" })
    </div>
    <div class="form-group">
        @Html.LabelFor(m => m.Apellido)
        @Html.TextBoxFor(m => m.Apellido, new { @class = "form-control" })
    </div>
    <div class="form-group">
        @Html.LabelFor(m => m.Edad)
        @Html.TextBoxFor(m => m.Edad, new { @class = "form-control" })
    </div>
    <input type="submit" value="Crear" class="btn btn-primary" />
}

@model Persona
@{
    ViewBag.Title = "Editar Persona";
}

<h1>Editar Persona</h1>

@using (Html.BeginForm())
{
    <div class="form-group">
        @Html.HiddenFor(m => m.Id)
        @Html.LabelFor(m => m.Nombre)
        @Html.TextBoxFor(m => m.Nombre, new { @class = "form-control" })
    </div>
    <div class="form-group">
        @Html.LabelFor(m => m.Apellido)
        @Html.TextBoxFor(m => m.Apellido, new { @class = "form-control" })
    </div>
    <div class="form-group">
        @Html.LabelFor(m => m.Edad)
        @Html.TextBoxFor(m => m.Edad, new { @class = "form-control" })
    </div>
    <input type="submit" value="Guardar" class="btn btn-primary" />
}

@model Persona
@{
    ViewBag.Title = "Eliminar Persona";
}

<h1>Eliminar Persona</h1>

<dl class="dl-horizontal">
    <dt>Id</dt>
    <dd>@Model.Id</dd>
    <dt>Nombre</dt>
    <dd>@Model.Nombre</dd>
    <dt>Apellido</dt>
    <dd>@Model.Apellido</dd>
    <dt>Edad</dt>
    <dd>@Model.Edad</dd>
</dl>

<form asp-action="Delete" asp-route-id="@Model.Id" method="post">
    <input type="hidden" asp-for="Id" />
    <input type="submit" value="Eliminar" class="btn btn-danger" />
</form>
```

Explicación:

* Este código es una aplicación web simple que permite gestionar una lista de personas.
* El modelo de datos `Persona` contiene información básica sobre una persona: `Id`, `Nombre`, `Apellido` y `Edad`.
* El repositorio de datos `IRepositorioPersona` define las operaciones CRUD (Crear, Leer, Actualizar y Eliminar).
* El repositorio de datos en memoria `RepositorioPersonaEnMemoria` implementa `IRepositorioPersona` y almacena los datos en memoria.
* El servicio de lógica de negocio `ServicioPersona` utiliza el repositorio de datos `IRepositorioPersona` para realizar las operaciones CRUD.
* El controlador web `PersonaController` es el punto de entrada de la aplicación web y maneja las solicitudes HTTP.
* Las vistas muestran la interfaz de usuario de la aplicación web.

Este código es complejo y difícil de repetir porque combina varias tecnologías y conceptos diferentes, como:

* Programación orientada a objetos
* Patrón de repositorio
* Patrón de servicio
* Modelo-Vista-Controlador (MVC)
* HTML
* CSS
* JavaScript