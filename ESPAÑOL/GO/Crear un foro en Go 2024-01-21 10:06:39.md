```go
// Crear un programa que permita a los usuarios registrarse, iniciar sesión y publicar mensajes en un foro.

// Definir las estructuras de Usuario y Mensaje
type Usuario struct {
	id          int
	nombre      string
	correo      string
	contraseña  string
	mensajes    []*Mensaje
}

type Mensaje struct {
	id          int
	autor       *Usuario
	contenido    string
	fechaPublicacion string
}

// Definir la función para registrar un nuevo usuario
func registrarUsuario(nombre, correo, contraseña string) error {
	// Comprobar si el usuario ya existe
	if existeUsuario(nombre, correo) {
		return errors.New("El usuario ya existe")
	}

	// Crear un nuevo usuario
	usuario := &Usuario{
		id:          len(usuarios) + 1,
		nombre:      nombre,
		correo:      correo,
		contraseña:  contraseña,
		mensajes:    []*Mensaje{},
	}

	// Añadir el nuevo usuario a la lista de usuarios
	usuarios = append(usuarios, usuario)

	// Devolver un error si el usuario no pudo ser registrado
	return nil
}

// Definir la función para iniciar sesión de un usuario
func iniciarSesion(nombre, contraseña string) (*Usuario, error) {
	// Buscar el usuario con el nombre y contraseña especificados
	for _, usuario := range usuarios {
		if usuario.nombre == nombre && usuario.contraseña == contraseña {
			return usuario, nil
		}
	}

	// Devolver un error si el usuario no pudo ser encontrado
	return nil, errors.New("El usuario no existe o la contraseña es incorrecta")
}

// Definir la función para publicar un mensaje en el foro
func publicarMensaje(autor *Usuario, contenido string) error {
	// Crear un nuevo mensaje
	mensaje := &Mensaje{
		id:          len(mensajes) + 1,
		autor:       autor,
		contenido:    contenido,
		fechaPublicacion: time.Now().Format("2006-01-02 15:04:05"),
	}

	// Añadir el nuevo mensaje a la lista de mensajes
	mensajes = append(mensajes, mensaje)

	// Devolver un error si el mensaje no pudo ser publicado
	return nil
}

// Definir la función para mostrar los mensajes del foro
func mostrarMensajes() {
	for _, mensaje := range mensajes {
		fmt.Println(mensaje)
	}
}

// Definir la función para buscar un mensaje por su ID
func buscarMensajePorID(id int) (*Mensaje, error) {
	for