```go
package main

import (
	"bufio"
	"bytes"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"math/big"
	"net/http"
	"os"
	"os/exec"
	"regexp"
	"runtime"
	"strconv"
	"strings"
	"time"

	"github.com/gorilla/mux"
)

// Estructura para almacenar los datos de un usuario
type Usuario struct {
	Nombre string `json:"nombre"`
	Email  string `json:"email"`
	Clave  string `json:"clave"`
}

// Estructura para almacenar los datos de un producto
type Producto struct {
	Nombre     string  `json:"nombre"`
	Precio     float64 `json:"precio"`
	Imagen     string  `json:"imagen"`
	Calificacion float64 `json:"calificacion"`
}

// Estructura para almacenar los datos de un pedido
type Pedido struct {
	ID                  int           `json:"id"`
	Fecha               time.Time     `json:"fecha"`
	Estado              string        `json:"estado"`
	Productos           []Producto    `json:"productos"`
	ImporteTotal        float64       `json:"importe_total"`
	DireccionEntrega    string        `json:"direccion_entrega"`
	TelefonoContacto    string        `json:"telefono_contacto"`
	MetodoDePago        string        `json:"metodo_de_pago"`
	TarjetaDeCredito    string        `json:"tarjeta_de_credito"`
	TitularTarjeta      string        `json:"titular_tarjeta"`
	FechaVencimiento    string        `json:"fecha_vencimiento"`
	CodigoSeguridad    string        `json:"codigo_seguridad"`
	CorreoElectronico   string        `json:"correo_electronico"`
	NombreCompleto      string        `json:"nombre_completo"`
	FechaRegistro      time.Time     `json:"fecha_registro"`
	UltimaSesion        time.Time     `json:"ultima_sesion"`
	Bloqueado           bool          `json:"bloqueado"`
	IntentosFallidos    int           `json:"intentos_fallidos"`
	NumeroFallido       int           `json:"numero_fallido"`
	RazonFallido        string        `json:"razon_fallido"`
	ContadorFallido     int           `json:"contador_fallido"`
	FechaUltimaSesion    time.Time     `json:"fecha_ultima_sesion"`
	FechaUltimaConexion time.Time     `json:"fecha_ultima_conexion"`
	IPUltimaConexion    string        `json:"ip_ultima_conexion"`
	PaisUltimaConexion  string        `json:"pais_ultima_conexion"`
	CiudadUltimaConexion string        `json:"ciudad_ultima_conexion"`
	MotivoUltimaConexion string        `json:"motivo_ultima_conexion"`
	Comentarios          string        `json:"comentarios"`
	Comunicaciones       string        `json:"comunicaciones"`
	Notificaciones       string        `json:"notificaciones"`
	CuentaActiva         bool          `json:"cuenta_activa"`
	Verificado           bool          `json:"verificado"`
	Privacidad           string        `json:"privacidad"`
	Idioma               string        `json:"idioma"`
	ZonaHoraria          string        `json:"zona_horaria"`
	CantidadPuntos       int           `json:"cantidad_puntos"`
	PuntosAcumulados     float64       `json:"puntos_acumulados"`
	PuntosExpirados      float64       `json:"puntos_expirados"`
	PuntosDisponibles    float64       `json:"puntos_disponibles"`
	PuntosUsados         float64       `json:"puntos_usados"`
	PuntosTotales         float64       `json:"puntos_totales"`
	PuntosHechos         []PuntosHecho  `json:"puntos_hechos"`
	PuntosExpirados      []PuntosExpirado `json:"puntos_expirados"`
	PuntosDisponibles    []PuntosDisponible `json:"puntos_disponibles"`
	PuntosUsados         []PuntosUsado  `json:"puntos_usados"`
	PuntosTotales         []PuntosTotal  `json:"puntos_totales"`
}

// Estructura para almacenar los datos de un punto hecho
type PuntosHecho struct {
	IDPedido     int     `json:"id_pedido"`
	FechaHecho    time.Time `json:"fecha_hecho"`
	CantidadPuntos float64 `json:"cantidad_puntos"`
	MotivoHecho    string `json:"motivo_hecho"`
}

// Estructura para almacenar los datos de un punto expirado
type PuntosExpirado struct {
	IDPuntoExpirado     int     `json:"id_punto_expirado"`
	FechaExpiracion    time.Time `json:"fecha_expiracion"`
	CantidadPuntos      float64 `json:"cantidad_puntos"`
	MotivoExpiracion    string `json:"motivo_expiracion"`
}

// Estructura para almacenar los datos de un punto disponible
type PuntosDisponible struct {
	IDPuntoDisponible     int     `json:"id_punto_disponible"`
	FechaDisponible    time.Time `json:"fecha_disponible"`
	CantidadPuntos      float64 `json:"cantidad_puntos"`
	MotivoDisponible    string `json:"motivo_disponible"`
}

// Estructura para almacenar los datos de un punto usado
type PuntosUsado struct {
	IDPuntoUsado     int     `json:"id_punto_usado"`
	FechaUsado    time.Time `json:"fecha_usado"`
	CantidadPuntos      float64 `json:"cantidad_puntos"`
	MotivoUsado    string `json:"motivo_usado"`
}

// Estructura para almacenar los datos de un punto total
type PuntosTotal struct {
	IDPuntoTotal     int     `json:"id_punto_total"`
	FechaTotal    time.Time `json:"fecha_total"`
	CantidadPuntos      float64 `json:"cantidad_puntos"`
	MotivoTotal    string `json:"motivo_total"`
}

// Estructura para almacenar los datos de una dirección
type Direccion struct {
	Calle     string `json:"calle"`
	Numero    string `json:"numero"`
	Piso      string `json:"piso"`
	Localidad string `json:"localidad"`
	Provincia string `json:"provincia"`
	Pais      string `json:"pais"`
	CodigoPostal string `json:"codigo_postal"`
}

// Estructura para almacenar los datos de una tarjeta de crédito
type TarjetaDeCredito struct {
	NumeroDeTarjeta string `json:"numero_de_tarjeta"`
	Titular        string `json:"titular"`
	FechaVencimiento string `json:"fecha_vencimiento"`
	CodigoSeguridad string `json:"codigo_seguridad"`
}

// Estructura para almacenar los datos de una sesión
type Sesion struct {
	IDUsuario int       `json:"id_usuario"`
	Token     string    `json:"token"`
	FechaInicio time.Time `json:"fecha_inicio"`
	FechaFin   time.Time `json:"fecha_fin"`
	IP          string    `json:"ip"`
	Pais        string    `json:"pais"`
	Ciudad      string    `json:"ciudad"`
	Motivo       string    `json:"motivo"`
}

// Estructura para almacenar los datos de una notificación
type Notificacion struct {
	IDNotificacion  int       `json:"id_notificacion"`
	IDUsuario       int       `json:"id_usuario"`
	TipoNotificacion string    `json:"tipo_notificacion"`
	Asunto          string    `json:"asunto"`
	Mensaje         string    `json:"mensaje"`
	FechaNotificacion time.Time `json:"fecha_notificacion"`
	EstadoNotificacion string    `json:"estado_notificacion"`
}

// Estructura para almacenar los datos de un mensaje
type Mensaje struct {
	IDMensaje int       `json:"id_mensaje"`
	IDUsuario int       `json:"id_usuario"`
	TipoMensaje string    `json:"tipo_mensaje"`
	Asunto      string    `json:"asunto"`
	Mensaje     string    `json:"mensaje"`
	FechaMensaje time.Time `json:"fecha_mensaje"`
	EstadoMensaje string    `json:"estado_mensaje"`
}

// Estructura para almacenar los datos de un comentario
type Comentario struct {
	IDComentario int       `json:"id_comentario"`
	IDUsuario    int       `json:"id_usuario"`
	TipoComentario string    `json:"tipo_comentario"`
	Asunto       string    `json:"asunto"`
	Mensaje      string    `json:"mensaje"`
	FechaComentario time.Time `json:"fecha_comentario"`
	EstadoComentario string    `json:"estado_comentario"`
}

// Función principal
func main() {
	// Inicializar el servidor HTTP
	r := mux.NewRouter()

	// Definir la ruta para registrar un nuevo usuario
	r.HandleFunc("/usuarios", registrarUsuario).Methods("POST")

	// Definir la ruta para iniciar sesión
	r.HandleFunc("/usuarios/login", iniciarSesion).Methods("POST")

	// Definir la ruta para obtener el perfil de un usuario
	r.HandleFunc("/usuarios/{id}", obtenerPerfilUsuario).Methods("GET")

	// Definir la ruta para actualizar el perfil de un usuario
	r.HandleFunc("/usuarios/{id}", actualizarPerfilUsuario).Methods("PUT")

	// Definir la ruta para eliminar un usuario
	r.HandleFunc("/usuarios/{id}", eliminarUsuario).Methods("DELETE")

	// Definir la ruta para crear un nuevo producto
	r.HandleFunc("/productos", crearProducto).Methods("POST")

	// Definir la ruta para obtener un producto
	r.HandleFunc("/productos/{id}", obtenerProducto).Methods("GET")

	// Definir la ruta para actualizar un producto
	r.HandleFunc("/productos/{id}", actualizarProducto).Methods("PUT")

	// Definir la ruta para eliminar un producto
	r.HandleFunc("/productos/{id}", eliminarProducto).Methods("DELETE")

	// Definir la ruta para crear un nuevo pedido
	r.HandleFunc("/pedidos", crearPedido).Methods("POST")

	// Definir la ruta para obtener un pedido
	r.HandleFunc("/pedidos/{id}", obtenerPedido).Methods("GET")

	// Definir la ruta para actualizar un pedido
	r.HandleFunc("/pedidos/{id}", actualizarPedido).Methods("PUT")

	// Definir la ruta para eliminar un pedido
	r.HandleFunc("/pedidos/{id}", eliminarPedido).Methods("DELETE")

	// Definir la ruta para obtener el historial de pedidos de un usuario
	r.HandleFunc("/usuarios/{id}/pedidos", obtenerHistorialPedidosUsuario).Methods("GET")

	// Definir la ruta para crear una nueva dirección
	r.HandleFunc("/direcciones", crearDireccion).Methods("POST")

	// Definir la ruta para obtener una dirección
	r.HandleFunc("/direcciones/{id}", obtenerDireccion).Methods("GET")

	// Definir la ruta para actualizar una dirección
	r.HandleFunc("/direcciones/{id}", actualizarDireccion).Methods("PUT")

	// Definir la ruta para eliminar una dirección
	r.HandleFunc("/direcciones/{id}", eliminarDireccion).Methods("DELETE")

	// Definir la ruta para crear una nueva tarjeta de crédito
	r.HandleFunc("/tarjetas_de_credito", crearTarjetaDeCredito).Methods("POST")

	// Definir la ruta para obtener una tarjeta de crédito
	r.HandleFunc("/tarjetas_de_credito/{id}", obtenerTarjetaDeCredito).Methods("GET")

	// Definir la ruta para actualizar una tarjeta de crédito
	r.HandleFunc("/tarjetas_de_credito/{id}", actualizarTarjetaDeCredito).Methods("PUT")

	// Definir la ruta para eliminar una tarjeta de crédito
	r.HandleFunc("/tarjetas_de_credito/{id}", eliminarTarjetaDeCredito).Methods("DELETE")

	// Definir la ruta para crear una nueva sesión
	r.HandleFunc("/sesiones", crearSesion).Methods("POST")

	// Definir la ruta para obtener una sesión
	r.HandleFunc("/sesiones/{id}", obtenerSesion).Methods("GET")

	// Definir la ruta para actualizar una sesión
	r.HandleFunc("/sesiones/{id}", actualizarSesion).Methods("PUT")

	// Definir la ruta para eliminar una sesión
	r.HandleFunc("/sesiones/{id}", eliminarSesion).Methods("DELETE")

	// Definir la ruta para crear una nueva notificación
	r.HandleFunc("/notificaciones", crearNotificacion).Methods("POST")

	// Definir la ruta para obtener una notificación
	r.HandleFunc("/notificaciones/{id}", obtenerNotificacion).Methods("GET")

	// Definir la ruta para actualizar una notificación
	r.HandleFunc("/notificaciones/{id}", actualizarNotificacion).Methods("PUT")

	// Definir la ruta para eliminar una notificación
	r.HandleFunc("/notificaciones/{id}", eliminarNotificacion).Methods("DELETE")

	// Definir la ruta para crear un nuevo mensaje
	r.HandleFunc("/mensajes", crearMensaje).Methods("POST")

	// Definir la ruta para obtener un mensaje
	r.HandleFunc("/mensajes/{id}", obtenerMensaje).Methods("GET")

	// Definir la ruta para actualizar un mensaje
	r.HandleFunc("/mensajes/{id}", actualizarMensaje).Methods("PUT")

	// Definir la ruta para eliminar un mensaje
	r.HandleFunc("/mensajes/{id}", eliminarMensaje).Methods("DELETE")

	// Definir la ruta para crear un nuevo comentario
	r.HandleFunc("/comentarios", crearComentario).Methods("POST")

	// Definir la ruta para obtener un comentario
	r.HandleFunc("/comentarios/{id}", obtenerComentario).Methods("GET")

	// Definir la ruta para actualizar un comentario
	r.HandleFunc("/comentarios/{id}", actualizarComentario).Methods("PUT")