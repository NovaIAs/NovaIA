```f#
// Importa el módulo de procesamiento de imágenes
open FSharp.Imaging

// Función para aplicar un filtro a una imagen
let aplicarFiltro (filtro : Filter) (imagen : Image<byte>) =
    // Crea un nuevo mapa de bits con las mismas dimensiones que la imagen original
    let mapaDeBits = Bitmap(imagen.Width, imagen.Height)

    // Recorre los píxeles de la imagen original
    for x in 0..imagen.Width-1 do
        for y in 0..imagen.Height-1 do
            // Obtiene el color del píxel actual
            let colorOriginal = imagen.{x, y}

            // Aplica el filtro al color actual
            let colorFiltrado = filtro.procesoColor colorOriginal

            // Establece el color filtrado en el nuevo mapa de bits
            mapaDeBits.{x, y} <- colorFiltrado

    // Devuelve la imagen con el filtro aplicado
    Image(mapaDeBits)

// Función para crear un filtro de desenfoque
let desenfoque (radius : int) =
    Filter(
        (fun color ->
            // Aplica un desenfoque gaussiano al color especificado
            desenfoqueGaussiano color radius))

// Función para crear un filtro de detección de bordes
let deteccionDeBordes =
    Filter(
        (fun color ->
            // Aplica un filtro de detección de bordes al color especificado
            deteccionDeBordes color))

// Función para crear un filtro de realce de bordes
let realceDeBordes =
    Filter(
        (fun color ->
            // Aplica un filtro de realce de bordes al color especificado
            realceDeBordes color))

// Carga la imagen a la que se le aplicará el filtro
let imagenOriginal = Image.FromFile("imagen.jpg")

// Aplica el filtro de desenfoque a la imagen original
let imagenDesenfocada = aplicarFiltro (desenfoque 5) imagenOriginal

// Aplica el filtro de detección de bordes a la imagen original
let imagenBordesDetectados = aplicarFiltro (deteccionDeBordes) imagenOriginal

// Aplica el filtro de realce de bordes a la imagen original
let imagenBordesRealzados = aplicarFiltro (realceDeBordes) imagenOriginal

// Guarda las imágenes filtradas en disco
imagenDesenfocada.Save("imagen_desenfocada.jpg")
imagenBordesDetectados.Save("imagen_bordes_detectados.jpg")
imagenBordesRealzados.Save("imagen_bordes_realzados.jpg")

```

Explicación del código:

1. Se importa el módulo de procesamiento de imágenes FSharp.Imaging.
2. Se define una función aplicarFiltro que toma un filtro y una imagen como argumentos y devuelve una imagen con el filtro aplicado.
3. Se definen tres filtros: desenfoque, detecciónDeBordes y realceDeBordes.
4. Se carga la imagen original del disco.
5. Se aplica el filtro de desenfoque, detección de bordes y realce de bordes a la imagen original.
6. Se guardan las imágenes filtradas en disco.