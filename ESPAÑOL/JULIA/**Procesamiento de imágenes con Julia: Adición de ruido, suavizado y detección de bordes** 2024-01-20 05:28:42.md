```julia
# Importamos la biblioteca de procesamiento de imágenes
using Images

# Cargamos una imagen de un archivo
imagen = readimage("imagen.jpg")

# Creamos una matriz de números aleatorios del mismo tamaño que la imagen
ruido = rand(size(imagen))

# Sumamos el ruido a la imagen
imagen_ruidosa = imagen + ruido

# Mostramos la imagen ruidosa
show(imagen_ruidosa)

# Aplicamos un filtro de suavizado a la imagen ruidosa
imagen_suavizada = smooth(imagen_ruidosa)

# Mostramos la imagen suavizada
show(imagen_suavizada)

# Aplicamos un filtro de detección de bordes a la imagen suavizada
imagen_bordes = edgedetect(imagen_suavizada)

# Mostramos la imagen de bordes
show(imagen_bordes)

# Guardamos la imagen de bordes en un archivo
saveimage(imagen_bordes, "imagen_bordes.jpg")
```

**Explicación:**

Este código realiza el procesamiento de imágenes en Julia. Primero, importamos la biblioteca de procesamiento de imágenes `Images`. Luego, cargamos una imagen de un archivo y creamos una matriz de números aleatorios del mismo tamaño que la imagen. Sumamos el ruido a la imagen y la mostramos. Después, aplicamos un filtro de suavizado a la imagen ruidosa y la mostramos. A continuación, aplicamos un filtro de detección de bordes a la imagen suavizada y la mostramos. Finalmente, guardamos la imagen de bordes en un archivo.