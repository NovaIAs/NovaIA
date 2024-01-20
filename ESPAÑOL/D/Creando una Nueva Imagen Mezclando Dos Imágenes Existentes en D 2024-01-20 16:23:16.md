```d
import std.stdio, std.algorithm;

enum RGBColor {
    rojo = 0xFF0000,
    verde = 0x00FF00,
    azul = 0x0000FF
}

struct Pixel {
    RGBColor color;

    // Convierte un píxel a un valor hexadecimal.
    string toHex() pure nothrow {
        return format("$%x", color);
    }

    // Mezcla dos píxeles juntos.
    Pixel mix(Pixel other) pure nothrow {
        return Pixel(mixColors(color, other.color));
    }

    // Mezcla dos colores RGB juntos.
    RGBColor mixColors(RGBColor color1, RGBColor color2) pure nothrow {
        int r = (color1 >> 16) & 0xFF;
        int g = (color1 >> 8) & 0xFF;
        int b = color1 & 0xFF;

        int r2 = (color2 >> 16) & 0xFF;
        int g2 = (color2 >> 8) & 0xFF;
        int b2 = color2 & 0xFF;

        return RGBColor((r + r2) / 2, (g + g2) / 2, (b + b2) / 2);
    }
}

class Imagen {
    int width;
    int height;
    Pixel[] pixels;

    // Crea una nueva imagen.
    this(int width, int height) pure {
        this.width = width;
        this.height = height;
        pixels = new Pixel[width * height];
    }

    // Consigue el píxel en la coordenada especificada.
    Pixel getPixel(int x, int y) const pure {
        return pixels[y * width + x];
    }

    // Establece el píxel en la coordenada especificada.
    void setPixel(int x, int y, Pixel color) pure {
        pixels[y * width + x] = color;
    }

    // Guarda la imagen en un archivo.
    void save(string filename) pure throw(IOException) {
        File file = File(filename, FileMode.Write);
        try {
            file.writeBytes(pixels.to!string.flatten());
        } finally {
            file.close();
        }
    }

    // Carga una imagen desde un archivo.
    static Imagen load(string filename) pure throw(IOException) {
        File file = File(filename, FileMode.Read);
        try {
            Imagen imagen = new Imagen(0, 0);
            imagen.pixels = Pixel.fromString(file.readBytes());
            imagen.width = imagen.pixels.length / imagen.height;
            return imagen;
        } finally {
            file.close();
        }
    }

    // Mezcla dos imágenes juntas.
    Imagen mix(Imagen other) pure nothrow {
        Imagen imagen = new Imagen(width, height);

        for (int y = 0; y < height; y++) {
            for (int x = 0; x < width; x++) {
                imagen.setPixel(x, y, getPixel(x, y).mix(other.getPixel(x, y)));
            }
        }

        return imagen;
    }
}

int main() {
    Imagen imagen1 = Imagen.load("imagen1.bmp");
    Imagen imagen2 = Imagen.load("imagen2.bmp");
    
    Imagen imagen3 = imagen1.mix(imagen2);
    
    imagen3.save("imagen3.bmp");
}
```

Este código crea una nueva imagen mezclando dos imágenes existentes. La clase Imagen representa una imagen y tiene métodos para obtener y establecer píxeles, guardar y cargar imágenes y mezclar dos imágenes juntas. La clase Pixel representa un píxel y tiene métodos para obtener el color del píxel, mezclar dos píxeles y convertir un píxel a un valor hexadecimal. El código principal carga dos imágenes, las mezcla y luego guarda la imagen resultante.