```d
import std.stdio, std.string;

// Definir una función que tome una cadena y devuelva su longitud
int longitudCadena(string cadena) {
    int longitud = 0;
    foreach (char c; cadena) {
        longitud++;
    }
    return longitud;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con cada carácter en mayúsculas
string convertirAMayusculas(string cadena) {
    string cadenaEnMayusculas = "";
    foreach (char c; cadena) {
        cadenaEnMayusculas += c.toUpperCase();
    }
    return cadenaEnMayusculas;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con cada carácter en minúsculas
string convertirAMinusculas(string cadena) {
    string cadenaEnMinusculas = "";
    foreach (char c; cadena) {
        cadenaEnMinusculas += c.toLowerCase();
    }
    return cadenaEnMinusculas;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con los caracteres invertidos
string invertirCadena(string cadena) {
    string cadenaInvertida = "";
    int longitud = cadena.length;
    for (int i = longitud - 1; i >= 0; i--) {
        cadenaInvertida += cadena[i];
    }
    return cadenaInvertida;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con los espacios eliminados
string eliminarEspacios(string cadena) {
    string cadenaSinEspacios = "";
    foreach (char c; cadena) {
        if (c != ' ') {
            cadenaSinEspacios += c;
        }
    }
    return cadenaSinEspacios;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con los dígitos eliminados
string eliminarDigitos(string cadena) {
    string cadenaSinDigitos = "";
    foreach (char c; cadena) {
        if (!c.isDigit()) {
            cadenaSinDigitos += c;
        }
    }
    return cadenaSinDigitos;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con los caracteres especiales eliminados
string eliminarCaracteresEspeciales(string cadena) {
    string cadenaSinCaracteresEspeciales = "";
    foreach (char c; cadena) {
        if (c.isAlpha() || c.isDigit()) {
            cadenaSinCaracteresEspeciales += c;
        }
    }
    return cadenaSinCaracteresEspeciales;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con las vocales eliminadas
string eliminarVocales(string cadena) {
    string cadenaSinVocales = "";
    foreach (char c; cadena) {
        if (!c.isVowel()) {
            cadenaSinVocales += c;
        }
    }
    return cadenaSinVocales;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con las consonantes eliminadas
string eliminarConsonantes(string cadena) {
    string cadenaSinConsonantes = "";
    foreach (char c; cadena) {
        if (c.isConsonant()) {
            cadenaSinConsonantes += c;
        }
    }
    return cadenaSinConsonantes;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con las palabras invertidas
string invertirPalabras(string cadena) {
    string cadenaConPalabrasInvertidas = "";
    string palabraActual = "";
    foreach (char c; cadena) {
        if (c != ' ') {
            palabraActual += c;
        } else {
            cadenaConPalabrasInvertidas += palabraActual.reverse();
            cadenaConPalabrasInvertidas += ' ';
            palabraActual = "";
        }
    }
    cadenaConPalabrasInvertidas += palabraActual.reverse();
    return cadenaConPalabrasInvertidas;
}

// Definir una función que tome una cadena y devuelva una nueva cadena con las palabras ordenadas alfabéticamente
string ordenarPalabras(string cadena) {
    string cadenaConPalabrasOrdenadas = "";
    string[] palabras = cadena.split(' ');
    palabras.sort();
    foreach (string palabra; palabras) {
        cadenaConPalabrasOrdenadas += palabra + ' ';
    }
    return cadenaConPalabrasOrdenadas.strip();
}

// Definir una función que tome una cadena y devuelva una nueva cadena con las palabras únicas
string obtenerPalabrasUnicas(string cadena) {
    string cadenaConPalabrasUnicas = "";
    string[] palabras = cadena.split(' ');
    HashSet