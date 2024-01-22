```rust
// Importación de las bibliotecas necesarias
use std::{
    io::{self, BufRead},
    collections::HashMap,
    string::String,
};

// Definición de la estructura de datos para representar un nodo del árbol sintáctico abstracto (AST)
#[derive(Debug)]
enum NodoAST {
    Programa(Vec<NodoAST>),
    DeclaracionVar(String, String),
    AsignacionVar(String, String),
    LlamadaFuncion(String, Vec<String>),
    ExpresionBinaria(String, String, String),
    ExpresionUnaria(String, String),
    Constante(String),
}

// Definición de la estructura de datos para representar la tabla de símbolos
#[derive(Debug)]
struct TablaSimbolos {
    tabla: HashMap<String, String>,
}

// Función principal del programa
fn main() {
    // Inicialización de la tabla de símbolos
    let mut tabla_simbolos = TablaSimbolos { tabla: HashMap::new() };

    // Lectura del código fuente desde la entrada estándar
    let mut codigo_fuente = String::new();
    io::stdin().read_line(&mut codigo_fuente).unwrap();

    // Generación del AST a partir del código fuente
    let ast = generar_ast(codigo_fuente);

    // Interpretación del AST
    interpretar_ast(ast, &mut tabla_simbolos);
}

// Función para generar el AST a partir del código fuente
fn generar_ast(codigo_fuente: String) -> NodoAST {
    // Conversión del código fuente a una lista de tokens
    let tokens = tokenizar_codigo_fuente(codigo_fuente);

    // Análisis sintáctico del código fuente para generar el AST
    analizar_sintaxis(tokens)
}

// Función para interpretar el AST
fn interpretar_ast(ast: NodoAST, tabla_simbolos: &mut TablaSimbolos) {
    // Interpretación del AST para generar el valor de la expresión
    let valor = interpretar_expresion(ast, tabla_simbolos);

    // Impresión del valor de la expresión
    println!("{}", valor);
}

// Función para interpretar una expresión
fn interpretar_expresion(ast: NodoAST, tabla_simbolos: &mut TablaSimbolos) -> String {
    match ast {
        NodoAST::Constante(valor) => valor,
        NodoAST::DeclaracionVar(nombre, valor) => {
            tabla_simbolos.tabla.insert(nombre, valor);
            String::new()
        }
        NodoAST::AsignacionVar(nombre, valor) => {
            let valor_evaluado = interpretar_expresion(NodoAST::Constante(valor), tabla_simbolos);
            tabla_simbolos.tabla.insert(nombre, valor_evaluado);
            String::new()
        }
        NodoAST::LlamadaFuncion(nombre_funcion, argumentos) => {
            // Búsqueda de la función en la tabla de símbolos
            let funcion = tabla_simbolos.tabla.get(&nombre_funcion).unwrap();

            // Evaluación de los argumentos de la función
            let argumentos_evaluados: Vec<String> = argumentos.iter().map(|argumento| interpretar_expresion(NodoAST::Constante(argumento.clone()), tabla_simbolos)).collect();

            // Llamada a la función con los argumentos evaluados
            funcion(argumentos_evaluados)
        }
        NodoAST::ExpresionBinaria(operador, izquierdo, derecho) => {
            let izquierdo_evaluado = interpretar_expresion(NodoAST::Constante(izquierdo.clone()), tabla_simbolos);
            let derecho_evaluado = interpretar_expresion(NodoAST::Constante(derecho.clone()), tabla_simbolos);

            // Realización de la operación binaria
            match operador.as_str() {
                "+" => sumar(izquierdo_evaluado, derecho_evaluado),
                "-" => restar(izquierdo_evaluado, derecho_evaluado),
                "*" => multiplicar(izquierdo_evaluado, derecho_evaluado),
                "/" => dividir(izquierdo_evaluado, derecho_evaluado),
                "==" => igual(izquierdo_evaluado, derecho_evaluado),
                "!=" => distinto(izquierdo_evaluado, derecho_evaluado),
                ">" => mayor(izquierdo_evaluado, derecho_evaluado),
                "<" => menor(izquierdo_evaluado, derecho_evaluado),
                ">=" => mayor_igual(izquierdo_evaluado, derecho_evaluado),
                "<=" => menor_igual(izquierdo_evaluado, derecho_evaluado),
                _ => panic!("Operador binario no reconocido: {}", operador),
            }
        }
        NodoAST::ExpresionUnaria(operador, expresión) => {
            let expresión_evaluada = interpretar_expresion(NodoAST::Constante(expresión.clone()), tabla_simbolos);

            // Realización de la operación unaria
            match operador.as_str() {
                "-" => negar(expresión_evaluada),
                _ => panic!("Operador unario no reconocido: {}", operador),
            }
        }
        _ => panic!("Nodo AST no reconocido: {:?}", ast),
    }
}

// Función para sumar dos números
fn sumar(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num + derecho_num).to_string()
}

// Función para restar dos números
fn restar(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num - derecho_num).to_string()
}

// Función para multiplicar dos números
fn multiplicar(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num * derecho_num).to_string()
}

// Función para dividir dos números
fn dividir(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num / derecho_num).to_string()
}

// Función para comparar si dos números son iguales
fn igual(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num == derecho_num).to_string()
}

// Función para comparar si dos números son distintos
fn distinto(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num != derecho_num).to_string()
}

// Función para comparar si un número es mayor que otro
fn mayor(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num > derecho_num).to_string()
}

// Función para comparar si un número es menor que otro
fn menor(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num < derecho_num).to_string()
}

// Función para comparar si un número es mayor o igual que otro
fn mayor_igual(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num >= derecho_num).to_string()
}

// Función para comparar si un número es menor o igual que otro
fn menor_igual(izquierdo: String, derecho: String) -> String {
    let izquierdo_num = izquierdo.parse::<i32>().unwrap();
    let derecho_num = derecho.parse::<i32>().unwrap();
    (izquierdo_num <= derecho_num).to_string()
}

// Función para negar un número
fn negar(expresión: String) -> String {
    let expresión_num = expresión.parse::<i32>().unwrap();
    (-expresión_num).to_string()
}

// Funciones auxiliares

// Función para tokenizar el código fuente
fn tokenizar_codigo_fuente(codigo_fuente: String) -> Vec<String> {
    let mut tokens: Vec<String> = Vec::new();
    let mut token_actual = String::new();