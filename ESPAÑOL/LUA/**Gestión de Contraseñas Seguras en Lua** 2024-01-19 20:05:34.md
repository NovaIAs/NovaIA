```
function cadenaAleatoria(longitud) -- Genera una cadena aleatoria de la longitud especificada.
    local caracteres = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    local cadena = ""
    for i = 1, longitud do
        cadena = cadena .. caracteres:sub(math.random(1, #caracteres), math.random(1, #caracteres))
    end
    return cadena
end

function generarContrasena(longitud) -- Genera una contraseña aleatoria de la longitud especificada.
    local contraseña = ""
    local caracteresPermitodos = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*"
    for i = 1, longitud do
        contraseña = contraseña .. caracteresPermitodos:sub(math.random(1, #caracteresPermitodos), math.random(1, #caracteresPermitodos))
    end
    return contraseña
end

function verificarContrasena(contraseña, longitudMinima, caracteresEspeciales) -- Verifica si la contraseña cumple con los requisitos especificados.
    local esValida = true
    if #contraseña < longitudMinima then -- Verifica que la contraseña sea lo suficientemente larga.
        esValida = false
    end
    if caracteresEspeciales and not contraseña:match("%[!@#$%^&*]") then -- Verifica que la contraseña contenga caracteres especiales.
        esValida = false
    end
    return esValida
end

function encriptarContrasena(contraseña) -- Encripta la contraseña utilizando la función bcrypt.
    local salt = cadenaAleatoria(16) -- Genera un salt aleatorio.
    return bcrypt.hashpw(contraseña, salt) -- Encripta la contraseña utilizando el salt.
end

function desencriptarContrasena(contraseñaEncriptada, contraseña) -- Desencripta la contraseña encriptada utilizando la función bcrypt.
    return bcrypt.checkpw(contraseña, contraseñaEncriptada) -- Desencripta la contraseña encriptada utilizando la contraseña.
end

-- Ejemplo de uso:
local contraseña = generarContrasena(12) -- Genera una contraseña aleatoria de 12 caracteres.
local esValida = verificarContrasena(contraseña, 8, true) -- Verifica si la contraseña cumple con los requisitos especificados.
if esValida then -- Si la contraseña es válida, la encripta y la almacena en la base de datos.
    local contraseñaEncriptada = encriptarContrasena(contraseña)
else -- Si la contraseña no es válida, muestra un mensaje de error.
    print("La contraseña no es válida.")
end

-- Más tarde, cuando el usuario intenta iniciar sesión, puedes desencriptar la contraseña encriptada y compararla con la contraseña introducida por el usuario para verificar si el usuario está autorizado.
```
Explicación del código:

* La función `cadenaAleatoria()` genera una cadena aleatoria de la longitud especificada.
* La función `generarContrasena()` genera una contraseña aleatoria de la longitud especificada.
* La función `verificarContrasena()` verifica si la contraseña cumple con los requisitos especificados.
* La función `encriptarContrasena()` encripta la contraseña utilizando la función bcrypt.
* La función `desencriptarContrasena()` desencripta la contraseña encriptada utilizando la función bcrypt.

El código utiliza estas funciones para generar una contraseña aleatoria, verificar si la contraseña cumple con los requisitos especificados, encriptar la contraseña y desencriptar la contraseña encriptada.

El código también incluye un ejemplo de uso que muestra cómo generar una contraseña aleatoria, verificar si la contraseña es válida, encriptar la contraseña y almacenarla en la base de datos. También muestra cómo desencriptar la contraseña encriptada y compararla con la contraseña introducida por el usuario para verificar si el usuario está autorizado.