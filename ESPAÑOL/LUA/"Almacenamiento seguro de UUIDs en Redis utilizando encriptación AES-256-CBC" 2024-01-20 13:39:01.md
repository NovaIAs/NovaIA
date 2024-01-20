```lua
-- Carga las librerías necesarias
local uuid = require("uuid")
local crypto = require("crypto")
local redis = require("redis")

-- Crea un nuevo cliente Redis
local redisClient = redis.connect(6379, "localhost")

-- Define una función para generar un nuevo UUID
local function generateUUID()
    return uuid.new()
end

-- Define una función para encriptar una cadena de texto
local function encryptString(plaintext)
    local key = "miClaveSecreta"
    local cipher = crypto.cipher("aes-256-cbc", key)
    local ciphertext = cipher:encrypt(plaintext)
    return ciphertext
end

-- Define una función para desencriptar una cadena de texto
local function decryptString(ciphertext)
    local key = "miClaveSecreta"
    local cipher = crypto.cipher("aes-256-cbc", key)
    local plaintext = cipher:decrypt(ciphertext)
    return plaintext
end

-- Define una función para guardar un valor en Redis
local function setRedisValue(key, value)
    redisClient:set(key, value)
end

-- Define una función para obtener un valor de Redis
local function getRedisValue(key)
    return redisClient:get(key)
end

-- Genera un nuevo UUID
local newUUID = generateUUID()

-- Encripta el UUID
local encryptedUUID = encryptString(newUUID)

-- Guarda el UUID encriptado en Redis
setRedisValue("miUUIDEncriptado", encryptedUUID)

-- Obtiene el UUID encriptado de Redis
local encryptedUUIDFromRedis = getRedisValue("miUUIDEncriptado")

-- Desencripta el UUID
local decryptedUUID = decryptString(encryptedUUIDFromRedis)

-- Imprime el UUID desencriptado
print("UUID desencriptado:", decryptedUUID)
```

**Explicación:**

1. Creamos un nuevo cliente Redis utilizando la librería `redis`.
2. Definimos una función `generateUUID` para generar un nuevo UUID.
3. Definimos una función `encryptString` para encriptar una cadena de texto utilizando el algoritmo AES-256-CBC.
4. Definimos una función `decryptString` para desencriptar una cadena de texto utilizando el algoritmo AES-256-CBC.
5. Definimos una función `setRedisValue` para guardar un valor en Redis.
6. Definimos una función `getRedisValue` para obtener un valor de Redis.
7. Generamos un nuevo UUID y lo encriptamos.
8. Guardamos el UUID encriptado en Redis.
9. Obtenemos el UUID encriptado de Redis.
10. Desencriptamos el UUID.
11. Imprimimos el UUID desencriptado.