```ruby
# Clase Base de datos que define el comportamiento básico de una base de datos
class BaseDeDatos
  attr_reader :nombre, :conexiones

  def initialize(nombre)
    @nombre = nombre
    @conexiones = []
  end

  def conectar(usuario)
    @conexiones << usuario
  end

  def desconectar(usuario)
    @conexiones.delete(usuario)
  end
end

# Clase MySQL que hereda de BaseDeDatos y define el comportamiento específico de una base de datos MySQL
class MySQL < BaseDeDatos
  def initialize(nombre, host, puerto, usuario, contraseña)
    super(nombre)
    @host = host
    @puerto = puerto
    @usuario = usuario
    @contraseña = contraseña
  end

  def conectar(usuario)
    puts "Conectando a MySQL con el usuario #{usuario}"
    super(usuario)
  end

  def desconectar(usuario)
    puts "Desconectando de MySQL con el usuario #{usuario}"
    super(usuario)
  end
end

# Clase PostgreSQL que hereda de BaseDeDatos y define el comportamiento específico de una base de datos PostgreSQL
class PostgreSQL < BaseDeDatos
  def initialize(nombre, host, puerto, usuario, contraseña)
    super(nombre)
    @host = host
    @puerto = puerto
    @usuario = usuario
    @contraseña = contraseña
  end

  def conectar(usuario)
    puts "Conectando a PostgreSQL con el usuario #{usuario}"
    super(usuario)
  end

  def desconectar(usuario)
    puts "Desconectando de PostgreSQL con el usuario #{usuario}"
    super(usuario)
  end
end

# Clase Oracle que hereda de BaseDeDatos y define el comportamiento específico de una base de datos Oracle
class Oracle < BaseDeDatos
  def initialize(nombre, host, puerto, usuario, contraseña)
    super(nombre)
    @host = host
    @puerto = puerto
    @usuario = usuario
    @contraseña = contraseña
  end

  def conectar(usuario)
    puts "Conectando a Oracle con el usuario #{usuario}"
    super(usuario)
  end

  def desconectar(usuario)
    puts "Desconectando de Oracle con el usuario #{usuario}"
    super(usuario)
  end
end

# Ejemplo de uso
mysql = MySQL.new("mi_base_de_datos_mysql", "localhost", 3306, "root", "password")
mysql.conectar("usuario1")
mysql.desconectar("usuario1")

postgresql = PostgreSQL.new("mi_base_de_datos_postgresql", "localhost", 5432, "postgres", "password")
postgresql.conectar("usuario2")
postgresql.desconectar("usuario2")

oracle = Oracle.new("mi_base_de_datos_oracle", "localhost", 1521, "oracle", "password")
oracle.conectar("usuario3")
oracle.desconectar("usuario3")
```

Explicaciones:

* La clase `BaseDeDatos` define el comportamiento básico de una base de datos, como el nombre de la base de datos y las conexiones activas.
* Las clases `MySQL`, `PostgreSQL` y `Oracle` heredan de la clase `BaseDeDatos` y definen el comportamiento específico de cada tipo de base de datos.
* El ejemplo de uso crea tres objetos de base de datos, uno para cada tipo de base de datos, y muestra cómo conectar y desconectar usuarios de cada base de datos.