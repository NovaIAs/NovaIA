```php
<?php

// Importar las clases necesarias
use Illuminate\Database\Eloquent\Model;
use Illuminate\Database\Eloquent\Builder;

// Definir el modelo `Usuario`
class Usuario extends Model
{
    // Nombre de la tabla en la base de datos
    protected $table = 'usuarios';

    // Llave primaria de la tabla
    protected $primaryKey = 'id';

    // Campos que se pueden modificar en masa
    protected $fillable = ['nombre', 'email', 'contrasena'];

    // Relación con el modelo `Rol`
    public function roles()
    {
        return $this->belongsToMany('Rol');
    }

    // Relación con el modelo `Permiso`
    public function permisos()
    {
        return $this->belongsToMany('Permiso');
    }

    // Método para obtener los usuarios con un rol específico
    public static function conRol(string $rol): Builder
    {
        return self::join('roles_usuarios', 'roles_usuarios.usuario_id', '=', 'usuarios.id')
            ->join('roles', 'roles.id', '=', 'roles_usuarios.rol_id')
            ->where('roles.nombre', '=', $rol);
    }

    // Método para obtener los usuarios con un permiso específico
    public static function conPermiso(string $permiso): Builder
    {
        return self::join('permisos_usuarios', 'permisos_usuarios.usuario_id', '=', 'usuarios.id')
            ->join('permisos', 'permisos.id', '=', 'permisos_usuarios.permiso_id')
            ->where('permisos.nombre', '=', $permiso);
    }
}

// Definir el modelo `Rol`
class Rol extends Model
{
    // Nombre de la tabla en la base de datos
    protected $table = 'roles';

    // Llave primaria de la tabla
    protected $primaryKey = 'id';

    // Campos que se pueden modificar en masa
    protected $fillable = ['nombre'];

    // Relación con el modelo `Usuario`
    public function usuarios()
    {
        return $this->belongsToMany('Usuario');
    }

    // Relación con el modelo `Permiso`
    public function permisos()
    {
        return $this->belongsToMany('Permiso');
    }
}

// Definir el modelo `Permiso`
class Permiso extends Model
{
    // Nombre de la tabla en la base de datos
    protected $table = 'permisos';

    // Llave primaria de la tabla
    protected $primaryKey = 'id';

    // Campos que se pueden modificar en masa
    protected $fillable = ['nombre'];

    // Relación con el modelo `Usuario`
    public function usuarios()
    {
        return $this->belongsToMany('Usuario');
    }

    // Relación con el modelo `Rol`
    public function roles()
    {
        return $this->belongsToMany('Rol');
    }
}

// Ejemplo de uso

// Obtener todos los usuarios con el rol "Administrador"
$usuariosAdministradores = Usuario::conRol('Administrador')->get();

// Obtener todos los usuarios con el permiso "Crear usuarios"
$usuariosConPermisoCrearUsuarios = Usuario::conPermiso('Crear usuarios')->get();

// Obtener todos los roles con el permiso "Editar usuarios"
$rolesConPermisoEditarUsuarios = Rol::with('permisos')->whereHas('permisos', function ($query) {
    $query->where('nombre', '=', 'Editar usuarios');
})->get();

// Obtener todos los permisos asignados al rol "Moderador"
$permisosRolModerador = Rol::find(2)->permisos()->get();

?>
```

Este código define tres modelos de Eloquent para representar las tablas `usuarios`, `roles` y `permisos` en una base de datos. También define las relaciones entre estos modelos y proporciona varios métodos para consultar la base de datos de forma eficiente.

Por ejemplo, el método `Usuario::conRol()` permite obtener todos los usuarios que tienen un rol específico, mientras que el método `Rol::with('permisos')->whereHas('permisos', ...)` permite obtener todos los roles que tienen un permiso específico.

En el ejemplo de uso, se muestran varios ejemplos de cómo utilizar estos modelos y métodos para realizar consultas a la base de datos.