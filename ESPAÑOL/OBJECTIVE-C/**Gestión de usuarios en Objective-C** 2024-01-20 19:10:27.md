```objective-c
// Clase para manejar usuarios
@interface Usuario : NSObject

// Propiedades
@property (nonatomic, copy) NSString *nombre;
@property (nonatomic, copy) NSString *apellido;
@property (nonatomic, copy) NSString *email;
@property (nonatomic, assign) NSInteger edad;

// Métodos
- (id)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido email:(NSString *)email edad:(NSInteger)edad;
- (NSString *)nombreCompleto;

@end

// Implementación de la clase Usuario
@implementation Usuario

- (id)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido email:(NSString *)email edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.apellido = apellido;
        self.email = email;
        self.edad = edad;
    }
    return self;
}

- (NSString *)nombreCompleto {
    return [NSString stringWithFormat:@"%@ %@", self.nombre, self.apellido];
}

@end

// Clase para manejar una lista de usuarios
@interface ListaUsuarios : NSObject

// Propiedades
@property (nonatomic, strong) NSMutableArray *usuarios;

// Métodos
- (id)init;
- (void)agregarUsuario:(Usuario *)usuario;
- (Usuario *)usuarioEnIndice:(NSInteger)indice;
- (NSInteger)cantidadUsuarios;

@end

// Implementación de la clase ListaUsuarios
@implementation ListaUsuarios

- (id)init {
    self = [super init];
    if (self) {
        self.usuarios = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)agregarUsuario:(Usuario *)usuario {
    [self.usuarios addObject:usuario];
}

- (Usuario *)usuarioEnIndice:(NSInteger)indice {
    return [self.usuarios objectAtIndex:indice];
}

- (NSInteger)cantidadUsuarios {
    return [self.usuarios count];
}

@end

// Clase principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creamos una lista de usuarios
        ListaUsuarios *listaUsuarios = [[ListaUsuarios alloc] init];

        // Agregamos algunos usuarios a la lista
        Usuario *usuario1 = [[Usuario alloc] initWithNombre:@"Juan" apellido:@"Pérez" email:@"juan.perez@gmail.com" edad:25];
        Usuario *usuario2 = [[Usuario alloc] initWithNombre:@"María" apellido:@"Gómez" email:@"maria.gomez@hotmail.com" edad:30];
        Usuario *usuario3 = [[Usuario alloc] initWithNombre:@"Pedro" apellido:@"Rodríguez" email:@"pedro.rodriguez@yahoo.com" edad:35];

        [listaUsuarios agregarUsuario:usuario1];
        [listaUsuarios agregarUsuario:usuario2];
        [listaUsuarios agregarUsuario:usuario3];

        // Obtenemos el número de usuarios en la lista
        NSInteger cantidadUsuarios = [listaUsuarios cantidadUsuarios];

        // Imprimimos el nombre completo de cada usuario en la lista
        for (int i = 0; i < cantidadUsuarios; i++) {
            Usuario *usuario = [listaUsuarios usuarioEnIndice:i];
            NSLog(@"Nombre completo: %@", [usuario nombreCompleto]);
        }
    }
    return 0;
}
```

**Explicación del código:**

En este código creamos una aplicación sencilla en Objective-C que maneja una lista de usuarios.

La aplicación consta de tres clases:

* `Usuario`: Esta clase representa a un usuario y tiene propiedades para su nombre, apellido, correo electrónico y edad.
* `ListaUsuarios`: Esta clase representa una lista de usuarios y tiene métodos para agregar, obtener y eliminar usuarios de la lista.
* `main`: Esta es la clase principal que crea una lista de usuarios y agrega algunos usuarios a la lista. Luego, imprime el nombre completo de cada usuario en la lista utilizando un ciclo `for`.

### Código comentado

```objective-c
// Clase para manejar usuarios
@interface Usuario : NSObject

// Propiedades
@property (nonatomic, copy) NSString *nombre;
@property (nonatomic, copy) NSString *apellido;
@property (nonatomic, copy) NSString *email;
@property (nonatomic, assign) NSInteger edad;

// Métodos
- (id)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido email:(NSString *)email edad:(NSInteger)edad;
- (NSString *)nombreCompleto;

@end

// Implementación de la clase Usuario
@implementation Usuario

- (id)initWithNombre:(NSString *)nombre apellido:(NSString *)apellido email:(NSString *)email edad:(NSInteger)edad {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.apellido = apellido;
        self.email = email;
        self.edad = edad;
    }
    return self;
}

- (NSString *)nombreCompleto {
    return [NSString stringWithFormat:@"%@ %@", self.nombre, self.apellido];
}

@end

// Clase para manejar una lista de usuarios
@interface ListaUsuarios : NSObject

// Propiedades
@property (nonatomic, strong) NSMutableArray *usuarios;

// Métodos
- (id)init;
- (void)agregarUsuario:(Usuario *)usuario;
- (Usuario *)usuarioEnIndice:(NSInteger)indice;
- (NSInteger)cantidadUsuarios;

@end

// Implementación de la clase ListaUsuarios
@implementation ListaUsuarios

- (id)init {
    self = [super init];
    if (self) {
        self.usuarios = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)agregarUsuario:(Usuario *)usuario {
    [self.usuarios addObject:usuario];
}

- (Usuario *)usuarioEnIndice:(NSInteger)indice {
    return [self.usuarios objectAtIndex:indice];
}

- (NSInteger)cantidadUsuarios {
    return [self.usuarios count];
}

@end

// Clase principal
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Creamos una lista de usuarios
        ListaUsuarios *listaUsuarios = [[ListaUsuarios alloc] init];

        // Agregamos algunos usuarios a la lista
        Usuario *usuario1 = [[Usuario alloc] initWithNombre:@"Juan" apellido:@"Pérez" email:@"juan.perez@gmail.com" edad:25];
        Usuario *usuario2 = [[Usuario alloc] initWithNombre:@"María" apellido:@"Gómez" email:@"maria.gomez@hotmail.com" edad:30];
        Usuario *usuario3 = [[Usuario alloc] initWithNombre:@"Pedro" apellido:@"Rodríguez" email:@"pedro.rodriguez@yahoo.com" edad:35];

        [listaUsuarios agregarUsuario:usuario1];
        [listaUsuarios agregarUsuario:usuario2];
        [listaUsuarios agregarUsuario:usuario3];

        // Obtenemos el número de usuarios en la lista
        NSInteger cantidadUsuarios = [listaUsuarios cantidadUsuarios];

        // Imprimimos el nombre completo de cada usuario en la lista
        for (int i = 0; i < cantidadUsuarios; i++) {
            Usuario *usuario = [listaUsuarios usuarioEnIndice:i];
            NSLog(@"Nombre completo: %@", [usuario nombreCompleto]);
        }
    }
    return 0;
}
```

**Explicación del código comentado:**

En este código creamos una aplicación sencilla en Objective-C que maneja una lista de usuarios.

La aplicación consta de tres clases:

* `Usuario`: Esta clase representa a un usuario y tiene propiedades para su nombre, apellido, correo electrónico y edad.
* `ListaUsuarios`: Esta clase representa una lista de usuarios y tiene métodos para agregar, obtener y eliminar usuarios de la lista.
* `main`: Esta es la clase principal que crea una lista de usuarios y agrega algunos usuarios a la lista. Luego, imprime el nombre completo de cada usuario en la lista utilizando un ciclo `for`.

El código está comentado para facilitar su comprensión. Los comentarios explican lo que hace cada parte del código.