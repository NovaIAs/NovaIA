**Clase Persona:**

```objective-c
@interface Persona : NSObject

@property (nonatomic, copy) NSString *nombre;
@property (nonatomic, copy) NSString *apellido;
@property (nonatomic, assign) NSInteger edad;

- (id)initWithNombre:(NSString *)nombre
              apellido:(NSString *)apellido
                  edad:(NSInteger)edad;

@end
```

La clase `Persona` representa a una persona con un nombre, un apellido y una edad.

**Clase Estudiante:**

```objective-c
@interface Estudiante : Persona

@property (nonatomic, copy) NSString *matricula;
@property (nonatomic, copy) NSArray *cursos;

- (id)initWithNombre:(NSString *)nombre
              apellido:(NSString *)apellido
                  edad:(NSInteger)edad
             matricula:(NSString *)matricula
               cursos:(NSArray *)cursos;

@end
```

La clase `Estudiante` hereda de la clase `Persona` y añade una propiedad `matricula` y una propiedad `cursos`.

**Clase Profesor:**

```objective-c
@interface Profesor : Persona

@property (nonatomic, copy) NSString *materia;
@property (nonatomic, assign) NSInteger añosExperiencia;

- (id)initWithNombre:(NSString *)nombre
              apellido:(NSString *)apellido
                  edad:(NSInteger)edad
              materia:(NSString *)materia
    añosExperiencia:(NSInteger)añosExperiencia;

@end
```

La clase `Profesor` hereda de la clase `Persona` y añade una propiedad `materia` y una propiedad `añosExperiencia`.

**Clase Escuela:**

```objective-c
@interface Escuela : NSObject

@property (nonatomic, copy) NSString *nombre;
@property (nonatomic, copy) NSArray *estudiantes;
@property (nonatomic, copy) NSArray *profesores;

- (id)initWithNombre:(NSString *)nombre
            estudiantes:(NSArray *)estudiantes
             profesores:(NSArray *)profesores;

@end
```

La clase `Escuela` representa a una escuela con un nombre, una lista de estudiantes y una lista de profesores.

**Uso de las clases:**

```objective-c
// Creamos una escuela
Escuela *escuela = [[Escuela alloc] initWithNombre:@"Ejemplo"
                                      estudiantes:@[]
                                       profesores:@[]];

// Creamos un estudiante
Estudiante *estudiante1 = [[Estudiante alloc] initWithNombre:@"Juan"
                                                 apellido:@"García"
                                                     edad:18
                                                 matricula:@"123456789"
                                                   cursos:@[@"Matemáticas", @"Física", @"Química"]];

// Creamos un profesor
Profesor *profesor1 = [[Profesor alloc] initWithNombre:@"María"
                                            apellido:@"González"
                                                edad:40
                                            materia:@"Español"
                                    añosExperiencia:10];

// Añadimos el estudiante y el profesor a la escuela
[escuela.estudiantes addObject:estudiante1];
[escuela.profesores addObject:profesor1];

// Imprimimos el nombre de la escuela, el nombre del estudiante y el nombre del profesor
NSLog(@"Escuela: %@", escuela.nombre);
NSLog(@"Estudiante: %@", estudiante1.nombre);
NSLog(@"Profesor: %@", profesor1.nombre);
```

Este código crea una escuela, un estudiante y un profesor. Luego, añade el estudiante y el profesor a la escuela y finalmente imprime el nombre de la escuela, el nombre del estudiante y el nombre del profesor.