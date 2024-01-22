```objective-c
#import <Foundation/Foundation.h>

@interface Persona : NSObject

@property (nonatomic, strong) NSString *nombre;
@property (nonatomic, strong) NSDate *fechaNacimiento;

- (instancetype)initWithNombre:(NSString *)nombre fechaNacimiento:(NSDate *)fechaNacimiento;

@end

@implementation Persona

- (instancetype)initWithNombre:(NSString *)nombre fechaNacimiento:(NSDate *)fechaNacimiento {
    self = [super init];
    if (self) {
        self.nombre = nombre;
        self.fechaNacimiento = fechaNacimiento;
    }
    return self;
}

@end

@interface Alumno : Persona

@property (nonatomic, strong) NSArray<NSString *> *asignaturas;

- (instancetype)initWithNombre:(NSString *)nombre fechaNacimiento:(NSDate *)fechaNacimiento asignaturas:(NSArray<NSString *> *)asignaturas;

@end

@implementation Alumno

- (instancetype)initWithNombre:(NSString *)nombre fechaNacimiento:(NSDate *)fechaNacimiento asignaturas:(NSArray<NSString *> *)asignaturas {
    self = [super initWithNombre:nombre fechaNacimiento:fechaNacimiento];
    if (self) {
        self.asignaturas = asignaturas;
    }
    return self;
}

@end

@interface Profesor : Persona

@property (nonatomic, strong) NSString *departamento;

- (instancetype)initWithNombre:(NSString *)nombre fechaNacimiento:(NSDate *)fechaNacimiento departamento:(NSString *)departamento;

@end

@implementation Profesor

- (instancetype)initWithNombre:(NSString *)nombre fechaNacimiento:(NSDate *)fechaNacimiento departamento:(NSString *)departamento {
    self = [super initWithNombre:nombre fechaNacimiento:fechaNacimiento];
    if (self) {
        self.departamento = departamento;
    }
    return self;
}

@end

@interface Escuela : NSObject

@property (nonatomic, strong) NSArray<Persona *> *personas;

- (instancetype)initWithPersonas:(NSArray<Persona *> *)personas;

@end

@implementation Escuela

- (instancetype)initWithPersonas:(NSArray<Persona *> *)personas {
    self = [super init];
    if (self) {
        self.personas = personas;
    }
    return self;
}

@end

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        Persona *persona1 = [[Persona alloc] initWithNombre:@"Juan" fechaNacimiento:[NSDate dateWithTimeIntervalSince1970:0]];
        Persona *persona2 = [[Alumno alloc] initWithNombre:@"María" fechaNacimiento:[NSDate dateWithTimeIntervalSince1970:0] asignaturas:@[@"Matemáticas", @"Física", @"Química"]];
        Persona *persona3 = [[Profesor alloc] initWithNombre:@"Pedro" fechaNacimiento:[NSDate dateWithTimeIntervalSince1970:0] departamento:@"Ciencias"];

        Escuela *escuela = [[Escuela alloc] initWithPersonas:@[persona1, persona2, persona3]];

        for (Persona *persona in escuela.personas) {
            NSLog(@"Nombre: %@\nFecha de nacimiento: %@\n", persona.nombre, persona.fechaNacimiento);

            if ([persona isKindOfClass:[Alumno class]]) {
                Alumno *alumno = (Alumno *)persona;
                NSLog(@"Asignaturas: %@\n", alumno.asignaturas);
            } else if ([persona isKindOfClass:[Profesor class]]) {
                Profesor *profesor = (Profesor *)persona;
                NSLog(@"Departamento: %@\n", profesor.departamento);
            }

            NSLog(@"\n");
        }
    }
    return 0;
}
```

Este código crea una jerarquía de clases en Objective-C. La clase `Persona` es la clase base, que define las propiedades comunes a todas las personas. Las clases `Alumno` y `Profesor` son clases derivadas de `Persona`, que definen propiedades y métodos específicos para cada tipo de persona. La clase `Escuela` es una clase que contiene una lista de personas.

El programa crea tres objetos `Persona`: un objeto `Persona` simple, un objeto `Alumno` y un objeto `Profesor`. Luego crea un objeto `Escuela` con estos tres objetos `Persona`.

El programa recorre la lista de personas en la escuela e imprime el nombre, la fecha de nacimiento y las asignaturas o el departamento de cada persona, dependiendo de si la persona es un alumno o un profesor.

Este código es complejo porque utiliza varias características de Objective-C, incluyendo clases, herencia, polimorfismo y colecciones.