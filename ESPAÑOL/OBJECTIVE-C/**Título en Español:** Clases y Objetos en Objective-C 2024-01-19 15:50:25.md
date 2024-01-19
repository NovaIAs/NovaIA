```objective-c
#import <Foundation/Foundation.h>

@interface Perro : NSObject

- (void)ladrar;
- (void)comer;
- (void)dormir;

@end

@implementation Perro

- (void)ladrar {
  NSLog(@"¡Guau!");
}

- (void)comer {
  NSLog(@"Estoy comiendo");
}

- (void)dormir {
  NSLog(@"Estoy durmiendo");
}

@end

@interface Gato : NSObject

- (void)maullar;
- (void)comer;
- (void)dormir;

@end

@implementation Gato

- (void)maullar {
  NSLog(@"¡Miau!");
}

- (void)comer {
  NSLog(@"Estoy comiendo");
}

- (void)dormir {
  NSLog(@"Estoy durmiendo");
}

@end

int main(int argc, const char * argv[]) {
  @autoreleasepool {
    Perro *perro = [[Perro alloc] init];
    [perro ladrar];
    [perro comer];
    [perro dormir];

    Gato *gato = [[Gato alloc] init];
    [gato maullar];
    [gato comer];
    [gato dormir];
  }
  return 0;
}
```

Este código crea dos clases, `Perro` y `Gato`, que heredan de la clase `NSObject`. Cada clase tiene tres métodos: `ladrar`, `comer` y `dormir`. El método `ladrar` hace que el perro ladre, el método `comer` hace que el perro coma y el método `dormir` hace que el perro duerma. El método `maullar` hace que el gato maulle, el método `comer` hace que el gato coma y el método `dormir` hace que el gato duerma.

En la función `main`, se crean dos objetos, un perro y un gato. Luego, se llama a los métodos `ladrar`, `comer` y `dormir` del perro, y a los métodos `maullar`, `comer` y `dormir` del gato.

Este código es complejo porque crea dos clases con tres métodos cada una, y luego crea dos objetos de esas clases y llama a los métodos de esos objetos. Esto es más complejo que un código simple, que sólo crea un objeto y llama a un método de ese objeto.