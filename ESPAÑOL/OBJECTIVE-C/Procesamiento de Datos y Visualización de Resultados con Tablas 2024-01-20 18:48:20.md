```objective-c
// Importar las bibliotecas necesarias
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

// Definir una clase de utilidad para el procesamiento de datos
@interface Datos : NSObject

// Método para obtener el número máximo de un array de números
- (int)maximo:(NSArray *)array;

// Método para obtener el número mínimo de un array de números
- (int)minimo:(NSArray *)array;

// Método para calcular el promedio de un array de números
- (float)media:(NSArray *)array;

@end

// Implementar la clase de utilidad para el procesamiento de datos
@implementation Datos

// Método para obtener el número máximo de un array de números
- (int)maximo:(NSArray *)array {
    int max = [[array firstObject] intValue];
    for (int i = 1; i < array.count; i++) {
        int num = [[array objectAtIndex:i] intValue];
        if (num > max) {
            max = num;
        }
    }
    return max;
}

// Método para obtener el número mínimo de un array de números
- (int)minimo:(NSArray *)array {
    int min = [[array firstObject] intValue];
    for (int i = 1; i < array.count; i++) {
        int num = [[array objectAtIndex:i] intValue];
        if (num < min) {
            min = num;
        }
    }
    return min;
}

// Método para calcular el promedio de un array de números
- (float)media:(NSArray *)array {
    int suma = 0;
    for (int i = 0; i < array.count; i++) {
        suma += [[array objectAtIndex:i] intValue];
    }
    return (float)suma / array.count;
}

@end

// Definir una clase de controlador de vista de tabla para mostrar los resultados del procesamiento de datos
@interface TablaResultadosControlador : UITableViewController

// Propiedad para almacenar los datos a mostrar en la tabla
@property (strong, nonatomic) NSArray *datos;

@end

// Implementar la clase de controlador de vista de tabla para mostrar los resultados del procesamiento de datos
@implementation TablaResultadosControlador

- (void)viewDidLoad {
    [super viewDidLoad];
    
    // Crear un array de números para procesar
    NSArray *numeros = @[@1, @2, @3, @4, @5, @6, @7, @8, @9, @10];
    
    // Crear una instancia de la clase de utilidad para el procesamiento de datos
    Datos *procesador = [[Datos alloc] init];
    
    // Obtener el número máximo, mínimo y promedio del array de números
    int max = [procesador maximo:numeros];
    int min = [procesador minimo:numeros];
    float media = [procesador media:numeros];
    
    // Crear un array con los resultados del procesamiento de datos
    self.datos = @[@(max), @(min), @(media)];
    
    // Recargar los datos de la tabla
    [self.tableView reloadData];
}

- (NSInteger)numberOfSectionsInTableView:(UITableView *)tableView {
    return 1;
}

- (NSInteger)tableView:(UITableView *)tableView numberOfRowsInSection:(NSInteger)section {
    return self.datos.count;
}

- (UITableViewCell *)tableView:(UITableView *)tableView cellForRowAtIndexPath:(NSIndexPath *)indexPath {
    UITableViewCell *cell = [tableView dequeueReusableCellWithIdentifier:@"celda" forIndexPath:indexPath];
    
    // Obtener el valor del resultado del procesamiento de datos correspondiente a la fila actual
    NSNumber *valor = [self.datos objectAtIndex:indexPath.row];
    
    // Formatear el valor como una cadena de texto
    NSString *texto = [NSString stringWithFormat:@"%@", valor];
    
    // Establecer el texto de la celda con el valor formateado
    cell.textLabel.text = texto;
    
    return cell;
}

@end

// Definir una clase delegada de la aplicación para configurar la ventana principal de la aplicación
@interface AppDelegate : UIResponder <UIApplicationDelegate>

@property (strong, nonatomic) UIWindow *window;

@end

// Implementar la clase delegada de la aplicación para configurar la ventana principal de la aplicación
@implementation AppDelegate

- (BOOL)application:(UIApplication *)application didFinishLaunchingWithOptions:(NSDictionary *)launchOptions {
    self.window = [[UIWindow alloc] initWithFrame:[[UIScreen mainScreen] bounds]];
    
    // Crear una instancia del controlador de vista de tabla para mostrar los resultados del procesamiento de datos
    TablaResultadosControlador *controlador = [[TablaResultadosControlador alloc] initWithStyle:UITableViewStylePlain];
    
    // Crear una instancia del controlador de navegación para mostrar el controlador de vista de tabla
    UINavigationController *navegacion = [[UINavigationController alloc] initWithRootViewController:controlador];
    
    // Establecer el controlador de navegación como el controlador raíz de la ventana principal de la aplicación
    self.window.rootViewController = navegacion;
    
    // Hacer visible la ventana principal de la aplicación
    [self.window makeKeyAndVisible];
    
    return YES;
}

@end

// Definir el punto de entrada de la aplicación
int main(int argc, char * argv[]) {
    @autoreleasepool {
        return UIApplicationMain(argc, argv, nil, NSStringFromClass([AppDelegate class]));
    }
}
```

Explicación del código:

* Se importan las bibliotecas necesarias para trabajar con la interfaz de usuario y el procesamiento de datos.
* Se define una clase de utilidad llamada `Datos` que contiene métodos para obtener el número máximo, mínimo y promedio de un array de números.
* Se define una clase de controlador de vista de tabla llamada `TablaResultadosControlador` que muestra los resultados del procesamiento de datos en una tabla.
* Se define una clase delegada de la aplicación llamada `AppDelegate` que configura la ventana principal de la aplicación y muestra el controlador de vista de tabla.
* Se define el punto de entrada de la aplicación, que crea una instancia de la clase delegada de la aplicación y la utiliza para configurar la ventana principal de la aplicación.

El código crea un array de números, utiliza la clase de utilidad `Datos` para procesar los números y obtener el número máximo, mínimo y promedio, y muestra los resultados en una tabla. El código es complejo y diferenciado porque combina técnicas de procesamiento de datos, interfaz de usuario y gestión de la aplicación.