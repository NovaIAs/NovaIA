```objective-c
// clase principal para gestionar el acceso a los datos de la base de datos
@interface GestorBaseDatos : NSObject

// método para inicializar la conexión con la base de datos
- (void)inicializarConexion;

- (BOOL)abrirBaseDatos;

- (BOOL)cerrarBaseDatos;

- (NSArray *)ejecutarConsulta:(NSString *)sentencia;

- (BOOL)ejecutarSentencia:(NSString *)sentencia;

- (NSInteger)obtenerUltimoIDInsertado;

@end

// clase principal para gestionar el acceso a los datos de la base de datos
@implementation GestorBaseDatos

// método para inicializar la conexión con la base de datos
- (void)inicializarConexion {

    _nombreBaseDatos = @"mi_base_de_datos";
    _rutaBaseDatos = [[NSHomeDirectory() stringByAppendingPathComponent:@"Documents"] stringByAppendingPathComponent:_nombreBaseDatos];

    // crear la base de datos si no existe
    if (![[NSFileManager defaultManager] fileExistsAtPath:_rutaBaseDatos]) {

        if (sqlite3_open([_rutaBaseDatos UTF8String], &_baseDatos) != SQLITE_OK) {

            NSLog(@"Error al crear la base de datos: %s", sqlite3_errmsg(_baseDatos));

        } else {

            NSLog(@"Base de datos creada correctamente");

        }

    } else {

        // abrir la base de datos
        if (sqlite3_open([_rutaBaseDatos UTF8String], &_baseDatos) != SQLITE_OK) {

            NSLog(@"Error al abrir la base de datos: %s", sqlite3_errmsg(_baseDatos));

        } else {

            NSLog(@"Base de datos abierta correctamente");

        }

    }
}

- (BOOL)abrirBaseDatos {

    if (sqlite3_open([_rutaBaseDatos UTF8String], &_baseDatos) == SQLITE_OK) {
        
        NSLog(@"Base de datos abierta correctamente");
        return YES;
    
    } else {
    
        NSLog(@"Error al abrir la base de datos: %s", sqlite3_errmsg(_baseDatos));
        return NO;
    
    }
}

- (BOOL)cerrarBaseDatos {

    if (sqlite3_close(_baseDatos) == SQLITE_OK) {
        
        NSLog(@"Base de datos cerrada correctamente");
        return YES;
        
    } else {
    
        NSLog(@"Error al cerrar la base de datos: %s", sqlite3_errmsg(_baseDatos));
        return NO;
    
    }
}

- (NSArray *)ejecutarConsulta:(NSString *)sentencia {

    NSMutableArray *resultado = [NSMutableArray array];
    sqlite3_stmt *sentenciaPreparada;

    if (sqlite3_prepare_v2(_baseDatos, [sentencia UTF8String], -1, &sentenciaPreparada, NULL) == SQLITE_OK) {

        while (sqlite3_step(sentenciaPreparada) == SQLITE_ROW) {

            // obtener el número de columnas de la consulta
            int numeroColumnas = sqlite3_column_count(sentenciaPreparada);

            // crear un diccionario para almacenar los datos de la fila
            NSMutableDictionary *fila = [NSMutableDictionary dictionary];

            for (int i = 0; i < numeroColumnas; i++) {

                // obtener el nombre de la columna
                NSString *nombreColumna = [NSString stringWithUTF8String:sqlite3_column_name(sentenciaPreparada, i)];

                // obtener el tipo de la columna
                int tipoColumna = sqlite3_column_type(sentenciaPreparada, i);

                // obtener el valor de la columna
                switch (tipoColumna) {
                    case SQLITE_INTEGER:
                        fila[nombreColumna] = [NSNumber numberWithInt:sqlite3_column_int(sentenciaPreparada, i)];
                        break;
                    case SQLITE_FLOAT:
                        fila[nombreColumna] = [NSNumber numberWithFloat:sqlite3_column_double(sentenciaPreparada, i)];
                        break;
                    case SQLITE_TEXT:
                        fila[nombreColumna] = [NSString stringWithUTF8String:(char *)sqlite3_column_text(sentenciaPreparada, i)];
                        break;
                    case SQLITE_BLOB:
                        fila[nombreColumna] = [NSData dataWithBytes:(void *)sqlite3_column_blob(sentenciaPreparada, i) length:sqlite3_column_bytes(sentenciaPreparada, i)];
                        break;
                    default:
                        fila[nombreColumna] = [NSNull null];
                        break;
                }
            }

            // agregar la fila al resultado
            [resultado addObject:fila];
        }

        sqlite3_finalize(sentenciaPreparada);
    } else {
        NSLog(@"Error al ejecutar la consulta: %s", sqlite3_errmsg(_baseDatos));
    }

    return resultado;
}

- (BOOL)ejecutarSentencia:(NSString *)sentencia {

    BOOL exito = NO;

    sqlite3_stmt *sentenciaPreparada;

    if (sqlite3_prepare_v2(_baseDatos, [sentencia UTF8String], -1, &sentenciaPreparada, NULL) == SQLITE_OK) {

        if (sqlite3_step(sentenciaPreparada) == SQLITE_DONE) {
            exito = YES;
        }

        sqlite3_finalize(sentenciaPreparada);
    } else {
        NSLog(@"Error al ejecutar la sentencia: %s", sqlite3_errmsg(_baseDatos));
    }

    return exito;
}

- (NSInteger)obtenerUltimoIDInsertado {
    return sqlite3_last_insert_rowid(_baseDatos);
}

@end
```
Este código crea una clase llamada GestorBaseDatos que se encarga de gestionar el acceso a una base de datos SQLite. La clase incluye métodos para inicializar la conexión con la base de datos, abrir la base de datos, cerrar la base de datos, ejecutar una consulta y ejecutar una sentencia. También incluye un método para obtener el último ID insertado en la base de datos.

El método `inicializarConexion` inicializa la conexión con la base de datos. Si la base de datos no existe, la crea. El método `abrirBaseDatos` abre la base de datos. El método `cerrarBaseDatos` cierra la base de datos. El método `ejecutarConsulta` ejecuta una consulta en la base de datos y devuelve un array con los resultados. El método `ejecutarSentencia` ejecuta una sentencia en la base de datos y devuelve un valor booleano que indica si la operación se realizó correctamente. El método `obtenerUltimoIDInsertado` devuelve el último ID insertado en la base de datos.