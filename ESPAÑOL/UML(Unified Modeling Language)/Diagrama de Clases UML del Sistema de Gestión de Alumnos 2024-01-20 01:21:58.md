```
┌───────────────────────────────────────────────────────────────────────────┐
│                                                                           │
│                                     Sistema de Gestión de Alumnos             │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                                                           │
│                                  Clases Principales                          │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                     Alumno                                  │
│                                                                           │
├───────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  - id: int                                                              │
│  - nombre: string                                                            │
│  - apellido: string                                                          │
│  - fecha_nacimiento: date                                                    │
│  - direccion: string                                                          │
│  - telefono: string                                                          │
│  - correo_electronico: string                                                │
│                                                                           │
├───────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  Métodos:                                                                  │
│                                                                           │
│  - constructor(id: int, nombre: string, apellido: string, fecha_nacimiento: date,│
│    direccion: string, telefono: string, correo_electronico: string)          │
│                                                                           │
│  - getId(): int                                                             │
│  - getNombre(): string                                                         │
│  - getApellido(): string                                                       │
│  - getFechaNacimiento(): date                                                   │
│  - getDireccion(): string                                                     │
│  - getTelefono(): string                                                       │
│  - getCorreoElectronico(): string                                             │
│                                                                           │
│  - setId(id: int): void                                                       │
│  - setNombre(nombre: string): void                                                │
│  - setApellido(apellido: string): void                                             │
│  - setFechaNacimiento(fecha_nacimiento: date): void                               │
│  - setDireccion(direccion: string): void                                          │
│  - setTelefono(telefono: string): void                                            │
│  - setCorreoElectronico(correo_electronico: string): void                        │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                     Profesor                                │
│                                                                           │
├───────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  - id: int                                                              │
│  - nombre: string                                                            │
│  - apellido: string                                                          │
│  - fecha_nacimiento: date                                                    │
│  - direccion: string                                                          │
│  - telefono: string                                                          │
│  - correo_electronico: string                                                │
│  - especialidad: string                                                        │
│                                                                           │
├───────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  Métodos:                                                                  │
│                                                                           │
│  - constructor(id: int, nombre: string, apellido: string, fecha_nacimiento: date,│
│    direccion: string, telefono: string, correo_electronico: string, especialidad: │
│    string)                                                                  │
│                                                                           │
│  - getId(): int                                                             │
│  - getNombre(): string                                                         │
│  - getApellido(): string                                                       │
│  - getFechaNacimiento(): date                                                   │
│  - getDireccion(): string                                                     │
│  - getTelefono(): string                                                       │
│  - getCorreoElectronico(): string                                             │
│  - getEspecialidad(): string                                                    │
│                                                                           │
│  - setId(id: int): void                                                       │
│  - setNombre(nombre: string): void                                                │
│  - setApellido(apellido: string): void                                             │
│  - setFechaNacimiento(fecha_nacimiento: date): void                               │
│  - setDireccion(direccion: string): void                                          │
│  - setTelefono(telefono: string): void                                            │
│  - setCorreoElectronico(correo_electronico: string): void                        │
│  - setEspecialidad(especialidad: string): void                                      │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                   Asignatura                                  │
│                                                                           │
├───────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  - id: int                                                              │
│  - nombre: string                                                            │
│  - descripcion: string                                                         │
│  - creditos: int                                                          │
│  - profesor_id: int                                                        │
│                                                                           │
├───────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  Métodos:                                                                  │
│                                                                           │
│  - constructor(id: int, nombre: string, descripcion: string, creditos: int,   │
│    profesor_id: int)                                                       │
│                                                                           │
│  - getId(): int                                                             │
│  - getNombre(): string                                                         │
│  - getDescripcion(): string                                                    │
│  - getCreditos(): int                                                        │
│  - getProfesorId(): int                                                      │
│                                                                           │
│  - setId(id: int): void                                                       │
│  - setNombre(nombre: string): void                                                │
│  - setDescripcion(descripcion: string): void                                        │
│  - setCreditos(creditos: int): void                                              │
│  - setProfesorId(profesor_id: int): void                                         │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                   Matricula                                   │
│                                                                           │
├───────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  - id: int                                                              │
│  - alumno_id: int                                                          │
│  - asignatura_id: int                                                       │
│  - nota: float                                                           │
│                                                                           │
├───────────────────────────────────────────────────────────────────────────┤
│                                                                           │
│  Métodos:                                                                  │
│                                                                           │
│  - constructor(id: int, alumno_id: int, asignatura_id: int, nota: float)       │
│                                                                           │
│  - getId(): int                                                             │
│  - getAlumnoId(): int                                                        │
│  - getAsignaturaId(): int                                                    │
│  - getNota(): float                                                        │
│                                                                           │
│  - setId(id: int): void                                                       │
│  - setAlumnoId(alumno_id: int): void                                              │
│  - setAsignaturaId(asignatura_id: int): void                                         │
│  - setNota(nota: float): void                                                  │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                                                           │
│                                    Relaciones                                │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                                                           │
│                                  Alumno - Profesor                          │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                                                           │
│  - Un alumno puede tener muchos profesores.                                 │
│                                                                           │
│  - Un profesor puede tener muchos alumnos.                                 │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                                                           │
│                                Alumno - Asignatura                          │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                                                           │
│  - Un alumno puede matricularse en muchas asignaturas.                   │
│                                                                           │
│  - Una asignatura puede tener muchos alumnos matriculados.                 │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                                                           │
│                              Profesor - Asignatura                          │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘

┌───────────────────────────────────────────────────────────────────────────┐
│                                                                           │
│  - Un profesor puede impartir muchas asignaturas.                          │
│                                                                           │
│  - Una asignatura puede ser impartida por muchos profesores.                 │
│                                                                           │
└───────────────────────────────────────────────────────────────────────────┘