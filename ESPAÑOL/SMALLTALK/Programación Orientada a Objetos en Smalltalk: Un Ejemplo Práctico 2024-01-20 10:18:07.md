```smalltalk
# Clase Persona
Persona subclass: #Persona [
    | nombre apellido edad domicilio telefono |

    # inicializador
    initialize: unNombre unApellido unaEdad unDomicilio unTelefono [
        nombre := unNombre.
        apellido := unApellido.
        edad := unaEdad.
        domicilio := unDomicilio.
        telefono := unTelefono
    ].

    # accesores
    nombre [ ^nombre ]
    nombre: unNombre [ nombre := unNombre ]

    apellido [ ^apellido ]
    apellido: unApellido [ apellido := unApellido ]

    edad [ ^edad ]
    edad: unaEdad [ edad := unaEdad ]

    domicilio [ ^domicilio ]
    domicilio: unDomicilio [ domicilio := unDomicilio ]

    telefono [ ^telefono ]
    telefono: unTelefono [ telefono := unTelefono ]

    # métodos de instancia
    presentarse [
        "Imprime el nombre, apellido y edad de la persona."
        Transcript show: 'Mi nombre es ', nombre.
        Transcript cr.
        Transcript show: 'Mi apellido es ', apellido.
        Transcript cr.
        Transcript show: 'Tengo ', edad, ' años.'
        Transcript cr
    ].

    llamar: unNumero [
        "Llama al número de teléfono especificado."
        Transcript show: 'Llamando al número ', unNumero.
        Transcript cr
    ]
].

# Clase Empleado
Empleado subclass: #Empleado [
    | ocupacion salario jefe |

    # inicializador
    initialize: unNombre unApellido unaEdad unDomicilio unTelefono
              unaOcupacion unSalario unJefe [
        super initialize: unNombre unApellido unaEdad unDomicilio unTelefono.
        ocupacion := unaOcupacion.
        salario := unSalario.
        jefe := unJefe
    ].

    # accesores
    ocupacion [ ^ocupacion ]
    ocupacion: unaOcupacion [ ocupacion := unaOcupacion ]

    salario [ ^salario ]
    salario: unSalario [ salario := unSalario ]

    jefe [ ^jefe ]
    jefe: unJefe [ jefe := unJefe ]

    # métodos de instancia
    trabajar [
        "Imprime un mensaje indicando que el empleado está trabajando."
        Transcript show: 'Estoy trabajando en ', ocupacion.
        Transcript cr
    ].

    recibirSalario [
        "Imprime un mensaje indicando que el empleado ha recibido su salario."
        Transcript show: 'He recibido mi salario de ', salario, ' pesos.'
        Transcript cr
    ]
].

# Clase Gerente
Gerente subclass: #Gerente [
    | subordinados |

    # inicializador
    initialize: unNombre unApellido unaEdad unDomicilio unTelefono
              unaOcupacion unSalario unJefe unosSubordinados [
        super initialize: unNombre unApellido unaEdad unDomicilio unTelefono unaOcupacion unSalario unJefe.
        subordinados := unosSubordinados
    ].

    # accesores
    subordinados [ ^subordinados ]
    subordinados: unosSubordinados [ subordinados := unosSubordinados ]

    # métodos de instancia
    asignarTarea: unaTarea a: unEmpleado [
        "Asigna una tarea a un empleado."
        Transcript show: 'Asigno la tarea ', unaTarea, ' al empleado ', unEmpleado.
        Transcript cr
    ].

    evaluarDesempeño: unEmpleado [
        "Evalúa el desempeño de un empleado."
        Transcript show: 'Evalúo el desempeño del empleado ', unEmpleado.
        Transcript cr
    ].
].

# Instanciar algunos objetos
juan := Persona new initialize: 'Juan' 'Perez' 25 'Calle 123' '456-7890'.
maria := Persona new initialize: 'María' 'Gómez' 30 'Calle 456' '987-6543'.
pedro := Empleado new initialize: 'Pedro' 'Rodríguez' 35 'Calle 789' '123-4567' 'Programador' 100000 juan.
ana := Gerente new initialize: 'Ana' 'Fernández' 40 'Calle 101112' '234-5678' 'Gerente' 150000 pedro juan maria.

# Invocar algunos métodos
juan presentarse.
maria presentarse.
pedro trabajar.
pedro recibirSalario.
ana asignarTarea: 'Desarrollar un nuevo módulo' a: pedro.
ana evaluarDesempeño: pedro.
```