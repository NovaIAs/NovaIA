**Diagrama de Casos de Uso**

* **Actores:**
    * Administrador
    * Cliente
    * Invitado
* **Casos de Uso:**
    * Registrarse
    * Iniciar Sesión
    * Cerrar Sesión
    * Buscar Productos
    * Ver Detalles del Producto
    * Agregar al Carrito
    * Eliminar del Carrito
    * Realizar Pedido
    * Rastrear Pedido
    * Cancelar Pedido
    * Devolver Producto
    * Obtener Reembolso

**Diagrama de Clases**

* **Clases:**
    * Usuario
    * Cliente
    * Administrador
    * Producto
    * Categoría
    * Pedido
    * Artículo del Pedido
    * Dirección
    * Tarjeta de Crédito
    * Historial de Pedidos
* **Relaciones:**
    * Usuario → Cliente
    * Usuario → Administrador
    * Producto → Categoría
    * Pedido → Cliente
    * Pedido → Artículo del Pedido
    * Artículo del Pedido → Producto
    * Dirección → Cliente
    * Tarjeta de Crédito → Cliente
    * Historial de Pedidos → Pedido

**Diagrama de Secuencia**

* **Escenario:** Un cliente realiza un pedido.
    * Actor: Cliente
    * Objetos: Cliente, Producto, Pedido, Artículo del Pedido, Dirección, Tarjeta de Crédito
    * Mensajes:
        * Cliente → Producto: Obtener Detalles del Producto
        * Cliente → Pedido: Crear Pedido
        * Pedido → Artículo del Pedido: Agregar Artículo al Pedido
        * Cliente → Dirección: Obtener Dirección
        * Pedido → Dirección: Establecer Dirección de Envío
        * Cliente → Tarjeta de Crédito: Obtener Tarjeta de Crédito
        * Pedido → Tarjeta de Crédito: Establecer Tarjeta de Crédito de Pago
        * Pedido → Cliente: Confirmar Pedido

**Diagrama de Estado**

* **Objeto:** Pedido
    * Estados:
        * Pendiente
        * Procesando
        * Enviado
        * Entregado
        * Cancelado
        * Devuelto
        * Reembolsado
    * Transiciones:
        * Pendiente → Procesando
        * Procesando → Enviado
        * Enviado → Entregado
        * Enviado → Cancelado
        * Entregado → Devuelto
        * Devuelto → Reembolsado

**Diagrama de Actividades**

* **Actividad:** Procesar Pedido
    * Pasos:
        * Recibir Pedido
        * Verificar Información del Pedido
        * Preparar Productos para el Envío
        * Enviar Productos al Cliente
        * Actualizar Estado del Pedido a Enviado

**Diagrama de Componentes**

* **Componentes:**
    * Interfaz de Usuario
    * Lógica de Negocios
    * Acceso a Datos
    * Seguridad
    * Pruebas
* **Dependencias:**
    * Interfaz de Usuario → Lógica de Negocios
    * Lógica de Negocios → Acceso a Datos
    * Lógica de Negocios → Seguridad
    * Lógica de Negocios → Pruebas
    * Acceso a Datos → Seguridad
    * Pruebas → Lógica de Negocios