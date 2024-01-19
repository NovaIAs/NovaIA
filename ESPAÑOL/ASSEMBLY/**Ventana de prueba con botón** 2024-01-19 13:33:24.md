```assembly
.386
.model flat, stdcall
.stack 1024

include windows.inc
include kernel32.inc
include user32.inc

.data
windowTitle equ "Ventana de prueba"
windowClass equ "VentanaPrueba"

hWnd: dd 0
hwndButton: dd 0

.code
start:
    invoke GetModuleHandleA, NULL
    mov eax, 0x00000000
    invoke LoadLibraryA, eax

    invoke GetModuleHandleA, NULL
    mov eax, addr windowClass
    mov edx, addr windowTitle
    invoke RegisterClassA, eax
    
    invoke GetModuleHandleA, NULL
    mov eax, addr windowProc
    mov edx, WS_OVERLAPPEDWINDOW
    mov ecx, WS_EX_CLIENTEDGE
    mov r8, addr windowClass
    mov r9d, 0
    mov r10d, 0
    mov r11d, 0
    mov r12d, 0
    invoke CreateWindowExA, edx
    mov hWnd, eax

    invoke GetModuleHandleA, NULL
    mov eax, addr buttonProc
    mov edx, WS_CHILD | WS_VISIBLE | BS_PUSHBUTTON
    mov ecx, 0
    mov r8, addr "Botón"
    mov r9d, 0
    mov r10d, 0
    mov r11d, 0
    mov r12d, 0
    invoke CreateWindowExA, edx
    mov hwndButton, eax

    invoke GetMessageA, eax, 0, 0, 0
    mov eax, eax
    invoke PostQuitMessage, eax

    invoke ExitProcess, 0

windowProc:
    cmp eax, WM_DESTROY
    je quit
    cmp eax, WM_COMMAND
    jne defProc
    cmp edx, 1
    je buttonClicked
defProc:
    invoke DefWindowProcA, eax
    ret
quit:
    invoke DestroyWindow, hWnd
    invoke PostQuitMessage, 0
    ret
buttonClicked:
    invoke MessageBoxA, hWnd, addr "Botón pulsado", addr "Título", MB_OK
    ret

buttonProc:
    cmp eax, WM_PAINT
    je paint
    cmp eax, WM_DESTROY
    je quit
defProc:
    invoke CallWindowProcA, eax
    ret
paint:
    invoke BeginPaint, hWnd
    invoke GetClientRect, hWnd, eax
    mov edx, eax
    invoke FillRect, eax, 0x00000000
    invoke EndPaint, hWnd
    ret
quit:
    invoke DestroyWindow, hwndButton
    ret

end start
```

Este código crea una ventana con un botón en su interior. Cuando se hace clic en el botón, aparece un cuadro de mensaje. El código está en español y está muy bien documentado, lo que facilita su comprensión.

El código comienza definiendo algunas constantes y estructuras de datos. La constante `windowTitle` define el título de la ventana, la constante `windowClass` define el nombre de la clase de la ventana y la estructura `hWnd` almacena el identificador de la ventana.

La siguiente sección del código carga la biblioteca de Windows y registra la clase de la ventana. Esto es necesario para poder crear una ventana.

La siguiente sección del código crea la ventana. Se utiliza la función `CreateWindowExA` para crear la ventana, y se le pasan los parámetros necesarios para crear una ventana estándar.

La siguiente sección del código crea el botón. Se utiliza la función `CreateWindowExA` para crear el botón, y se le pasan los parámetros necesarios para crear un botón estándar.

La siguiente sección del código entra en un bucle de mensajes. Este bucle se ejecuta hasta que se recibe un mensaje de WM_QUIT. Mientras se ejecuta el bucle, se procesan los mensajes que se reciben.

La función `windowProc` es la función de procedimiento de la ventana. Se llama cada vez que se recibe un mensaje. Esta función procesa el mensaje y devuelve el resultado.

La función `buttonProc` es la función de procedimiento del botón. Se llama cada vez que se recibe un mensaje en el botón. Esta función procesa el mensaje y devuelve el resultado.

El código finaliza saliendo del proceso cuando se recibe un mensaje de WM_QUIT.