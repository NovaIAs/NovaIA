```assembly
.data
welcome_msg: .asciz "Welcome to the world of assembly programming!"
new_line: .asciz "\n"

.text
.global main
main:
  # Print the welcome message
  mov rax, 1  # System call number for write
  mov rdi, 1  # File descriptor for stdout
  mov rsi, welcome_msg  # Address of the welcome message
  mov rdx, 36  # Length of the welcome message
  syscall

  # Print a new line
  mov rax, 1  # System call number for write
  mov rdi, 1  # File descriptor for stdout
  mov rsi, new_line  # Address of the new line
  mov rdx, 1  # Length of the new line
  syscall

  # Exit the program with status 0
  mov rax, 60  # System call number for exit
  mov rdi, 0  # Exit status
  syscall

# System call definitions
.equ SYS_WRITE, 1
.equ SYS_EXIT, 60
```

Explanation:

* The `.data` section contains data. In this case, it contains two strings: "Welcome to the world of assembly programming!" and "\n". The `.text` section contains code. The `.global main` directive makes the `main` function visible to the linker.
* The `main` function is the entry point of the program. It prints the welcome message and a new line to the standard output, and then exits the program with status 0.
* The `mov` instruction moves data from one location to another. For example, `mov rax, 1` moves the value 1 into the register `rax`.
* The `syscall` instruction invokes a system call. The system call number is specified in the `rax` register, and the arguments to the system call are specified in the `rdi`, `rsi`, `rdx`, `rcx`, and `r8` registers.
* The `exit` system call exits the program with the status code specified in the `rdi` register. In this case, the status code is 0, which means the program exited successfully.