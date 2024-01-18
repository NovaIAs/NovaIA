```
LOOP:
```

```
MOV CX, 10000
```

```
DEC CX
```

```
CMP CX, 0
```

```
JNE LOOP
```

```
MOV AX, 4C00H
```

```
INT 21H
```

```
LOOP1:
```

```
MOV CX, 640
```

```
INC CX
```

```
CMP CX, 800
```

```
JNE LOOP1
```

```
INT 21H
```

```
JMP $
```

```
END
```
Explanation:
- This program uses the DOS Exit interrupt (21H) to terminate the program.

- The outer loop (LOOP) repeats the inner loop (LOOP1) 10,000 times.

- The inner loop (LOOP1) increments the CX register by 1 and compares it to 800. If CX is not equal to 800, the loop continues.

- After the outer loop has completed, the program executes a file exit interrupt (21H) to terminate the program.

- The JMP $ instruction is used to jump to the end of the program.