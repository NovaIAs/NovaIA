```assembly
title Desktop Assembly Program

include Irvine32.inc

.data
szTitle BYTE "This Program Demonstrates ASCII Manipulation",0
szText  BYTE "Please enter a word and I'll ",  0
szText2 BYTE "append .com to it.",            0
szIn1   BYTE 80 DUP(0)                  ; Buffer for input
szTemp1 BYTE 80 DUP(0)                  ; Buffer for temporary results
szIn2   BYTE 80 DUP(0)                  ; Buffer for input
szTemp2 BYTE 80 DUP(0)                  ; Buffer for temporary results
szOut1  BYTE 80 DUP(0)                  ; Buffer for output
szOut2  BYTE 80 DUP(0)                  ; Buffer for output
szDo     BYTE "Do it again? (Y/N):",13,10,0

;---------------------------------------------------------------------
begin:
    call ClrScr                             ; Clear screen
    call VisCursorOff                       ; Do not display the cursor
    lea edi, szTitle                         ; EDI points to the title string
    call DisplayString                      ; Display title string

    lea edi, szText                           ; Display the prompt
    call DisplayString
    call InputString                         ; Read string from keyboard
    lea ecx, szIn1                           ; ECX points to the string buffer
    mov cl, 255                               ; Set the string length to 255
    lea edi, szTemp1                          ; EDI points to the temporary buffer
    call CharsToLongs                        ; Convert In1 to numbers
    lea ecx, szTemp1                          ; ECX points to the temporary buffer
    mov cl, 80                                ; Set the string length to 80 bytes
    lea edi, szOut1                           ; EDI points to the output buffer
    call LongsToChars                        ; Convert numbers to ASCII characters
    lea edi, szOut1                           ; Display the ASCII string
    call DisplayString
    lea edi, szText2                          ; Display additional text
    call DisplayString

    lea edi, szTemp1                          ; EDI points to the temporary buffer
    mov cl, 80                                ; Set the string length to 80 bytes
    call SpaceItOut                           ; Remove non-alpha characters
    lea edi, szTemp1                          ; EDI points to the temporary buffer
    mov cl, 80                                ; Set the string length to 80 bytes
    lea edi, szTemp2                          ; EDI points to the temporary buffer
    call AddComExtension                    ; Remove leading spaces and add ".com"
    lea ecx, szTemp2                          ; ECX points to the temporary buffer
    mov cl, 80                                ; Set the string length to 80 bytes
    lea edi, szOut2                           ; EDI points to the output buffer
    call LongsToChars                        ; Convert numbers to ASCII characters
    lea edi, szOut2                           ; Display the ASCII string
    call DisplayString

    lea edi, szDo                              ; Display "Do it again? (Y/N)"
    call DisplayString
    call InputCharacter                       ; Get response from user
    call ShiftAndRotate                        ; Rotate input character right
    cmp al, 'y'                                ; Check if response is 'Y' or 'y'
    jne Exit                                   ; If not, exit the program

    jmp begin                                  ; Otherwise, start over

Exit:
    mov eax, 0                                ; Terminate program
    call ExitProcess                          ; Exit the program

;---------------------------------------------------------------------
; Procedures
;---------------------------------------------------------------------
; CharsToLongs
;   Converts ASCII characters to numeric values.  (1 byte -> 4 bytes)
;   Parameters: ESI - Pointer to source ASCII string
;               EDI - Pointer to destination numeric array
;               ECX - Length of each string
;---------------------------------------------------------------------
CharsToLongs proc
    push ebx                                  ; Save EAX and EBX registers
    push eax
    xor ebx, ebx                                ; EBX = 0 (used as an index)
    mov edx, ecx                                ; Save length of string
    rep movsb                                  ; Copy characters to temporary buffer

    xor ebx, ebx                                ; EBX = 0 (used as an index)
    mov ecx, edx                                ; ECX = Length of string
    rep movsd                                  ; Copy ASCII codes to array
    pop eax                                   ; Restore EAX and EBX registers
    pop ebx
    ret                                       ; Return to calling program
CharsToLongs endp

;---------------------------------------------------------------------
; LongsToChars
;   Converts numeric values to ASCII characters.  (4 bytes -> 1 byte)
;   Parameters: ESI - Pointer to source numeric array
;               EDI - Pointer to destination ASCII string
;               ECX - Length of each string
;---------------------------------------------------------------------
LongsToChars proc
    push ebx                                  ; Save EAX and EBX registers
    push eax
    xor ebx, ebx                                ; EBX = 0 (used as an index)
    mov edx, ecx                                ; Save length of string
    rep movsd                                  ; Copy ASCII codes to array

    xor ebx, ebx                                ; EBX = 0 (used as an index)
    mov ecx, edx                                ; ECX = Length of string
    rep movsb                                  ; Copy characters to temporary buffer
    pop eax                                   ; Restore EAX and EBX registers
    pop ebx
    ret                                       ; Return to calling program
LongsToChars endp

;---------------------------------------------------------------------
; VisCursorOff
;   Turns off the cursor.
;---------------------------------------------------------------------
VisCursorOff proc                            ; Turns off the cursor
    mov ax, 1                                  ; Set AX = 1
    mov bh, 0                                  ; Set BH = 0
    mov cx, 0                                  ; Set CX = 0
    int 10h                                   ; Call BIOS
    ret                                       ; Return to calling program
VisCursorOff endp

;---------------------------------------------------------------------
; InputString
;   Reads a string from the keyboard, up to the specified length.
;   Parameters: EDI - Pointer to buffer to store input
;               ECX - Length of buffer
;---------------------------------------------------------------------
InputString proc
    push esi                                  ; Save ESI register
    mov esi, edi                                ; ESI points to buffer
    call DisplayCursor                          ; Display the cursor
    mov ah, 0ah                                ; Get ASCII code from keyboard
    mov dx, INPUT_BUFFER                       ; Set DX = Input buffer address
    int 21h                                   ; Call DOS API to get ASCII code
    mov [esi], al                               ; Save ASCII code into buffer
    inc esi                                   ; Increment pointer to buffer
    cmp al, 13                                 ; Check for Carriage Return
    jne InputString                             ; If not CR, loop back
    call RemoveTrailingSpaces                    ; Remove trailing spaces
    dec esi                                   ; Back up pointer to buffer
    mov byte ptr [esi], 0                       ; Set last character to zero (null)
    call HideCursor                             ; Hide the cursor
    pop esi                                   ; Restore ESI register
    ret                                       ; Return to calling program
InputString endp

;---------------------------------------------------------------------
; InputCharacter
;   Reads a character from the keyboard.
;   Returns: AL contains the ASCII code of the input character.
;---------------------------------------------------------------------
InputCharacter proc
    call DisplayCursor                          ; Display the cursor
    mov ah, 0ah                                ; Get ASCII code from keyboard
    mov dx, INPUT_BUFFER                       ; Set DX = Input buffer address
    int 21h                                   ; Call DOS API to get ASCII code
    mov al, [INPUT_BUFFER]                      ; Move character from buffer to AL
    call HideCursor                             ; Hide the cursor
    ret                                       ; Return to calling program
InputCharacter endp

;---------------------------------------------------------------------
; DisplayString
;   Displays a string on the screen.
;   Parameters: EDI - Pointer to string to display
;---------------------------------------------------------------------
DisplayString proc
    push esi                                  ; Save ESI register
    mov esi, edi                                ; ESI points to string
    mov ah, 09h                                ; Display characters with AH=09h
    mov dx, esi                                ; Set DX = Pointer to string
    int 21h                                   ; Call DOS API to display string
    pop esi                                   ; Restore ESI register
    ret                                       ; Return to calling program
DisplayString endp

;---------------------------------------------------------------------
; DisplayCursor
;   Displays the cursor on the screen.
;---------------------------------------------------------------------
DisplayCursor proc
    mov ah, 01h                                ; Set cursor position with AH=01h
    mov cx, 0                                  ; Set CX = Cursor row
    mov dx, 0                                  ; Set DX = Cursor column
    int 10h                                   ; Call BIOS to display cursor
    ret                                       ; Return to calling program
DisplayCursor endp

;---------------------------------------------------------------------
; HideCursor
;   Hides the cursor on the screen.
;---------------------------------------------------------------------
HideCursor proc
    mov ah, 01h                                ; Set cursor position with AH=01h
    mov cx, 25                                 ; Set CX = Cursor row (off screen)
    mov dx, 0                                  ; Set DX = Cursor column
    int 10h                                   ; Call BIOS to display cursor
    ret                                       ; Return to calling program
HideCursor endp

;---------------------------------------------------------------------
; ClrScr
;   Clears the screen.
;---------------------------------------------------------------------
ClrScr proc
    mov ah, 06h                                ; Set AH = 06h for Clear Screen
    mov al, 00h                                ; Set AL = 00h for entire screen
    int 10h                                   ; Call the BIOS to clear the screen
    ret                                       ; Return to calling program
ClrScr endp

;---------------------------------------------------------------------
; SpaceItOut
;   Removes non-alpha characters from a string.
;   Parameters: EDI - Pointer to string to process
;               ECX - Length of string
;---------------------------------------------------------------------
SpaceItOut proc
    push esi                                  ; Save ESI register
    mov esi, edi                                ; ESI points to buffer
    xor ebx, ebx                                ; Set EBX = 0 (used as an index)
    mov ecx, DWORD PTR [esi + ebx]              ; Get string length
    dec ecx                                   ; Subtract 1 (null terminator)
    mov edx, ecx                                ; Save string length
    inc esi                                   ; Move pointer to first character
    repne scasb                                 ; Search for non-alpha characters
    not ecx                                   ; Take the one's complement of ECX
    and ecx, edx                                ; Mask off any characters before first non-alpha
    mov esi, edi                                ; Move pointer back to start of string
    add esi, ecx                                ; Move pointer to first non-alpha character
    push edx                                   ; Save string length
    dec ecx                                   ; Subtract 1 (null terminator)
    mov edx, ecx                                ; Save string length
    mov ecx, DWORD PTR [esi + ebx]              ; Get string length
    dec ecx                                   ; Subtract 1 (null terminator)
    rep movsb                                  ; Copy characters to the end of the string
    mov DWORD PTR [esi + ebx], ecx              ; Set new string length
    mov esi, edi                                ; Move pointer back to start of string
    pop edx                                   ; Restore string length
    mov DWORD PTR [esi + edx], 0                ; Set null terminator
    pop esi                                   ; Restore ESI register
    ret                                       ; Return to calling program
SpaceItOut endp

;---------------------------------------------------------------------
; ShiftAndRotate
;   Shifts and rotates an input character.
;   Parameters: AL - Input character
;   Returns: AL - Shifted and rotated character
;---------------------------------------------------------------------
ShiftAndRotate proc
    rol al, 1                                  ; Rotate AL right 1 bit
    shr al, 1                                  ; Shift AL right 1 bit
    ret                                       ; Return to calling program
ShiftAndRotate endp

;---------------------------------------------------------------------
; RemoveTrailingSpaces
;   Removes trailing spaces from a string.
;   Parameters: EDI - Pointer to string to process
;---------------------------------------------------------------------
RemoveTrailingSpaces proc
    push esi                                  ; Save ESI register
    mov esi, edi                                ; ESI points to string
    mov eax, DWORD PTR [esi + ebx]              ; Get string length
    dec eax                                   ; Subtract 1 (null terminator)
    mov ecx, eax                                ; ECX = String length
    dec esi                                   ; Move pointer to last character
    repne scasb                                 ; Search for non-space characters
    not ecx                                   ; Take the one's complement of ECX
    and ecx, eax                                ; Mask off any characters after last non-space
    mov esi, edi                                ; Move pointer back to start of string
    add esi, ecx                                ; Move pointer to last non-space character
    mov DWORD PTR [esi + ebx], 0                ; Set null terminator
    pop esi                                   ; Restore ESI register
    ret                                       ; Return to calling program
RemoveTrailingSpaces endp

;---------------------------------------------------------------------
; AddString
;   Concatenates two strings.
;   Parameters: EDI - Destination string
;               ESI - Source string
;---------------------------------------------------------------------
AddString proc
    push esi                                  ; Save ESI register
    push edi                                  ; Save EDI register
    mov ecx, DWORD PTR [esi + ebx]              ; Get source string length
    mov edx, DWORD PTR [edi + ebx]              ; Get destination string length
    add ecx, edx                                ; Add the two lengths together
    mov DWORD PTR [edi + ebx], ecx              ; Set new destination string length
    mov esi, edi                                ; Move pointer to destination string
    add esi, edx                                ; Move pointer to end of destination string
    mov edi, esi                                ; Copy source string to end of destination string
    rep movsb                                  ; Copy source string to destination string
    pop edi                                   ; Restore EDI register
    pop esi                                   ; Restore ESI register
    ret                                       ; Return to calling program
AddString endp
```