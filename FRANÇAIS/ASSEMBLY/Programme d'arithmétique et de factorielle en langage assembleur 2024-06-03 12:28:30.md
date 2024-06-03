```assembly
; Déclaration des registres
mov eax, 0
mov ebx, 1
mov ecx, 2
mov edx, 3
mov esi, 4
mov edi, 5

; Calcul d'une expression complexe
mov eax, [ebx]
add eax, [ecx]
sub eax, [edx]
mul eax, [esi]
div eax, [edi]

; Affichage du résultat
mov ebx, eax
mov edx, 4
mov esi, 1
int 80h

; Boucle pour factorielle
mov ebx, 5
fact:
    dec ebx
    cmp ebx, 0
    je end
    mul eax, ebx
    jmp fact
end:

; Affichage du résultat
mov ebx, eax
mov edx, 4
mov esi, 1
int 80h
```

**Explication du code :**

* Le code commence par déclarer les registres utilisés.
* Il calcule ensuite une expression complexe en utilisant les opérations arithmétiques add, sub, mul et div. Le résultat est stocké dans le registre eax.
* Le résultat est ensuite affiché à l'écran en utilisant l'interruption 80h.
* Le code calcule ensuite la factorielle de 5 en utilisant une boucle.
* Le résultat est ensuite affiché à l'écran en utilisant l'interruption 80h.