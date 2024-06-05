**Fonction de tri à bulles en français**

```assembly
.data
  arr: .array 10, 20, 30, 40, 50, 60, 70, 80, 90, 100
  len: .int 10

.text
.global main

main:
  # Charger la longueur du tableau dans le registre EAX
  mov eax, [len]

  # Boucle externe : itérer sur les éléments du tableau
  mov ebx, 0
1:
  cmp ebx, eax
  jge 999 # Si ebx >= eax, le tableau est trié

  # Boucle interne : comparer les éléments adjacents et les échanger si nécessaire
  mov ecx, ebx
  add ecx, 1
2:
  cmp ecx, eax
  jge 989 # Si ecx >= eax, la boucle interne est terminée

  # Comparer arr[ecx] et arr[ecx+1]
  mov edx, [arr+ecx*4]
  mov eax, [arr+ecx*4+4]
  cmp eax, edx
  jle 987 # Si arr[ecx] <= arr[ecx+1], les éléments sont dans le bon ordre

  # Échanger arr[ecx] et arr[ecx+1]
  mov ebx, edx
  mov edx, eax
  mov eax, ebx
  mov [arr+ecx*4], eax
  mov [arr+ecx*4+4], edx

  # Incrémenter ecx pour passer à l'élément suivant
987:
  inc ecx
  jmp 2 # Revenir au début de la boucle interne

  # Incrémenter ebx pour passer à l'élément suivant
989:
  inc ebx
  jmp 1 # Revenir au début de la boucle externe

  # Le tableau est trié
999:
  # Afficher le tableau trié
  ; TODO

  # Arrêter le programme
  mov eax, 1
  mov ebx, 0
  int 0x80

```

**Explication**

Cette fonction assemble le code d'une fonction de tri à bulles en français. Elle charge d'abord la longueur du tableau dans le registre EAX. Ensuite, elle utilise une boucle externe pour itérer sur les éléments du tableau et une boucle interne pour comparer et échanger les éléments adjacents s'ils ne sont pas dans le bon ordre. La fonction s'arrête lorsque le tableau est trié.