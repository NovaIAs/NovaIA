**Programme d'analyse de données météorologiques en langage assembleur**

```assembly
; Déclaration des segments
segment data
; Données d'entrée
	.db "donnees_meteo.txt",0
	donnees_meteo db 128 dup(?)
segment code

; Début du programme principal
main:
	; Ouverture du fichier de données
	mov ah,3dh
	mov dx,offset donnees_meteo
	int 21h
	jecxz erreur_ouverture

; Analyse des températures
	mov si,0
	mov cx,128
boucle_températures:
	lodsb
	cmp al,0
	je fin_températures
	cmp al,20
	jbe froide
chaude:
	add [nb_chaudes],1
	jmp fin_boucle
froide:
	add [nb_froides],1
fin_boucle:
	loop boucle_températures
fin_températures:
	; Analyse des précipitations
	mov si,0
	boucle_précipitations:
	lodsb
	cmp al,0
	je fin_précipitations
	cmp al,20
	jbe sec
humide:
	add [nb_humides],1
	jmp fin_boucle
sec:
	add [nb_secs],1
fin_boucle:
	loop boucle_précipitations
fin_précipitations:

; Affichage des résultats
	; Nb de journées chaudes
	mov ah,02h
	mov dl,'Nb de journées chaudes : '
	int 21h
	mov ah,02h
	mov dl,[nb_chaudes]
	add dl,48
	int 21h
	mov ah,02h
	mov dl,'\n'
	int 21h

; Nb de journées froides
	mov ah,02h
	mov dl,'Nb de journées froides : '
	int 21h
	mov ah,02h
	mov dl,[nb_froides]
	add dl,48
	int 21h
	mov ah,02h
	mov dl,'\n'
	int 21h

; Nb de journées humides
	mov ah,02h
	mov dl,'Nb de journées humides : '
	int 21h
	mov ah,02h
	mov dl,[nb_humides]
	add dl,48
	int 21h
	mov ah,02h
	mov dl,'\n'
	int 21h

; Nb de journées sèches
	mov ah,02h
	mov dl,'Nb de journées sèches : '
	int 21h
	mov ah,02h
	mov dl,[nb_secs]
	add dl,48
	int 21h
	mov ah,02h
	mov dl,'\n'
	int 21h

; Fin du programme
	mov ah,4ch
	int 21h

; Gestion d'erreur : Ouverture de fichier
erreur_ouverture:
	mov ah,09h
	mov dx,offset message_erreur
	int 21h
	jmp fin

; Message d'erreur
message_erreur db "Erreur lors de l'ouverture du fichier de données",10,13,'$'

; Variables
nb_chaudes db 0
nb_froides db 0
nb_humides db 0
nb_secs db 0
```

**Explication du code :**

* **Déclaration des segments :** Le code est divisé en deux segments : `data` pour les données et `code` pour le code exécutable.
* **Analyse des températures :** La boucle `boucle_températures` parcourt les octets du fichier de données et analyse chaque octet comme une température. Les températures sont ensuite classées comme chaudes (supérieures à 20) ou froides (inférieures ou égales à 20).
* **Analyse des précipitations :** La boucle `boucle_précipitations` parcourt les octets du fichier de données et analyse chaque octet comme le niveau de précipitations. Les précipitations sont ensuite classées comme humides (supérieures à 20) ou sèches (inférieures ou égales à 20).
* **Affichage des résultats :** Le code utilise la fonction d'écriture sur la console INT 21H pour afficher les résultats de l'analyse.
* **Gestion d'erreur :** Le code gère les erreurs d'ouverture de fichier en affichant un message d'erreur et en terminant le programme.