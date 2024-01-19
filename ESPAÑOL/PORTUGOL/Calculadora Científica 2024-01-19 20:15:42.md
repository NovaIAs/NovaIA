```portuguol
programa calculadora_cientifica

definir PI=3.14159265

definir função fatorial(n)
    se n=0 ou n=1 então
        retornar(1)
    senão
        retornar(n*fatorial(n-1))
    fimse
fimfunção

definir função potencia(base,expoente)
    se expoente=0 então
        retornar(1)
    senão
        retornar(base*potencia(base,expoente-1))
    fimse
fimfunção

definir função raiz_quadrada(n)
    definir x0=n/2
    definir x1=(x0+n/x0)/2
    enquanto x1!=x0 faça
        definir x0=x1
        definir x1=(x0+n/x0)/2
    fimenquanto
    retornar(x1)
fimfunção

definir função seno(angulo)
    retornar(math.sin(angulo*PI/180))
fimfunção

definir função cosseno(angulo)
    retornar(math.cos(angulo*PI/180))
fimfunção

definir função tangente(angulo)
    retornar(math.tan(angulo*PI/180))
fimfunção

definir função arcseno(x)
    retornar(math.asin(x))
fimfunção

definir função arcocosseno(x)
    retornar(math.acos(x))
fimfunção

definir função arcotangente(x)
    retornar(math.atan(x))
fimfunção

definir função log(x)
    retornar(math.log(x))
fimfunção

definir função log10(x)
    retornar(math.log10(x))
fimfunção

definir função log2(x)
    retornar(math.log2(x))
fimfunção

definir função ln(x)
    retornar(math.log(x))
fimfunção

definir função exp(x)
    retornar(math.exp(x))
fimfunção

definir função abs(x)
    se x<0 então
        retornar(-x)
    senão
        retornar(x)
    fimse
fimfunção

definir função arredondar(x)
    retornar(math.round(x))
fimfunção

definir função truncar(x)
    retornar(math.trunc(x))
fimfunção

definir função maximo(x,y)
    se x>y então
        retornar(x)
    senão
        retornar(y)
    fimse
fimfunção

definir função minimo(x,y)
    se x<y então
        retornar(x)
    senão
        retornar(y)
    fimse
fimfunção

definir função primo(n)
    se n<=1 então
        retornar(falso)
    fimse
    definir i=2
    enquanto i*i<=n faça
        se n%i=0 então
            retornar(falso)
        fimse
        definir i=i+1
    fimenquanto
    retornar(verdadeiro)
fimfunção

definir função gerar_primos(n)
    definir crivo=[verdadeiro,verdadeiro]+[falso]*n
    definir i=2
    enquanto i*i<=n faça
        se crivo[i] então
            definir j=i*i
            enquanto j<=n faça
                definir crivo[j]=falso
                definir j=j+i
            fimenquanto
        fimse
        definir i=i+1
    fimenquanto
    definir primos=[]
    definir i=2
    enquanto i<=n faça
        se crivo[i] então
            adicionar(primos,i)
        fimse
        definir i=i+1
    fimenquanto
    retornar(primos)
fimfunção

definir função fatorial_duplo(n)
    retornar(fatorial(2*n)/fatorial(n)**2)
fimfunção

definir função coeficiente_binomial(n,k)
    retornar(fatorial(n)/fatorial(k)/fatorial(n-k))
fimfunção

definir função permutações(n,k)
    retornar(fatorial(n)/fatorial(n-k))
fimfunção

definir função combinações(n,k)
    retornar(fatorial(n)/fatorial(k)/fatorial(n-k))
fimfunção

definir função distribuição_hipergeométrica(N,K,n,k)
    retornar(combinações(K,k)*combinações(N-K,n-k)/combinações(N,n))
fimfunção

definir função distribuição_binomial(n,p,k)
    retornar(coeficiente_binomial(n,k)*p**k*(1-p)**(n-k))
fimfunção

definir função distribuição_normal(media,desvio_padrão,x)
    retornar((1/(desvio_padrão*math.sqrt(2*PI)))*math.exp(-((x-media)**2)/(2*(desvio_padrão**2))))
fimfunção

definir função distribuição_t_student(graus_de_liberdade,x)
    retornar((math.gamma((graus_de_liberdade+1)/2)/(math.sqrt(graus_de_liberdade*PI)*math.gamma(graus_de_liberdade/2)))*(1+x**2/graus_de_liberdade)**(-(graus_de_liberdade+1)/2))
fimfunção

definir função distribuição_qui_quadrado(graus_de_liberdade,x)
    retornar((1/2**