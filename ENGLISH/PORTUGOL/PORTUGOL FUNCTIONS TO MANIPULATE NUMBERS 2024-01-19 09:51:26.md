```portugal
DEFINE P0(N:INTEGER):BOOLEAN;
BEGIN
  IF N=0 THEN FALSE ELSE (N>0)AND ODD(N-1);
END P0;

DEFINE P1(N:INTEGER):BOOLEAN;
BEGIN
  IF N=0 THEN TRUE ELSE (N>0)OR ODD(N-1);
END P1;

DEFINE P2(N:INTEGER):INTEGER;
BEGIN
  IF N=0 THEN 0 ELSE
  IF N=1 THEN 1 ELSE N;
END P2;

DEFINE P3(N:INTEGER):INTEGER;
BEGIN
  IF N=0 THEN 1 ELSE
  IF N=1 THEN 0 ELSE
  IF P1(N) THEN N ELSE 0;
END P3;

DEFINE P4(N:INTEGER):INTEGER;
BEGIN
  IF N=0 THEN 1 ELSE
  IF N=1 THEN 2 ELSE
  IF N=2 THEN 4 ELSE
  IF N=3 THEN 8 ELSE
  IF N=4 THEN 16 ELSE
  IF N=5 THEN 32 ELSE
  IF N=6 THEN 64 ELSE
  IF MOD(N,2)=0 THEN 2*P4(N-1) ELSE 4*P4(N-1);
END P4;

DEFINE P5(N:INTEGER):INTEGER;
BEGIN
  IF N=0 OR N=1 THEN 1 ELSE
  IF MOD(N,2)=0 THEN P5(N/2)*P5(N/2) ELSE
  IF N=3 THEN 1 ELSE 2*P5((N-1)/2)*P5((N-1)/2);
END P5;

DEFINE P6(N:INTEGER):INTEGER;
BEGIN
  IF N=0 THEN 0 ELSE
  IF N=1 THEN 1 ELSE
  IF MOD(N,2)=0 THEN P6(N/2)+1 ELSE
  IF N=3 THEN 2 ELSE 2*P6((N-1)/2)+1;
END P6;

DEFINE P7(N:INTEGER):INTEGER;
BEGIN
  IF N=0 THEN 1 ELSE
  IF N=1 THEN 1 ELSE
  IF N=2 THEN 2 ELSE
  IF N=3 THEN 6 ELSE
  IF N=4 THEN 24 ELSE
  IF N=5 THEN 120 ELSE
  IF MOD(N,2)=0 THEN P7(N/2)*P7(N/2) ELSE
  IF N=7 THEN 5040 ELSE 2*P7((N-1)/2)*P7((N-1)/2);
END P7;

DEFINE P8(N:INTEGER):STRING;
BEGIN
  IF N=0 THEN "ZERO" ELSE
  IF N=1 THEN "UM" ELSE
  IF N=2 THEN "DOIS" ELSE
  IF N=3 THEN "TRES" ELSE
  IF N=4 THEN "QUATRO" ELSE
  IF N=5 THEN "CINCO" ELSE
  IF N=6 THEN "SEIS" ELSE
  IF N=7 THEN "SETE" ELSE
  IF N=8 THEN "OITO" ELSE
  IF N=9 THEN "NOVE" ELSE
  IF N=10 THEN "DEZ" ELSE
  IF N=11 THEN "ONZE" ELSE
  IF N=12 THEN "DOZE" ELSE
  IF N=13 THEN "TREZE" ELSE
  IF N=14 THEN "QUATORZE" ELSE
  IF N=15 THEN "QUINZE" ELSE
  IF N=16 THEN "DEZESSEIS" ELSE
  IF N=17 THEN "DEZESSETE" ELSE
  IF N=18 THEN "DEZOITO" ELSE
  IF N=19 THEN "DEZENOVE" ELSE
  IF N=20 THEN "VINTE" ELSE
  IF N<30 THEN P8(N-10)+" E "+P8(10) ELSE
  IF N<40 THEN "TRINTA "+P8(N-30) ELSE
  IF N<50 THEN "QUARENTA "+P8(N-40) ELSE
  IF N<60 THEN "CINQUENTA "+P8(N-50) ELSE
  IF N<70 THEN "SESSENTA "+P8(N-60) ELSE
  IF N<80 THEN "SETENTA "+P8(N-70) ELSE
  IF N<90 THEN "OITENTA "+P8(N-80) ELSE
  IF N<100 THEN "NOVENTA "+P8(N-90) ELSE
  IF N<200 THEN "CENTO E "+P8(N-100) ELSE
  IF N<300 THEN "DUZENTOS "+P8(N-200) ELSE
  IF N<400 THEN "TREZENTOS "+P8(N-300) ELSE
  IF N<500 THEN "QUATROCENTOS "+P8(N-400) ELSE
  IF N<600 THEN "QUINHENTOS "+P8(N-500) ELSE
  IF N<700 THEN "SEISCENTOS "+P8(N-600) ELSE
  IF N<800 THEN "SETECENTOS "+P8(N-700) ELSE
  IF N<900 THEN "OITOCENTOS "+P8(N-800) ELSE
  IF N<1000 THEN "NOVECENTOS "+P8(N-900) ELSE
  IF N<2000 THEN "HUM MIL "+P8(N-1000) ELSE
  IF N<3000 THEN "DOIS MIL "+P8(N-2000) ELSE
  IF N<4000 THEN "TRES MIL "+P8(N-3000) ELSE
  IF N<5000 THEN "QUATRO MIL "+P8(N-4000) ELSE
  IF N<6000 THEN "CINCO MIL "+P8(N-5000) ELSE
  IF N<7000 THEN "SEIS MIL "+P8(N-6000) ELSE
  IF N<8000 THEN "SETE MIL "+P8(N-7000) ELSE
  IF N<9000 THEN "OITO MIL "+P8(N-8000) ELSE
  IF N<10000 THEN "NOVE MIL "+P8(N-9000) ELSE P8(N/1000)+" MIL "+P8(N-N/1000*1000);
END P8;

DEFINE P9(N:STRING):STRING;
BEGIN
  N:=LRT(N);
  IF SUBS(N,1,3)="UM" THEN "ZERO" ELSE
  IF SUBS(N,1,4)="ZERO" THEN "NOVE" ELSE
  IF SUBS(N,1,4)="SEIS" THEN "TRES" ELSE
  IF SUBS(N,1,5)="TRINTA" THEN "SETE" ELSE
  IF SUBS(N,1,5)="QUATRO" THEN "UM" ELSE
  IF SUBS(N,1,4)="QUATRO" THEN "CINCO" ELSE
  IF SUBS(N,1,6)="DEZESSEIS" THEN "OITO" ELSE
  IF SUBS(N,1,9)="ONZE" THEN "DOIS" ELSE
  IF SUBS(N,1,6)="DEZESSEIS" THEN "DOIS" ELSE
  IF SUBS(N,1,6)="TRINTA" THEN "SEIS" ELSE
  IF SUBS(N,1,9)="DEZENOVE" THEN "QUATRO" ELSE
  IF SUBS(N,1,6)="DEZESSEIS" THEN "SETE" ELSE
  IF SUBS(N,1,7)="TRINTA" THEN "TRES" ELSE
  IF SUBS(N,1,8)="VINTE" THEN "NOVE" ELSE
  IF SUBS(N,1,5)="VINTE" THEN "OITO" ELSE
  IF SUBS(N,1,8)="SESSENTA" THEN "CINCO" ELSE
  IF SUBS(N,1,6)="TRINTA" THEN "QUATRO" ELSE N;
END P9;

DEFINE P10(N:STRING):INTEGER;
BEGIN
  N:=LRT(N);
  IF SUBS(N,1,3)="UM" THEN 0 ELSE
  IF SUBS(N,1,4)="ZERO" THEN 9 ELSE
  IF SUBS(N,1,4)="SEIS" THEN 3 ELSE
  IF SUBS(N,1,5)="TRINTA" THEN 7 ELSE
  IF SUBS(N,1,5)="QUATRO" THEN 1 ELSE
  IF SUBS(N,1,4)="QUATRO" THEN 5 ELSE
  IF SUBS(N,1,6)="DEZESSEIS" THEN 8 ELSE
  IF SUBS(N,1,9)="ONZE" THEN 2 ELSE
  IF SUBS(N,1,6)="DEZESSEIS" THEN 2 ELSE
  IF SUBS(N,1,6)="TRINTA" THEN 6 ELSE
  IF SUBS(N,1,9)="DEZENOVE" THEN 4 ELSE
  IF SUBS(N,1,6)="DEZESSEIS" THEN 7 ELSE
  IF SUBS(N,1,7)="TRINTA" THEN 3 ELSE
  IF SUBS(N,1,8)="VINTE" THEN 9 ELSE
  IF SUBS(N,1,5)="VINTE" THEN 8 ELSE
  IF SUBS(N,1,8)="SESSENTA" THEN 5 ELSE
  IF SUBS(N,1,6)="TRINTA" THEN 4 ELSE -1;
END P10;
```