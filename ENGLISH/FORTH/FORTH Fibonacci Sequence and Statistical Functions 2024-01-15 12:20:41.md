```forth
: FIB ( n -- n-th Fibonacci number )
  dup 0= if drop 0 exit then
  dup 1= if drop 1 exit then
  2dup - FIB FIB + ;

: FIB-TABLE ( n -- n-th Fibonacci number )
  0 FIB 1 FIB ?DO
    I FIB .R LOOP
  DROP ;

: PI ( -- pi (floating) )
  4 3 / * 4 atan ;

: SIN ( -- radian sine )
  PI * / SIN ;

: COS ( -- radian cosine )
  PI * / COS ;

: TAN ( -- radian tangent )
  PI * / TAN ;

: FACTORIAL ( n -- n! )
  0= if 1 exit then
  1 -1 DO I 1 + LOOP * ;

: POWER ( b n -- b^n )
  0= if drop 1 exit then
  dup 0= if drop 0 exit then
  swap 2dup = if drop 1 exit then
  dup 1= if drop b exit then
  over 1 and if drop b-1 POWER b * exit then
  over 2 and if drop b-1 POWER b b * exit then
  POWER b * ;

: ACK ( m n -- ? )
  0= if n exit then
  1=?DO
    dup 0= if 1+ exit then
    dup n < if n Ack n-1 - exit then
    m Ack m-1 - Ack n Ack + LOOP ;

: FIB-TABLE-SUM ( n -- sum of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - ;

: FIB-TABLE-PRODUCT ( n -- product of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE / ;

: FIB-TABLE-MEAN ( n -- mean of Fibonacci numbers up to n )
  1 FIB-TABLE-SUM n / ;

: FIB-TABLE-MEDIAN ( n -- median of Fibonacci numbers up to n )
  2/ FIB-TABLE-SUM ;

: FIB-TABLE-MODE ( n -- mode of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - ;

: FIB-TABLE-RANGE ( n -- range of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - ;

: FIB-TABLE-VARIANCE ( n -- variance of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - SQ ;

: FIB-TABLE-STANDARD-DEVIATION ( n -- standard deviation of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - SQ SQRT ;

: FIB-TABLE-KURTOSIS ( n -- kurtosis of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - 4 POWER FIB-TABLE-VARIANCE / FIB-TABLE-VARIANCE / - 3 ;

: FIB-TABLE-SKEWNESS ( n -- skewness of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - 3 POWER FIB-TABLE-VARIANCE / FIB-TABLE-STANDARD-DEVIATION / - ;

: FIB-TABLE-ENTROPY ( n -- entropy of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / ;

: FIB-TABLE-INFORMATION-GAIN ( n -- information gain of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / - ;

: FIB-TABLE-MUTUAL-INFORMATION ( n -- mutual information of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 / ;

: FIB-TABLE-CONDITIONAL-ENTROPY ( n -- conditional entropy of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 - ;

: FIB-TABLE-JOINT-ENTROPY ( n -- joint entropy of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 + ;

: FIB-TABLE-COPULA ( n -- copula of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 ;

: FIB-TABLE-RANK-CORRELATION ( n -- rank correlation of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 - ;

: FIB-TABLE-PEARSON-CORRELATION ( n -- Pearson correlation of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 + ;

: FIB-TABLE-SPEARMAN-CORRELATION ( n -- Spearman correlation of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 - 2 / ;

: FIB-TABLE-KENDALL-CORRELATION ( n -- Kendall correlation of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 + 2 / ;

: FIB-TABLE-TAU-CORRELATION ( n -- tau correlation of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 + ;

: FIB-TABLE-GAMMA-CORRELATION ( n -- gamma correlation of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 - ;

: FIB-TABLE-CHI-SQUARED-CORRELATION ( n -- chi-squared correlation of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 + 2 / ;

: FIB-TABLE-CONTINGENCY-COEFFICIENT ( n -- contingency coefficient of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 + ;

: FIB-TABLE-PHI-CORRELATION ( n -- phi correlation of Fibonacci numbers up to n )
  1 FIB-TABLE 0 FIB-TABLE - LOG LN 1 / 2 - 2 / ;

: FIB-TABLE-TETRACHORIC-CORRELATION ( n -- tetr
```