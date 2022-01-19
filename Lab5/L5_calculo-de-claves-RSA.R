# L5 Sesion de laboratorio claves RSA
#  (Aritmetika Modular: inverso modular)
#  (Teoría de números: factorización de números enteros)

# "numbers" para instalarlo:
# install.packages("numbers")
# "numbers" para cargarlo:
library("numbers")
# ayuda sobre el paquete:
library(help="numbers")

# Para el cálculo de las claves RSA nos interesan las siguientes funciones

# útiles para seleccionar don números primos:
Primes(100000,100020)
nextPrime(100000)
previousPrime(100019)

# Cálculo del inverso modular. 
# se puede obtener utilizando dos funciones del paquete numbers:
# modinv(n,m)  (Modular Inverse)
# Computes the modular inverse of n modulo m.
# para calcular el inverso modulo 64 del número 3:
modinv(3,64)
#  3^-1 mod 64=43
# Fíjate en el código de la función: 
modinv

# El inverso modular se calcula utilizando el algoritmo de Euclides.
# La quinta propiedad de máximo comun divisor 
# (Identidad de Bezout): mcd(a,b)=xa+yb 
# extGCD(a,b) (Extended Euclidean Algorithm)
# Computes the greatest common divisor and solves Bezout's identity.
# calculemos el inverso modular de 3 módulo 64: 3^-1 mod 64=?
extGCD(3,64)
1==-21*3+1*64
# El inverso modular de 3 es el coeficiente que multiplica al 3 en la 
# combinación lineal. mcd(a,b)=xa+yb --> 1==-21*3+1*64 --> x=-21
# en la congruencia de módulo 64, -21+64=43. Por tanto, 3^-1 mod 64=43
-21+64

# Factorización de números (descomposición en números primos):
primeFactors(1448)
primeFactors(1001)

# El softwarte R muestra los nùmeros en formato científico.
# Si queremos evitarlo, podemos desactivar la escritura científica:
a=nextPrime(2634758697353)
a
# 2.634759e+12 --> ¿cuáles son los dígitos?
# Si queremos visualizar todos los dígitos...

format(a, scientific=FALSE)
# "2634758697451"

###############################################################
# Ejercicio 1. Función que calcula las claves del algoritmo RSA.
# dados dos números primos  p y q, la función claves_RSA(p,q) calcula 
# n, r y s (con r elmínimo posble)

# usaremos la función desarrollada en la sesión de laboratorio anterior:
primo_relativo_pequeno <- function(m)
{
  zbki<-2;
  while(GCD(m,zbki)!=1) ## L4 laboko zkh funtzioa erabil daiteke
  {
    zbki<-zbki+1;
  };
  return(zbki);
}

claves_RSA <- function(p,q)
{
  
  
  
 #n=p*q
 #m=(p-1)*(q-1)                     #primera opcion
 #r= primo_relativo_pequeno(m)
 #s= modinv(r,m)
 #cat("clave publica: n= ",n," r=",r,"nclave privada: s=",s)
 
 
 
 
 if (!isPrime(p))         #segunda opcion
 {
   print("le primer parametro(", p ,"que me has pasaddo no es primo. Voy a usar", pr)
   p = pr
 }
 if (!isPrime(q))
 {
   print("le primer parametro(", q ,"que me has pasaddo no es primo. Voy a usar", pr)
   q = pr
 }
 
 n = p*q
 m = (p-1)*(q-1)  # fi(n)
 r = primo_relativo_pequeno(m) #r in Z fi(n)
 s = modinv(r,m)
 
 
 
  return(c(n,r,s))
}

# Pruebas:
# En la hoja de ejercicios de aritmética modular en el 
# ejercicio 9. aparecen estos ejemplos.
# claves_RSA(17,23) aparece resuelto manualmente en los apuntes del algoritmo RSA
v=claves_RSA(5,17)
v
v=claves_RSA(17,23)
v
v=claves_RSA(97,101)
v
v=claves_RSA(307,397)

v
modpower(27, 3*43,85)


moculto = modpower(27,3,85)
moculto
moculto = modpower(moculto,43,85)
moculto


# Resulta que cualquier numero elevado a r*s en la aritmética modular de n 
# es el propio número:
modpower(97,v[2]*v[3],v[1])
mensaje_oculto = modpower(97, v[2], v[1])
mensaje_oculto
mensaje_original = modpower(mensaje_oculto, v[3], v[1])
mensaje_original
modpower(122,v[2]*v[3],v[1])
modpower(424,v[2]*v[3],v[1])
modpower(252,v[2]*v[3],v[1])


# Demuestra que esas claves no son seguras, utiliza R para ellos (primeFactors)



# Utiliza dos números primos grandes de la lista de primos de wikipedia y 
# mira si son seguros...




###########################################################
## Ejercicio 2. Función claves_grandes_RSA 
# mejoras a la función claves_RSA:
# Orientados a trabajar con números grandes.
# 1.- Recibe dos numeros grandes como parametros (no tienen por qué ser primos). 
# 2.- queremos que r (parte de la clave publica) sea grande.

## Puedes utilizar la función implementada en la sesión de laboratorio L4: primo_relativo
#
primo_relativo <- function(m,umbral)
{
  zbki<-umbral;
  while(GCD(m,zbki)!=1)
  {
    zbki<-zbki+1;
  };
  return(zbki);
}


claves_grandes_RSA <- function(a,b)
{
  
  
  
if (isNatural(a) & isNatural(b))      #primera opcion
{
 p = nextPrime(a)
  q = nextPrime(b)
 if (p==q) q = nextPrime (p+q)
  n=p*q
  m=(p-1)*(q-1)
  r= primo_relativo(m,a+b)
  s= modinv(r,m)
  cat("clave publica: n= ",n," r=",r,"\nclave privada: s=",s)
  return(c(n,r,s))
}
  else 
{

  cat("no puedo calcular claves sin numeros positivos")
  return(NA)
}
  
  
  
  
  
  if (!isPrime(a))
  {
    cat("el primer parametro(", format(a, scientific = FALSE) ,"que me has pasaddo no es primo. Voy a usar", format(pr, scientific = FALSE))
    print("")
    p = pr
  }
  else p = a
  
  if (!isPrime(b))
  {
    cat("el primer parametro(", format(b, scientific = FALSE),"que me has pasaddo no es primo. Voy a usar", format(pr, scientific = FALSE))
    print("")
    q = pr
  }
  
  else q = b
  n = p*q
  m = (p-1)*(q-1)  # fi(n)
  r = primo_relativo(m,a+b) #r in Z fi(n)
  s = modinv(r,m)
  
  
  
  return(c(n,r,s))
  
  
  
  
  
  
  
}


# Pruebas:
v = claves_grandes_RSA(2634758697353,293756536383)
v[1]
format(v[1], scientific = FALSE)
# Clave pública. n= 7.739776e+23 r= 2.928515e+12 Clave privada, s= 4.665414e+23 
# oh,oh! no puedo publicar la clave pública!
# Hay que desactivar el formato cientifico en la escritura de las claves:

claves_grandes_RSA <- function(a,b)
{
  if (isNatural(a) & isNatural(b))
  {
   p = nextPrime(a)
   q = nextPrime(b)
    if (p==q) q = nextPrime (p+q)
    n=p*q
    m=(p-1)*(q-1)
    r= primo_relativo(m,a+b)
   s= modinv(r,m)
    cat("clave publica: n= ",format (n, scientific=FALSE)," r=",format (r, scientific=FALSE),"\nclave privada: s=",format (s, scientific=FALSE))
    cat("\np=", format(p, scientific= FALSE), "q =", format(q, scientific= FALSE))
    return(c(n,r,s))
  }
  else 
 {
  
  cat("no puedo calcular claves sin numeros positivos")
  return(NA)
}
  
  
  
}

# Ahora si que va a funcionar...
claves_grandes_RSA(2634758697353,293756536383)
# p = 2634758697451  q = 293756536399
# Clave pública. n= 773977589210346518740992 r= 2928515233737 
# Clave privada, s= 466541382405816867356672 

# Pruebas con números grandes

claves_grandes_RSA(30000000,300000000)
# p = 30000001  q = 300000007
# Clave pública. n= 9000000510000007 r= 330000001 
# Clave privada, s= 8839285950000001 

claves_grandes_RSA(300000000,3000000000)
# p = 300000007  q = 3000000019
# Clave pública. n= 900000026700000128 r= 3300000001 
# Clave privada, s= 876039123147913984 

claves_grandes_RSA(3000000000,1234567890123456)
# p = 3000000019  q = 1234567890123481
# Clave pública. n= 3703703693827232891928576 r= 1234570890123457 
# Clave privada, s= 2059462519232863097847808 

claves_grandes_RSA(3000000000,12345678901234567)
# Error in Primes(n, n + d1) : 
#   Upper bound 'n2' must be smaller than 2^53-1. 
claves_grandes_RSA(8000000000000000,1234567890123456)
# me aburro esperando respuesta.... 
#
primeFactors(773977589210346518740992)
#Warning message:
#  In primeFactors(7.73977589210347e+23) :
#  Argument 'n' too big: use 'gmp::factorize()' for this.


# ¿Cual es el límite?? 
# Mira el codigo de  primeFactors
primeFactors


# ¿Qué condiciones han de cumplir los números que elijamos??




