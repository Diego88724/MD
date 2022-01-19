# Sesión de laboratorio: Teoría de números y R
# 
# Para instalar el paquete "numbers" elimina la almohadilla (#):
install.packages("numbers")
# Para poder utilizar las funciones definidas en el paquete "numbers":
library("numbers")
# para mostrar los contenidos del paquete:
library(help="numbers")

# Primer ejercicio. Familiarizarse con algunas funciones del paquete "numbers".
# El nombre de una función es una variable, al que se le asocia un código, por lo que
# si escribimos el nombre de la función, sin paréntesis ni parámetros, el intérprete
# de R nos responde con el código asociado a la función:
isNatural

# function (n) 
# {
#   stopifnot(is.numeric(n))
#   floor(n) == ceiling(n) & n >= 1 & n <= 2^53 - 1
# }

# Nótese que la fución isNatural responderá con FALSE si el número es mayor 
# que  $2^{53}-1$ 

# Números naturales, caracteres y números primos:
isNatural(8)
# TRUE
2^53
# 9.007199e+15
2^52
# 4.5036e+15
isNatural(2^52)
# TRUE
isNatural(2^53)
# FALSE

isNatural(1000000000000000) # 16 dígitos
# TRUE

isNatural(10000000000000000)  # 17 dígitos
# FALSE

is.character(8)
# FALSE
is.character("8")
# TRUE

abs(-8)
#  8
isPrime(7)
#  TRUE
isPrime(8)
#  FALSE
Primes(1,20)
#   2  3  5  7 11 13 17 19
Primes(100,200)
# 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 
# 179 181 191 193 197 199
nextPrime(2000)
#  2003
nextPrime(99999)
#  100003
previousPrime(100019)
#  100003

# Podemos comprobar si dos números son primos relativos o coprimos.
coprime(1448,1001)
#  TRUE
coprime(1448,1002)
#  FALSE

# Descomposición de númetros en factores primos
primeFactors(1448)
#    2   2   2 181
primeFactors(1001)
#  7 11 13

# División Euclidiana. 
# Dados n y m el cociente de la división es div(n,m) ò n%/%m,
# el resto:  mod(n,m) ó n%%m
div(7,2)
# 3
7%/%2
# 3
mod(7,2)
# 1
7%%2
# 1

# Máximo común divisor
# (greatest common divisor): GCD(n,m)
GCD(1448,1001)
# 1
GCD(1448,1002)
# 2

# Máximo común divisor: 5ª propiedad (identidad de Bezout).
# Cálculo de mcd(a,b) además de la combinación lineal de a y b que lo define: 
# mcd(a,b)=xa+yb. La función extGCD(a,b) devuelve mcd(a,b), x e y.
# Para los cálculos utiliza el algoritmo de Euclides. extGCD(a,b)
extGCD(1448,1001)
#    1 -421  609
1==-421*1448+609*1001
# TRUE
extGCD(1448,1002)
#    2 -164  237
2==-164*1448+237*1002
# TRUE

###############################################################
###############################################################
# Segundo ejercicio

# 2.1 Dados dos enteros, a y b, implementa la función que calcule el  
#     máximo común divisor de ambos, basándote en el algoritmo de Euclides: 

mcd <- function(a,b)
{ 
 c= abs (a)
 d= abs (b)
while (d !=0)
{ 
  r <- mod (c,d)
  c = d
  d = r
}
return(c)

}



  


 
  
## llamadas a la función. a=231, b=1820  --------------------------
mcd(231,1820)
# 7
coprime(231,1820)
# FALSE
GCD(231,1820)
# 7
extGCD(231,1820)
# 7 -63 8

## lamadas a la función. a=1369, b=2597  ----------------------------
mcd(1369,2597)
# 1
coprime(1369,2597)
# TRUE
GCD(1369,2597)
# 1
extGCD(1369,2597)
# 1 -1013 534

## lamadas a la función. a=2689, b=4001  ----------------------------
mcd(2689,4001)
# 1
coprime(2689,4001)
# TRUE
GCD(2689,4001)
# 1
extGCD(2689,4001)
#  1 1662 -1117

## ¿Qué pasa si llamamos a la función con parámetros no enteros?
mcd(-10,5)
# 5
mcd(3.2,4)
# 8.881784e-16
mcd("a","b")
# Error in abs(a) : non-numeric argument to mathematical function

## -------------------------------------------------------
# 2.2 Mejoras a la función.
## En caso de que reciba parámetros reales (no enteros), que escriba
## un mensaje de error.
## Si recibe parámetros de tipo caracter, otro tanto, pero diferente mensaje.
## Nota: la función is.character(a) nos dice si la variable es de tipo alfanumérico.
# mientras que isNatural(a) nos dice si se trata de un número entero positivo.

mcd <- function(a,b)
{ 
  sonchar = is.character(a) | is.character(b)
  if (!sonchar)
  { 
    c= abs (a)
    d= abs (b)
    sonnat = isNatural (c) & isNatural (d)
    if (sonnat)
    {
      while (d !=0)
      {
        r <- mod (c,d)
        c = d
        d = r
      }
      return(c)
    }  
  else
  {
    print ("los parametros tienes que ser enteros")
  }
  
  }    
 else
 {
   print ("los parametros no pueden ser alfanumericos")
 }
  
}


## Llamada a la funcióncion con números reales
mcd(3.2,4)
# "a y b deben ser enteros"

## Llamada a la funcióncion con caracteres
mcd("a","b")
# "a ey b no pueden ser de tipo cararcter"

#############################################################
##########################################################
## Ejercicio 3.
## Implementa la función que recibe un número entero y devuelve 
## un número primo relativo al que ha recibido.

## 3.1 La función que devuelve el primo relativo más pequeño del parámetro m.
# Devuelve un número entero y positivo.
##
primo_relativo_pequeno <- function(m)
{
pr = 2
maxcomdiv = mcd (m,pr)
while(maxcomdiv != 1)
{
  pr = nextPrime(pr)
  maxcomdiv = mcd (m,pr)
}
return (pr)
  
}
## Llamadas a la función. 
primo_relativo_pequeno(13797)
# 2
primo_relativo_pequeno(16974)
# 5
primo_relativo_pequeno(56970)
# 7
primo_relativo_pequeno(1000000000000000)
# 3
##-----------------------------------------------------------------
## 3.2 Modifica la función de forma que en vez de devolver el 
## primo relativo más pequeño posible devuelva el primo relativo 
## más pequeño posible pero mayor que el número que se le pasa como umbral.

primo_relativo <- function(m, umbral)
{
  
  
  #pr = 2
  #maxcomdiv = mcd (m,pr)     ##opcion alternativa
  #while(maxcomdiv != 1)
  #{
  #  pr = nextPrime(pr)
  #  maxcomdiv = mcd (m,pr)
  #}
  #return (pr)
  
  
  
  
  sol = umbral
  while (!coprime(m, sol)) {
    sol = sol + 1
  }
  return(sol)
  
}

## Cuidado don los números grandes
primo_relativo(13797, 50)
# 50
primo_relativo(16974, 54687)
# 54689
primo_relativo(56970,26378)
# 26381
primo_relativo(9674,26378)
# 26379
primo_relativo(1000000000000000,26378)
# 26379
primo_relativo(1000000000000000,2637800000)
# 2637800001

# En criptografía, en el cáclulo de claves del algoritmo RSA es importante 
# que los números sean muy grandes.
##################################################
##########################################################
## Ejercico 4. El código ASCII

## Podemos calcular los códigos ASCII de las letras
## Y podemos calcularlos letra por letra o de golpe todas las letras a la vez:

strtoi(charToRaw("k"),16L)
strtoi(charToRaw("a"),16L)
strtoi(charToRaw("i"),16L)
strtoi(charToRaw("x"),16L)
strtoi(charToRaw("o"),16L)
strtoi(charToRaw("kaixo"),16L)

## Podemos realizar la operación inversa, Partiendo del código ASCII 
## podemos pasar a letras, y de nuevo es posible realizar la operación 
## código a código o de una sola vez pasando un vector de códigos a un string.

rawToChar(as.raw(107))
rawToChar(as.raw(97))
rawToChar(as.raw(105))
rawToChar(as.raw(120))
rawToChar(as.raw(111))
hitza<-c(107,97,105,120,111)
rawToChar(as.raw(hitza))

## Modifica los códigos de los caracteres de un texto 
## para ocultarlo (suma 5 a cada código) y devuelve el string asociado 
## a los nuevos códigos:
texto_secreto <- function(testua)
{ 
  
  vector <- strtoi(charToRaw(testua), 16L)
  for (i in 1:length(vector)) vector[i]= vector[i] + 5
  n = rawToChar(as.raw(vector))
  return(n)
  
}
# LLamada a la función:
texto_secreto("kaixo")
# "pfn}t"
