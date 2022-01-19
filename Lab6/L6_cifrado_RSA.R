# Sesión de laboratorio L6: cifrado RSA
#  (Aritmética Modular: exponenciación modular)
# 
# para instalar el paquete "numbers":
# install.packages("numbers")
# para utilizar "numbers":
library("numbers")
# info:
library(help="numbers")

# Para la cocificación y cifrado del mensaje nos interesan 
# las siguientes funciones de R:

# Cálculo de la exponenciación modular: modpower (a,b,n) = a^b mod n
modpower(65,43,85)
#[1] 10

# si realizaramos las operaciones en aritmética normal...
expmod<-65^43
expmod
format(expmod, scientific=FALSE)
# "902140823020221853069382267866204722292810765838240893757986389015590648938496"
# Pero ese número con tantos dígitos es intratable...
mod(expmod,85)#error


## Cálculo de los códigos ASCII de las letras:
## letra a letra o todo el string de golpe
strtoi(charToRaw("k"),16L)
strtoi(charToRaw("a"),16L)
strtoi(charToRaw("i"),16L)
strtoi(charToRaw("x"),16L)
strtoi(charToRaw("o"),16L)
strtoi(charToRaw("kaixo"),16L)
#[1] 107  97 105 120 111

## Proceso inverso: de código ASCII a letra
## o de golpe, de vector de códigos ASCII a string 
rawToChar(as.raw(107))
rawToChar(as.raw(97))
rawToChar(as.raw(105))
rawToChar(as.raw(120))
rawToChar(as.raw(111))
## necesitamos vector de código!!! --> función de crear vector: c(...)
vcodificado<-c(107,97,105,120,111)
rawToChar(as.raw(vcodificado))
# [1] "kaixo"

##################################################################
# Ejercicio 1. codificación / decodificación
# Recibe un string con el mensaje a codificar y 
# devuelve un vector de códigos ascii

codifica <- function(txt)
{
  codetxt<-strtoi(charToRaw(txt),16L)
  return(codetxt)
}

## Pruebas:
texto1<-"kaixo"
codevector1<-codifica(texto1)
codevector1
# 107  97 105 120 111

texto2<-"este profe no calla"
codevector2<-codifica(texto2)
codevector2
# 101 115 116 101  32 112 114 111 102 101  32 110 111  32  99  97 108 108
#  97

texto3<-"Zer moduz?"
codevector3<-codifica(texto3)
codevector3
#  90 101 114  32 109 111 100 117 122  63

###########################################################################
# Función que recibe el vector de códigos ASCII y 
# devuelve el string que le corresponde 
decodifica <- function(codetxt)
{
  code<-rawToChar(as.raw(codetxt))
  return(code)
}

# Pruebas:
txt1<-decodifica(codevector1)
txt1
  # "kaixo"

txt2<-decodifica(codevector2)
txt2
  # "este profe no calla"

txt3<-decodifica(codevector3)
txt3
  # "Zer moduz?"

#####################################################
# Ejercicio 2.  Cifrado / Descifrado
# Recibe el vector de códigos ASCII y 
# devuelve el vector cifrado. Cada código se cifra mediante modpower(codigo, r, n)

# La longitud del vector de salida es igual a la del parámetro de entrada.

cifrar <- function(codevector,r,n)
  {
    vcifrado<-codevector
    for (i in 1:length(codevector)) {
      vcifrado[i]<-modpower(codevector[i],r,n)  
    }
    return(vcifrado)
  }

# Pruebas:
# Las claves a utilizar:
# n=9797, r=7, s=2743
vectorcifrado1<-cifrar(codevector1,7,9797)
vectorcifrado1
# 2792 5432 4668 4973 7969

vectorcifrado2<-cifrar(codevector2,7,9797)
vectorcifrado2
# 7976 4764 2565 8540 4974

vectorcifrado3<-cifrar(codevector3,7,9797)
vectorcifrado3
#  375 2222 7721 3675  493 7969 6261 8564 4122 4604

cifrar(vectorcifrado1, 2743, 9797)

#####################################################
# Función para descifrar el mensaje cifrado
# Recive un vector de códigos cifrados
# Debe descifrar cada código y devolver el vector con los códigos descifrados
# se supone que los códigos descifrados serán códigos ASCII.
# para descifrar cada código cifrado debe realizar la operación modpower(cifrado,s,n)
# es por tanto la misma operación que realiza la función cifrar, solo que
# esta vez el parámetro r es sustituido por s.
# la pregunta es: ¿realmente nos hace falta la función de descifrar?

descifra <- function(vectorcifrado,s,n)
{
  vcifrado<-vectorcifrado
  for (i in 1:length(vectorcifrado)) {
    vcifrado[i]<-modpower(vectorcifrado[i],s,n)  
  }
  return(vcifrado)
}
  

# Pruebas:
descifrado1<-descifra(vectorcifrado1,2743,9797)
descifrado1
# 107  97 105 120 111
descifrado11<-cifrar(vectorcifrado1,2743,9797)
descifrado11
# 107  97 105 120 111 (?)
descifrado2<-descifra(vectorcifrado2,2743,9797)
descifrado2
# 75 65 73 88 79
descifrado21<-cifrar(vectorcifrado2,2743,9797)
descifrado21
# 75 65 73 88 79
descifrado3<-descifra(vectorcifrado3,2743,9797)
descifrado3
#  90 101 114  32 109 111 100 117 122  63
descifrado31<-cifrar(vectorcifrado3,2743,9797)
descifrado31
#  90 101 114  32 109 111 100 117 122  63

#############################################################
# Podemos hacerlo todo de golpe:
# Claves generadas mediante la función  claves_RSA(97,101) 
# basados en los primos 97 y 101:
# n=9797, r=7, s=2743
n<-9797
r<-7
s<-2743
nuevotexto<-"veamos si funciona..."

texto_cifrado <- cifrar(codifica(nuevotexto),r,n)
texto_cifrado
# 9710 2222 5432  493 7969 1622 3675 1622 4668 3675 3435 8564 
# 9305 6336 4668 7969 9305 5432 3037 3037 3037
texto_recuperado <- decodifica(descifra(texto_cifrado,s,n))
texto_recuperado
# "veamos si funciona..."
## SI!!!! funciona!!! 
# y si lo hacemos solo con cifrar?
texto_recuperado <- decodifica(cifrar(texto_cifrado,s,n))
texto_recuperado
# "veamos si funciona..."
## También funciona!!! 
####################################################################
## Pruebas con otras claves:
# claves_RSA(17,23):
# n=391, r=3, s=235
n<-391
r<-3
s<-235

texto_cifrado <- cifrar(codifica(nuevotexto),r,n)
texto_cifrado
#  50  16  79  37 304 276 315 276 265 315  34  77  
#  36 228 265 304  36  79 368 368 368
texto_recuperado <- decodifica(descifra(texto_cifrado,s,n))
texto_recuperado
# "veamos si funciona..."

####################################################################
## Pruebas con otras claves:
# claves_RSA(307,397):
# n=121879, r=5, s=96941
n<-121879
r<-5
s<-96941

texto_cifrado <- cifrar(codifica(nuevotexto),r,n)
texto_cifrado
# 36115 108694 111554 112710  78527   2384  37707   2384  12382  37707  33180  32684
#  8940  47766  12382  78527   8940 111554 109345 109345 109345
texto_recuperado <- decodifica(descifra(texto_cifrado,s,n))
texto_recuperado
# "veamos si funciona..."

####################################################################
## Pruebas con otras claves:
# claves_RSA(5,17):
# n=85, r=3, s=43
# CIUDADO!!!! mod 85 solo puede representar valores de 0 a 84,
# los códigos ASCII son 256. ¿qué va a pasar?
n<-85
r<-3
s<-43

texto_cifrado <- cifrar(codifica(nuevotexto),r,n)
texto_cifrado
# 67 16 28 54 66 55 43 55 10 43 68 43 70 24 10 66 70 28 11 11 11
texto_recuperado <- decodifica(descifra(texto_cifrado,s,n))
texto_recuperado
#"!\020\f\030\032\036 \036\024 \021 \031\016\024\032\031\f..."
# No hemos recuperado el mensaje original... Pero los puntos finales y 
# los espaciós entre palabras se han recuperado correctamente.
# sus códigos ASCII son inferiores a 84!!


########################################################
## Vamos a jugar....
## Ejercicio 3. 
##
## Necesitamos dos funciones de la sesión de laboratorio L5:
## primo_relativo_pequeno() y claves_RSA()
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
  n<-p*q;
  m=(p-1)*(q-1);
  r<- primo_relativo_pequeno(m) # r parte de la clave pública (pequeño)
  s<-modinv(r,m) # s clave privada
  cat("Clave pública. n=", n, "r=", r,"Clave privada, s=", s, "\n\n")
  return(c(n,r,s))
}

# Amaia envia mensaje a Beñat
#-----------------------------------------------------------------
# Recibe Beñat, por tanto necesitamos su clave pública
# Amaia tiene el mensaje a enviar:
# Beñat elige dos números primos entre 500 y 600, por ejemplo
Primes(500,600)
# 503 509 521 523 541 547 557 563 569 571 577 587 593 599
# Elige p=521 y q=577
# Llama a la función que calcula las claves:
claves_RSA(547,599)
# Clave pública. n= 300617 r= 7 Clave privada, s= 213943
# Beñat escribe en la pizarra su clave pública.
n<-327653
r<-5
#--------------------------------------------------------
# Amaia debe enviar el mensaje.
# Su mensaje es: "Mañana, ¿donde?"
nuevotexto<-"im watching you"
# Como el mensaje va dirigido a Beñat necesitamos su clave pública
texto_cifrado <- cifrar(codifica(nuevotexto),r,n)
texto_cifrado
# 248476 205535 173164 249968 205535 241982 205535   9389 117119 154801 184928 253472
# 139977 241982 253472 276551 296805
# Amaia escribe el mensaje cifrado en la pizarra.
#-----------------------------------------------------------
# Beñat recibe el mensaje cifrado de Amaia y ahora debe descifrarlo
# Para ello necesita su clave privada:
s<-195905
# genera un vector de códigos cifrados, copiando desde la pizarra:
mensaje_pizarra<-c(309975, 210433, 300580, 69262, 210433, 299744, 210433, 133826, 
                   264817, 210433, 244657, 210433, 30440, 43067)
# Y descifra y decodifica el mensaje
texto_recuperado <- decodifica(descifra(mensaje_pizarra,s,n))
texto_recuperado
texto_recuperado <- decodifica(descifra(texto_cifrado,s,n))
texto_recuperado
# "Mañana, ¿donde?"
## Si no nos equivocamos al copiar en pizarra y desde pizarra la cosa debe ir bien...
#-----------------------------------------------------------------------
# Pero hay alguien que se quiere enterar de todo....
# Ha visto la información de la pizarra:
# Clave pública:n<-300617, r<-7
# Mensaje: 
# 248476 205535 173164 249968 205535 241982 205535   9389 117119 154801 184928 253472
# 139977 241982 253472 276551 296805
mensaje_kuxkuxero=c(309975, 210433, 300580, 69262, 210433, 299744, 210433, 133826, 
                    264817, 210433, 244657, 210433, 30440, 43067)
# Descompone n en números primos:
primeFactors(n)
# 521 577
# con ellos calcula la clave privada de Beñat:
claves_RSA(547,599)
# Clave pública. n= 300617 r= 7 Clave privada, s= 213943 
# Ya puede enterarse de todo....
texto_recuperado <- decodifica(descifra(mensaje_kuxkuxero,s,n))
texto_recuperado
# Nos ha pillado!...
##


## Juguemos con la pizarra: Hay tres papeles:

## 1.- Receptor de mensajes.
## Escribe en la pizarra tu nombre y tu clave pública (n y r). 
## Quedate a la espera a ver si alguien te escribe un mensaje...Si alguien te envia
## un mensaje, descífralo y escribe el mensaje de forma que se pueda leer. 
## No utilices claves demasiado grandes, si no los códigos serán grandes, 
## y para copiar en pizarra...
n<-563
s<-485


## 2.- Enviar mensajes.
## Utiliza la clave pública de alguien que la haya publicado en la pizarra 
## y escribe el mensaje cifrado al lado de su clave pública. A ver si lo descifra!

## 3.- kuxkuxero:
## a partir de las claves públicas de la pizarra obtén la clave privada del receptor 
## y descifra antes que el propio receptor los mensajes que le pasen

## Podemos evitar la pizarra y pasar los mensajes por correo electŕonico




