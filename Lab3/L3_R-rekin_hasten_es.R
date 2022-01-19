##
## Sesión de laboratorio: primeros pasos con RStudio
## 1.- Ayuda de R
help.start()

## Más información:
## http://stat.ethz.ch/R-manual/R-devel/library/base/html/00Index.html
##
## Information on package ‘base’:
library(help="base")

## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Algunas funciones básicas
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Assignment Operators (<-, ->, =)
x<-2
3-> y
z=4
x
y
z

## Arithmetic Operators (+, -, *, /, ^, %%, %\%)
x+y
(x+y)-z
((x+y)-z)*z/x
x^y
## Pide ayuda sobre la operación %% 
## En Value dice:
##   %% indicates x mod y and %/% indicates integer division. 
(y+z) %% x
(y+z) %/% x
  
## Relational Operators (<, >, <=, >=, ==, !=)
x<y
x>y
x<=y
x>=y
x==y
x!=y

## Logical Operators (logical negation !,  logical AND &, logical OR |)
!(x<y)
x<y
y<x
(x<y)&(y<x)
(x<y)|(y<x)

## Sequence Generation (c, colon :, seq), length
## Combine Values into a Vector or List (c)
a<-c(1,2,3)
a
b<-c(x,y,z)
b
length(a)
length(x)

## Para acceder a una componente (o a un elemento) del vector:
  a[1]
  a[7]  #NA

1:10
4:20
x:z
## de 2 a 20 de dos en dos
seq(2,20,2)
seq(2,20,4)

## 

## Print, Concatenate and print
print(5+5)
print(x,y,z)  #2

cat("tres mas tres (", y, "+", y, ") equivale a seis", y+y) #concatenar

## Control flow (if, if else, while, for)
if (x<y) y-x
if (y<x) x-y else 0
while (x<z) {print(x); x<-x+1}
for (i in 1:10) {print(i)}
for (i in 1:10) {print("a")}

## Definición de una función

devolver_longitud<-function(v)
{lon_de_vector=length(v);
return(lon_de_vector);
}
v<-c(1,2)
devolver_longitud(v)

devolver_longitud(a)

## otras funciones (is.numeric, as.numeric)
is.numeric(5)
## [1] TRUE
is.numeric("5")
## [1] FALSE
hitza<-"kaixo"
is.numeric(hitza)
## [1] FALSE

v=c("kaixo", 1,2,3)
v[1]
is.numeric(v[1])
v[2]
is.numeric(v[2])
v[2]+v[3]     #error
as.numeric(v[2])+as.numeric(v[3])

## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##  Ejercicio 3.Definir funciones que operen con elementos 
##             de vectores: suma, multiplicación...
## %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Función que calcula la suma de los dos elemento de un vector

## Ddefinición de la función:


suma<-function(v)
{v1<-v[1];
v2<-v[2];
cat("El vector es =(",v1,",",v2,"), ","La suma de sus dos elementos es=", v1+v2, "\n");
}

## Llamada a la función:

v<-c(1,2)
length(v)
## [1] 2
suma(v)
## El vector es=( 1 , 2 ),  La suma de sus dos elementos es= 3 

## ############################################################
## Ejercicio 3.1 Suma de todos los elementos del vectol, cualquiera que se su longitud.
## Desconocemos la longitud del vector.
##
## para saber la longitud eel vector podemos usar length(v) 
## Con ello utilizando un for recorreremos los elementos del vector
## Para acceder al elemento i-esimo del vector v: v[i]

## Definición de la función:
suma2<-function(v)
{
  if(is.numeric(v)) {
  res=0
  lon=length(v)
  for (i in 1:length(v)) 
    res= res+v[i]
  return(res)
  }
  else {print("no puedo sumar valores no numericos")}
}


## Llamadas a la función para verificar el funcionamiento:
w1<-c(3,8,10,2,4,9)
w2<-c(3,8,10)
w3<-c(3,8,10,0,-1,9)
suma2(w1)
## [1] 36
suma2(w2)
## [1] 21
suma2(w3)
## [1] 29

suma2(x)
## [1] 4

suma2(c("kaka", 1,1,1)) #error



##################################################################
## ejercicio 3.2. Suma o multiplicación de los elementos del vector.
##
## El primer elemnto indica la operación a realizar (caracteres).
## Resto de elemntos son nùmeros. ¿Para R qué tipo de datos son?
## Supone que tosos son de tipo caracter... Problemas para sumar...
## Para decirle que los considere numéricos: as.numeric(v[i])

##  Derinición de la función:
suma_o_multiplica<-function(v)
{
  #lon = length(v)
  
    
    if (v[1] =="suma")
    {
      
      return (suma2(as.numeric(v(2:length(v)))))
    }
    
    
    else
    {
      res=1
      for (i in 2:length(v)) res = res* as.numeric(v[i])
    }
    
  return(res)

  
  
  
}





## Llamadas

v1=c("suma", 1,2,3,4,5,6)
suma_o_multiplica(v1)
#[1] 21

v2=c("multiplica", 1,2,3,4,5,6)
suma_o_multiplica(v2)
#[1] 720

##########################################################3
## Mejoras a la función
suma_o_multiplica<-function(v)
{
  
  
  
  #lon = length(v)
  
  if (length(v)>2)
  {
    
    if (v[1] =="suma")
    {
      
      return(suma2(as.numeric(v[2:length(v)])))
      
      
      #s=0
      #for (i in 2:lon) s = s+ as.numeric(v[i])
      
      
    }
    else {
      print("no se qu� hacer")
    }
    
    
    
    
      #if (v[1]=="multiplica")
       # res = 1
        #for (i in 2:length) 
        #  res = res* as.numeric(v[i])
        #return(res)
    
    
  }
  
  else
  {
    s=-1
    print("minimo dos elementos")
    
  }
  
  
  

  
  
  
  
  
}

## Deiak funtzioari probak egiteko:

v1=c("suma", 1,2,3,4,5,6)
suma_o_multiplica(v1)
#[1] 21

v2=c("multiplica", 1,2,3,4,5,6)
suma_o_multiplica(v2)
#[1] 720

v3=c(1,2,3,4,5,6)
suma_o_multiplica(v3)
# No ha sido posible

v4=c("suma")
suma_o_multiplica(v4)
# No ha sido posible 

#####################################################################
## 3.3 Ariketa
## en un vector de cualquier longitud buscar los dos más grandes
## y que diga las posiciones que ocupan en el vector

## Funtzioaren definizioa:

los_dos_mayores<-function(v)
{
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

## Deiak funtzioari probak egiteko:

v5<-c(3,8,10,2,4,9)
los_dos_mayores(v5)
# El mayor: 10 Puesto: 3 Segundo mayor: 9 Puesto: 6 

v6<-c(3,8)
los_dos_mayores(v6)
# El mayor: 8 Puesto: 2 Segundo mayor: 3 Puesto: 1 

v7<-c(8,3)
los_dos_mayores(v7)
# El mayor: 8 Puesto: 1 Segundo mayor: 3 Puesto: 2 

v8<-c(8,8)
los_dos_mayores(v8)
# El mayor: 8 Puesto: 1 Segundo mayor: 8 Puesto: 2 

v9<-c(8)
los_dos_mayores(v9)
# Necesito minimo dos elementos en el vector

v10<-c(8,8,0,2,12,8,12,12)
los_dos_mayores(v10)
# El mayor: 12 Puesto: 5 Segundo mayor: 12 Puesto: 7 



