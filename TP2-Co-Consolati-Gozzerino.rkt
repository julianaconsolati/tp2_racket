;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname TP2-Co-Consolati-Gozzerino) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 2: Listas

Integrantes:
- [Co, Santiago].
- [Consolati, Juliana].
- [Gozzerino, Franco].
|#

;---------------------Diseño de datos------------------------

; Utilizamos exact-round
(require racket/math)

; Representamos los nombres de los países con String y
; las fechas (año) y el número de habitantes de un país con Number.

(define-struct Registro [Fecha Poblacion])
; Registro es (Number, Number)
; Interpretación: un elemento en Registro representa el número
; de habitantes de un país (Poblacion) en una determinada fecha (Fecha).

(define-struct Pais [Nombre Registros])
; Pais es (String, List(Registro))
; Interpretación: un elemento en Pais representa un pais de nombre
; Nombre con su lista de registros de censo asociado Registros.

;---------------------Constantes para casos de prueba------------------------

(define ANDORRA (make-Pais "Andorra" (list (make-Registro 2005 35000000)
                                           (make-Registro 2006 -1))))
(define GUATEMALA (make-Pais "Guatemala" (list (make-Registro 20018 -1)
                                               (make-Registro 20019 32500000))))
(define ARGENTINA (make-Pais "Angentina" (list (make-Registro 2014 35000000)
                                               (make-Registro 2015 40000000))))
(define CHINA (make-Pais "China" (list (make-Registro 2018 999999956)
                                       (make-Registro 2019 1000000045))))

(define LISTA-PAISES-TEST (list ANDORRA GUATEMALA ARGENTINA CHINA))
(define LISTA-PAISES-TEST-S-CHINA (list ANDORRA GUATEMALA ARGENTINA ))

;---------------------Preparación de los datos------------------------

; Datos de entrada
(define INPUT-PAISES (read-csv-file "BDPoblacional.csv"))
; Removemos el encabezado
(define DATOS-PAISES (rest INPUT-PAISES))

; Primer registro censo
(define FECHA-INICIO 1960)
; Ultimo registro censo
(define FECHA-FIN 2019)
; Años en los que se realizaron censos [1960,2019]
(define RANGO-FECHA (range FECHA-INICIO (+ FECHA-FIN 1) 1))

; armar-registros: List(Number) List(String) -> List(Registro)
; armar-registros toma una lista de años *lf* y una lista de strings que representan números
; de habitantes *lnh* y devuelve una lista de registros *lr* en donde el i-ésimo
; elemento de *lr* se contruye a partir del i-ésimo elemento de *lf* y
; el i-ésimo elemento de *lnh* convertido a número
(check-expect (armar-registros (list 2005 2006) (list "3" "4"))
              (list (make-Registro 2005 3) (make-Registro 2006 4)))
(check-expect (armar-registros (list 2005 2006) empty) empty)
(check-expect (armar-registros empty (list (make-Registro 2005 3))) empty)
(check-expect (armar-registros empty empty) empty)

(define (armar-registros lf lnh)
  (cond [(empty? lnh) empty]
        [(empty? lf) empty]
        [else (cons (make-Registro (first lf) (string->number (first lnh)))
                    (armar-registros (rest lf) (rest lnh)))]))

; armar-lista-registros List(List(String)) List(Number) -> List(List(Registro))
; armar-lista-registros toma una lista de listas de strings que representan cantidad de habitantes
; *lnh* y una lista de años *lf* y devuelve una lista de listas de Registro *lr*
; Los campos Población de los elementos de *lr* coinciden con los elementos de *lnh*
; y su campo fecha se completa con los años de *lf*.
(check-expect (armar-lista-registros empty empty) empty)
(check-expect (armar-lista-registros (list (list "3" "4")) empty) empty)
(check-expect (armar-lista-registros empty (list 2005 2006)) empty)
(check-expect (armar-lista-registros
                  (list (list "3" "4")
                        (list "5" "6")
                        (list "7" "8"))
                  (list 2005 2006))
              (list (list (make-Registro 2005 3) (make-Registro 2006 4))
                    (list (make-Registro 2005 5) (make-Registro 2006 6))
                    (list (make-Registro 2005 7) (make-Registro 2006 8))))

(define (armar-lista-registros lnh lf)
  (cond [(empty? lnh) empty]
        [(empty? lf) empty]
        [else (cons (armar-registros lf (first lnh))
                    (armar-lista-registros (rest lnh) lf))]))

; Datos para construir registros (sin nombres de países)
(define LISTA-DATOS-REGISTRO (map rest DATOS-PAISES))
; Lista de registros de cada país
(define LISTA-REGISTROS (armar-lista-registros LISTA-DATOS-REGISTRO RANGO-FECHA))

; armar-paises: List(String) List(Registro) -> List(Pais)
; armar-paises toma una lista de nombres de países *lnp* y una lista de registros
; poblacionales *lr* y devuelve una lista de estructura Pais *lp* en donde
; el i-ésimo elemento de *lp* tiene de campo Nombre el i-ésimo elemento
; de *lnp* y de campo Registros el i-ésimo elemento de *lr*
(check-expect (armar-paises (list "Angola" "Andorra" "Argentina") empty) empty)
(check-expect (armar-paises empty (list (list (make-Registro 2005 3) (make-Registro 2006 4)))) empty)
(check-expect (armar-paises (list "Angola" "Andorra" "Argentina")
                            (list (list (make-Registro 2005 3) (make-Registro 2006 4))
                                  (list (make-Registro 2005 5) (make-Registro 2006 6))
                                  (list (make-Registro 2005 7) (make-Registro 2006 8))))
              (list (make-Pais "Angola" (list (make-Registro 2005 3) (make-Registro 2006 4)))
                    (make-Pais "Andorra" (list (make-Registro 2005 5) (make-Registro 2006 6)))
                    (make-Pais "Argentina" (list (make-Registro 2005 7) (make-Registro 2006 8)))))

(define (armar-paises lnp lr)
  (cond [(empty? lnp) empty]
        [(empty? lr) empty]
        [else (cons   (make-Pais (first lnp) (first lr) ) 
                      (armar-paises (rest lnp) (rest lr) ) 
              )
        ]))

; Nombres de países
(define LISTA-NOMBRE-PAISES (map first DATOS-PAISES))
; Lista de países
(define LISTA-PAISES (armar-paises LISTA-NOMBRE-PAISES LISTA-REGISTROS))

;---------------------Funciones de alto orden sobre listas de países------------------------

; transformar-paises: List(Pais) (Pais -> Pais) -> List(Pais)
; Toma una lista de paises y una funcion "predicado", en este caso
; llamada transformar que toma un pais (primer elemento de la lista)
; lo transforma y lo concatena con el resto de forma recursiva.
(define (transformar-paises lp transformacion)
  (cond [(empty? lp) empty]
        [else (cons (transformacion (first lp))
                    (transformar-paises (rest lp) transformacion))]))

; filtrar-paises: List(Pais) (Pais -> Boolean)-> List(Pais)
; filtrar-paises toma una lista de paises lp*
; y segun el predicado que se le haya pasado predicado*
; los va filtrando, agregando a los aprobados por el predicado
; en una nueva lista y eliminando los que no.
(check-expect filtrar-paises LISTA-PAISES-TEST)
(define (filtrar-paises lp predicado)
  (cond [(empty? lp) empty]
        [else (if (predicado (first lp))
                  (cons (first lp) (filtrar-paises (rest lp) predicado))
                  (filtrar-paises (rest lp) predicado))]))

; operar-sobre-paises: List(Pais) (Pais X -> X) X -> X
; Toma una lista de paises lp* y le aplica un operador a cada uno operador*,
; cuando llegue al final de la lista se le aplica el numero neutro
; que depende del tipo de operacion que se haga hecho, nuetro*.
(define (operar-sobre-paises lp operador neutro)
  (cond [(empty? lp) neutro]
        [else (operador (first lp) (operar-sobre-paises (rest lp) operador neutro))]))

;---------------------Consultas y actualizaciones------------------------

;---------------------------------------------
; Sección 1 - Datos incompletos y datos incorrectos
;---------------------------------------------

; Algunos países no tienen datos oficiales completos. Removerlos de la lista de paises.
; Definir una función predicado-registro-incompleto, transformacion-registro-incompleto
; u operacion-registro-incompleto para pasarle como argumento a
; alguno de los patrones de alto orden

; registro-incompleto: (list Registro -> Boolean)
; Funcion auxiliar para predicado-registro-incompleto
; Toma una lista de registros de un pais e indica si en alguna
; de sus instancias el Registro-Poblacion es igual a -1
; indicando que no se realizo el censo en ese año.
(check-expect (registro-incompleto (Pais-Registros ANDORRA) ) #f)
(check-expect (registro-incompleto (Pais-Registros ARGENTINA) ) #t)

(define (registro-incompleto  lr) (cond
  [(empty? lr) #t]
  [else (if (= (Registro-Poblacion (first lr) ) -1)
            #f
         (registro-incompleto (rest lr) ))]))

; predicado-registro-incompleto: (Pais -> Boolean)
; Funcion auxiliar para LISTA-PAISES-REGISTRO-COMPLETO
; Toma un pais y devuelve true o false dependiendo si se verifica
; registro-incompleto para el registro del pais que se le paso.
(check-expect (predicado-registro-incompleto ANDORRA) #f)
(check-expect (predicado-registro-incompleto ARGENTINA) #t)
(define (predicado-registro-incompleto p) (registro-incompleto (Pais-Registros p)))

; Lista de paises con registro completo.
; Filtra la lista de paises registrados
; eliminado los que poseen un registro incompleto.
(check-expect (member "West Bank and Gaza" (map Pais-Nombre LISTA-PAISES)) #t)
(check-expect (member "West Bank and Gaza" (map Pais-Nombre LISTA-PAISES-REGISTRO-COMPLETO)) #f)
(define LISTA-PAISES-REGISTRO-COMPLETO
  (filtrar-paises LISTA-PAISES predicado-registro-incompleto))

; Algunos casos de test para LISTA-PAISES-REGISTRO-COMPLETO
; "West Bank and Gaza" no tiene que estar en el listado de países
; con registro completo pero sí tienen que estar en el listado de países

; Los censos del 2014 se calcularon mal. Aumentarlos un 10%.
; Trabajamos con la lista de países con registro completo

; recalculo: Registro -> Registro
; Función auxiliar extra de ayuda, se recomienda utilizar
; recalculo toma un registro e incrementa la población en un 10%
; si la fecha en la que se condujo el censo fue 2014
; En caso de que el incremento resulte en un número no entero,
; se redondea
(check-expect (recalculo (make-registro 2014 10))
              (make-registro 2014 11))
(check-expect (recalculo (make-registro 2016 10))
              (make-registro 2016 10))

(define (recalculo reg)
  (if (= (registro-Fecha reg) 2014)
      (make-registro (registro-Fecha reg) (exact-round (* 1.1 (registro-Poblacion reg))))
      (make-registro (registro-Fecha reg) (registro-Poblacion reg))))

; transformacion-recalcular: Pais -> Pais
; transformacion-recalcular toma un país y aumenta en 10% la población asociada
; al año 2014 en sus registros.
(check-expect (transformacion-recalcular ARGENTINA)
              (make-Pais "Angentina" (list (make-Registro 2014 38500000)
                                           (make-Registro 2015 40000000))))
(check-expect (transformacion-recalcular CHINA) CHINA)

(define (transformacion-recalcular pais)
  (make-Pais (Pais-Nombre pais) (map recalculo (Pais-Registros pais)))) #|COMPLETADO|#

; Lista de paises recalculados.
; Agarra la lista de paises con registro completo
; y los transforma utilizanco la funcion transformacion-recalcular.
(define LISTA-PAISES-RECALCULADA
  (transformar-paises LISTA-PAISES-REGISTRO-COMPLETO transformacion-recalcular))

; Algunos casos de test para LISTA-PAISES-RECALCULADA
; Aruba 2014 original: 103774
; Afghanistan 2014 original: 33370794
(check-expect (member (make-Registro 2014 103774)
                      (Pais-Registros (first LISTA-PAISES-REGISTRO-COMPLETO))) #t)
(check-expect (member (make-Registro 2014 33370794)
                      (Pais-Registros (second LISTA-PAISES-REGISTRO-COMPLETO))) #t)
(check-expect (member (make-Registro 2014 114151)
                      (Pais-Registros (first LISTA-PAISES-RECALCULADA))) #t)
(check-expect (member (make-Registro 2014 36707873)
                      (Pais-Registros (second LISTA-PAISES-RECALCULADA))) #t)

;---------------------------------------------
; Sección 2 - Países superpoblados
;---------------------------------------------

; Los países con una población mayor a 1000 millones de habitantes en el 2019
; son paises superpoblados
; IMPORTANTE: Trabajamos con la lista de paises con registro completo
; y los valores del 2014 recalculados generada en la sección 1.

; Constante para establecer el mínimo de habitantes
; que hace que un país sea superpoblado
(define MILMILLONES 1000000000)

; last: (list a) -> a | Boolean
; Función auxiliar extra de ayuda, se recomienda utilizar
; last toma una lista y devuelve el último elemento si es no vacia
; Caso contrario devuelve false
(check-expect (last empty) #f)
(check-expect (last (list "trabajo practico 2")) "trabajo practico 2")
(check-expect (last (list 1 2 3 4 5 6 7)) 7)

(define (last l)
  (cond [(empty? l) #f]
        [(= (length l) 1) (first l)] 
        [else (last (rest l))]))

; predicado-superpoblados Pais -> Boolean
; predicado-superpoblados toma un país y devuelve #t en el caso que haya
; censado más de 1000 millones de habitantes en el 2019
; Suponemos que la última entrada de cada país se corresponde con el año 2019
(check-expect (predicado-superpoblados GUATEMALA) #f)
(check-expect (predicado-superpoblados CHINA) #t)

; predicado-superpoblados: Pais -> Boolean
; Es la funcion predicado usada en LISTA-PAISES-SUPERPOBLADOS
; Toma un pais y compara si ese pais tiene un regstro de poblacion
; mayor a MILMILLONES en el ultimo año censado.
; Luego la funcion filtrar-paises lo recursa hasta recorrer
; todos los registro de poblacion de ese pais.
(define (predicado-superpoblados pais)
  (> (Registro-Poblacion (last (Pais-Registros pais))) MILMILLONES)) 

; Lista de los paises superpoblados
; Usando la funcion filtrar-paises, filtra los paises que tengan 
; una poblacion mayor a MILMILLONES.
(define LISTA-PAISES-SUPERPOBLADOS
  (filtrar-paises LISTA-PAISES-RECALCULADA predicado-superpoblados)) 


; Nombres de los países superpoblados
; Toma el atributo de "Pais-Nombre" a la estructura de pais,
; que es cada elemento de la lista.
(define NOMBRES-PAISES-SUPERPOBLADOS (map Pais-Nombre LISTA-PAISES-SUPERPOBLADOS))

; Cantidad de países sobrepoblados
(define CANT-PAISES-SUPERPOBLADOS (length NOMBRES-PAISES-SUPERPOBLADOS))

; Porcentaje de población en países superpoblados:
; Suma de poblaciones de países superpoblados / población mundial
; Suponemos que la población mundial se calcula a partir de las poblaciones
; de todos los países de los cuales tenemos registro.

; operacion-sumar-poblaciones: Pais Number -> Pais
; Suma de poblaciones de un pais indicado, utilizando foldr.
; Se le pasa un pais en el cual se hace la suma
; y un numero n* que funciona como primer elemento para el foldr.
(check-expect (operacion-sumar-poblaciones ARGENTINA 0) 75000000)
(check-expect (operacion-sumar-poblaciones GUATEMALA 0) )

(define (operacion-sumar-poblaciones pais n)
  (+ n (foldr + 0 (map Registro-Poblacion (Pais-Registros pais)))))

; Indica la tasa de paises superpoblados con respecto al total.
(define TASA-POBLACION-SUPERPOBLADOS
  (/ (operar-sobre-paises LISTA-PAISES-SUPERPOBLADOS operacion-sumar-poblaciones 0)
     (operar-sobre-paises LISTA-PAISES operacion-sumar-poblaciones 0)))
