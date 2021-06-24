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
(define (armar-registros lf lnh)
  (cond [(empty? lnh) empty]
        [(empty? lf) empty]
        [else (cons (make-Registro (first lf) (string->number (first lnh)))
                    (armar-registros (rest lf) (rest lnh)))]))

; Algunos casos de test para la funcion armar registros
; (list 2005 2006) (list "3" "4") : (list (make-Registro 2005 3) (make-Registro 2006 4))
; (list 2005 2006) empty : empty
; empty (list (make-Registro 2005 3) : empty
; empty empty : empty
(check-expect (armar-registros (list 2005 2006) (list "3" "4"))
              (list (make-Registro 2005 3) (make-Registro 2006 4)))
(check-expect (armar-registros (list 2005 2006) empty) empty)
(check-expect (armar-registros empty (list (make-Registro 2005 3))) empty)
(check-expect (armar-registros empty empty) empty)


; armar-lista-registros List(List(String)) List(Number) -> List(List(Registro))
; armar-lista-registros toma una lista de listas de strings que representan cantidad de habitantes
; *lnh* y una lista de años *lf* y devuelve una lista de listas de Registro *lr*
; Los campos Población de los elementos de *lr* coinciden con los elementos de *lnh*
; y su campo fecha se completa con los años de *lf*.
(define (armar-lista-registros lnh lf)
  (cond [(empty? lnh) empty]
        [(empty? lf) empty]
        [else (cons (armar-registros lf (first lnh))
                    (armar-lista-registros (rest lnh) lf))]))

; Algunos casos de test para la funcion armar-lista-registros
; empty empty : empty
; (list (list "3" "4")) empty : empty
; empty (list 2005 2006) : empty
; (list 
;   (list "3" "4")
;   (list "5" "6")
;   (list "7" "8"))
;   (list 2005 2006))
; :
; (list 
;   (list 
;     (make-Registro 2005 3)
;     (make-Registro 2006 4)) 
;   (list 
;     (make-Registro 2005 5)
;     (make-Registro 2006 6))
;   (list 
;     (make-Registro 2005 7)
;     (make-Registro 2006 8))))
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


; Datos para construir registros (sin nombres de países)
(define LISTA-DATOS-REGISTRO (map rest DATOS-PAISES))

; Algunos casos de prueba para LISTA-DATOS-REGISTRO
; member "54211" : True
; member "0" : False
(check-expect (member "54211" (first LISTA-DATOS-REGISTRO)) #t)
(check-expect (member "0" (first LISTA-DATOS-REGISTRO)) #f)


; Lista de registros de cada país
(define LISTA-REGISTROS (armar-lista-registros LISTA-DATOS-REGISTRO RANGO-FECHA))
; Algunos casos de prueba para LISTA-REGISTROS
; Nos fijamos que dentro de los registros este el registro 1960 54211 que corresponde al primer registro de la DB
(check-expect (member (make-Registro 1960 54211) (first LISTA-REGISTROS)) #t)
; Nos fijamos que dentro de los registros no este 0 0 pues el año 0 no deberia estar y no deberia haber paises con 0 habitantes
(check-expect (member (make-Registro 0 0) (first LISTA-REGISTROS)) #f)

; armar-paises: List(String) List(Registro) -> List(Pais)
; armar-paises toma una lista de nombres de países *lnp* y una lista de registros
; poblacionales *lr* y devuelve una lista de estructura Pais *lp* en donde
; el i-ésimo elemento de *lp* tiene de campo Nombre el i-ésimo elemento
; de *lnp* y de campo Registros el i-ésimo elemento de *lr*
(define (armar-paises lnp lr)
  (cond [(empty? lnp) empty]
        [(empty? lr) empty]
        [else (cons   (make-Pais (first lnp) (first lr) ) 
                      (armar-paises (rest lnp) (rest lr) ) 
              )
        ]))

(check-expect (armar-paises (list "Angola" "Andorra" "Argentina") empty) empty)
(check-expect (armar-paises empty (list (list (make-Registro 2005 3) (make-Registro 2006 4)))) empty)
(check-expect (armar-paises (list "Angola" "Andorra" "Argentina")
                            (list (list (make-Registro 2005 3) (make-Registro 2006 4))
                                  (list (make-Registro 2005 5) (make-Registro 2006 6))
                                  (list (make-Registro 2005 7) (make-Registro 2006 8))))
              (list (make-Pais "Angola" (list (make-Registro 2005 3) (make-Registro 2006 4)))
                    (make-Pais "Andorra" (list (make-Registro 2005 5) (make-Registro 2006 6)))
                    (make-Pais "Argentina" (list (make-Registro 2005 7) (make-Registro 2006 8)))))


; Nombres de países
(define LISTA-NOMBRE-PAISES (map first DATOS-PAISES))
; Nos fijamos que china este en la lista de nombres de paises
; (member "China") : True
(check-expect (member "China" LISTA-NOMBRE-PAISES) #t)
; Nos fijamos que Hyrule no este en la lista de nombres de paises (puesto que no existe)
; (member "Hyrule") : False
(check-expect (member "Hyrule" LISTA-NOMBRE-PAISES) #f)

; Lista de países
(define LISTA-PAISES (armar-paises LISTA-NOMBRE-PAISES LISTA-REGISTROS))
; Nos fijamos que china este en la lista de paises
; (member "China") : True
(check-expect (member "China" (map Pais-Nombre LISTA-PAISES)) #t)
; Nos fijamos que Hyrule no este en la lista de paises (puesto que no existe)
; (member "Hyrule") : False
(check-expect (member "Hyrule" (map Pais-Nombre LISTA-PAISES)) #f)

;---------------------Funciones de alto orden sobre listas de países------------------------

; transformar-paises: List(Pais) (Pais -> Pais) -> List(Pais)
; Toma una lista de paises y una funcion "predicado", en este caso
; llamada transformar que toma un pais (primer elemento de la lista)
; lo transforma y lo concatena con el resto de forma recursiva.
(define (transformar-paises lp transformacion)
  (cond [(empty? lp) empty]
        [else (cons (transformacion (first lp))
                    (transformar-paises (rest lp) transformacion))]))
; Algunos casos de test para la funcion transformar-paises
; Caso en el que le pasemos un lista vacia
; '() : '()
(check-expect (transformar-paises '() transformacion-recalcular) '() )
; LISTA-PAISES-TEST : list ANDORRA GUATEMALA (make-Pais "Angentina" (list (make-Registro 2014 38500000)
;                                                                     (make-Registro 2015 40000000))) CHINA)
(check-expect (transformar-paises LISTA-PAISES-TEST transformacion-recalcular) 
              (list ANDORRA GUATEMALA (make-Pais "Angentina" (list (make-Registro 2014 38500000)
                                                                  (make-Registro 2015 40000000))) 
                    CHINA))   

; filtrar-paises: List(Pais) (Pais -> Boolean)-> List(Pais)
; filtrar-paises toma una lista de paises lp*
; y segun el predicado que se le haya pasado predicado*
; los va filtrando, agregando a los aprobados por el predicado
; en una nueva lista y eliminando los que no.
(define (filtrar-paises lp predicado)
  (cond [(empty? lp) empty]
        [else (if (predicado (first lp))
                  (cons (first lp) (filtrar-paises (rest lp) predicado))
                  (filtrar-paises (rest lp) predicado))]))
 
; Algunos casos de test para la funcion filtrar-paises
; '() : '()
; LISTA-PAISES-TEST : list ARGENTINA CHINA
(check-expect (filtrar-paises '() predicado-registro-incompleto ) '() )
(check-expect (filtrar-paises LISTA-PAISES-TEST predicado-registro-incompleto ) (list ARGENTINA CHINA) )

; operar-sobre-paises: List(Pais) (Pais X -> X) X -> X
; Toma una lista de paises lp* y le aplica un operador a cada uno operador*,
; cuando llegue al final de la lista se le aplica el numero neutro
; que depende del tipo de operacion que se haga hecho, nuetro*.
(define (operar-sobre-paises lp operador neutro)
  (cond [(empty? lp) neutro]
        [else (operador (first lp) (operar-sobre-paises (rest lp) operador neutro))]))
 
; Algunos casos de test de la funcion operar-sobre-paises
; '() operacion-sumar-poblaciones 0 : 0 
; (list ARGENTINA CHINA) operacion-sumar-poblaciones 0 : 2075000001
(check-expect (operar-sobre-paises '() operacion-sumar-poblaciones 0) 0)
(check-expect (operar-sobre-paises (list ARGENTINA CHINA) operacion-sumar-poblaciones 0) 2075000001)

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
(define (registro-incompleto  lr) (cond
  [(empty? lr) #t]
  [else (if (= (Registro-Poblacion (first lr) ) -1)
            #f
         (registro-incompleto (rest lr) ))]))
; Algunos casos de test para registro-incompleto
; ANDORRA : False
; ARGENTINA : True
(check-expect (registro-incompleto (Pais-Registros ANDORRA) ) #f)
(check-expect (registro-incompleto (Pais-Registros ARGENTINA) ) #t)


; predicado-registro-incompleto: (Pais -> Boolean)
; Funcion auxiliar para LISTA-PAISES-REGISTRO-COMPLETO
; Toma un pais y devuelve true o false dependiendo si se verifica
; registro-incompleto para el registro del pais que se le paso.
(define (predicado-registro-incompleto p) (registro-incompleto (Pais-Registros p)))
; Algunos casos de test para la funcion predicado-registro-incompleto
; ANDORRA : False
; ARGENTINA : True
(check-expect (predicado-registro-incompleto ANDORRA) #f)
(check-expect (predicado-registro-incompleto ARGENTINA) #t)

; Lista de paises con registro completo.
; Filtra la lista de paises registrados
; eliminado los que poseen un registro incompleto.
(define LISTA-PAISES-REGISTRO-COMPLETO
  (filtrar-paises LISTA-PAISES predicado-registro-incompleto))
; Algunos casos de test para LISTA-PAISES-REGISTRO-COMPLETO
; "West Bank and Gaza" no tiene que estar en el listado de países
; con registro completo pero sí tienen que estar en el listado de países
(check-expect (member "West Bank and Gaza" (map Pais-Nombre LISTA-PAISES)) #t)
(check-expect (member "West Bank and Gaza" (map Pais-Nombre LISTA-PAISES-REGISTRO-COMPLETO)) #f)


; Los censos del 2014 se calcularon mal. Aumentarlos un 10%.
; Trabajamos con la lista de países con registro completo

; recalculo: Registro -> Registro
; Función auxiliar extra de ayuda, se recomienda utilizar
; recalculo toma un registro e incrementa la población en un 10%
; si la fecha en la que se condujo el censo fue 2014
; En caso de que el incremento resulte en un número no entero,
; se redondea

(define (recalculo reg)
  (if (= (Registro-Fecha reg) 2014)
      (make-Registro (Registro-Fecha reg) (exact-round (* 1.1 (Registro-Poblacion reg))))
      (make-Registro (Registro-Fecha reg) (Registro-Poblacion reg))))
; Algunos casos de test para la funcion recalculo
; (make-Registro 2014 10) : (make-Registro 2014 11)
; (make-Registro 2016 10) : (make-Registro 2016 10)
(check-expect (recalculo (make-Registro 2014 10)) (make-Registro 2014 11))
(check-expect (recalculo (make-Registro 2016 10)) (make-Registro 2016 10))

; transformacion-recalcular: Pais -> Pais
; transformacion-recalcular toma un país y aumenta en 10% la población asociada
; al año 2014 en sus registros.
(define (transformacion-recalcular pais)
  (make-Pais (Pais-Nombre pais) (map recalculo (Pais-Registros pais))))
; Algunos casos de test de transformacion-recalcular
; Argentina : (make-Pais "Angentina" (list (make-Registro 2014 38500000)
;                                          (make-Registro 2015 40000000)))
; CHINA : CHINA
(check-expect (transformacion-recalcular ARGENTINA)
              (make-Pais "Angentina" (list (make-Registro 2014 38500000)
                                           (make-Registro 2015 40000000))))
(check-expect (transformacion-recalcular CHINA) CHINA)


; Lista de paises recalculados.
; Agarra la lista de paises con registro completo
; y los transforma utilizanco la funcion transformacion-recalcular.
(define LISTA-PAISES-RECALCULADA
  (transformar-paises LISTA-PAISES-REGISTRO-COMPLETO transformacion-recalcular))
; Algunos casos de test para LISTA-PAISES-RECALCULADA
; Aruba 2014 original : 103774
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
(define (last l)
  (cond [(empty? l) #f]
        [(= (length l) 1) (first l)] 
        [else (last (rest l))]))
; Algunos casos de test para la funcion transformacion-recalcular
; empty : False
; (list "trabajo practico 2") : "trabajo practico 2"
; (list 1 2 3 4 5 6 7) : 7
(check-expect (last empty) #f)
(check-expect (last (list "trabajo practico 2")) "trabajo practico 2")
(check-expect (last (list 1 2 3 4 5 6 7)) 7)

; predicado-superpoblados Pais -> Boolean
; predicado-superpoblados toma un país y devuelve #t en el caso que haya
; censado más de 1000 millones de habitantes en el 2019
; Suponemos que la última entrada de cada país se corresponde con el año 2019
(define (predicado-superpoblados pais)
  (> (Registro-Poblacion (last (Pais-Registros pais))) MILMILLONES)) 
; Algunos casos de test redicado-superpoblados
; En este caso le pasamos el pais GUATEMALA
; y debe devolver false
; GUATEMALA : False
(check-expect (predicado-superpoblados GUATEMALA) #f)
; Y en este caso le pasamos el pais CHINA
; y debe devolver true
; CHINA : True
(check-expect (predicado-superpoblados CHINA) #t)

; Lista de los paises superpoblados
; Usando la funcion filtrar-paises, filtra los paises que tengan 
; una poblacion mayor a MILMILLONES.
(define LISTA-PAISES-SUPERPOBLADOS
  (filtrar-paises LISTA-PAISES-RECALCULADA predicado-superpoblados)) 
; Algunos casos de test para la lista LISTA-PAISES-SUPERPOBLADOS
; member "China" : True
; member "Aruba" : False
(check-expect (member "China" (map Pais-Nombre LISTA-PAISES-SUPERPOBLADOS)) #t)
(check-expect (member "Aruba" (map Pais-Nombre LISTA-PAISES-SUPERPOBLADOS)) #f)

; Nombres de los países superpoblados
; Toma el atributo de "Pais-Nombre" a la estructura de pais,
; que es cada elemento de la lista.
(define NOMBRES-PAISES-SUPERPOBLADOS (map Pais-Nombre LISTA-PAISES-SUPERPOBLADOS))
; Algunos casos de test para la lista NOMBRES-PAISES-SUPERPOBLADOS
; member "China" : True
; member "Bermuda" : False
(check-expect (member "China" NOMBRES-PAISES-SUPERPOBLADOS) #t)
(check-expect (member "Bermuda" NOMBRES-PAISES-SUPERPOBLADOS) #f)

; Cantidad de países sobrepoblados
(define CANT-PAISES-SUPERPOBLADOS (length NOMBRES-PAISES-SUPERPOBLADOS))
; Caso de test utilizando la base de datos DB brindada
(check-expect CANT-PAISES-SUPERPOBLADOS 25)


; Porcentaje de población en países superpoblados:
; Suma de poblaciones de países superpoblados / población mundial
; Suponemos que la población mundial se calcula a partir de las poblaciones
; de todos los países de los cuales tenemos registro.

; operacion-sumar-poblaciones: Pais Number -> Pais
; Suma de un tal numero n* con la suma de los Registro-Poblacion
; del registro de del pais que se le paso a la funcion pais*.
(define (operacion-sumar-poblaciones pais n)
  (+ n (foldr + 0 (map Registro-Poblacion (Pais-Registros pais))))) 
; Algunos casos de test para la funcion operacion-sumar-poblaciones
; ARGENTINA : 75000000
; GUATEMALA : 32499999
(check-expect (operacion-sumar-poblaciones ARGENTINA 0) 75000000)
(check-expect (operacion-sumar-poblaciones GUATEMALA 0) 32499999)

; Indica la tasa de paises superpoblados con respecto al total.
(define TASA-POBLACION-SUPERPOBLADOS
  (/ (operar-sobre-paises LISTA-PAISES-SUPERPOBLADOS operacion-sumar-poblaciones 0)
     (operar-sobre-paises LISTA-PAISES operacion-sumar-poblaciones 0)))
; Caso en el que calculamos la tasa-poblacion-superpoblados. Para esto nos basamos en los datos de la DB brindada
(check-expect (/ (round (* 1000 TASA-POBLACION-SUPERPOBLADOS)) 1000) 0.817)

