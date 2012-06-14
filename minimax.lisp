;;; minimax.lsp
;;; Procedimientos minimax.
;;;============================================================================

;;;****************************************************************************
;;;  Funciones y variables externas
;;;****************************************************************************

;;; Procedentes de la representacion del juego:
;;; (ES-ESTADO-FINAL ESTADO)
;;;    => T, si el ESTADO es final; NIL, en caso contrario.
;;; (F-E-ESTATICA ESTADO JUGADOR)
;;;    => Valor de la funcion de evaluacion estatica para el ESTADO desde
;;;       el punto de vista del JUGADOR.
;;; *MINIMO-VALOR*
;;;    => Una cota inferior para los valores de la F-E-ESTATICA
;;; *MAXIMO-VALOR*
;;;    => Una cota superior para los valores de la F-E-ESTATICA

;;; Procedentes del proceso de control del juego:
;;; (ESTADO NODO-J)
;;;    => El estado del NODO-J
;;; (JUGADOR NODO-J)
;;;    => El jugador del NODO-J
;;; (VALOR NODO-J)
;;;    => El valor del NODO-J
;;; (CREA-NODO-J [:ESTADO ESTADO] [:JUGADOR JUGADOR] [:VALOR VALOR])
;;;    => El nodo-j para el cual posiblemente se han especificado el ESTADO, el
;;;       JUGADOR y el VALOR.

;;;****************************************************************************
;;;  Sucesores
;;;****************************************************************************

;;; (SUCESORES ESTADO)
;;; Valor: La lista de los sucesores del NODO-J.

(defun sucesores (nodo-j)
  (let ((resultado ()))
    (loop for movimiento in *movimientos* do
	  (let ((siguiente (aplica-movimiento movimiento 
					      (estado nodo-j))))
	    (when siguiente 
		  (push (crea-nodo-j :estado siguiente
				     :jugador (contrario (jugador nodo-j)))
			resultado))))
    (nreverse resultado)))

;;; Dependencia funcional:
;;;    
;;;    SUCESORES
;;;       *MOVIMIENTOS*          <- Representacion del juego
;;;       APLICA-MOVIMIENTO      <- Representacion del juego
;;;       ESTADO                 <- Control del juego
;;;       CREA-NODO-J            <- Control del juego
;;;       CONTRARIO       
;;;       JUGADOR                <- Control del juego

;;; (CONTRARIO JUGADOR)
;;; Valor: Devuelve MIN si JUGADOR es MAX, en caso contrario devuelve MAX.

(defun contrario (jugador)
  (if (eq jugador 'max)
      'min
      'max))

;;;****************************************************************************
;;;  Forma de aplicar los procedimientos de decision:
;;;****************************************************************************

;;; (APLICA-DECISION PROCEDIMIENTO NODO-J)
;;; Valor: El resultado de aplicar el PROCEDIMIENTO de decision al NODO-J. En
;;;   este caso la descripcion del PROCEDIMIENTO de decision consiste en una
;;;   lista cuyo primer elemento es el nombre de la funcion a aplicar (MINIMAX
;;;   o MINIMAX-A-B) y cuyo segundo argumento es la profundidad de analisis.

(defun aplica-decision (procedimiento nodo-j)
  (funcall (symbol-function (first procedimiento))
	   nodo-j
	   (second procedimiento)))

;;;****************************************************************************
;;;  Procedimiento minimax
;;;****************************************************************************

;;; (MINIMAX NODO-J PROFUNDIDAD)
;;; Valor: Un nodo con el valor estatico del estado de NODO-J, si este es un
;;;   estado final, se ha alcanzado la profundidad de analisis maxima o no
;;;   tiene sucesores. En caso contrario el mejor sucesor del NODO-J segun el
;;;   procedimiento minimax.
;;; Procedimiento:
;;; 1. Si el estado de NODO-J es un final o la profundidad de analisis restante
;;;    es cero,
;;;    1.1. devolver un nodo-j cuyo valor sea el valor estatico del estado de
;;;    NODO-J.
;;;    1.2. en caso contrario, hacer
;;;         1.2.1 Calcular los SUCESORES del NODO-J
;;;         1.2.2 Si la lista de SUCESORES es vacia
;;;               1.2.2.1 devolver un nodo-j cuyo valor sea el valor estatico
;;;                       del estado de NODO-J.
;;;               1.2.2.2 en caso contrario, si el jugador del NODO-J es MAX
;;;                       1.2.2.2.1 devolver el nodo de la lista de SUCESORES
;;;                                 con mayor valor minimax 
;;;                       1.2.2.2.2 devolver el nodo de la lista de SUCESORES
;;;                                 con menor valor minimax 

(defun minimax (nodo-j profundidad)
  (if (or (es-estado-final (estado nodo-j))
          (= profundidad 0))		                         ;1
      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
					(jugador nodo-j)))       ;1.1
      (let ((sucesores (sucesores nodo-j)))			 ;1.2.1
	(if (null sucesores)					 ;1.2.2
	    (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
					      (jugador nodo-j))) ;1.2.2.1
	    (if (eq (jugador nodo-j) 'max)			 ;1.2.2.2
		(maximizador sucesores profundidad)		 ;1.2.2.2.1
	        (minimizador sucesores profundidad))))))	 ;1.2.2.2.2

;;; Dependencia funcional:
;;;    
;;;    MINIMAX
;;;       ES-ESTADO-FINAL        <- Representacion del juego
;;;       CREA-NODO-J            <- Control del juego
;;;       F-E-ESTATICA           <- Representacion del juego
;;;       ESTADO                 <- Control del juego
;;;       JUGADOR                <- Control del juego
;;;       SUCESORES
;;;       MINIMIZADOR
;;;       MAXIMIZADOR

;;; (MAXIMIZADOR SUCESORES PROFUNDIDAD)
;;; Valor: El nodo de la lista de SUCESORES con mayor valor minimax
;;; Procedimiento:
;;; 1. Crear las siguientes variables locales
;;;    1.1. MEJOR-SUCESOR (para almacenar el mejor sucesor encontrado hasta el
;;;         momento), cuyo valor es el primer elemento de la lista de SUCESORES
;;;    1.2. MEJOR-VALOR (para almacenar el mejor valor encontrado hasta el
;;;         momento), cuyo valor es el *MINIMO-VALOR* que puede llegar a tomar
;;;         la funcion de evaluacion estatica.
;;; 2. Para todo SUCESOR de la lista de SUCESORES hacer
;;;    2.1. Calcular el VALOR minimax de SUCESOR, disminuyendo en 1 la
;;;         profundidad de analisis
;;;    2.2. Cuando el VALOR calculado sea mayor que el MEJOR-VALOR, hacer
;;;         2.2.1 Actualizar el MEJOR-VALOR, que pasa a ser VALOR
;;;         2.2.2 Actualizar el MEJOR-SUCESOR, que pasa a ser SUCESOR
;;; 3. Poner el MEJOR-VALOR como valor del MEJOR-SUCESOR.
;;; 4. Devolver el MEJOR-SUCESOR.

(defun maximizador (sucesores profundidad)
  (let ((mejor-sucesor (first sucesores))			      ;1.1
	(mejor-valor *minimo-valor*))				      ;1.2
    (loop for sucesor in sucesores do				      ;2
	  (setf valor (valor (minimax sucesor (1- profundidad))))     ;2.1
	  (when (> valor mejor-valor)				      ;2.2
		(setf mejor-valor valor)			      ;2.2.1
		(setf mejor-sucesor sucesor)))			      ;2.2.2
    (setf (valor mejor-sucesor) mejor-valor)			      ;3
    mejor-sucesor))						      ;4

;;; Dependencia funcional:
;;;    
;;;    MAXIMIZADOR
;;;       *MINIMO-VALOR*         <- Representacion del juego
;;;       VALOR                  <- Control del juego
;;;       MINIMAX

;;; (MINIMIZADOR SUCESORES PROFUNDIDAD)
;;; Valor: El nodo de la lista de SUCESORES con menor valor minimax
;;; Procedimiento:
;;; 1. Crear las siguientes variables locales
;;;    1.1. MEJOR-SUCESOR (para almacenar el mejor sucesor encontrado hasta el
;;;         momento), cuyo valor es el primer elemento de la lista de SUCESORES
;;;    1.2. MEJOR-VALOR (para almacenar el mejor valor encontrado hasta el
;;;         momento), cuyo valor es el *MAXIMO-VALOR* que puede llegar a tomar
;;;         la funcion de evaluacion estatica.
;;; 2. Para todo SUCESOR de la lista de SUCESORES hacer
;;;    2.1. Calcular el VALOR minimax de SUCESOR, disminuyendo en 1 la
;;;         profundidad de analisis
;;;    2.2. Cuando el VALOR calculado sea menor que el MEJOR-VALOR, hacer
;;;         2.2.1 Actualizar el MEJOR-VALOR, que pasa a ser VALOR
;;;         2.2.2 Actualizar el MEJOR-SUCESOR, que pasa a ser SUCESOR
;;; 3. Poner el MEJOR-VALOR como valor del MEJOR-SUCESOR.
;;; 4. Devolver el MEJOR-SUCESOR.

(defun minimizador (sucesores profundidad)
  (let ((mejor-sucesor (first sucesores))			      ;1.1
	(mejor-valor *maximo-valor*))				      ;1.2
    (loop for sucesor in sucesores do				      ;2
	  (setf valor (valor (minimax sucesor (1- profundidad))))     ;2.1
	  (when (< valor mejor-valor)				      ;2.2
		(setf mejor-valor valor)			      ;2.2.1
		(setf mejor-sucesor sucesor)))			      ;2.2.2
    (setf (valor mejor-sucesor) mejor-valor)			      ;3
    mejor-sucesor))						      ;4

;;; Dependencia funcional:
;;;    
;;;    MINIMIZADOR
;;;       *MAXIMO-VALOR*         <- Representacion del juego
;;;       VALOR                  <- Control del juego
;;;       MINIMAX

;;;****************************************************************************
;;;  Procedimiento alfa-beta
;;;****************************************************************************

;;; ALFA y BETA representan, respectivamente, las cotas inferior y superior de
;;; los valores que se van a ir buscando en la parte del subarbol que queda por
;;; explorar. Si ALFA llega a superar o igualar a BETA, entonces no sera
;;; necesario seguir analizando las ramas del subarbol.

;;; (MINIMAX-A-B NODO-J PROFUNDIDAD ALFA BETA)
;;; Valor: Un nodo con el valor estatico del estado de NODO-J, si este es un
;;;   estado final, se ha alcanzado la profundidad de analisis maxima o no
;;;   tiene sucesores. En caso contrario el mejor sucesor del NODO-J segun el
;;;   procedimiento minimax-a-b.
;;; Procedimiento:
;;; 1. Si el estado de NODO-J es un final o la profundidad de analisis restante
;;;    es cero,
;;;    1.1. devolver un nodo-j cuyo valor sea el valor estatico del estado de
;;;    NODO-J.
;;;    1.2. en caso contrario, hacer
;;;         1.2.1 Calcular los SUCESORES del NODO-J
;;;         1.2.2 Si la lista de SUCESORES es vacia
;;;               1.2.2.1 devolver un nodo-j cuyo valor sea el valor estatico
;;;                       del estado de NODO-J.
;;;               1.2.2.2 en caso contrario, si el jugador del NODO-J es MAX
;;;                       1.2.2.2.1 devolver el nodo de la lista de SUCESORES
;;;                                 obtenido con la funcion MAXIMIZADOR-A-B 
;;;                       1.2.2.2.2 devolver el nodo de la lista de SUCESORES
;;;                                 obtenido con la funcion MINIMIZADOR-A-B 

(defun minimax-a-b (nodo-j profundidad
			   &optional (alfa *minimo-valor*) 
			   (beta *maximo-valor*))
  (if (or (es-estado-final (estado nodo-j))
          (= profundidad 0))
      (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
					(jugador nodo-j)))
      (let ((sucesores (sucesores nodo-j)))
        (if (null sucesores)
            (crea-nodo-j :valor (f-e-estatica (estado nodo-j)
					      (jugador nodo-j)))
	  (if (eq (jugador nodo-j) 'max)
	      (maximizador-a-b
	       (sort sucesores #'>
		     :key
		     (lambda (nodo) (f-e-estatica (estado nodo) 'min)))
	       profundidad alfa beta)
	    (minimizador-a-b
	     (sort sucesores #'<
		   :key
		   (lambda (nodo) (f-e-estatica (estado nodo) 'max)))
	     profundidad alfa beta))))))



;;; Dependencia funcional:
;;;    
;;;    MINIMAX-A-B
;;;       *MINIMO-VALOR*         <- Representacion del juego
;;;       *MAXIMO-VALOR*         <- Representacion del juego
;;;       ES-ESTADO-FINAL        <- Representacion del juego
;;;       CREA-NODO-J            <- Control del juego
;;;       F-E-ESTATICA           <- Representacion del juego
;;;       ESTADO                 <- Control del juego
;;;       JUGADOR                <- Control del juego
;;;       SUCESORES
;;;       MINIMIZADOR-A-B
;;;       MAXIMIZADOR-A-B

;;; (MAXIMIZADOR-A-B SUCESORES PROFUNDIDAD ALFA BETA)
;;; Valor: El nodo de la lista de SUCESORES con mayor valor minimax-a-b,
;;;   siempre que este supere al valor de ALFA, en caso contrario, un sucesor
;;;   cualquiera (el primero que se genere).
;;; Procedimiento:
;;; 1. Crear las siguientes variables locales
;;;    1.1. MEJOR-SUCESOR (para almacenar el mejor sucesor encontrado hasta el
;;;         momento o el sucesor por defecto en caso de no encotrar ninguno que
;;;         mejore la cota ALFA), cuyo valor es el primer elemento de la lista
;;;         de SUCESORES
;;;    1.2. VALOR (para almacenar el mejor valor encontrado hasta el momento),
;;;         cuyo valor inicial es 0.
;;; 2. Para todo SUCESOR de la lista de SUCESORES hacer
;;;    2.1. Calcular el VALOR minimax-a-b de SUCESOR, disminuyendo en 1 la
;;;         profundidad de analisis y utilizando los valores de ALFA y BETA.
;;;    2.2. Cuando el VALOR calculado sea mayor que ALFA, hacer
;;;         2.2.1 Actualizar ALFA, que pasa a ser VALOR
;;;         2.2.2 Actualizar el MEJOR-SUCESOR, que pasa a ser SUCESOR
;;;    2.3. Cuando el valor de ALFA supere o iguale al de BETA, terminar el
;;;         bucle: se ha producido un corte alfa.
;;; 3. Poner ALFA como valor del MEJOR-SUCESOR.
;;; 4. Devolver el MEJOR-SUCESOR.

(defun maximizador-a-b (sucesores profundidad alfa beta)
  (let ((mejor-sucesor (first sucesores))
        (valor 0))
    (loop for sucesor in sucesores do
	  (setf valor
		(valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
	  (when (> valor alfa)
		(setf alfa valor)
		(setf mejor-sucesor sucesor))
	  (when (>= alfa beta)
;		(format t "~&Corte alfa~%")
		(return)))
    (setf (valor mejor-sucesor) alfa)
    mejor-sucesor))

;;; Dependencia funcional:
;;;    
;;;    MAXIMIZADOR-A-B
;;;       VALOR                  <- Control del juego
;;;       MINIMAX-A-B

;;; (MAXIMIZADOR-A-B SUCESORES PROFUNDIDAD ALFA BETA)
;;; Valor: El nodo de la lista de SUCESORES con menor valor minimax-a-b,
;;;   siempre que este sea menor que BETA, en caso contrario, un sucesor
;;;   cualquiera (el primero que se genere).
;;; Procedimiento:
;;; 1. Crear las siguientes variables locales
;;;    1.1. MEJOR-SUCESOR (para almacenar el mejor sucesor encontrado hasta el
;;;         momento o el sucesor por defecto en caso de no encotrar ninguno que
;;;         mejore la cota BETA), cuyo valor es el primer elemento de la lista
;;;         de SUCESORES
;;;    1.2. VALOR (para almacenar el mejor valor encontrado hasta el momento),
;;;         cuyo valor inicial es 0.
;;; 2. Para todo SUCESOR de la lista de SUCESORES hacer
;;;    2.1. Calcular el VALOR minimax-a-b de SUCESOR, disminuyendo en 1 la
;;;         profundidad de analisis y utilizando los valores de ALFA y BETA.
;;;    2.2. Cuando el VALOR calculado sea menor que BETA, hacer
;;;         2.2.1 Actualizar BETA, que pasa a ser VALOR
;;;         2.2.2 Actualizar el MEJOR-SUCESOR, que pasa a ser SUCESOR
;;;    2.3. Cuando el valor de ALFA supere o iguale al de BETA, terminar el
;;;         bucle: se ha producido un corte beta.
;;; 3. Poner BETA como valor del MEJOR-SUCESOR.
;;; 4. Devolver el MEJOR-SUCESOR.

(defun minimizador-a-b (sucesores profundidad alfa beta)
  (let ((mejor-sucesor (first sucesores))
        (valor 0))
    (loop for sucesor in sucesores do
	  (setf valor
		(valor (minimax-a-b sucesor (1- profundidad) alfa beta)))
	  (when (< valor beta)
		(setf beta valor)
		(setf mejor-sucesor sucesor))
	  (when (>= alfa beta)
;		(format t "~&Corte beta~%")
		(return)))
    (setf (valor mejor-sucesor) beta)
    mejor-sucesor))

;;; Dependencia funcional:
;;;    
;;;    MINIMIZADOR-A-B
;;;       VALOR                  <- Control del juego
;;;       MINIMAX-A-B

