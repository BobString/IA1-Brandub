;;; juego.lsp
;;; Procedimiento de control de un juego.
;;;============================================================================


;;;****************************************************************************
;;;  Funciones y variables externas
;;;****************************************************************************

;;; Procedentes de la representacion del juego:
;;; *ESTADO-INICIAL*
;;;    => Estado inicial del juego.
;;; (ES-ESTADO-FINAL ESTADO)
;;;    => T, si el ESTADO es final; NIL, en caso contrario.
;;; (ES-ESTADO-GANADOR ESTADO TURNO JUGADOR)
;;;    => T, si y solo si en el ESTADO, cuando le toca mover al jugador TURNO,
;;;       el JUGADOR ha ganado la partida.
;;; *MOVIMIENTOS*
;;;    => Lista de las representaciones del los movimientos definidos en el
;;;       juego.
;;; (APLICA-MOVIMIENTO MOVIMIENTO ESTADO)
;;;    => Aplica el MOVIMIENTO al ESTADO para generar un nuevo estado. Si el
;;;       MOVIMIENTO no se puede aplicar devuelve NIL. 

;;; CREA-JUEGO
;;; ESCRIBE-ESTADO
;;; (ES-ESTADO-FINAL ESTADO)
;;;    => T, si el ESTADO es final; NIL, en caso contrario.
;;; ES-ESTADO-GANADOR
;;; ES-MOVIMIENTO-LEGAL
;;; APLICA-MOVIMIENTO

;;;****************************************************************************
;;;  Procedimientos de control de juegos.
;;;****************************************************************************

;;; La variable global *NODO-J-INICIAL* va a almacenar el nodo inicial del
;;;   arbol de desarrollo del juego.

(defvar *nodo-j-inicial*)

;;; Los nodos del arbol de desarrollo del juego seran del tipo de la estructura
;;;   NODO-J. Esta estructura tiene 3 campos: ESTADO, JUGADOR y VALOR. ESTADO
;;;   describira la situacion del juego, JUGADOR valdra MAX, si le toca jugar
;;;   al ordenador, o MIN, si le toca jugar al humano, y VALOR almacenara el
;;;   valor de la situacion del juego. 
;;; Funciones asociadas a la estructura:
;;;   CREA-NODO-J:     Funcion constructora.
;;;   ESTADO:          Funcion de acceso al campo ESTADO.
;;;   JUGADOR:         Funcion de acceso al campo JUGADOR.
;;;   VALOR:           Funcion de acceso al campo VALOR.
;;;   ESCRIBE-NODO-J:  Funcion de escritura.

(defstruct (nodo-j (:constructor crea-nodo-j)
		   (:conc-name nil)
		   (:print-function escribe-nodo-j))
  estado
  jugador
  valor)

;;; (ESCRIBE-NODO-J NODO-J &OPTIONAL CANAL PROFUNDIDAD)
;;; Efecto: Escribe en pantalla los campos ESTADO y JUGADOR del NODO-J.

(defun escribe-nodo-j (nodo-j &optional (canal t) profundidad)
  (format canal "~%Estado:~%~a~%Jugador : ~a"
	  (estado nodo-j)
	  (jugador nodo-j)))

;;; Dependencia funcional:
;;;    
;;;    SUCESORES
;;;       ESTADO
;;;       JUGADOR

;;; (CREA-NODO-J-INICIAL JUGADOR)
;;; Valor: El NODO-J asociado al *ESTADO-INICIAL* del juego, en el que el
;;;   JUGADOR comienza la partida.

(defun crea-nodo-j-inicial (jugador)
  (setf *nodo-j-inicial*
    (crea-nodo-j :estado *estado-inicial*
		 :jugador jugador)))

;;; Dependencia funcional:
;;;    
;;;    CREA-NODO-J-INICIAL
;;;       *NODO-J-INICIAL*
;;;       CREA-NODO-J
;;;       *ESTADO-INICIAL*    <- Representacion del juego

;;; La variable global *PROCEDIMIENTO* va a almacenar la descripcion del
;;;   procedimiento de decision que usara el ordenador para elegir movimientos.

(defvar *procedimiento*)

;;; (JUEGO &KEY EMPIEZA-LA-MAQUINA? PROCEDIMIENTO)
;;; Efecto: Controla el desarrollo de un juego.
;;; Procedimiento:
;;; 1. Asignar el PROCEDIMIENTO a la variable global *PROCEDIMIENTO*. Este
;;;    argumento contendra la descripcion del procedimiento de decision de la
;;;    maquina. 
;;; 2. Si el argumento EMPIEZA-LA-MAQUINA? vale T,
;;;    2.1.1 crear el nodo inicial del juego con MAX como jugador que comienza
;;;          la partida.
;;;    2.1.2 Si el *ESTADO-INICIAL* es un estado final,
;;;          2.1.2.1 analizar el nodo final *NODO-J-INICIAL*
;;;          2.1.2.2 en caso contrario, procesar la jugada de la maquina.
;;;    2.2 en caso contrario
;;;        2.2.1 crear el nodo inicial del juego con MIN como jugador que
;;;              comienza la partida.
;;;        2.2.2 Si el *ESTADO-INICIAL* es un estado final,
;;;              2.2.2.1 analizar el nodo final *NODO-J-INICIAL*
;;;              2.2.2.2 en caso contrario, procesar la jugada del humano.

(defun juego (&key (empieza-la-maquina? nil)
                   (procedimiento '(minimax-a-b 5))) ;;; minimax 3 modo easy
  (setf *procedimiento* procedimiento)
  (cond (empieza-la-maquina? (crea-nodo-j-inicial 'max)
			     (if (es-estado-final *estado-inicial*)
				 (analiza-final *nodo-j-inicial*)
				 (jugada-maquina *nodo-j-inicial*)))
        (t (crea-nodo-j-inicial 'min)
	   (if (es-estado-final *estado-inicial*)
	       (analiza-final *nodo-j-inicial*)
	       (jugada-humana *nodo-j-inicial*)))))

;;; Dependencia funcional:
;;;    
;;;    JUEGO
;;;       *PROCEDIMIENTO*
;;;       CREA-NODO-J-INICIAL
;;;       ES-ESTADO-FINAL     <- Representacion del juego
;;;       ANALIZA-FINAL    
;;;       JUGADA-MAQUINA   
;;;       JUGADA-HUMANA    

;;; (ANALIZA-FINAL NODO-J-FINAL)
;;; Efecto: Escribe en pantalla el NODO-J-FINAL y si en el NODO-J-FINAL gana la
;;;   maquina, escribe en pantalla "La maquina ha ganado", si gana el humano,
;;;   escribe en pantalla "El humano ha ganado", en caso contrario escribe en
;;;   pantalla "Empate". 

(defun analiza-final (nodo-j-final)
  (escribe-nodo-j nodo-j-final)
  (cond ((es-estado-ganador (estado nodo-j-final) 
			    (jugador nodo-j-final) 'max)
	 (format t "~&La maquina ha ganado"))
	((es-estado-ganador (estado nodo-j-final) 
			    (jugador nodo-j-final) 'min)
	 (format t "~&El humano ha ganado"))
	(t (format t "~&Empate"))))

;;; Dependencia funcional:
;;;    
;;;    ANALIZA-FINAL
;;;       ESCRIBE-NODO-J
;;;       ES-ESTADO-GANADOR   <- Representacion del juego
;;;       ESTADO
;;;       JUGADOR

;;; (ESCRIBE-MOVIMIENTOS)
;;; Efecto: Escribe en pantalla la lista de los movimientos permitidos en el
;;;   turno. 

;;; Versiones anteriores:
;(defun escribe-movimientos ()
;  (format t "~%Los movimientos permitidos son:")
;  (let ((numero 0))
;    (loop for i in *movimientos* do
;	  (format t "~%      ~a (~a)" i numero)
;	  (setf numero (+ numero 1)))))

;; (defun escribe-movimientos ()
;;   (format t "~%Los movimientos permitidos son:")
;;   (let ((numero 0))
;;     (loop for i in *movimientos* do
;; 	  (if (= (mod numero 3) 0)
;; 	      (format t "~%      ~a (~a)" i numero)
;; 	      (format t "      ~a (~a)" i numero))
;; 	  (setf numero (+ numero 1)))))



(defun movimientos-legales (estado)
  (loop for m in *movimientos*
	when (aplica-movimiento m estado)
	collect m))

(defun escribe-movimientos (movimientos)
  (format t "~%Los movimientos permitidos son:")
  (let ((numero 0))
    (loop for m in movimientos
	  do
	  (if (= (mod numero 4) 0)
	      (format t "~%   ~a (~a)" m numero)
	      (format t "   ~a (~a)" m numero))
	  (setf numero (+ numero 1)))))



;;; Dependencia funcional:
;;;    
;;;    ESCRIBE-MOVIMIENTOS
;;;       *MOVIMIENTOS*       <- Representacion del juego

;;; (JUGADA-HUMANA NODO-J)
;;; Efecto: Procesa la jugada humana.
;;; Procedimiento:
;;; 1. Escribe en pantalla el NODO-J
;;; 2. Escribe en pantalla la lista de los movimientos definidos en el juego.
;;; 3. Solicita del jugador humano una jugada, se trata de un numero que
;;;    representa el movimiento que el humano quiere hacer. Este numero es la
;;;    posicion en la que el movimiento aparece en la lista de *MOVIMIENTOS*.
;;; 4. Si el dato introducido por el jugador humano es un numero que se
;;;    corresponde con una posicion en la lista de *MOVIMIENTOS*, hacer:
;;;    4.1.1 Aplicar el movimiento elegido al estado del NODO-J, creando un
;;;          NUEVO-ESTADO.
;;;    4.1.2 Si el NUEVO-ESTADO no es NIL, hacer:
;;;          4.1.2.1.1 Crear el nodo SIGUIENTE con el NUEVO-ESTADO y en el que
;;;                    el jugador es MAX. 
;;;          4.1.2.1.2 Si el NUEVO-ESTADO es un estado final,
;;;                    4.1.2.1.2.1 analizar el nodo final *NODO-J-INICIAL*
;;;                    4.1.2.1.2.2 en caso contrario, procesar la jugada de la
;;;                                maquina. 
;;;          4.1.2.2 en caso contrario
;;;                  4.1.2.2.1 Escribir en pantalla que el movimiento elegido
;;;                            no se puede usar. 
;;;                  4.1.2.2.2 Procesar la jugada del humano.
;;;    4.2 en caso contrario hacer,
;;;        4.2.1 Escribir en pantalla que la eleccion es incorrecta.
;;;        4.2.2 Procesar la jugada del humano.



(defun jugada-humana (nodo-j)
  (escribe-nodo-j nodo-j)
  (let ((movimientos (movimientos-legales (estado nodo-j))))
    (escribe-movimientos movimientos) 
    (format t "~%Tu turno: ")
    (let ((m (read)))
      (cond ((and (integerp m)
		  (< -1 m (length movimientos)))
	     (let ((nuevo-estado 
		    (aplica-movimiento (nth m movimientos) (estado nodo-j))))
	       (cond (nuevo-estado
		      (let ((siguiente (crea-nodo-j
					:estado nuevo-estado
					:jugador 'max)))
			(if (es-estado-final nuevo-estado)
			    (analiza-final siguiente)
			  (jugada-maquina siguiente))))
		     (t (format t "~&   El movimiento ~a no se puede usar. " m)
			(jugada-humana nodo-j)))))
	    (t (format t "~&   ~a es ilegal. " m)
	       (jugada-humana nodo-j))))))



;;; Dependencia funcional:
;;;    
;;;    JUGADA-HUMANA
;;;       ESCRIBE-NODO-J
;;;       ESCRIBE-MOVIMIENTOS
;;;       APLICA-MOVIMIENTO   <- Representacion del juego
;;;       ESTADO
;;;       CREA-NODO-J
;;;       ES-ESTADO-FINAL     <- Representacion del juego
;;;       ANALIZA-FINAL
;;;       JUGADA-MAQUINA
;;;       JUGADA-HUMANA

;;; (JUGADA-MAQUINA NODO-J)
;;; Efecto: Procesa la jugada de la maquina.
;;; Procedimiento:
;;; 1. Escribe en pantalla el NODO-J
;;; 2. Aplica el *PROCEDIMIENTO* de decision al NODO-J y determina el nodo
;;;    SIGUIENTE 
;;; 3. Si el estado del nodo SIGUIENTE es un estado final,
;;;    3.1 analizar el nodo final SIGUIENTE
;;;    3.2 en caso contrario, procesar la jugada humana.

(defun jugada-maquina (nodo-j)
  (escribe-nodo-j nodo-j)
  (format t "~%Mi turno.~&")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))
    (if (es-estado-final (estado siguiente))
        (analiza-final siguiente)
        (if *maquina-vs-maquina?*
            (jugada-maquina siguiente)
            (jugada-humana siguiente)))))

;;; Dependencia funcional:
;;;    
;;;    JUGADA-MAQUINA
;;;       ESCRIBE-NODO-J
;;;       *PROCEDIMIENTO*     (procedimiento de decision empleado)
;;;       ES-ESTADO-FINAL     <- Representacion del juego
;;;       ANALIZA-FINAL    
;;;       JUGADA-HUMANA    

