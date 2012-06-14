;;; juego-nuevo.lisp
;;; 
;;; Modificación del archivo juego para el modo máquina contra máquina
;;;============================================================================

(defvar *nodo-j-inicial*)

;Variable para contar los pasos
(defvar *pasos*)

;Estructura estadistica unitaria
(defstruct (solucion (:constructor crea-solucion)
		   (:conc-name nil)
		   (:print-function escribe-solucion))
  ganador
  pasos)
  
(defvar *sol*)

(defstruct (nodo-j (:constructor crea-nodo-j)
		   (:conc-name nil)
		   (:print-function escribe-nodo-j))
  estado
  jugador
  valor)

(defun escribe-nodo-j (nodo-j &optional (canal t) profundidad)
  (incf *pasos*)
  (format canal "~%Estado:~%~a~%Jugador : ~a ~%Pasos : ~a"
	  (estado nodo-j)
	  (jugador nodo-j)
		*pasos*))

(defun crea-nodo-j-inicial (jugador)
  (setf *nodo-j-inicial*
    (crea-nodo-j :estado *estado-inicial*
		 :jugador jugador)))

(defvar *procedimiento*)

(defun juego (&key (empieza-la-maquina? nil)
                   (procedimiento '(minimax-a-b 5))) ;;; minimax 3 modo easy
  (setf *procedimiento* procedimiento)
  (setf *pasos* 0)
  (incf (NumPartidas *estadisticas*))
  (cond (empieza-la-maquina? (crea-nodo-j-inicial 'max)
			     (if (es-estado-final *estado-inicial*)
				 (analiza-final *nodo-j-inicial*)
				 (jugada-maquina *nodo-j-inicial*)))
        (t (crea-nodo-j-inicial 'min)
	   (if (es-estado-final *estado-inicial*)
	       (analiza-final *nodo-j-inicial*)
	       (jugada-humana *nodo-j-inicial*)))))


(defun gana-negras-estadisticas ()
  (format t "~&Las negras han ganado")
  (incf (MovNegras *estadisticas*) *pasos*)
  (incf (PartidasGanadasNegras *estadisticas*)))

(defun gana-blancas-estadisticas ()
  (format t "~&Las blancas han ganado")
  (incf (MovBlancas *estadisticas*) *pasos*)
  (incf (PartidasGanadasBlancas *estadisticas*)))

(defun analiza-final (nodo-j-final)
  (escribe-nodo-j nodo-j-final)
  (format t "~&Pasos: ~a" (1+ *pasos*)) ;;RR
  (cond ((es-estado-ganador (estado nodo-j-final) 
			    (jugador nodo-j-final) 'max)
	 (gana-negras-estadisticas))
	((es-estado-ganador (estado nodo-j-final) 
			    (jugador nodo-j-final) 'min)
	 (gana-blancas-estadisticas))
	(t (format t "~&Empate"))))


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


(defun jugada-maquina (nodo-j)
  (escribe-nodo-j nodo-j)
  (format t "~%Mi turno.~&")
  (let ((siguiente (aplica-decision *procedimiento* nodo-j)))
    (if (es-estado-final (estado siguiente))
        (analiza-final siguiente)
        (if *maquina-vs-maquina?*
            (jugada-maquina siguiente)
            (jugada-humana siguiente)))))

