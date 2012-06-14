;;; ========================================
;;; Trabajo de Inteligencia Artificial 1
;;;
;;; Martin Muñoz, Roberto
;;; Rodas de Paz, Alejandro
;;; ========================================


;;; ===== Funciones auxiliares =====

;;; Devuelve el primer índice de un elemento en un estado
(defun index (elem estado)
  (let ((lista (tablero estado)))
    (loop for i below (length lista) when (member elem (nth i lista)) do
	 (return (list (position elem (nth i lista)) i)))))

;;; Devuelve la suma las coordenadas de dos posiciones
(defun suma-coordenadas (pos1 pos2)
  (list (+ (first pos1) (first pos2)) (+ (second pos1) (second pos2))))

;;; Devuelve el rango de puntos entre el origen y el destino (ambos inclusive)
(defun diferencia-puntos (origen destino)
  (let* ((i-origen (first origen))
	 (j-origen (second origen))
	 (i-destino (first destino))
	 (j-destino (second destino))
	 (i-min (min i-origen i-destino))
	 (i-max (max i-origen i-destino))
	 (j-min (min j-origen j-destino))
	 (j-max (max j-origen j-destino)))
    (loop for x from i-min to i-max append
	 (loop for y from j-min to j-max collect (list x y)))))

(defun obtiene-pieza (casilla estado)
  (let ((i (first casilla))
	(j (second casilla)))
    (nth i (nth j (tablero estado)))))

(defun coloca-pieza (casilla pieza estado)
  (let ((i (first casilla))
	(j (second casilla)))
    (setf (nth i (nth j (tablero estado))) pieza)))

(defun borra-pieza (casilla estado)
  (coloca-pieza casilla 'XX  estado))

(defun fuera-tablero (casilla)
  (let ((x (first casilla))
	(y (second casilla)))
    (or (> 0 x) (< 6 x) (> 0 y) (< 6 y))))


;;; ===== Representación del "Brandub" =====

(defstruct (estado (:constructor crea-estado)
		   (:conc-name nil)
		   (:print-function escribe-estado))
  tablero
  bando)

(defun copy-estado (estado)
  (crea-estado :tablero (copy-tree (tablero estado)) :bando (bando estado)))

(defun escribe-estado (estado canal prof)
  (let ((tablero (tablero estado)))
    (loop for fila in tablero do (format canal "~a~%" fila))
    (format canal "Bando: ~a~%" (bando estado))))

(defun crea-casilla (i j)
  (cond ((and (= i 3) (= j 3)) 'RB) ((and (= i 3) (= j 2)) 'B1)
	((and (= i 3) (= j 4)) 'B2) ((and (= i 2) (= j 3)) 'B3)
	((and (= i 4) (= j 3)) 'B4) ((and (= i 3) (= j 0)) 'N1)
	((and (= i 3) (= j 1)) 'N2) ((and (= i 3) (= j 5)) 'N3)
	((and (= i 3) (= j 6)) 'N4) ((and (= i 0) (= j 3)) 'N5)
	((and (= i 1) (= j 3)) 'N6) ((and (= i 5) (= j 3)) 'N7)
	((and (= i 6) (= j 3)) 'N8) ((and (= i 0) (= j 0)) 'QQ)
	((and (= i 6) (= j 0)) 'QQ) ((and (= i 0) (= j 6)) 'QQ)
	((and (= i 6) (= j 6)) 'QQ) (t 'XX)))

(defparameter *estado-inicial* (crea-estado :bando 'negro :tablero (loop for i below 7 collect
									 (loop for j below 7 collect
									      (crea-casilla i j)))))

(defparameter *blancas* '(B1 B2 B3 B4 RB))
(defparameter *negras* '(N1 N2 N3 N4 N5 N6 N7 N8))
(defparameter *piezas* (append *blancas* *negras*))

;;; Determina el color de la pieza en la casilla indicada
(defun color-pieza (pieza)
  (if (member pieza *blancas*) 'blanco 'negro))

(defun color-pos (casilla estado)
  (if (fuera-tablero casilla)
      NIL
      (let ((pieza (obtiene-pieza casilla estado)))
	(cond ((member pieza *blancas*) 'blanco)
	      ((member pieza *negras*) 'negro)
	      ((equal '(3 3) casilla) T)
	      ((equal 'XX pieza) 'XX)
	      ((equal 'QQ pieza) T)))))

;;; Representación de los movimientos
(defparameter *movimientos*
  (let ((direcciones '(AR AB IZQ DRCH)) (casillas (loop for c from 1 to 6 collect c)))
    (loop for pieza in *piezas* append
	 (loop for direccion in direcciones append
	      (loop for casilla in casillas collect (list pieza direccion casilla))))))

(defun es-estado-final (estado)
  (let ((posicion-rey (index 'RB estado)))
    (or (not posicion-rey)
	(equal '(0 0) posicion-rey)
	(equal '(0 6) posicion-rey)
	(equal '(6 6) posicion-rey)
	(equal '(6 0) posicion-rey))))

(defun es-estado-ganador (estado turno jugador)
  (and (es-estado-final estado)
       (not (equal turno jugador))))

(defun comprueba-borrar (color centro apoyo estado)
  (let ((color-centro (color-pos centro estado))
	(color-apoyo (color-pos apoyo estado)))
    (if (and (not (equal (index 'RB estado) centro )) ;;; La pieza del centro no es el rey
	     (or (equal color color-apoyo) ;;; O es del mismo color
		 (equal T color-apoyo))    ;;; o es una pieza que ayuda a capturar (trono o escape)
	     (not (equal color (color-pos centro estado))))  ;;; La pieza del centro es de distinto color
	(borra-pieza centro estado))))

(defun comprueba-borrar-rey (estado)
  (let ((posicion-rey (index 'RB estado)))
    (if (= 4
	   (loop for pos in '((0 1) (1 0) (-1 0) (0 -1)) count
		(let* ((pos-adyacente (suma-coordenadas posicion-rey pos))
		       (color-adyacente (color-pos pos-adyacente estado)))
		  (not (or (equal 'blanco color-adyacente)
			   (equal 'XX color-adyacente))))))
	(borra-pieza posicion-rey estado))))

(defun cambia-bando (estado)
  (setf (bando estado) (if (equal (bando estado) 'negro) 'blanco 'negro)))

(defun ejecutar-movimiento (origen destino estado)
  (let ((res (copy-estado estado)))
    (coloca-pieza destino (obtiene-pieza origen res) res)
    (borra-pieza origen res)
    (let ((color-actual (color-pos destino res)))
      (comprueba-borrar color-actual (suma-coordenadas destino '(1 0)) (suma-coordenadas destino '(2 0)) res)
      (comprueba-borrar color-actual (suma-coordenadas destino '(-1 0)) (suma-coordenadas destino '(-2 0)) res)
      (comprueba-borrar color-actual (suma-coordenadas destino '(0 1)) (suma-coordenadas destino '(0 2)) res)
      (comprueba-borrar color-actual (suma-coordenadas destino '(0 -1)) (suma-coordenadas destino '(0 -2)) res))
    (comprueba-borrar-rey res)
    (cambia-bando res)
    res))

;;; Determina si el movimiento es válido según las reglas del juego
;;; La comprobación con la pieza de origen se hace ya que diferencia-puntos
;;; incluye los extremos, y por tanto, la propia casilla
;;; Con lo cual, si no se hiciese esta comprobación, la función siempre devolvería NIL
(defun es-ilegal (origen destino estado)
  (or (fuera-tablero destino)
      (if (equal (obtiene-pieza origen estado) 'RB)
	  (es-ilegal-rey origen destino estado)
	  (es-ilegal-peon origen destino estado))))

(defun es-ilegal-peon (origen destino estado)
  (or (equal destino '(3 3))     ;;; Casilla del trono (se puede pasar por ella, pero no ser el destino)
      (loop for coord in (diferencia-puntos origen destino) thereis
	   (not (or (equal 'XX (obtiene-pieza coord estado))
		    (equal coord origen))))))

(defun es-ilegal-rey (origen destino estado)
  (loop for coord in (diferencia-puntos origen destino) thereis
       (not (or (equal 'XX (obtiene-pieza coord estado))
		 (equal 'QQ (obtiene-pieza coord estado))
		 (equal coord origen)))))

(defun calcula-destino (origen movimiento)
  (let ((direccion (second movimiento))
	(num-casillas (third movimiento)))
    (cond
      ((not origen) NIL)
      ((equal direccion 'AR) (suma-coordenadas origen (list 0 (- num-casillas))))
      ((equal direccion 'AB) (suma-coordenadas origen (list 0 num-casillas)))
      ((equal direccion 'IZQ) (suma-coordenadas origen (list (- num-casillas) 0)))
      ((equal direccion 'DRCH) (suma-coordenadas origen (list num-casillas 0))))))

;;; Función para aplicar un movimiento
;;; Devuelve NIL si el movimiento no es aplicable
(defun aplica-movimiento (movimiento estado)
  (let* ((pieza (first movimiento))
	 (origen (index pieza estado))
	 (destino (calcula-destino origen movimiento)))
    (if (or (not origen) (not destino))
	NIL
	(if (or (es-ilegal origen destino estado)
		(not (equal (bando estado) (color-pieza pieza))))
	    NIL
	    (ejecutar-movimiento origen destino estado)))))


;;; ===== Funciones auxiliares para F-E-ESTATICA =====

; Devuelve el número de piezas de un bando
(defun num-piezas (bando estado)
  (loop for pieza in bando count (index pieza estado)))

; Devuelve la diferencia entre peones blancos y negros, teniendo en cuenta que,
; según nuestra heurística, un peón blanco equivale a dos negros
(defun diferencia-blancas-negras (estado)
  (- (* 2 (1- (num-piezas *blancas* estado))) (num-piezas *negras* estado)))

; Devuelve la distancia Manhattan entre dos posiciones
(defun distancia-manhattan (pos1 pos2)
  (+ (abs (- (first pos1) (first pos2))) (abs (- (second pos1) (second pos2)))))

; Devuelve la menor distancia Manhattan de una posicion a un escape
(defun menor-distancia-escape (posicion)
  (min (distancia-manhattan posicion '(0 0))
       (distancia-manhattan posicion '(6 0))
       (distancia-manhattan posicion '(6 6))
       (distancia-manhattan posicion '(0 6))))

(defparameter *MINIMO-VALOR* -99999)
(defparameter *MAXIMO-VALOR* 99999)

(defun f-e-estatica (estado jugador)
  (if *maquina-vs-maquina?*
      (if (equal jugador 'min)
	  (f-e-estatica-b estado jugador)
	  (f-e-estatica-n estado jugador))
      (if (equal *bando-maquina* 'blanco)
	  (f-e-estatica-b estado jugador)
	  (f-e-estatica-n estado jugador))))

(defun f-e-estatica-n (estado jugador)
  (let ((posicion-rey (index 'RB estado)))
    (if (not posicion-rey)
	*MAXIMO-VALOR*
	(+ (- (diferencia-blancas-negras estado))
	   (* 2 (- (menor-distancia-escape posicion-rey) 6))))))

(defun f-e-estatica-b (estado jugador)
  (let ((posicion-rey (index 'RB estado)))
    (if (not posicion-rey)
	*MINIMO-VALOR*
	(+ (diferencia-blancas-negras estado)
	   (- 6   ;;; Máxima distancia del Rey al escape
	      (menor-distancia-escape posicion-rey))))))


;;; ===== Heurísticas =====

(defun negras-cuadrantes (pos1 pos2 estado)
  (loop for punto in (diferencia-puntos pos1 pos2) count
       (equal 'NEGRO (color-pos punto estado))))

;;; Número de peones negros adyacentes al Rey
(defun heuristica-2 (estado)
  (let ((posicion-rey (index 'RB estado)))
    (loop for pos in '((0 1) (1 0) (-1 0) (0 -1)) count
	 (equal 'NEGRO (color-pos (suma-coordenadas posicion-rey pos) estado)))))

;;; Cuadrante con menor número de peones negros
(defun heuristica-3 (estado)
  (min (negras-cuadrantes '(0 0) '(3 3) estado)
       (negras-cuadrantes '(3 3) '(6 6) estado)
       (negras-cuadrantes '(3 0) '(6 3) estado)
       (negras-cuadrantes '(0 3) '(3 6) estado)))


(defstruct (stats (:constructor crea-estadisticas)
		   (:conc-name nil)
		   (:print-function muestra-estadisticas))
  NumPartidas
  PartidasGanadasBlancas
  PartidasGanadasNegras
  MovBlancas
  MovNegras)


(defun muestra-estadisticas (stats canal prof)
    (format canal 
"~%Partidas Totales: ~a | Partidas ganadas por BLANCAS: ~a  | Partidas ganadas por NEGRAS: ~a  |
 Proporción partidas ganadas por BLANCAS: ~a | Proporción partidas ganadas por NEGRAS: ~a |
 Movimientos medios para ganar por BLANCAS: ~a  | Movimientos medios para ganar por NEGRAS: ~a~%"
	    (NumPartidas stats)
	    (PartidasGanadasBlancas stats)
	    (PartidasGanadasNegras stats)
	    (/ (PartidasGanadasBlancas stats) (NumPartidas stats))
	    (/ (PartidasGanadasNegras stats) (NumPartidas stats))
	    (/ (MovBlancas stats) (PartidasGanadasBlancas stats))
	    (/ (MovNegras stats) (PartidasGanadasNegras stats))))

(defparameter *estadisticas* (crea-estadisticas))
(setf (NumPartidas *estadisticas*) 0)
(setf (PartidasGanadasBlancas *estadisticas*) 0)
(setf (PartidasGanadasNegras *estadisticas*) 0)
(setf (NumPartidas *estadisticas*) 0)
(setf (MovBlancas *estadisticas*) 0)
(setf (MovNegras *estadisticas*) 0)

(load "juego.lisp")
;;; Usar esta variante para generar estadísticas en el modo
;;; máquina contra máquina
 (load "juego-nuevo.lisp")
;(load "minimax.lisp")

(defparameter *bando-maquina* 'blanco)
(defparameter *maquina-vs-maquina?* nil)

(defun juega-negras-humano ()
  (juego))

(defun juega-blancas-humano ()
  (setf *bando-maquina* 'negro)
  (juego :empieza-la-maquina? T))

(defun maquina-contra-maquina ()
  (setf *maquina-vs-maquina?* T)
  (juega-blancas-humano))

(defun genera-estadísticas (n-partidas)
  (loop repeat n-partidas do
       (maquina-contra-maquina)))
