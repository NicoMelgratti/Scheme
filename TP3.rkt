; Ejemplo de uso:
(define agenda-semanal
'((lunes ("Francisco Perez" 8 8.5) ("Mariano López" 9 9.5))
(martes ("José Álvarez" 10 11) ("Fernanda Martínez" 11 12))
(miercoles) (jueves) (viernes)))
;caso de prueba 2

(define agenda-semanal2
  '((lunes ("Francisco Soltermann" 8 8.5) ("Antonio Bortoli" 9 9.5))
    (martes ("Nicolas Melgratti" 8 8.5) ("Fernanda Martínez" 9 9.5))
    (miercoles ("Nicolas Melgratti" 9 10)("Antonio Bortoli" 10 10.5) ("Fidel Castro" 11 11.5) ("Jose Cammisi" 14 14.5)("Richard Argona" 15 15.5)("Javier Milei" 15.5 16)("Justin Biber" 16 16.5))
    (jueves)
    (viernes)
    )
  )

;caso de prueba 3

(define agenda-semanal3
  '((lunes)
    (martes ("Antoni Kross" 11 11.5) ("Sofia Pitchai" 16 17))
    (miercoles)
    (jueves("Francisco Perez" 10 10.5) ("Mariano López" 11 12))
    (viernes("José Álvarez" 11 12) ("Fernanda Martínez" 14 15))))


;parte 1

(define (buscar-horario agenda tipo dia)
  (let* ((dia-agenda (assoc dia agenda))
         (turnos (cdr dia-agenda))
         (horarios-disponibles (if (eq? tipo 'estandar) 
                                   '(8 8.5 9 9.5 10 10.5 11 11.5 12 14 14.5 15 15.5 16 16.5 17)
                                   '(8 8.5 9 9.5 10 10.5 11 11.5 12 14 14.5 15 15.5 16 16.5 17))))
    (buscar-horario-aux turnos horarios-disponibles tipo)))

(define (buscar-horario-aux turnos horarios-disponibles tipo)
  (cond ((null? horarios-disponibles) 'horario-no-disponible)
        ((not (horario-ocupado? turnos (car horarios-disponibles) tipo)) (car horarios-disponibles))
        (else (buscar-horario-aux turnos (cdr horarios-disponibles) tipo))))

(define (horario-ocupado? turnos horario tipo)
  (let ((duracion (if (eq? tipo 'estandar) 0.5 1)))
    (or (member horario (map horario-ini turnos))
        (member (+ horario  duracion) (map  horario-fini turnos)))))

(define horario-ini(lambda (turno) (cadr turno)) )
(define horario-fini(lambda (turno) (caddr turno)))

;parte 2

(define (inserta-turno agenda nombre-persona tipo dia hora-inicio)
  (let* ((dia-agenda (assoc dia agenda))
         (turnos (cdr dia-agenda))
         (duracion (if (eq? tipo 'estandar) 0.5 1))
         (hora-fin (+ hora-inicio duracion)))
    (if (horario-ocupado? turnos hora-inicio tipo)
        'turno-no-disponible
        (let ((nuevo-turno (list nombre-persona hora-inicio hora-fin)))
          (cons (cons dia (insertar-turno-dia turnos nuevo-turno))
                (remover-dia agenda dia))))))

(define (insertar-turno-dia turnos nuevo-turno)
  (let ((inicio (cadr nuevo-turno)))
    (cond ((null? turnos) (list nuevo-turno))
          ((< inicio (cadr (car turnos))) (cons nuevo-turno turnos))
          (else (cons (car turnos) (insertar-turno-dia (cdr turnos) nuevo-turno))))))

(define (remover-dia agenda dia)
  (cond ((null? agenda) '())
        ((eq? (car (car agenda)) dia) (cdr agenda))
        (else (cons (car agenda) (remover-dia (cdr agenda) dia)))))

;parte 3

(define (lista-huecos agenda)
    (filtrar (map hueco agenda))
  )


(define (hueco dia-agenda)
  (let ((dia (car dia-agenda))
        (turnos (cdr dia-agenda)))
    (cons dia (huecos-dia turnos))))

(define (huecos-dia turnos)
  (let ((horas '((8 12) (14 17))))
    (huecos-dia-aux turnos horas horas)))

(define (huecos-dia-aux turnos todas-horas horas)
  (cond
    ((null? horas) '())
    ((null? turnos) horas)
    (else
     (let* ((intervalo (car horas))
            (inicio (car intervalo))
            (fin (cadr intervalo))
            (turno-inicio (if (null? turnos) fin (cadr (car turnos))))
            (turno-fin (if (null? turnos) inicio (caddr (car turnos)))))
       (cond
         ((< turno-inicio inicio)
          (huecos-dia-aux (cdr turnos) todas-horas horas))
         ((< turno-inicio fin)
          (append
           (if (> turno-inicio inicio)
               (list (list inicio turno-inicio))
               '())
           (huecos-dia-aux (cdr turnos) todas-horas
                           (cons (list turno-fin fin) (cdr horas)))))
         (else
          (huecos-dia-aux turnos todas-horas (cdr horas))))))))

(define (filtrar lista)
  (if(null? lista)lista
     (cons (filtrar-dia (car lista))
          (filtrar (cdr lista))
          )
     )
  )

(define (filtrar-dia lista)
  (cons (car lista) (filtrar-hora (cdr lista))
  ))

(define (filtrar-hora lista)
  (if(null? lista)
     '()
     (if(equal? (caar lista) (cadar lista))
        (filtrar-hora (cdr lista))
        (cons (car lista) (filtrar-hora (cdr lista)))
        )
     )
  )



;parte 4

(define (dia agenda) caar agenda)
(define (turno unTurno)(cadr unTurno))
(define (turnos agenda)(cdr agenda))
(define (nombre turno)(car turno))
(define (horaInicio turno)(cadr turno))
(define (horaFin turno)(caddr turno))

(define (reagendar-dia agenda dia-cancelar)
  (let ((turnos-cancelar (cdr (assoc dia-cancelar agenda))))
  ((ordenGenerico <) (cons (list dia-cancelar ) (reagendar-turnos (remover-dia agenda dia-cancelar)  turnos-cancelar)))))

(define (reagendar-turnos agenda turnos-cancelar)
  (if (null? turnos-cancelar)
      agenda
      (let* ((turno (car turnos-cancelar))
             (nombre (nombre turno))
             (tipo (if (= (- (horaFin turno) (horaInicio turno)) 1) 'complejo 'estandar))
             (nuevo-horario (buscar-nuevo-horario agenda tipo)))
        (if (equal? nuevo-horario 'horario-no-disponible)
            (list 'falla-asignacion (horaInicio turno))
            (let ((nuevo-dia (car nuevo-horario))
                  (hora-inicio (cadr nuevo-horario)))
              (reagendar-turnos
               (inserta-turno agenda nombre tipo nuevo-dia hora-inicio)
               (cdr turnos-cancelar)
               )
              )
            )
        )
      )
  )

(define (buscar-nuevo-horario agenda tipo)
  (let buscar-en-dias ((dias agenda))
    (cond ((null? dias) 'horario-no-disponible)
          (else
           (let* ((dia (car (car dias)))
                  (horario (buscar-horario agenda tipo dia)))
             (if (equal? horario 'horario-no-disponible)
                 (buscar-en-dias (cdr dias))
                 (list dia horario)
                 )
             )

           )
          )
    )
  )


(define (valorExtremo funcion lista)
  (if (null? (cdr lista))
     (car lista)
      (if (funcion (dia-a-num (caar lista)) (dia-a-num (car (valorExtremo funcion (cdr lista)))))
          (car lista)
          (valorExtremo funcion (cdr lista)))))


(define (filtro lista num)
  (if(null? lista)
     lista
     (if(equal? (dia-a-num (caar lista) ) num)
        (filtro (cdr lista) num)
        (cons (car lista ) (filtro (cdr lista) num ))
        ))
  )


(define (ordenGenerico criterio)
  (lambda (lista)
    (if(null? lista)
       lista
       (cons (valorExtremo criterio lista) ((ordenGenerico criterio) (filtro lista (dia-a-num(car (valorExtremo criterio lista)))) ) ))
       )
    )
  
(define (dia-a-num dia)
  (cond
    ((equal? dia 'lunes) 1)
    ((equal? dia 'martes) 2)
    ((equal? dia 'miercoles) 3)
    ((equal? dia 'jueves) 4)
    ((equal? dia 'viernes) 5)))
