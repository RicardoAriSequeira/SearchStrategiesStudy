;; Instituto Superior Técnico
;; Procura e Planeamento - 1ºSemeste (2018/2019)
;; Project
;; Grupo 21
;; Ricardo Ari Sequeira (79750)

(in-package :user)

(compile-file "procura.lisp")
(load "procura.fas")

;; TO-DO

;; implementar duas heuristicas diferentes para A*
;; implementar uma abordagem alternativa

;; FUNÇÕES AUXILIARES

(defun remove-nth (n list)
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (remove-nth (1- n) (cdr list)))))

(defun insert-after (lst index newelt)
  (push newelt (cdr (nthcdr index lst))) 
  lst)

;; ESTRUTURAS

(defstruct estado
	custo-pai
	tarefas-por-afectar
	afectacao)

;; OPERAÇÕES PARA TAREFAS

(defun tarefa= (tarefa1 tarefa2)
	(and (string= (first tarefa1) (first tarefa2))
		 (string= (second tarefa1) (second tarefa2))
		 (eq (third tarefa1) (third tarefa2))
		 (eq (fourth tarefa1) (fourth tarefa2))))

(defun duracao-tarefa (tarefa)
	(- (fourth tarefa) (third tarefa)))

(defun intervalo-tarefas (tarefa1 tarefa2)
	(- (third tarefa2) (fourth tarefa1)))

(defun tarefas-continuas (tarefa1 tarefa2)
	(string= (second tarefa1) (first tarefa2)))

(defun add-duracao-tarefa (tempo tarefa1 tarefa2)
	(+ tempo (- (fourth tarefa2) (fourth tarefa1))))

;; OPERAÇÕES PARA LISTA DE TAREFAS

(defun ordena (tarefas)

	(let ((res nil))

		(loop for tarefa in tarefas do
			(block ordem
				(cond ((= (length res) 0)
					(push tarefa res)
					(return-from ordem)))
				(loop for i from 0 to (- (length res) 1) do
					(cond ((<= (third tarefa) (third (nth i res)))
						(if (= i 0)
							(setf res (append (list tarefa) res))
							(setf res (insert-after res (- i 1) tarefa)))
						(return-from ordem)))
					(if (= i (- (length res) 1))
						(setf res (append res (list tarefa)))))))
	res))

(defun lista-tarefas= (tarefas1 tarefas2)

	(if (not (eq (length tarefas1) (length tarefas2))) (return-from lista-tarefas= nil))

	(let ((copia-tarefas2 (copy-list tarefas2)))

		(loop for tarefa1 from 0 to (- (length tarefas1) 1) do

			(let ((tarefa-igual nil))

				(block procura-tarefa

					(loop for tarefa2 from 0 to (- (length copia-tarefas2) 1) do

						(cond ((tarefa= (nth tarefa1 tarefas1) (nth tarefa2 copia-tarefas2))
							(setf copia-tarefas2 (remove-nth tarefa2 copia-tarefas2))
							(setf tarefa-igual t)
							(return-from procura-tarefa)))))

				(if tarefa-igual (setf tarefa-igual nil )(return-from lista-tarefas= nil)))

			(if (and (= (length copia-tarefas2) 0) (= tarefa1 (- (length tarefas1) 1)))
				(return-from lista-tarefas= t))))
	t)

;; OPERAÇÕES PARA TURNOS

(defun turno-valido (turno)

	(let ((tempo-sem-refeicao 0))
		
		(cond ((> (length turno) 0)

			;; verificar se turno nao ultrapassa as 8h=480min de duracao
			(if (> (duracao-turno turno) 480) (return-from turno-valido nil))

			;; acrescentar os min da primeira tarefa
			(setf tempo-sem-refeicao (duracao-tarefa (first turno)))

			(loop for tarefa from 1 to (- (length turno) 1) do

				(let ((tarefa-atual (nth tarefa turno))
					  (tarefa-anterior (nth (- tarefa 1) turno)))

					;; trabalhador nao pode passar 4h=240min sem refeicao
					(if (> tempo-sem-refeicao 240) (return-from turno-valido nil))

					;; turno invalido se houver sobreposicao de tarefas ou intervalos insuficientes para deslocacoes
					(if (or (< (intervalo-tarefas tarefa-anterior tarefa-atual) 0) 
							(and (not (tarefas-continuas tarefa-anterior tarefa-atual)) 
								 (< (intervalo-tarefas tarefa-anterior tarefa-atual) 40)))
						(return-from turno-valido nil))

					;; atualizar tempos sem refeicao de acordo com intervalos entre tarefas
					(if (or (and (tarefas-continuas tarefa-anterior tarefa-atual)
							 	 (>= (intervalo-tarefas tarefa-anterior tarefa-atual) 40))
							(and (not (tarefas-continuas tarefa-anterior tarefa-atual))
								 (>= (intervalo-tarefas tarefa-anterior tarefa-atual) 80)))
						(setf tempo-sem-refeicao 0)
						(setf tempo-sem-refeicao (add-duracao-tarefa tempo-sem-refeicao tarefa-anterior tarefa-atual)))))

			;; verificar se limite de tempo sem refeicao é ultrapassado
			(if (> tempo-sem-refeicao 240) (return-from turno-valido nil)))))
	
	t)

(defun duracao-turno (turno)

	(if (string= (first (first turno)) "L1")

		(if (string= (second (first (last turno))) "L1")
			(- (fourth (first (last turno))) (third (first turno)))
			(+ (- (fourth (first (last turno))) (third (first turno))) 40))

		(if (string= (second (first (last turno))) "L1")
			(+ (- (fourth (first (last turno))) (third (first turno))) 40)
			(+ (- (fourth (first (last turno))) (third (first turno))) 80))))

(defun turno= (turno1 turno2)

	(if (not (eq (length turno1) (length turno2))) (return-from turno= nil))

	(loop for tarefa1 from 0 to (- (length turno1) 1) do

		(loop for tarefa2 from 0 to (- (length turno2) 1) do

			(if (not (tarefa= (nth tarefa1 turno1) (nth tarefa2 turno2))) (return-from turno= nil))))
	t)

(defun desperdicio-turno (turno)

	(let ((desperdicio 0) (tempo-turno (duracao-turno turno)))

		(if (< tempo-turno 360) (setf desperdicio (- 360 tempo-turno)))

		(if (not (string= (first (first turno)) "L1")) (setf desperdicio (+ desperdicio 40)))

		(if (not (string= (second (first (last turno))) "L1")) (setf desperdicio (+ desperdicio 40)))
		
		(loop for tarefa from 1 to (- (length turno) 1) do
			
			(setf desperdicio (+ desperdicio (intervalo-tarefas (nth (- tarefa 1) turno) (nth tarefa turno)))))

	desperdicio))

;; OPERAÇÕES PARA AFECTAÇÕES

(defun afectacao-valida (afectacao)

	(loop for turno in afectacao do

		(if (not (turno-valido turno)) (return-from afectacao-valida nil)))

	t)

(defun desperdicio-afectacao (afectacao)
	(let ((desperdicio 0))
		(loop for turno in afectacao do
			(setf desperdicio (+ desperdicio (desperdicio-turno turno))))
	desperdicio))

(defun afectacao= (afectacao1 afectacao2)

	(if (not (eq (length afectacao1) (length afectacao2))) (return-from afectacao= nil))

	(let ((turnos-afectacao2 (copy-tree afectacao2)))

		(loop for turno1 from 0 to (- (length afectacao1) 1) do

			(let ((turno-igual nil))

				(block procura-turno

					(loop for turno2 from 0 to (- (length turnos-afectacao2) 1) do

						(cond ((turno= (nth turno1 afectacao1) (nth turno2 afectacao2))
							(setf turnos-afectacao2 (remove-nth turno2 turnos-afectacao2))
							(setf turno-igual t)
							(return-from procura-turno)))))

				(if turno-igual (setf turno-igual nil )(return-from afectacao= nil)))

			(if (and (= (length turnos-afectacao2) 0) (= turno1 (- (length afectacao1) 1)))
				(return-from afectacao= t))))
	t)

;; FUNÇÕES PRINCIPAIS

(defun operadores (estado)

	(let ((novo-tarefas-por-afectar (copy-list (estado-tarefas-por-afectar estado)))
		  (novo-afectacao (copy-tree (estado-afectacao estado)))
		  (tarefa-a-afectar nil)
		  (novo-turno nil)
		  (novo-estado nil)
		  (novo-turno-estado nil)
		  (resultado '()))

		(setf tarefa-a-afectar (pop novo-tarefas-por-afectar))
		(setf novo-afectacao (append novo-afectacao (list (list tarefa-a-afectar))))
		(setf novo-estado (make-estado :custo-pai (custo-acumulado estado) :tarefas-por-afectar novo-tarefas-por-afectar :afectacao novo-afectacao))
		(setf novo-turno-estado (make-estado :custo-pai (custo-acumulado estado) :tarefas-por-afectar novo-tarefas-por-afectar :afectacao novo-afectacao))
		(push novo-estado resultado)

		(loop for turno from 0 to (- (length (estado-afectacao estado)) 1) do 

			(loop for tarefa from -1 to (- (length (nth turno (estado-afectacao estado))) 1) do

				(setf novo-afectacao (copy-tree (estado-afectacao estado)))
				(if (= tarefa -1)
					(setf novo-turno (append (list tarefa-a-afectar) (nth turno novo-afectacao)))
					(setf novo-turno (insert-after (nth turno novo-afectacao) tarefa tarefa-a-afectar)))

				(cond ((turno-valido novo-turno)
					(setf (nth turno novo-afectacao) novo-turno)
					(setf novo-estado (make-estado :custo-pai (custo-acumulado estado) :tarefas-por-afectar novo-tarefas-por-afectar :afectacao novo-afectacao))
					(push novo-estado resultado)))))

	resultado))

(defun operadores-cortados (estado)

	(let ((novo-tarefas-por-afectar (copy-list (estado-tarefas-por-afectar estado)))
		  (novo-afectacao (copy-tree (estado-afectacao estado)))
		  (desperdicio-atual (desperdicio-afectacao (estado-afectacao estado)))
		  (diferencial-desperdicio nil)
		  (tolerancia-desperdicio -100)
		  (tarefa-a-afectar nil)
		  (novo-turno nil)
		  (novo-estado nil)
		  (novo-turno-estado nil)
		  (bom-desperdicio 40)
		  (resultado-rapido '())
		  (menos-desperdicio-estados '())
		  (mesmo-desperdicio-estados '())
		  (mais-desperdicio-estados '()))

		(setf tarefa-a-afectar (pop novo-tarefas-por-afectar))
		(setf novo-afectacao (append novo-afectacao (list (list tarefa-a-afectar))))
		(setf diferencial-desperdicio (- desperdicio-atual (desperdicio-afectacao novo-afectacao)))
		(setf novo-estado (make-estado :custo-pai (custo-acumulado estado) :tarefas-por-afectar novo-tarefas-por-afectar :afectacao novo-afectacao))
		(setf novo-turno-estado (make-estado :custo-pai (custo-acumulado estado) :tarefas-por-afectar novo-tarefas-por-afectar :afectacao novo-afectacao))

		(if (> diferencial-desperdicio (duracao-tarefa tarefa-a-afectar)) (push novo-estado menos-desperdicio-estados))
		(if (= diferencial-desperdicio (duracao-tarefa tarefa-a-afectar)) (push novo-estado mesmo-desperdicio-estados))
		(if (> diferencial-desperdicio tolerancia-desperdicio) (push novo-estado mais-desperdicio-estados))

		(loop for turno from 0 to (- (length (estado-afectacao estado)) 1) do 

			(setf novo-afectacao (copy-tree (estado-afectacao estado)))
			(setf novo-turno (append (nth turno novo-afectacao)(list tarefa-a-afectar)))

			(cond ((turno-valido novo-turno)
				(setf (nth turno novo-afectacao) novo-turno)
				(setf diferencial-desperdicio (- desperdicio-atual (desperdicio-afectacao novo-afectacao)))
				(setf novo-estado (make-estado :custo-pai (custo-acumulado estado) :tarefas-por-afectar novo-tarefas-por-afectar :afectacao novo-afectacao))

				(cond ((or (and (string= (first tarefa-a-afectar)
										 (second (first (last (nth turno (estado-afectacao estado))))))
						 	   	(< (- (fourth (first (last (nth turno (estado-afectacao estado)))))
						 	   		  (third tarefa-a-afectar)) bom-desperdicio))

						   (and (not (string= (first tarefa-a-afectar)
						  	   		 (second (first (last (nth turno (estado-afectacao estado)))))))
						 	   	(< (- (fourth (first (last (nth turno (estado-afectacao estado)))))
						 	   		  (third tarefa-a-afectar)) (+ bom-desperdicio 40))))
						(push novo-estado resultado-rapido)))

				(if (> diferencial-desperdicio (duracao-tarefa tarefa-a-afectar)) (push novo-estado menos-desperdicio-estados))
				(if (= diferencial-desperdicio (duracao-tarefa tarefa-a-afectar)) (push novo-estado mesmo-desperdicio-estados))
				(if (> diferencial-desperdicio tolerancia-desperdicio) (push novo-estado mais-desperdicio-estados)))))

	(cond ((> (length resultado-rapido) 0)
		(return-from operadores-cortados resultado-rapido)))
	(if (> (length menos-desperdicio-estados) 0)
		(return-from operadores-cortados menos-desperdicio-estados))
	(if (> (length mesmo-desperdicio-estados) 0)
		(return-from operadores-cortados mesmo-desperdicio-estados))
	(if (= (length mais-desperdicio-estados) 0)
		(push novo-turno-estado mais-desperdicio-estados))
	mais-desperdicio-estados))

(defun custo-acumulado (estado-pai)

	(let ((custo-pai 0))

		(loop for turno in (estado-afectacao estado-pai) do

			(let ((tempo-turno (duracao-turno turno)))

				(if (< tempo-turno 360) (setf tempo-turno 360))

				(setf custo-pai (+ custo-pai tempo-turno))))

	custo-pai))

(defun custo-afectacao (afectacao)

	(let ((custo-afectacao 0))

		(loop for turno in afectacao do

			(let ((tempo-turno (duracao-turno turno)))

				(if (< tempo-turno 360) (setf tempo-turno 360))

				(setf custo-afectacao (+ custo-afectacao tempo-turno))))

	custo-afectacao))

(defun custo (estado)

	(let ((custo-atual 0))

		(loop for turno in (estado-afectacao estado) do

			(let ((tempo-turno (duracao-turno turno)))

				(if (< tempo-turno 360) (setf tempo-turno 360))

				(setf custo-atual (+ custo-atual tempo-turno))))

	(- custo-atual (estado-custo-pai estado))))

(defun objectivo? (estado)

	(if (> (length (estado-tarefas-por-afectar estado)) 0) (return-from objectivo? nil))

	t)

(defun estado= (estado1 estado2)
	(and (lista-tarefas= (estado-tarefas-por-afectar estado1) (estado-tarefas-por-afectar estado2))
		(afectacao= (estado-afectacao estado1) (estado-afectacao estado2))))

(defun heuristica (estado)

	(let ((desperdicio 0))

		(loop for tarefa-por-afectar in (estado-tarefas-por-afectar estado) do
			
			(if (< (duracao-tarefa tarefa-por-afectar) 360)
				(setf desperdicio (+ desperdicio (- 360 (duracao-tarefa tarefa-por-afectar))))
				(setf desperdicio (+ desperdicio (duracao-tarefa tarefa-por-afectar)))))

		(+ desperdicio (desperdicio-afectacao (estado-afectacao estado)))))
					
;; SONDAGEM ITERATIVA

(defun sondagem-iterativa (problema)

	(let ((estado (problema-estado-inicial problema))
          (objectivo? (problema-objectivo? problema))
          (sucessores nil)
          (solucao '())
		  (nos-gerados 0)
	      (nos-expandidos 0)
	      (tempo (get-internal-run-time)))

    	(setf sucessores (operadores estado))
    	(incf nos-expandidos)
    	(incf nos-gerados (length sucessores))

      	(loop while t do

	      	(push estado solucao)

	      	(cond ((funcall objectivo? estado)
	      		(setf tempo (- (get-internal-run-time) tempo))
	      		(setf solucao (list (reverse solucao) tempo nos-expandidos nos-gerados))
	      		(return-from sondagem-iterativa solucao)))

	      	(if (null sucessores)
	      		(return-from sondagem-iterativa (sondagem-iterativa problema)))

	      	(setf sucessores (operadores estado))

	      	(incf nos-expandidos)
    		(incf nos-gerados (length sucessores))

	      	(setf estado (nth (random (length sucessores)) sucessores))))

	nil)

;; ILDS

(defun ordena-sucessores (sucessores)

	(let ((res nil))

		(loop for sucessor from 0 to (- (length sucessores) 1) do
			(block ordem
				(cond ((= (length res) 0)
					(push (nth sucessor sucessores) res)
					(return-from ordem)))
				(loop for i from 0 to (- (length res) 1) do
					(cond ((<= (heuristica (nth sucessor sucessores)) (heuristica (nth i res)))
						(if (= i 0)
							(setf res (append (list (nth sucessor sucessores)) res))
							(setf res (insert-after res (- i 1) (nth sucessor sucessores))))
						(return-from ordem)))
					(if (= i (- (length res) 1))
						(setf res (append res (list (nth sucessor sucessores))))))))
	res))

(defun iteracao-ilds (problema solucao estado k depth nos-expandidos nos-gerados)

	(let ((objectivo? (problema-objectivo? problema))
		  (novo-estado nil)
		  (sucessores nil))

		(if (funcall objectivo? estado)
			(return-from iteracao-ilds (list estado nos-expandidos nos-gerados)))

		(setf sucessores (ordena-sucessores (operadores estado)))
		(setf sucessores (append (list (first sucessores)) (list(second sucessores))))

		(incf nos-expandidos)
    	(incf nos-gerados (length sucessores))

		(cond 
			((null sucessores)
				(return-from iteracao-ilds nil))
			((> depth k)
				(setf novo-estado (first sucessores))
				(setf (first solucao) (append (first solucao) (list novo-estado)))
				(return-from iteracao-ilds (iteracao-ilds problema solucao novo-estado k (- depth 1) nos-expandidos nos-gerados)))
			((> k 0) 
				(setf novo-estado (nth (+ (random (- (length sucessores) 1)) 1) sucessores))
				(setf (first solucao) (append (first solucao) (list novo-estado)))
				(return-from iteracao-ilds (iteracao-ilds problema solucao novo-estado (- k 1) (- depth 1) nos-expandidos nos-gerados))))

	nil))

(defun ilds (problema)

	(let ((estado (problema-estado-inicial problema))
          (profundidade-maxima (length (estado-tarefas-por-afectar (problema-estado-inicial problema))))
          (k 0)
          (solucao '())
		  (nos-gerados 0)
	      (nos-expandidos 0)
	      (tempo (get-internal-run-time)))

    	(loop while (and (< k profundidade-maxima) (null solucao)) do

    		(setf solucao (iteracao-ilds problema (list (list estado) nos-expandidos nos-gerados) estado k profundidade-maxima 0 0))
    		(setf nos-expandidos (second solucao))
    		(setf nos-gerados (third solucao))
    		(incf k))

    	(setf tempo (- (get-internal-run-time) tempo))
    	(setf solucao (append solucao (list tempo)))
    	solucao))

;; FAZ-AFECTAÇÃO

(defun faz-afectação (problema estrategia)

	(let ((estado-inicial (make-estado :custo-pai 0 :tarefas-por-afectar (ordena problema) :afectacao '()))
		  (prob nil)
		  (resultado nil))

		(setf prob (cria-problema estado-inicial (make-list 1 :initial-element 'operadores-cortados)
		    					:objectivo? #'objectivo?
		    					:custo #'custo
		    					:heuristica #'heuristica
		    					:estado= #'estado=))

		(cond ((string= "melhor.abordagem" estrategia)
				(setf resultado (procura prob "a*" :espaco-em-arvore? t)))

			  ((string= "a*.melhor.heuristica" estrategia)
			  	(setf resultado (procura prob "a*" :espaco-em-arvore? t)))

			  ((string= "a*.melhor.heuristica.alternativa" estrategia)
			  	(format t "Não Implementado"))

			  ((string= "sondagem.iterativa" estrategia)
			  	(setf resultado (sondagem-iterativa prob)))

			  ((string= "ILDS" estrategia)
			  	(setf resultado (ilds prob))
			  	(return-from faz-afectação (estado-afectacao (first resultado))))

			  ((string= "abordagem.alternativa" estrategia)
			  	(format t "Não Implementado")))

		(if (not resultado)
			nil
			(estado-afectacao (first (last (first resultado)))))))
