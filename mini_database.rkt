#lang racket

(define NULL 'null)

;====================================
;=            Cerința 1             =
;= Definirea elementelor de control =
;=          20 de puncte            =
;====================================

;= Funcțiile de acces
(define init-database
  (λ ()
    '()))

(define create-table
  (λ (table columns-name)
    (cons table columns-name)
    ))
    
(define get-name
  (λ (table)
    (car table)
    ))

(define get-columns
  (λ (table)
    (if (not (list? (cadr table)))
        (cdr table)
        (map car (cdr table))
    )))

(define get-tables
  (λ (db)
    db))

(define get-table
  (λ (db table-name)
    (if (equal? (get-name (car db)) table-name)
        (car db)
        (get-table (cdr db) table-name)
    )))

(define add-table
  (λ (db table)
    (reverse (cons table db))
    ))

(define remove-table
  (λ (db table-name)
    (if (equal? (get-name (car db)) table-name)
        (cdr db)
        (cons (car db) (remove-table (cdr db) table-name))
    )))

;= Pentru testare, va trebui să definiți o bază de date (având identificatorul db) cu următoarele tabele

;============================================================================================
;=                         Tabela Studenți                                                   =
;= +----------------+-----------+---------+-------+-------+                                  =
;= | Număr matricol |   Nume    | Prenume | Grupă | Medie |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;= |            123 | Ionescu   | Gigel   | 321CA |  9.82 |                                  =
;= |            124 | Popescu   | Maria   | 321CB |  9.91 |                                  =
;= |            125 | Popa      | Ionel   | 321CC |  9.99 |                                  =
;= |            126 | Georgescu | Ioana   | 321CD |  9.87 |                                  =
;= +----------------+-----------+---------+-------+-------+                                  =
;=                                                                                           =
;=                                         Tabela Cursuri                                    =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | Anul | Semestru |            Disciplină             | Număr credite | Număr teme |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;= | I    | I        | Programarea calculatoarelor       |             5 |          2 |      =
;= | II   | II       | Paradigme de programare           |             6 |          3 |      =
;= | III  | I        | Algoritmi paraleli și distribuiți |             5 |          3 |      =
;= | IV   | I        | Inteligență artificială           |             6 |          3 |      =
;= | I    | II       | Structuri de date                 |             5 |          3 |      =
;= | III  | II       | Baze de date                      |             5 |          0 |      =
;= +------+----------+-----------------------------------+---------------+------------+      =
;============================================================================================
(define db
  (add-table
   (add-table (init-database) (create-table "Studenți" '(("Număr matricol" 123 124 125 126) ("Nume" "Ionescu" "Popescu" "Popa" "Georgescu")
   ("Prenume" "Gigel" "Maria" "Ionel" "Ioana") ("Grupă" "321CA" "321CB" "321CC" "321CD") ("Medie" 9.82 9.91 9.99 9.87))))
   (create-table "Cursuri" '(("Anul" "I" "II" "III" "IV" "I" "III") ("Semestru" "I" "II" "I" "I" "II" "II")
   ("Disciplină" "Programarea calculatoarelor" "Paradigme de programare" "Algoritmi paraleli și distribuiți" "Inteligență artificială" "Structuri de date" "Baze de date")
   ("Număr credite" 5 6 5 6 5 5) ("Număr teme" 2 3 3 3 3 0)))))

;====================================
;=            Cerința 2             =
;=         Operația insert          =
;=            10 puncte             =
;====================================
(define orice
  (lambda (ceva record)
    (if (equal? ceva (caar record))
        (list (cdar record))
        (if (equal? (cdr record) '())
            (list 'null)
            (orice ceva (cdr record))
        ))))

(define orice2
  (lambda (L record)
    (if (null? L)
        '()
        (append (list (orice (car L) record)) (orice2 (cdr L) record))
    )))

(define insert
  (λ (db table-name record)
         (if (and (not (list? (car (cdr (get-table db table-name))))) (not (list? (car (cdr (get-table db table-name))))))
             (add-table (remove-table db table-name) (cons table-name (map append (map list (cdr (get-table db table-name))) (orice2 (get-columns (get-table db table-name)) record))))
             (add-table (remove-table db table-name) (cons table-name (map append (cdr (get-table db table-name)) (orice2 (get-columns (get-table db table-name)) record))))
         )))

;====================================
;=            Cerința 3 a)          =
;=     Operația simple-select       =
;=             10 puncte            =
;====================================
(define get-col
  (lambda (table column-name)
    (if (equal? (car (car (cdr table))) column-name)
        (cdr (car (cdr table)))
        (get-col (cdr table) column-name)
        )))
    
(define simple-select
  (λ (db table-name columns)
    (if (null? (cdr columns))
        (if (member (car columns) (get-columns (get-table db table-name)))
            (list (get-col (get-table db table-name) (car columns)))
            '())
    (if (member (car columns) (get-columns (get-table db table-name)))
        (cons (get-col (get-table db table-name) (car columns)) (simple-select db table-name (cdr columns)))
        (simple-select db table-name (cdr columns))
        ))))

;====================================
;=            Cerința 3 b)          =
;=           Operația select        =
;=            30 de puncte          =
;====================================

(define (transpose matrice)
  (apply map list matrice))

(define select-var1
  (λ (db table-name columns conditions)
    (for/list ([i columns])
      (for/list ([z conditions])
        (for/list ([j (get-col (get-table db table-name) (car (cdr z)))] [k (get-col (get-table db table-name) i)])
          (if (equal? (apply (car z) j (list (last z))) #t)
              #t
              #f
              )
        )))))

(define select-var1b
  (λ (db table-name columns conditions)
    (transpose (append*
     (select-var1 db table-name columns conditions)
     ))))

(define select-var1c
  (λ (db table-name columns conditions)
    (for/list ([i (select-var1b db table-name columns conditions)])
      (if (member #f i)
          #f
          #t
          )
      )))

(define select-var1d
  (λ (db table-name columns conditions)
    (map (lambda (L) (filter-not boolean? L))
    (for/list ([i columns])
      (for/list ([k (get-col (get-table db table-name) i)] [j (select-var1c db table-name columns conditions)])
        (if (equal? j #t)
            k
            #f
            )
        )))))

(define comparemin
  (lambda (a b)
    (if (< a b)
        a
        b
    )))

(define (minimum L)
  (foldr comparemin (car L) (cdr L)))

(define comparemax
  (lambda (a b)
    (if (> a b)
        a
        b
    )))

(define (maximum L)
  (foldr comparemax (car L) (cdr L)))

(define select-var4
  (λ (db table-name columns conditions)
    (if (null? conditions)
        (simple-select db table-name 
        (for/list ([i columns])
        (if (not (pair? i))
          i
          (cdr i)
          )))
        (select-var1d db table-name
        (for/list ([j columns])
        (if (not (pair? j))
          j
          (cdr j)
          ))
        conditions))
    ))

(define select-var5
  (λ (db table-name columns conditions)
    (for/list ([i columns])
      (if (not (pair? i))
          #f
          (car i)))
    ))

(define count
  (lambda (L)
    (if (null? L)
        0
        (if (member (car L) (cdr L))
           (count (cdr L)) 
           (add1 (count (cdr L)))
           )
        )))

(define sum
  (lambda (L)
    (if (null? L)
        0 
        (+ (car L) (sum (cdr L)))
        )
    ))

(define avg
  (lambda (L)
    (/ (sum L) (length L))
    ))

(define sort-asc
  (lambda (L)
    (if (null? L)
        '()
        (cons (minimum L) (sort-asc (remove (minimum L) L =))))
    ))

(define sort-desc
  (lambda (L)
    (if (null? L)
        '()
        (cons (maximum L) (sort-desc (remove (maximum L) L =))))
    ))

(define select-var6
  (λ (db table-name columns conditions)
    (for/list ([i (select-var4 db table-name columns conditions)] [j (select-var5 db table-name columns conditions)])
      (if (boolean? j)
        i
        (if (equal? j 'max)
         (maximum i)
         (if (equal? j 'min)
            (minimum i)
            (if (equal? j 'count)
                (count i)
                (if (equal? j 'sum)
                    (sum i)
                    (if (equal? j 'avg)
                        (avg i)
                        (if (equal? j 'sort-asc)
                            (sort-asc i)
                            (sort-desc i)
                            )
                        )))))))))

(define select
  (λ (db table-name columns conditions)
    (select-var6 db table-name columns conditions)
    ))

;====================================
;=             Cerința 4            =
;=           Operația update        =
;=            20 de puncte          =
;====================================
(define cesterg
  (λ (db table-name conditions)
    (transpose
    (select-var1d db table-name (get-columns (get-table db table-name)) conditions)
    )))

(define dincesterg
  (λ (db table-name conditions)
    (transpose
    (for/list ([i (get-columns (get-table db table-name))])
      (cons i
      (for/list ([j (get-col (get-table db table-name) i)])
        j
      ))))))

(define sterg
  (λ (db table-name conditions)
    (remove* (cesterg db table-name conditions) (dincesterg db table-name conditions))
    ))

(define update-var1
  (λ (db table-name values conditions)
    (transpose
     (for/list ([i (select-var1d db table-name (get-columns (get-table db table-name)) conditions)] [k (get-columns (get-table db table-name))])
         (cons k i)
         ))))

(define valoare
  (λ (k values)
    (filter-not boolean?
    (for/list ([i values])
      (if (equal? k (car i))
          (cdr i)
          #f
          )
    ))))
                
(define update-var3
  (λ (db table-name values conditions)
    (list (cons table-name
    (for/list ([x (transpose                         
    (for/list ([i (cdr (transpose (cdr (get-table db table-name))))])
      (if (member i (cdr (sterg db table-name conditions)))
          i
          (for/list ([k (car (update-var1 db table-name values conditions))] [j i])
            (if (null? (valoare k values))
              j
              (append* (valoare k values))
             )
     ))))] [y (get-columns (get-table db table-name))])
      (cons y x)
      )
    ))))

(define update
  (λ (db table-name values conditions)
    (update-var3 db table-name values conditions)
    ))

;====================================
;=             Cerința 5            =
;=           Operația delete        =
;=              10 puncte           =
;====================================

(define delete
  (λ (db table-name conditions)
    (if (equal? (transpose (sterg db table-name conditions)) (map list (get-columns (get-table db table-name))))
         (list (list table-name (get-columns (get-table db table-name))))
         (list (cons table-name (transpose (sterg db table-name conditions))))
    )))

;====================================
;=               Bonus              =
;=            Natural Join          =
;=            20 de puncte          =
;====================================
(define intersect-2-lists
  (lambda (L1 L2)
    (filter-not boolean?
    (for/list ([i L1])
      (if (member i L2)
          i
          #f
      )))
    (filter-not boolean?
    (for/list ([j L2])
      (if (member j L1)
          j
          #f
      )))
    ))

(define natural-join-var1
  (λ (db tables columns conditions)
    (list (cons "Tabel Comun" (list
    (filter-not boolean?
    (if (> (length (get-col (get-table db (car tables))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )
      )))))
           (length (get-col (get-table db (append* (cdr tables)))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )
      ))))))
        (cons (append*
        (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )
      )))
        (for/list ([z (get-col (get-table db (car tables))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )
      ))))]) (if (member z (intersect-2-lists
     (get-col (get-table db (car tables))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )
      ))))
     (get-col (get-table db (append* (cdr tables)))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )
      ))))))
              z
              #f
              )))
(cons
 (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          ))))
(for/list ([y (get-col (get-table db (append* (cdr tables)))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )))))]) (if (member y (intersect-2-lists
     (get-col (get-table db (car tables))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )))))
     (get-col (get-table db (append* (cdr tables)))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )))))))
              y
              #f
              ))))))))))

(define natural-join-var1b
  (λ (db tables columns conditions)
    (if (> (length (get-col (get-table db (car tables))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          ))))))
           (length (get-col (get-table db (append* (cdr tables)))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          )))))))
        (cons (car tables)
        (transpose
        (cons (car (transpose (cdr (get-table db (car tables)))))
        (filter-not boolean?
        (for/list ([x (cdr (transpose (cdr (get-table db (car tables)))))])
          (if (member #t (for/list ([m x])
            (if (member m (cdr (car (cdr (append* (natural-join-var1 db tables columns conditions))))))
                #t
                #f
                )))
              x
              #f
              ))))))
        (cons (append* (cdr tables))
        (transpose
        (cons (car (transpose (cdr (get-table db (append* (cdr tables))))))
        (filter-not boolean?
        (for/list ([y (cdr (transpose (cdr (get-table db (append* (cdr tables))))))])
          (if (member #t (for/list ([n y])
            (if (member n (cdr (car (cdr (append* (natural-join-var1 db tables columns conditions))))))
                #t
                #f
                )))
              y
              #f
              )))))))))

(define natural-join-var1c
  (λ (db tables columns conditions)
    (if (> (length (get-col (get-table db (car tables))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          ))
    ))))
           (length (get-col (get-table db (append* (cdr tables)))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          ))
    )))))
        (cons (append* (cdr tables))
        (append (list (car (cdr (append* (natural-join-var1 db tables columns conditions)))))      
        (transpose (cons
          (filter-not boolean?
          (for/list ([f (car (transpose (cdr (get-table db (append* (cdr tables))))))])
            (if (equal? f (car (car (cdr (append* (natural-join-var1 db tables columns conditions))))))
                #f
                f
                )))
         (for/list ([x (cdr (car (cdr (append* (natural-join-var1 db tables columns conditions)))))])
              (append*
              (filter-not boolean?
              (for/list ([m (cdr (transpose (cdr (get-table db (append* (cdr tables))))))])
                (if (member #t (for/list ([a m])
                  (if (equal? a x)
                      #t
                      #f
                      )))
                    (filter-not boolean?
                    (for/list ([a m])
                      (if (equal? a x)
                          #f
                          a
                          )))
                    #f
                 )))))))))
        (cons (car tables)
        (append (list (car (cdr (append* (natural-join-var1 db tables columns conditions)))))    
        (transpose (cons
          (filter-not boolean?
          (for/list ([g (car (transpose (cdr (get-table db (append* (cdr tables))))))])
            (if (equal? g (car (car (cdr (append* (natural-join-var1 db tables columns conditions))))))
                #f
                g
                )))
         (for/list ([y (cdr (car (cdr (append* (natural-join-var1 db tables columns conditions)))))])
              (append*
              (filter-not boolean?
              (for/list ([n (cdr (transpose (cdr (get-table db (car tables)))))])
                (if (member #t (for/list ([b n])
                  (if (equal? b y)
                      #t
                      #f
                      )
                  ))
                    (filter-not boolean?
                    (for/list ([b n])
                      (if (equal? b y)
                          #f
                          b
                          )))
                    #f
                    )
                )))))))))))

(define natural-join-var2
  (λ (db tables columns conditions)
    (list (cons "Tabel Comun"
    (if (> (length (get-col (get-table db (car tables))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          ))
    ))))
           (length (get-col (get-table db (append* (cdr tables)))
     (append* (filter-not boolean?
    (for/list ([i (get-columns (get-table db (car tables)))])
      (if (member i (get-columns (get-table db (append* (cdr tables)))))
          i
          #f
          ))
    )))))
        (append* (list
        (for/list ([x (get-columns (get-table db (car tables)))])
          (if (not (equal? x (car (car (cdr (append* (natural-join-var1 db tables columns conditions)))))))
              (cons x (get-col (natural-join-var1b db tables columns conditions) x))
              (car (cdr (append* (natural-join-var1 db tables columns conditions))))
              ))
        (filter-not boolean?     
        (for/list ([s (get-columns (get-table db (append* (cdr tables))))])
          (if (not (equal? s (car (car (cdr (append* (natural-join-var1 db tables columns conditions)))))))
              (cons s (get-col (natural-join-var1c db tables columns conditions) s))
              #f
              )
          ))
        ))
        (append* (list 
        (for/list ([y (get-columns (get-table db (append* (cdr tables))))])
          (if (not (equal? y (car (car (cdr (append* (natural-join-var1 db tables columns conditions)))))))
              (cons y (get-col (natural-join-var1b db tables columns conditions) y))
              (car (cdr (append* (natural-join-var1 db tables columns conditions))))
              ))
        (filter-not boolean?     
        (for/list ([t (get-columns (get-table db (car tables)))])
          (if (not (equal? t (car (car (cdr (append* (natural-join-var1 db tables columns conditions)))))))
              (cons t (get-col (natural-join-var1c db tables columns conditions) t))
              #f
              ))
        ))
    ))
    ))
    ))

(define natural-join
  (λ (db tables columns conditions)
    (select (natural-join-var2 db tables columns conditions) "Tabel Comun" columns conditions)
    ))