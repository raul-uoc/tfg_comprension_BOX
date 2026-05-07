(define-module (printer)
  #:use-module (srfi-1)
  #:export (imprime-lema))


(define (imprime-narrower onto-url sublemas)
  (if (< 0 (length sublemas))
      (string-append
       "\n\t" "skos:narrower" "\t<" onto-url (car sublemas) ">"
       (reduce string-append "" (map (lambda (sublema)
				       (string-append
			      		" ,\n\t\t<" onto-url sublema ">")) (cdr sublemas)))
       ";")
      ""))

(define (imprime-lema onto-url
		      lema)
  (buscar-lema lema)
  (let* ((sublemas (map cadadr (obtener-sublemas1-href el-s-articulo))))
    (string-append "###  " onto-url "/" lema
		   "\n<" onto-url "/" lema "> rdf:type owl:NamedIndividual ,"
		   "\n\t" "skos:Concept ;"
		   (imprime-narrower onto-url sublemas)
		   "\n")))
