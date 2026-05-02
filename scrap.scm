(use-modules (sxml ssax)
	     (sxml xpath)
	     (curl)
	     (sxml simple)
	     (rnrs bytevectors)
	     (ice-9 iconv)
	     (srfi srfi-11)
	     (web client)
	     (ice-9 match)
	     (ice-9 pretty-print)
	     (tirones))

(define (descargar-peticion lema)
  (let* ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'url (string-append "https://dpej.rae.es/lema/" lema))
					;(curl-easy-setopt handle 'useragent "Mozilla/5.0 (compatible;  MSIE 7.01; Windows NT 5.0)")
    (curl-easy-setopt handle 'useragent "Mozilla/5.0 (platform; rv:gecko-version)")
    (curl-easy-perform handle)))

(define (limpiar-pagina pagina-bruta)
  (regexp-substitute #f (string-match "<head.*?</head>" pagina-bruta)
		     'pre " " 'post))

(define (iso-to-utf-8 the-string)
  (bytevector->string
   (string->bytevector the-string "ISO-8859-1")
   "UTF-8"))

(define (last-elem list) (car (reverse list)))

(define (extraer-articulo pagina-bruta)
  (let* ((puro-articulo		(match:substring (string-match "<article.*</article>" pagina-bruta)))
	 (borra-doble-span	(regexp-substitute/global #f "</span></span>" puro-articulo
							  'pre "</span>" 'post))
	 (borra-script		(regexp-substitute/global #f "<script.*</script>" borra-doble-span
							  'pre "" 'post))
	 (borra-img		(regexp-substitute/global #f "<img[^>]*>" borra-script
							  'pre "" 'post))
	 (borra-nowrap		(regexp-substitute/global #f " nowrap " borra-img
							  'pre "" 'post))
	 (borra-styles		(regexp-substitute/global #f "style=\"[^\"]*\"" 
							  borra-nowrap
							  'pre "" 'post))
	 ;(borra-comentarios	(regexp-substitute/global #f "<div class=\"field-name-field-comentario.*?</div>" borra-styles
							  ;'pre "" 'post))
	 (elimina-compartir	(regexp-substitute/global #f "<div class=\"compartir\">.*</dl>" borra-styles
							  'pre "</dl>" 'post))
	 (to-utf-8 		(iso-to-utf-8 elimina-compartir)))
    to-utf-8))

(define (obtener-definiciones s-articulo)
  (let* ((campo-definiciones ((sxpath  '(// (span (@ class (equal? "field-name-field-definicion") ) ))) s-articulo))
	 (las-definiciones ((select-kids string?) campo-definiciones)))
    las-definiciones))


(define pag-bruta (descargar-peticion "auto"))
(define pagina-limpia (limpiar-pagina pag-bruta))
(define el-articulo (extraer-articulo pagina-limpia))
(define el-s-articulo (xml->sxml el-articulo))


(define (write-the text filename)
  (let* ((port (open-output-file filename)))
    (if (list? text)
	(pretty-print text port)
	(display text port))
    (close-port port)))

(let* ((peticion-auto    (descargar-peticion "auto"))
       (peticion-poder   (descargar-peticion "poder"))
       (pag-limpia-auto  (limpiar-pagina peticion-auto))
       (pag-limpia-poder (limpiar-pagina peticion-poder))
       (art-auto         (extraer-articulo pag-limpia-auto))
       (art-poder        (extraer-articulo pag-limpia-poder))
       (s-auto		 (xml->sxml art-auto))
       (s-poder		 (xml->sxml art-poder)))
  
  (write-the peticion-auto "peti-auto.html")
  (write-the pag-limpia-auto   "limpio-auto.html")
  (write-the art-auto      "art-auto.html")
  (write-the s-auto	"s-auto.scm")

  (write-the peticion-poder "peti-poder.html")
  (write-the pag-limpia-poder   "limpio-poder.html")
  (write-the art-poder      "art-poder.html")
  (write-the s-poder	"s-poder.scm"))

