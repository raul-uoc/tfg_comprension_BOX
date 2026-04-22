(use-modules (sxml ssax)
	     (sxml xpath)
	     (curl)
	     (sxml simple)
	     (rnrs bytevectors)
	     (ice-9 iconv)
	     (srfi srfi-11)
	     (web client)
	     (ice-9 match))

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
  (let* ((puro-articulo (match:substring (string-match "<article.*?</article>" pagina-bruta)))
	 (borra-doble-span (regexp-substitute/global #f "</span></span>" puro-articulo
						     'pre "</span>" 'post))
	 (elimina-compartir (regexp-substitute/global #f "<div class=\"compartir\">.*?</dl>" borra-doble-span
						      'pre "</dl>" 'post))
	 (to-utf-8 (iso-to-utf-8 elimina-compartir)))
    to-utf-8))

(define el-poder (descargar-peticion "poder"))
(define pagina-limpia (limpiar-pagina el-poder))
(define el-articulo (extraer-articulo pagina-limpia))
(define el-s-articulo (xml->sxml el-articulo))

(define (obtener-definiciones s-articulo)
  (let* ((campo-definiciones ((sxpath  '(// (span (@ class (equal? "field-name-field-definicion") ) ))) s-articulo))
	 (las-definiciones ((select-kids string?) campo-definiciones)))
    las-definiciones))

((sxpath  '(// (span (@ class (equal? "field-name-field-definicion") ) ))) el-s-articulo)
((sxpath  '(// (div (@ class (equal? "cuerpo-lema") ) ))) el-s-articulo)
((sxpath  '(// (span (@ class (equal? "field-name-field-definicion") ) ))) el-s-articulo)
((sxpath  '(// (div (@ class (equal? "field-name-field-sublema") ) ))) el-s-articulo)

((sxpath  '(// (div (@ class (equal? "field-name-field-sublema"))
		    (span (@ (class (equal? "sublema1"))))))) el-s-articulo)
