(use-modules (sxml ssax)
	     (sxml xpath)
	     (curl)
	     (sxml simple))

(define (descargar-peticion lema)
  (let* ((handle (curl-easy-init)))
    (curl-easy-setopt handle 'url (string-append "https://dpej.rae.es/lema/" lema))
					;(curl-easy-setopt handle 'useragent "Mozilla/5.0 (compatible;  MSIE 7.01; Windows NT 5.0)")
    (curl-easy-setopt handle 'useragent "Mozilla/5.0 (platform; rv:gecko-version)")
    (curl-easy-perform handle)))

(define (limpiar-pagina pagina-bruta)
  (regexp-substitute #f (string-match "<head.*?</head>" pagina-bruta)
		     'pre " " 'post))

(define (extraer-articulo pagina-bruta)
  (let* ((puro-articulo (match:substring (string-match "<article.*?</article>" pagina-bruta)))
	 (borra-doble-span (regexp-substitute/global #f "</span></span>" puro-articulo
						     'pre "</span>" 'post))
	 (elimina-compartir (regexp-substitute/global #f "<div class=\"compartir\">.*?</dl>" borra-doble-span
						      'pre "</dl>" 'post)))
    elimina-compartir))

(define el-poder (descargar-peticion "poder"))
(define pagina-limpia (limpiar-pagina el-poder))
(define el-articulo (extraer-articulo pagina-limpia))
(define el-s-articulo (xml->sxml el-articulo))

((sxpath  '(// (span (@ class (equal? "field-name-field-definicion") ) ))) el-s-articulo)
