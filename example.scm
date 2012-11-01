;; Currently the module only contains 2 libraries.
(import (soap lite)
	(soap transport http))

(define-soap-type
  ;; creates foo:Item procedure, and it generates foo:Item tag
  (:namespace foo Item)
  ;; the same as above but foo:Value
  (:namespace foo Value)
  ;; creates bar:BarItem procedure, but the tag does not have namespace.
  ;; so the output tag is BarItem
  (:prefix bar BarItem)
  ;; the same as BarItem but BarValue
  (:prefix bar BarValue)
  ;; creates Buz procedure and the tag is Baz
  Baz
  ;; If you want to create shorter prefix but long namespace this can be
  ;; the solution
  (:prefix soap (:namespace soapenv Encode))
  )

;; creates a context
(define context (make-soap-context
		 +soap-version-1.1+
		 :transport 
		 (make-http-transport "localhost" "/service"
				      ;; the rest keywords are converted to
				      ;; extra http header
				      :authorization "Basic base64user&pass")
		 ;; default namespace.
		 ;; NOTE: if this namespace(s) and the one given to
		 ;; soap:envelop are duplicated, however it won't raise any
		 ;; error. so programmers need to make sure there is not
		 ;; duplication.
		 :namespace '((bar "urn://bar.namespace"))))

;; almost the same style as (text html-lite) library
;; just top level of soap:envelop must take at least 2 arguments
;; context and body
;; keyword arguments below are optional
;;  :header     - header part of SOAP message. default value is (soap:header)
;;  :namespaces - alist of extra namespaces.
;;        ex) '((type "http://namespace.example/"))
(define message (soap:envelope
		 context
		 (soap:body
		  (foo:Item :name "item-name"
			    (foo:Value "item-value")))
		 :namespaces '((foo "urn://foo.namespace"))))

;; sending message with transport context has
;; default http-transport returns reponse of the request.
(soap-send-request context message)

;;
;; creating own transport
;; transport is just a closure which must accepts an string message, and other
;; than that it's implementation dependent. so the below transport is totally
;; legal.
;; It's just do nothing and useless.
(define (make-foo-transport) (lambda (message) message))

