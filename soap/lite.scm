;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; soap/lite.scm - Simple SOAP library.
;;;  
;;;   Copyright (c) 2012  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(library (soap lite)
    (export +soap-version-1.1+
	    +soap-version-1.2+

	    make-soap-context
	    define-soap-type
	    soap-send-request
	    soap:envelope
	    (rename (%soap:Header soap:header)
		    (%soap:Body soap:body)))
    (import (rnrs)
	    (clos user)
	    (text tree)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object))
  
  (define-constant +soap-version-1.2+ 'soap-1.2)
  (define-constant +soap-version-1.1+ 'soap-1.1)

  (define-constant +encoding-namespace-1.2+ 
    "http://www.w3.org/2003/05/soap-encoding")
  (define-constant +envelope-namespace-1.2+
    "http://www.w3.org/2003/05/soap-envelope")

  (define-constant +encoding-namespace-1.1+ 
    "http://schemas.xmlsoap.org/soap/encoding/")
  (define-constant +envelope-namespace-1.1+
    "http://schemas.xmlsoap.org/soap/envelope/")

  ;; hold SOAP varsion and etc
  (define-class <soap-context> ()
    (;; version either 1.1 or 1.2 see constant variables
     (version :init-keyword :version :reader context-version
	      :init-form +soap-version-1.2+)
     ;; transport, procedure or #f 
     (transport :init-keyword :transport :reader context-transport
		:init-value #f)
     ;; base namespace
     (namespace :init-keyword :namespace :reader context-namespace
		:init-value '())))

  (define (make-soap-context version :key (transport #f) (namespace '()))
    (make <soap-context> :version version :transport transport
	  :namespace namespace))

  ;; unfortunately (text html-lite) does not export make-html-element
  ;; but it can be reused in this library, so get it.
  (define %make-soap-element (with-library (text html-lite) make-html-element))
  
  (define-syntax define-soap-type
    (lambda (x)
      (define (id->string id)
	(if id
	    (->string (syntax->datum id))
	    :no-prefix))
      (define (make-name prefix n)
	(if (and prefix (not (eq? prefix :no-prefix)))
	    (string->symbol (format "~a:~a" prefix (syntax->datum n)))
	    (string->symbol (->string (syntax->datum n)))))
      (define (build specs acc)
	(define (parse-spec spec)
	  (syntax-case spec ()
	    ((:namespace namespace localname)
	     (identifier? #'localname)
	     (values (id->string #'namespace) #'(localname)))
	    ((:namespace namespace localname)
	     (values (id->string #'namespace) #'localname))
	    (localname 
	     (identifier? #'localname)
	     (values #f #'(localname)))
	    (localname (values #f #'(localname)))))
	(define (construct prefix spec)
	  (receive (namespace locals) (parse-spec spec)
	    (let loop ((locals locals) (r '()))
	      (syntax-case locals ()
		(() (reverse! r))
		((local . locals)
		 (let1 name (make-name (if prefix prefix namespace) #'local)
		   (loop #'locals
			 (cons 
			  #`(define #,name
			      (%make-soap-element 
			       '#,(make-name namespace #'local) #f))
			  r))))))))
	(syntax-case specs ()
	  (() acc)
	  (((:prefix prefix name) rest ...)
	   (build #'(rest ...)
		  (append acc (construct (id->string #'prefix) #'name))))
	  ((name rest ...)
	   (build #'(rest ...) (append acc (construct #f #'name))))))
      (syntax-case x ()
	((_ specs ...) #`(begin #,@(build #'(specs ...) '()))))))

  ;; send soap request
  ;; context must have transport slot set.
  (define (soap-send-request context message)
    (let1 message (if (string? message) message (tree->string message))
      ((context-transport context) message)))

  (define-soap-type 
    (:prefix %soap (:namespace soapenv Envelope))
    (:prefix %soap (:namespace soapenv Header))
    (:prefix %soap (:namespace soapenv Body)))

  (define (soap:envelope context body :key
			 (header (%soap:Header)) (namespaces '()))
    (define (namespace->attributes namespaces)
      (define (get-namespace context)
	(let1 version (context-version context)
	  (cond ((eq? version +soap-version-1.2+) +envelope-namespace-1.2+)
		((eq? version +soap-version-1.1+) +envelope-namespace-1.1+)
		(else (error 'soap:envelope
			     "context has invalid SOAP version" version)))))
      (let loop ((namespaces (append (context-namespace context) namespaces))
		 (r (list header body)))
	(if (null? namespaces)
	    (cons* :xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"
		   :xmlns:xsd  "http://www.w3.org/2001/XMLSchema"
		   :xmlns:soapenv (get-namespace context)
		   r)
	    (let1 ns (car namespaces)
	      (loop (cdr namespaces)
		    (cons* (make-keyword
			      (string->symbol (format "xmlns:~a" (car ns))))
			   (cadr ns)
			   r))))))
    (apply %soap:Envelope (namespace->attributes namespaces)))
)