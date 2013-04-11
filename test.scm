(add-load-path ".")
(import (rnrs)
	(soap lite)
	(text tree)
	(srfi :64)
	(pp))

(test-begin "SOAP Lite tests")

(define-soap-type (:prefix order shipto) name)

(test-equal "soap element"
	    '("<" shipto () ">"
	      (("<" name () ">" ("Name") "</" name ">"))
	      "</" shipto ">")
	    (order:shipto (name "Name")))

(test-equal "A request"
	    "<soapenv:Envelope \
              xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \
              xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" \
              xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\">\
              <soapenv:Header></soapenv:Header>\
              <soapenv:Body><shipto><name>Name</name></shipto></soapenv:Body>\
             </soapenv:Envelope>"
	    (tree->string
	     (soap:envelope (make-soap-context +soap-version-1.1+)
			    (soap:body (order:shipto (name "Name"))))))

(define-soap-type
  ;; for convenient
  (:namespace foo (tag1 tag2))
  (:prefix bar (tag3 tag4))
  (:prefix boo (:namespace brr (tag5 tag6)))
  (:prefix #f (:namespace brr (tag7 tag8))))

(test-equal "new APIs"
	    "<foo:tag1>foo1</foo:tag1>" 
	    (tree->string (foo:tag1 "foo1")))

(test-equal "new APIs(2)"
	    "<brr:tag7>brr</brr:tag7>" 
	    (tree->string (tag7 "brr")))



(test-end)