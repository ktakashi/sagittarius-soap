(add-load-path ".")
(import (rnrs)
	(soap lite)
	(text tree)
	(srfi :64))

(test-begin "SOAP Lite tests")

(define-soap-type (:prefix order shipto) name)
(test-equal "soap element"
	    '("<" shipto () ">"
	      (("<" name () ">" ("Name") "</" name ">"))
	      "</" shipto ">")
	    (order:shipto (soap:name "Name")))

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
			    (soap:body (order:shipto (soap:name "Name"))))))

(test-end)