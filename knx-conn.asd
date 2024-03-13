(defsystem "knx-conn"
  :version "0.0.1"
  :author "Manfred Bergmann"
  :license "Apache-2"
  :depends-on ("alexandria"
               "usocket"
               "babel"
               "log4cl"
               "binding-arrows"
               "sento")
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "knx-obj")
                 (:file "address")
                 (:file "dpt")
                 (:file "hpai")
                 (:file "dib")
                 (:file "cri")
                 (:file "crd")
                 (:file "cemi")
                 (:file "descr-info")
                 (:file "connect")
                 (:file "tunneling")
                 (:file "knx-connect")
                 )))
  :in-order-to ((test-op (test-op "knx-conn/tests"))))

(defsystem "knx-conn/tests"
  :author "Manfred Bergmann"
  :depends-on ("knx-conn"
               "fiveam"
               "cl-mock"
               )
  :components ((:module "test"
                :components
                ((:file "test-all")
                 (:file "address-test")
                 (:file "cemi-test")
                 (:file "dpt-test")
                 (:file "connect-test")
                 (:file "tunnelling-test")
                 (:file "knx-connect-test")
                 )))
  :description "Test system for knx"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:knx-conn.tests))))

#|
TODOs:

OK - separate 'write' request, without waiting for response
OK - separate 'read' request, 'read' will get a result asynchonously.
OK - move to separate project
OK - implement disconnect
OK - do something with channel-id and seq-counter
OK - more dpt types
OK - parse dpts from byte array
OK - rename `dpt-len' to `dpt-byte-len'
=> - add a 'client' top-level package (rename knx-connect to knx-client)
- add separate 'server' package based on Sento
- more dpts: 5.001 (Stellgröße), 5.010 (Betriebsmodus), 10.001 (Datum)
- default cemi tests
- more error handling tests
- dpt1, more on/off options like (t / nil)

|#
