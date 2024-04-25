(defsystem "knx-conn"
  :version "0.1.0"
  :author "Manfred Bergmann"
  :license "GNU GPL, version 3"
  :description "KNXnet/IP implementation in Common Lisp"
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
                 (:module "knx-model"
                  :serial t
                  :components
                  ((:file "knx-obj")
                   (:file "address")
                   (:file "dpt")
                   (:file "hpai")
                   (:file "dib")
                   (:file "cri")
                   (:file "crd")
                   (:file "cemi")))
                 (:module "knx-msg"
                  :serial t
                  :components
                  ((:file "descr-info")
                   (:file "connect")
                   (:file "tunneling")))
                 (:file "ip-client")
                 (:file "knx-client")
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
                 (:file "descr-info-test")
                 (:file "connect-test")
                 (:file "tunnelling-test")
                 (:file "knx-client-test")
                 (:file "knx-connect-test")
                 )))
  :description "Test system for knx"
  :perform (test-op (op c) (symbol-call :fiveam :run!
                                        (uiop:find-symbol* '#:test-suite
                                                           '#:knx-conn.tests))))

#|
TODOs:
- remove use of *conn* and *local-host-and-port*
- connstate (heartbeat): check for 3 failed requests
- allow hooks to be registered for value updates on certain group-addresses
- make nice DSL for the supported dpt types
? - register awaiting response before sending (!), or the response can be received before wait
|#
