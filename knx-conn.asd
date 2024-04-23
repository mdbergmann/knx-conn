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

OK - separate 'write' request, without waiting for response
OK - separate 'read' request, 'read' will get a result asynchonously.
OK - move to separate project
OK - implement disconnect
OK - do something with channel-id and seq-counter
OK - more dpt types
OK - parse dpts from byte array
OK - rename `dpt-len' to `dpt-byte-len'
OK - make top-level client-like
OK - implement CONNECTIONSTATE_REQUEST/RESPONSE
OK - received disconnect-request closes tunnel connection and cleans up
OK - make starting retrieve configurable
OK - restructure code: folders knx-model, knx-msg
OK - split knx-conn into: ip-client (dep: knx-model/knx-msg), knx-client (dep: ip-client),
    knx-conn (dep: knx-client)
OK - add heartbeat (every 60 seconds) to keep connection alive
OK - make starting heartbeat configurable
OK - connstate: check for up-to 10 seconds response delay
OK - make with- macro for setting single value only (disabled retrieve and heartbeat)
OK - do tunnel connection in knx-conn-init
OK - do tunnel close in knx-conn-destroy
OK - read-request -> await-value use-case
OK - rework *rceived-things* to be a hash-table with key (req type) and value (the received response) that fits to the request. The entry is cleaned up after the response is received.
OK - use atomic for *tunnel-request-listener* (handled through actor)
OK - default cemi tests
OK - more error handling tests
OK - implement optimized dpt-1,2,3 data packaging
? - cleanup routine for *awaited-things* that cleans up orphaned things
OK - implement proper protocol. Send Ack after receiving ind.
OK - check for .con (confirmation) after sending .req
OK - set endpoint address also for disconnect-req
OK - check on connect-resp whether a new connection for data has tobe established.
When port is the same as for ctrl, then not.
-> not absolutely necessary, one connection is enough, can be controlled by client via conn
OK - revert back to single channel for simplicity
OK - fix apci-gv-response parsing. is read as gv-read
OK - with-knx/ip, bail out if no connection-response received
OK - test for heartbeat in knx-client-test
NO - make :wait-for-resp be more dynamic by providing a function that checks the response
-> not doing: because awaiting the response-type is sufficient for this layer of communication.
OK - implement proper tunneling-ack handling
OK - test with ack timeout in knx-client
OK - resend tunnel-req once if no ack received within timeout
OK - tunnel-ack should arrive in 1 second, though we can be more forgiving.
OK - wait for tunnel-ack before allowing next tunnel-request
OK - map the ack for 'write-value' in knx-connect to just `T`, or error condition, but hide the ack.
OK - sending L_Data.req wait for L_Data.ind-response, etc.
OK - toggle dpt1
OK - allow a mapping from ga to dpt type for parsing.
OK - supply ga->dpt mapping on knx-init
OK - more dpts: 5.001 (Stellgröße), 5.010 (Betriebsmodus), 10.001 (time-of-day), 11.001 (date)
OK - extract receive-handlers to separate functions.
- is response `T' or an error really good and allows a smooth mapping of values with read/write, etc.?
OK - remove 'start-async-receive' flag from public interface, because it always needed to receive. But keep internal.
=> - support the implemented dpts in 'write-value'
- remove use of *conn* and *local-host-and-port*
- connstate (heartbeat): check for 3 failed requests
- allow hooks to be registered for value updates on certain group-addresses
- make nice DSL for the supported dpt types
? - register awaiting response before sending (!), or the response can be received before wait

(defparameter *knx-if* "192.168.50.41")
|#
