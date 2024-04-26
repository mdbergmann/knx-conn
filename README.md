# knx-conn
KNX client connectivity in Common Lisp

## What is KNX?

KNX is an industry standard fieldbus system used for facility/building automation. It is the successor of EIB (European Installation Bus). Read more here: [KNX](https://en.wikipedia.org/wiki/KNX).

While not necessarily meant for private, one-family homes, some use it also there.  
There are currently quite a few home-automation solutions out there which come with support for KNX. Some of them are: [openHAB](https://www.openhab.org/) (Java based), [Home assistant](https://www.home-assistant.io/) (Python based).

KNX installations usually use a two-wire twisted pair cable (TP). But there is support for ethernet (IP) via gateways/routers and tunnelling. This is often used to connect buildings over longer distances into one KNX installation.

The KNXnet/IP gateways are also utilized by the home-automation solutions mentioned above, by listening on changes on the bus and also send change requests to devices or range of devices via so called group-addresses.

KNXnet/IP is supported by a variety of languages and runtimes. Among others: Java, Python, C, Elixir, Perl, ...

This project provides KNXnet/IP support for Common Lisp.

It is currently still considered alpha/beta. Some interfaces may still change.

## What works

Currently tunnelling is supported, routing is not.

Implemented DPTs: `dpt-1.001`, `dpt-5.001`, `dpt-5.010`, `dpt-9.001`, `dpt-10.001`, `dpt-11.001`.  
More are on the todo list (pull-requests welcome).

Tested under SBCL 2.4.3 and CCL 1.12.2

### Establish a tunnel connection

In order to use all this one has to establish a 'tunnel' connection to the bus. This works by connecting via UDP protocol to a KNXnet/IP gateway/router in tunnel mode. When the connection is established it will be possible to listen on bus events read or write values from/to GAs.

*Let's see how this works:*

The user facing package is: `knxc` (`knx-connect`).

In order to establish a tunnel connection one can do (in package `knxc`):

```lisp
(knx-conn-init "192.168.50.123") ;; hostname or IP address of your KNXnet router/interface
```

Once connected there may be a lot of events coming from the bus. Depending on the log level (log4cl) the logging can be a bit noisy (in `:debug`). It is possible to switch to `:info` level using: `(log:config '(knx-conn) :info)`. This should reduce the logging to just 'L_Data.ind', the data change indications. Like:

```
<INFO> [21:43:18] knx-conn.knx-client file77Nnnx (%async-handler-knx-received) - Tunnelling ind 1: 13.13.255 -> 3/0/0 = #(85 43 18)
```

Which shows the device (individual address) issuing a state change and the target (group-address) plus the value as byte array.

The byte array is used unless the DPT for individual GAs (group-addresses) is known. To provide a mapping for GA->DPT one can setup a list as this:

```lisp
(defparameter *dpt-map*
  '(("3/0/0" dpt:dpt-10.001 "time-of-day")
    ("1/2/3" dpt:dpt-1.001 "foobar")))
```

This is a list of lists each containing three elements:

1. the GA as string representation
2. the dpt type
3. a label

The list can be provided via parameter to `knx-conn-init`, or deployed later by setting it to:

```lisp
(setf knx-client:*group-address-dpt-mapping* *dpt-map*)
```

It can also be ammended anytime.

From the moment of setting the mapping all data indications are checked for if there exists a mapping so that the DPT can be parsed properly. I.e. then you will see two loggings, one as previous and another that applied the right DPT:

```
 <INFO> [21:59:18] knx-conn.knx-client file77Nnnx (%async-handler-knx-received) - Tunnelling ind 1: 13.13.255 -> 3/0/0 = #(85 59 18)
 <INFO> [21:59:18] knx-conn.knx-client file77Nnnx (%async-handler-knx-received) - Tunnelling ind 2: 13.13.255 -> 3/0/0 = #S(DPT10 :VALUE-TYPE DPT-10.001 :RAW-VALUE #(85 59 18) :VALUE 2024-04-23T21:59:18.404282+02:00) (time-of-day)
```

### Read requests

Is it possible to request reading a value from a GA by:

```lisp
(request-value "3/2/0" 'dpt:dpt-9.001)
```

The `request-value` call returns a `future`, because the requested value is received asynchronously from the bus. So we can do something like below to handle the received value:

```
KNX-CONNECT> (fcompleted 
                 (request-value "3/2/0" 'dpt:dpt-9.001)
                 (result)
               (format t "Temperature outside: ~a~%" result))
#<FUTURE promise: #<PROMISE finished: NIL errored: NIL forward: NIL #x302003B3CC1D>>
Temperature outside: 6.5
```

`fcompleted` is non-blocking and sets up a completion handler function that is called when the future is resolved. `result` is then populated and the handler form is called, which here just prints the received value.
 
### Write requests

It is also possible to send write requests to GAs in order to change state/values. I.e. to toggle a light or so. This can be done with:

```lisp
(write-value "0/0/4" 'dpt:dpt-1.001 t)
```

The last argument `T` here means 'switch on' the light. `NIL` would switch it off.  
Again `write-value` returns `future`.

```
KNX-CONNECT> (fawait (write-value "0/0/4" 'dpt:dpt-1.001 t)
                     :timeout 1.0)
T
#<FUTURE promise: #<PROMISE finished: T errored: NIL forward: NIL #x302003D321DD>>
```

This uses a blocking alternative to `fcompleted`. `fawait` waits for the resolution of the `future` by `:timeout` seconds at most. The return value of `fawait` is `values` of `T` in case of success or a condition in case of error. The second value is always the future for reference.

### Combined read write

Sometimes the use case is to read a value from a GA, do some logic and write a new value to the GA based on the logic. This can be done i.e. using `fmap`.

```lisp
(fmap 
    (request-value "0/0/4" 'dpt:dpt-1.001)
    (result)
  (write-value "0/0/4" 'dpt:dpt-1.001 
               (case result
                 (:on nil)
                 (:off t))))
```

This will toggle the state of a light and is fully asynchronous. The future of `request-value` is mapped to `write-value`. The result of `fmap` is again a future.

For more info on futures have a look here (except `fawait` which is not in there yet): [Futures API](https://mdbergmann.github.io/cl-gserver/index.html#SENTO.FUTURE:@FUTURE%20MGL-PAX:SECTION).

The api may still be subject of change.

### Custom L_Data listeners

It is possible to register custom listeners to get notified for change events coming from the bus.

I.e. suppose you want to get notified when a certain device state changed. Let's define the listener function:

```lisp
(defun room-xyz-light-listener (req)
  (let ((message-code
          (tunnelling-cemi-message-code req)))
    (when (eql message-code +cemi-mc-l_data.ind+)
      (let* ((cemi (tunnelling-request-cemi req))
             (ga-str (address-string-rep 
                      (cemi-destination-addr cemi))))
        (when (string= "0/0/4" ga-str)
          (format t "received event for light in room xyz: ~a~%" req)
          (terpri))))))
```

The listener function takes one argument which is the received tunnelling request object (`knx-tunnelling-request`). This listener function implementation filters for an `L_Data.ind` message code and for a specific destination group-address (GA), "0/0/4" here, which is a specific light in room xyz.  
The manual filtering is a bit combersome and some laters versions might improve on that, but it works.

Now the listener function must be registered. There are two ways to do this:

1. the `knx-conn-init` allows to specify a list of listener functions up-front via parameter
2. after initialization of `knx-conn-init` register each listener function via `add-tunnelling-request-listener` like so:

```lisp
(knx-client:add-tunnelling-request-listener
  #'room-xyz-light-listener)
```

Once done and the listener function receives such a message this listener code dumps the request message:

```
received event for light in room xyz: #S(KNX-TUNNELLING-REQUEST :HEADER #S(KNX-HEADER :LEN 6 :KNXNETIP-VERSION 16 :TYPE 1056 :BODY-LEN 15) :CONN-HEADER #S(CONNECTION-HEADER :LEN 4 :CHANNEL-ID 79 :SEQ-COUNTER 132 :RESERVED 0) :CEMI #S(CEMI-L-DATA :MESSAGE-CODE 41 :INFO-LEN 0 :ADDITIONAL-INFO NIL :CTRL1 #*10111100 :CTRL2 #*11010000 :SOURCE-ADDR #S(KNX-INDIVIDUAL-ADDRESS :ADDR #(19 14) :STRING-REP 1.3.14) :DESTINATION-ADDR #S(KNX-GROUP-ADDRESS :ADDR #(0 4) :STRING-REP 0/0/4) :NPDU-LEN 1 :TPCI 0 :PACKET-NUM 0 :APCI #S(APCI-GV-WRITE :START-CODE 128 :MASK 191) :DATA #(0)))
```

!!! Beware that the listener function is executed by the thread that is responsible for handling received bus tunnelling requests. It should not be blocked, keep it light, or hand the work over to another thread context. I.e. via [Sento Tasks API](https://mdbergmann.github.io/cl-gserver/index.html#SENTO.TASKS:@TASKS%20MGL-PAX:SECTION).


### Cleaning up

Disconnect and stop everything and clean up resources is done with:

```
(knx-conn-destroy)
```

