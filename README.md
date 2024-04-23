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

## What works

Currently only tunnelling is supported.

Implemented DPTs: `dpt-1.001`, `dpt-5.001`, `dpt-5.010`, `dpt-9.001`, `dpt-10.001`, `dpt-11.001`. More are on the todo list (pull-requests welcome).

The user facing package is: `knxc` (`knx-connect`).

In order to establish a tunnel connection one can do (in package `knxc`):

```lisp
(knx-conn-init "192.168.50.123") ;; hostname or IP address of your KNXnet router/interface
```

Depending on the log level (log4cl) the logging can be a bit noisy (in `:debug`). It is possible to switch to `:info` level using: `(log:config '(knx-conn) :info)`. This should reduce the logging to just 'L_Data.ind', the data change indications. Like:

```
<INFO> [21:43:18] knx-conn.knx-client file77Nnnx (%async-handler-knx-received) - Tunnelling ind 1: 13.13.255 -> 3/0/0 = #(85 43 18)
```

Which shows the device (individual address) issuing a state change and the target (group-address) plus the value as byte array.

The byte array is used unless the DPT for individual GAs (group-addresses) is known. To provide a mapping for GA->DPT one can setup such a list:

```
(defparameter *dpt-map*
  '(("3/0/0" dpt:dpt-10.001 "time-of-day")
    ("1/2/3" dpt:dpt-1.001 "foobar")))
```

This is a list of lists containing of three elements:

1. the GA
2. the dpt
3. a label

The list can be deployed via `knx-conn-init` as parameter, or it can be set and ammended later by setting it to:

```
(setf knx-client:*group-address-dpt-mapping* *dpt-map*)
```

From that moment all data indications are check for if there exists a mapping so that the DPT can be parsed properly. I.e.:

```
 <INFO> [21:59:18] knx-conn.knx-client file77Nnnx (%async-handler-knx-received) - Tunnelling ind 1: 13.13.255 -> 3/0/0 = #(85 59 18)
 <INFO> [21:59:18] knx-conn.knx-client file77Nnnx (%async-handler-knx-received) - Tunnelling ind 2: 13.13.255 -> 3/0/0 = #S(DPT10 :VALUE-TYPE DPT-10.001 :RAW-VALUE #(85 59 18) :VALUE 2024-04-23T21:59:18.404282+02:00) (time-of-day)
```


Disconnect and stop everything is done with:

```
(knx-conn-destroy)
```

