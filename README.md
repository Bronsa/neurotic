# neurotic

A library to get traits support to clojure's deftype and defrecord

## Installation

In Leiningen:

```clojure
:dependencies [[bronsa/neurotic "0.2.0"]]
```

## Usage

```clojure
(ns my-ns
  (:refer-clojure :exclude [deftype])
  (:require [neurotic.traits :refer [deftype deftrait]]))
  
(defprotocol AProtocol (f [_]))

(deftrait AProtocol-trait [foo] AProtocol  (f [_] foo))

(deftype AType [foo] :defaults [AProtocol-trait])
```

## License

Copyright © 2012 Bronsa

Distributed under the Eclipse Public License, the same as Clojure.
