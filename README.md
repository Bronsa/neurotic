# neurotic

A library to get traits support to clojure's deftype

## Installation

In Leiningen:

```clojure
:dependencies [[bronsa/neurotic "0.1.0"]]
```

## Usage

```clojure
(ns my-ns
  (:refer-clojure :exclude [deftype])
  (:require [neurotic.traits :refer [deftype deftraits]]))
  
(defprotocol AProtocol (f [_]))

(deftrait AProtocol-trait [foo] (f [_] foo))

(deftype AType [foo] :defaults [AProtocol-traits])
```

## License

Copyright © 2012 Bronsa

Distributed under the Eclipse Public License, the same as Clojure.
