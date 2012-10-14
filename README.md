# neurotic

A library to get traits support to clojure's deftype and defrecord.

## Installation

In Leiningen:

```clojure
:dependencies [[bronsa/neurotic "0.2.1"]]
```

## Usage

First of all, require neurotic.traits:

```clojure
user=> (ns my-ns
         (:refer-clojure :exclude [deftype])
         (:require [neurotic.traits :refer [deftype deftrait]]))
nil
``
Next, let's define a protocol with two functions:
```clojure
my-ns=> (defprotocol AProtocol (f [_]) (g [_] [_ _]))
AProtocol
```
We can create a trait using `deftrait`:
```clojure
my-ns=> (deftrait ATrait [] AProtocol (g [_] 1))
#'my-ns/ATrait
```
`deftrait` requires the following args: 
* `name`: the name of the trait
* `required args`: a vector containing the required args an implementing deftype/defrecord must provide that must match the mutable declaration (will be explained later)
* `body`: the body of the trait, containing protocols implementation, and possibly the protocols that are going to be implemented (it's not required but strongly reccomended)

Now that we have created the trait, let's see how to use it in a `deftype`
```clojure
my-ns=> (deftype AType [] :defaults [ATrait])
my_ns.AType
```

We can see that it worked:
```clojure
my-ns=> (g (AType.))
1
```

Let's create another trait, implementing the complete protocol

```clojure
my-ns=> (deftrait ATrait2 [] AProtocol (g [_] 0) (g [_ i] i) (f [_] 0))
#'my-ns/ATrait2
```

Both ATrait and ATrait2 implements the single-arity version of `g`; what happens when we implements both the traits in a new type?

```clojure
my-ns=> (deftype AType [] :defaults [ATrait2 ATrait])
my_ns.AType
my-ns=> (let [a (AType2.)] [(g a) (g a :foo) (f a)]) 
[1 :foo 0]
```

We can see that when multiple traits implementing the same function and arity, the implementation of the last trait specified wins.

Additionaly, the implementation provided in the body of a `deftype` wind over all the traits:
```clojure
my-ns=> (deftype AType3 [] :defaults [ATrait2 ATrait] (f [_] 2))
my_ns.AType3
my-ns=> (f (AType3.))
2
```
You can notice that we didn't need to specify in the body of deftype that we were implementing the protocol `AProtocol` since it's already specified in at least one of the implemented traits.

Let's now see how the required-args mechanism works:

Lets' define two traits
```clojure
my-ns=> (deftrait ATrait3 [foo])
#'my-ns/ATrait3
my-ns=> (deftrait ATrait4 [^:volatile-mutable foo])
#'my-ns/ATrait4
```

Let's see what happens when implementing those traits
```clojure
my-ns=> (deftype AType4 [foo] :defaults [ATrait3])
my_ns.AType4
my-ns=> (deftype AType5 [foo] :defaults [ATrait4])
Exception Mutable declaration mismatching for one or more args  my-ns/eval960 (NO_SOURCE_FILE:1)
my-ns=> (deftype AType6 [^:volatile-mutable foo] :defaults [ATrait4])
my_ns.AType6
my-ns=> (deftype AType7 [] :defaults [ATrait4])
Exception deftype declaration is missing the following args: foo, required by one or more implementing traits  my-ns/eval967 (NO_SOURCE_FILE:1)
```

As you can see, `deftype` is throwing exceptions if a required-arg is missing, or if it's mutable declaration is mismatching between one or more traits, or between traits and deftype declaration.

Everything shown to work with `deftype` works with `defrecord` too.


## License

Copyright © 2012 Bronsa

Distributed under the Eclipse Public License, the same as Clojure.
