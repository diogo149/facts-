# facts+

Similar to Midje's `fact` or `facts` macro, but with modifications centered around a hierarchical structure of facts and being less repetitive.

[![Build Status](https://travis-ci.org/diogo149/facts-.png?branch=master)](https://travis-ci.org/diogo149/facts-)

## Differences

```clojure
(declare f)
```

### Share provided* forms

```clojure
(facts+
 (f 1) => 1
 (f 1) => 1
 (provided* (f 1) => 1))
```

### Inherit provided* forms

```clojure
(facts+
 (facts
   (+ (f 1) (f 2)) => 3
   (provided (f 2) => 2))
 (facts
   (+ (f 1) (f 2)) => 4
   (provided (f 2) => 3))
 (provided* (f 1) => 1))
```

### Stop writing repetitve `provided` forms

```clojure
(defprovided odd-calls (odd-calls & anything) => irrelevant :times odd?)

(facts+
 (f 3) => irrelevant
 (provided+ :odd-calls f))

(defnprovided three [f num] (f num) => 3)

(facts+
 :facts+
 (f 42) => 3
 (provided+ :three [f 42]))
```

### Propagate metadata

```clojure
(facts+
 :facts+
 (fact (fact (fact 1 => 1)))) ;; gets called when filtering for facts+

(facts+ :a
  (fact :b 1 => 1))
```

## License

Copyright © 2014 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
