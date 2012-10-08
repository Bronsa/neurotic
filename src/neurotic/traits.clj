(ns neurotic.traits
  (:refer-clojure :exclude [deftype]))

(def ^:private separate (juxt filter remove))

(defn- annotate [[name args & _]]
  (list name (meta name) (meta args) (map meta args)))

(defn- mismatching-mutable? [meta1 meta2]
  (not (every? true?  (map #(= (% meta1) (% meta2)) [:unsynchronized-mutable :volatile-mutable]))))

(defn- validate-elements [args provided]
  (let [required (set (mapcat identity provided))
        missing (reduce disj required args)]
    (if-not (empty? missing)
      `(throw (Exception. (str "deftype declaration is missing the following args: " ~@(map str missing)
                               "required by one or more implementing traits")))
      (let [hashize (fn [args] (into {} (map #(vector % (meta %)) args)))
            args (hashize args)
            provided (map hashize provided)]
        (if (some true? (mapcat (fn [pr] (map #(mismatching-mutable? (args %) (pr %)) required)) provided))
          `(throw (Exception. "Mutable declaration mismatching for one or more args")))))))

(defmacro deftrait
  "Usage: (deftrait ATtrait [^:unsyncronized-volatile elem]
           AProtocol
           (protocol-fn [this] elem))"
  [name required-elements & impl]
  (let [[declarations protocols-or-interfaces] (separate seq? impl)]
    `(def ~name
       '{:required-elements ~required-elements
         :protocols-or-interfaces ~protocols-or-interfaces
         :declarations ~declarations})))

(defmacro deftype
  "Like clojure.core/deftype, but allows traits implementations.
Usage: (deftype AType [] :defaults [ATrait])"
  [name args & body]
  (if (= :defaults (first body))
    (let [traits (map eval (second body))
          body (rest (rest body))]
      (if-let [err (validate-elements args (map :required-elements traits))]
        err
        (let [[declarations protocols-or-interfaces] (separate seq? body)
              protocols-or-interfaces (reduce conj (set protocols-or-interfaces) (mapcat :protocols-or-interfaces traits))
              annotate (fn [decs] (into {} (map #(vector (annotate %) %) decs)))
              declarations (vals (merge (apply merge (map #(annotate (:declarations %)) traits)) (annotate declarations)))]
          `(clojure.core/deftype ~name ~args ~@protocols-or-interfaces ~@declarations))))
    `(clojure.core/deftype ~name ~args ~@body)))