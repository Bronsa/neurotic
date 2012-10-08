(ns neurotic.core
  (:refer-clojure :exclude [deftype]))

(defn merge-methods [[defaults provideds]]
  (let [explicitize (fn [[fname args & body]]
                      [[(meta fname) fname (list (meta args) (map meta args)) (count args)] [args body]])
        implicitize (fn [[[_ name _ _] [args body]]]
                      `(~name ~args ~@body))
        defaults (apply hash-map (mapcat explicitize defaults))
        provideds (apply hash-map (mapcat explicitize provideds))
        methods (merge defaults provideds)]
    (map implicitize methods)))

(defn mismatching-mutable? [k1 k2]
  (not (apply =  
              (map (comp (juxt :unsynchronized-mutable
                               :volatile-mutable) meta) [k1 k2]))))

(defn validate-args [provided required]
  (let [pr (set provided)
        req (set required)
        missing (remove pr req)]
    (if-not (empty? missing)
      `(throw (Exception. (str "deftype declaration is missing the following args: " ~@(map str missing)
                               ", required by one ")))
      (if (some true? (map mismatching-mutable? (filter req pr) req))
        `(throw (Exception. "mutable declaration mismatching for one or more args"))))))

(defmacro deftype [name args & body]
  (if (= :defaults (first body))
    (let [body (next body)
          [defaults abstracts-or-required] ((juxt (partial map rest)
                                                  (partial map first))
                                            (map eval (first body)))
          [abstracts required] ((juxt remove (comp (partial mapcat identity) filter))
                                vector? abstracts-or-required)
          separate (juxt (comp set (partial mapcat second))
                          (partial map first))
          [protocols methods] (separate (map (partial (juxt filter remove) list?) defaults))
          methods  (if (> (count methods) 1)
                     (reduce (fn [acc cur] (merge-methods [acc cur])) methods)
                     (first methods))]
      (if-let [err (validate-args args required)] 
        err
        (if (empty? abstracts)
          (let [[m p] ((juxt filter remove) list? (rest body))
                protocols (into protocols p)]
            `(clojure.core/deftype ~name ~args
               ~@protocols
               ~@(merge-methods [methods m])))
          `(throw (Exception. "not yet implemented :(")))))
    `(clojure.core/deftype ~name ~args ~@body)))

(defmacro deftrait [name elems & body]
  (if (= :abstract (first body))
    `(throw (Exception. "not yet implemented :("))
    `(def ~name '(~elems ~@body))))
