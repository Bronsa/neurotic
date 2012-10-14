(ns neurotic.traits
  (:refer-clojure :exclude [deftype defrecord extend])
  (:require [clojure.string :as s]))

(def ^:private separate (juxt filter remove))

(defn- annotate [[name args & _]]
  (list name (meta name) (meta args) (map meta args)))

(defn- mismatching-mutable? [meta1 meta2]
  (not (every? true?  (map #(= (% meta1) (% meta2)) [:unsynchronized-mutable :volatile-mutable]))))

(defn- validate-elements [args provided]
  (let [required (set (mapcat identity provided))
        missing (reduce disj required args)]
    (if-not (empty? missing)
      `(throw (Exception. (str "deftype declaration is missing the following args: " ~(s/join ", "(map str missing))
                               ", required by one or more implementing traits")))
      (let [hashize (fn [args] (into {} (map #(vector % (meta %)) args)))
            args (hashize args)
            provided (map hashize provided)]
        (if (some true? (mapcat (fn [pr] (map #(mismatching-mutable? (args %) (pr %)) required)) provided))
          `(throw (Exception. "Mutable declaration mismatching for one or more args")))))))

(defrecord Trait [required-elements protocols-or-interfaces declarations])

(defmacro deftrait
  "Usage: (deftrait ATtrait [^:unsyncronized-volatile elem]
           AProtocol
           (protocol-fn [this] elem))"
  [name required-elements & impl]
  (let [[declarations protocols-or-interfaces] (separate seq? impl)]
    `(def ~name
       (map->Trait '{:required-elements ~required-elements
                     :protocols-or-interfaces ~protocols-or-interfaces
                     :declarations ~declarations}))))

(defn- emit-deftype* [name fields opts+specs]
  (let [[interfaces methods opts] (#'clojure.core/parse-opts+specs opts+specs)
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." name))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))]
    `(let []
       (declare ~(symbol (str  '-> name)))
       ~(#'clojure.core/emit-deftype* name name (vec hinted-fields) (vec interfaces) methods)
       (import ~classname)
       ~(#'clojure.core/build-positional-factory name classname fields)
       ~classname)))

(defn- deftype-raw
  [name args body]
  (if (= :traits (first body))
    (let [traits (map eval (second body))
          body (rest (rest body))]
      (if-let [err (validate-elements args (map :required-elements traits))]
        err
        (let [[declarations protocols-or-interfaces] (separate seq? body)
              protocols-or-interfaces (reduce conj (set protocols-or-interfaces) (mapcat :protocols-or-interfaces traits))
              annotate (fn [decs] (into {} (map #(vector (annotate %) %) decs)))
              declarations (vals (merge (apply merge (map #(annotate (:declarations %)) traits)) (annotate declarations)))]
          (emit-deftype* name args (concat protocols-or-interfaces declarations)))))
    (emit-deftype* name args body)))

(defmacro deftype [name args & body]
  (#'clojure.core/validate-fields args)
  (deftype-raw name args body))

(defmacro defrecord
  [name fields & opts+specs]
  (#'clojure.core/validate-fields fields)
  (when (some #{:volatile-mutable :unsynchronized-mutable} (mapcat (comp keys meta) fields))
    (throw (IllegalArgumentException. ":volatile-mutable or :unsynchronized-mutable not supported for record fields")))
  (let [classname (with-meta (symbol (str (namespace-munge *ns*)  "." name)) (meta name))
        gs (gensym)
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        base-fields fields
        fields (conj fields '__meta '__extmap)
        type-hash (hash classname)]
    `(let []
       (declare ~(symbol (str 'map-> name)))
       ~(deftype-raw name (conj hinted-fields '__meta '__extmap)
          `(clojure.lang.IRecord

            clojure.lang.IHashEq
            (hasheq [this#] (bit-xor ~type-hash (.hashCode this#)))
            (hashCode [this#] (clojure.lang.APersistentMap/mapHash this#))
            (equals [this# ~gs] (clojure.lang.APersistentMap/mapEquals this# ~gs))

            clojure.lang.IObj
            (meta [this#] ~'__meta)
            (withMeta [this# ~gs] (new ~name ~@(replace {'__meta gs} fields)))

            clojure.lang.ILookup
            clojure.lang.IKeywordLookup

            (valAt [this# k#] (.valAt this# k# nil))
            (valAt [this# k# else#]
                   (case k# ~@(mapcat (fn [fld] [(keyword fld) fld])
                                      base-fields)
                         (get ~'__extmap k# else#)))
            (getLookupThunk [this# k#]
                            (let [~'gclass (class this#)]
                              (case k#
                                ~@(let [hinted-target (with-meta 'gtarget {:tag name})]
                                    (mapcat
                                     (fn [fld]
                                       [(keyword fld)
                                        `(reify clojure.lang.ILookupThunk
                                           (get [~'thunk ~'gtarget]
                                             (if (identical? (class ~'gtarget) ~'gclass)
                                               (. ~hinted-target ~(symbol fld))
                                               ~'thunk)))])
                                     base-fields))
                                nil)))

            clojure.lang.IPersistentMap
            (count [this#] (+ ~(count base-fields) (count ~'__extmap)))
            (empty [this#] (throw (UnsupportedOperationException. (str "Can't create empty: " ~(str classname)))))
            (cons [this# e#] (#'clojure.core/imap-cons this# e#))
            (equiv [this# ~gs]
                   (boolean
                    (or (identical? this# ~gs)
                        (when (identical? (class this#) (class ~gs))
                          (let [~gs ~(with-meta gs {:tag name})]
                            (and  ~@(map (fn [fld] `(= ~fld (. ~gs ~(symbol fld)))) base-fields)
                                  (= ~'__extmap (. ~gs ~'__extmap))))))))
            (containsKey [this# k#] (not (identical? this# (.valAt this# k# this#))))
            (entryAt [this# k#] (let [v# (.valAt this# k# this#)]
                                  (when-not (identical? this# v#)
                                    (clojure.lang.MapEntry. k# v#))))
            (seq [this#] (seq (concat [~@(map #(list `new `clojure.lang.MapEntry (keyword %) %) base-fields)]
                                      ~'__extmap)))
            (iterator [this#] (clojure.lang.SeqIterator. (.seq this#)))
            (assoc [this# k# ~gs]
              (condp identical? k#
                ~@(mapcat (fn [fld]
                            [(keyword fld) (list* `new name (replace {fld gs} fields))])
                          base-fields)
                (new ~name ~@(remove #{'__extmap} fields) (assoc ~'__extmap k# ~gs))))
            (without [this# k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                  (dissoc (with-meta (into {} this#) ~'__meta) k#)
                                  (new ~name ~@(remove #{'__extmap} fields)
                                       (not-empty (dissoc ~'__extmap k#)))))

            java.util.Map
            java.io.Serializable
            (size [this#] (.count this#))
            (isEmpty [this#] (= 0 (.count this#)))
            (containsValue [this# v#] (boolean (some #{v#} (vals this#))))
            (get [this# k#] (.valAt this# k#))
            (put [this# k# v#] (throw (UnsupportedOperationException.)))
            (remove [this# k#] (throw (UnsupportedOperationException.)))
            (putAll [this# m#] (throw (UnsupportedOperationException.)))
            (clear [this#] (throw (UnsupportedOperationException.)))
            (keySet [this#] (set (keys this#)))
            (values [this#] (vals this#))
            (entrySet [this#] (set this#))

            ~@opts+specs))

       (defn ~(symbol (str 'map-> name))
         ~(str "Factory function for class " classname ", taking a map of keywords to field values.")
         ([m#] (~(symbol (str classname "/create")) m#)))
       ~classname)))

;;check  (. (eval type) getBasis), mutable declarations, and interfaces
(defmacro extend [type & body]
  (if (= :traits (first body))
    (let [traits (map eval (second body))
          body (rest (rest body))
          protocols (set (mapcat :protocols-or-interfaces traits))
          methods (->> (mapcat :declarations traits)
                       (reduce (fn [r [k a & b]] (merge-with conj r {(keyword k) {(count a) (list* a b)}})) {})
                       (map (fn [m] [(first m) (concat '(fn) (vals (second m)))]))
                       (into {}))
          traits (mapcat (fn [p] (let [m (keys (:method-map (eval p)))]
                                   [p (into {} (map #(vector % (methods %)) m))])) protocols)]
      `(clojure.core/extend ~type ~@traits ~@body))
    `(clojure.core/extend ~type ~@body)))
