(ns sg.flybot.pullable.core2)

(defn- data-of
  [m]
  (get m '&))

(defn- assoc-data
  [m v]
  (assoc m '& v))

(defprotocol Query
  (-value [this data]
    "query `data`, returns a value, `::none` if not found")
  (-accept [this data v]
    "accept val `v` to `data`, returns a new data"))

(defrecord SimpleQuery [k]
  Query
  (-value
    [_ data]
    (get data k ::none))
  (-accept
    [_ data v]
    (assoc data k v)))

(defn query
  [q f-next f-val]
  (fn [m]
    (let [data   (data-of m)
          v      (-value q data)
          [m' v] (f-val m v)]
      (when-not (= v ::none)
        (let [m-next (f-next (assoc-data m' v))
              m* (assoc-data m-next (-accept q (empty m) (data-of m-next)))]
          (first (f-val m* (-value q (data-of m*)))) ;second pass for backtracking
          )))))

;; value query
(defn id [m v] [m v])

(defn pred
  [f]
  (fn [m v]
    (if (f v) [m v] [nil ::none])))

(defn bind
  [sym]
  (fn [m v]
    (let [sv (get m sym)]
      (if (nil? sv)
        [(assoc m sym v) v]
        ((pred #(= % sv)) m v)))))

^:rct/test
(comment
  ;A simple query
  ((query (->SimpleQuery :a) identity id) '{& {:a 1 :b 2}})
  ;=> {& {:a 1}} 
  
  ;A simple query with nested query  
  ((query (->SimpleQuery :a) (query (->SimpleQuery :b) identity id) id) '{& {:a {:b 1 :c 2}}})
  ;=> {& {:a {:b 1}}} 
  
  ;a simple query with ::none value
  ((query (->SimpleQuery :a) identity id) '{& {:b 1}})
  ;=> nil
  
  ; simple query with pred
  ((query (->SimpleQuery :a) identity (pred even?)) '{& {:a 1}})
  ;=> nil 
  ((query (->SimpleQuery :a) identity (pred even?)) '{& {:a 2}})
  ;=> {& {:a 2}}
  
  ;simple query using bound value
  ((query (->SimpleQuery :a) identity (bind '?a)) '{& {:a 1}})
  ;=> {& {:a 1} ?a 1}
  ((query (->SimpleQuery :a) identity (bind '?a)) '{& {:a 1} ?a 2})
  ;=> nil
  
  ;backtrack bound value
  ((query (->SimpleQuery :a) #(assoc % '?a 2) (bind '?a)) '{& {:a 1}})
  ;=> nil
  )