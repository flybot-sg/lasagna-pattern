(ns sg.flybot.pullable.malli-rct-test
  "RCT blocks that need Malli. Outside src so rct-clr skips them."
  (:require [malli.core :as m]
            [sg.flybot.pullable.impl :refer [compile-pattern vmr]]
            [sg.flybot.pullable.malli]))

^:rct/test
(comment
  ;; [:vector {:ilookup true} ...] allows indexed lookup patterns like {{:id 1} ?u}

  ;; Without :ilookup - indexed lookup pattern on seq schema returns failing matcher
  (let [m (compile-pattern '{:users {{:id 1} ?u}}
                           {:schema (m/schema [:map [:users [:vector [:map [:id :int] [:name :string]]]]])})]
    m ;=>> fn?
    (:matcher-type (m (vmr {})))) ;=> :schema

  ;; With :ilookup true - indexed lookup pattern compiles
  (compile-pattern '{:users {{:id 1} ?u}}
                   {:schema (m/schema [:map [:users [:vector {:ilookup true}
                                                     [:map [:id :int] [:name :string]]]]])})
  ;=>> fn?

  ;; Invalid field in indexed lookup value pattern returns failing matcher
  (let [m (compile-pattern '{:users {{:id 1} {:invalid ?x}}}
                           {:schema (m/schema [:map [:users [:vector {:ilookup true}
                                                             [:map [:id :int] [:name :string]]]]])})]
    m ;=>> fn?
    (:matcher-type (m (vmr {})))) ;=> :schema

  ;; Valid field in indexed lookup value pattern
  (compile-pattern '{:users {{:id 1} {:name ?n}}}
                   {:schema (m/schema [:map [:users [:vector {:ilookup true}
                                                     [:map [:id :int] [:name :string]]]]])})
  ;=>> fn?

  ;; Deeply nested: indexed lookup inside indexed lookup
  (compile-pattern '{:depts {{:id "eng"} {:members {{:id 1} ?member}}}}
                   {:schema (m/schema [:map [:depts [:vector {:ilookup true}
                                                     [:map
                                                      [:id :string]
                                                      [:members [:vector {:ilookup true}
                                                                 [:map [:id :int] [:name :string]]]]]]]])})
  ;=>> fn?
  )

^:rct/test
(comment
  (def keyed (m/schema [:map [:users [:vector {:ilookup [:map {:closed true} [:id :int]]}
                                      [:map [:id :int] [:name :string]]]]]))

  (:vars ((compile-pattern '{:users {{:id 1} ?u}} {:schema keyed})
          (vmr {:users {{:id 1} {:id 1 :name "Alice"}}})))
  ;=> {'u {:id 1 :name "Alice"}}

  (:reason ((compile-pattern '{:users {{:id "1"} ?u}} {:schema keyed}) (vmr {})))
  ;=> "Invalid key {:id \"1\"}: {:id [\"should be an integer\"]}"

  (:reason ((compile-pattern '{:users {{:id 1 :x 2} ?u}} {:schema keyed}) (vmr {})))
  ;=> "Invalid key {:id 1, :x 2}: {:x [\"disallowed key\"]}"
  (:matcher-type ((compile-pattern '{:users {nil ?u}} {:schema keyed}) (vmr {})))
  ;=> :schema
  (:matcher-type ((compile-pattern '{:users {:admin ?u}} {:schema keyed}) (vmr {})))
  ;=> :schema

  (:reason ((compile-pattern '{:users {{:id "1"} {:name (?n :when string?)}}}
                             {:schema keyed})
            (vmr {})))
  ;=> "Invalid key {:id \"1\"}: {:id [\"should be an integer\"]}"

  (:vars ((compile-pattern '{:users ?all} {:schema keyed}) (vmr {:users [1 2]})))
  ;=> {'all [1 2]}
  (:vars ((compile-pattern '{:users {{:id "1"} ?u}}
                           {:schema (m/schema [:map [:users [:vector {:ilookup true}
                                                             [:map [:id :int]]]]])})
          (vmr {:users {{:id "1"} {:id 1}}})))
  ;=> {'u {:id 1}}

  (def bad-key (m/schema [:map [:a :int] [:xs [:vector {:ilookup 42} :any]]]))
  (:vars ((compile-pattern '{:a ?x} {:schema bad-key}) (vmr {:a 1 :xs [1]})))
  ;=> {'x 1}
  (:reason ((compile-pattern '{:xs {{:id 1} ?v}} {:schema bad-key}) (vmr {})))
  ;=> ":malli.core/invalid-schema"

  (def by-id (m/schema [:map [:history [:map-of [:map {:closed true} [:id :int]]
                                        [:map [:title :string]]]]]))

  (:vars ((compile-pattern '{:history {{:id 1} ?v}} {:schema by-id})
          (vmr {:history {{:id 1} {:title "t"}}})))
  ;=> {'v {:title "t"}}
  (:reason ((compile-pattern '{:history {{:id "1"} ?v}} {:schema by-id}) (vmr {})))
  ;=> "Invalid key {:id \"1\"}: {:id [\"should be an integer\"]}"
  (:reason ((compile-pattern '{:history {{:id 1} {:bogus ?x}}} {:schema by-id}) (vmr {})))
  ;=> "Key :bogus not in schema"
  )
