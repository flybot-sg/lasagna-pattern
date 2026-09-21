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
