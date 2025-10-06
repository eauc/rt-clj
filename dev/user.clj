(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.spec.alpha :as s]
            [clj-async-profiler.core :as prof]
            [expound.alpha :as expound]
            [orchestra.spec.test :as st]))

(defn reset
  []
  (refresh))

(defn init
  []
  (println "**** REPL init")
  (alter-var-root #'s/*explain-out* (constantly expound/printer))
  (st/instrument))

(defn test-post-load-hook 
  [test-plan]
  (init)
  test-plan)

(defn serve-profiler
  [_]
  (prof/serve-ui 8080))

(comment
  (init))
