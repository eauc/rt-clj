(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.spec.alpha :as s]
            [clj-async-profiler.core :as prof]
            [expound.alpha :as expound]
            [babashka.http-server :as http]
            [nextjournal.clerk :as clerk]
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

(defn build-doc!
  []
  (clerk/build!
   {:paths ["examples/rt_clj/**" "src/**"]}))

(defn serve-doc!
  []
  (http/exec
   {:port 7777 :dir "public/build"}))

(defn watch-doc!
  []
  (clerk/serve!
    {:browse? true
     :paths ["examples/rt_clj/**" "src/**"]
     :watch-paths ["examples/rt_clj" "src"]}))

(defn serve-profiler
  []
  (prof/serve-ui 8080))
