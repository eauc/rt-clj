(defproject rt-clj "0.1.0-SNAPSHOT"

  :source-paths ["src"]

  :repositories [["snapshots" "https://oss.sonatype.org/content/repositories/snapshots"]]
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [expound "0.7.2"]
                 [org.bytedeco/mkl "2024.0-1.5.10" :classifier linux-x86_64-redist]
                 [org.bytedeco/cuda "12.3-8.9-1.5.10" :classifier linux-x86_64-redist]
                 [uncomplicate/neanderthal "0.53.2"]]

  :profiles {:dev {:source-paths ["dev" "examples" "test"]
                   :jvm-opts ["-Xss8m"                             ;; increase stack size
                              "--enable-native-access=ALL-UNNAMED" ;; required by clojurecl
                              "-Djdk.attach.allowAttachSelf"       ;; used by profiler
                              "-XX:+EnableDynamicAgentLoading"]    ;; used by profiler
                   ; :global-vars {*unchecked-math* :warn-on-boxed
                   ;               *warn-on-reflection* true}
                   :dependencies [[orchestra "2021.01.01-1"]
                                  [org.clojure/tools.namespace "1.5.0"]
                                  [io.github.nextjournal/clerk "0.17.1102"]
                                  [criterium "0.4.6"]
                                  [com.clojure-goes-fast/clj-async-profiler "1.4.0"]
                                  [org.babashka/http-server "0.1.13"]
                                  [io.github.nextjournal/clerk "0.17.1102"]
                                  [uncomplicate/clojurecl "0.16.0"]]}

             :test {:global-vars {*unchecked-math* false
                                  *warn-on-reflection* false}
                    :dependencies [[lambdaisland/kaocha "1.91.1392"]
                                   [lambdaisland/kaocha-cloverage "1.1.89"]]}}

  :aliases {"build-doc" ["run" "-m" "user/build-doc!"]
            "serve-doc" ["run" "-m" "user/serve-doc!"]
            "watch-doc" ["run" "-m" "user/watch-doc!"]
            "test" ["with-profile" "+test" "run" "-m" "kaocha.runner"]})
