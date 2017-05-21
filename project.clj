(defproject com.plantilus/plantilus "1.5.0-SNAPSHOT"
  :description "Plantilus plantdb HTML generator"
  :min-lein-version "2.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/data.csv "0.1.4"]
                 #_ [com.velisco/herbert "0.7.0"]
                 [org.slf4j/slf4j-simple "1.7.25"]
                 [metrics-clojure "2.9.0"]
                 [me.raynes/fs "1.4.6"]
                 [com.velisco/clj-ftp "0.3.9"]
                 [digest "1.4.5"]
                 [rhizome "0.2.7"]
                 #_ [com.velisco/halfbaked "0.2.3"]
                 [enlive "1.1.6"]]
  :profiles {:dev {:dependencies [[criterium "0.4.4"]]}
             :snapshot {:dependencies [[org.clojure/clojure "1.9.0-master-SNAPSHOT"]]}
             :alpha {:dependencies [[org.clojure/clojure "1.9.0-alpha15"]]}
             }
  :jvm-opts ["-Xms2g" "-Xmx4g"]
  ;; :global-vars {*warn-on-reflection* true}
  :repl-options {:init-ns plantilus.repl}
    ;;  :jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n"]
    ;;  :hooks [leiningen.hooks.cdt]
  )
