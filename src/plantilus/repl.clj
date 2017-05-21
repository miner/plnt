
(ns plantilus.repl
  ;;(:use clj-stacktrace.repl)
  (:use clojure.repl)
  (:use plantilus.webgen)
  (:use plantilus.resources)
  (:use plantilus.bento)
  (:require clojure.java.shell
            [metrics.timers :as tmr]))


;; For the REPL
;; 
;; (pst)
;; ;prints nice stacktrace

(defn duplicate-report [label ids]
  (if (seq ids)
    (do (println "Duplicate" label)
        (doseq [id ids]
          (println " " id)))
    (println "No duplicate" label)))
    
(defn duplicates-report []
  (duplicate-report "Plant IDs" (check-dups))
  (duplicate-report "Gen IDs" (check-gen-dups))
  (duplicate-report "Auth IDs" (check-authors-dups))
  (duplicate-report "Pub IDs" (check-pub-dups))
  (println "done"))

(defn precheck-missing []
  (println)
  (println "Precheck missing")
  (doseq [[link id pid] (check-for-all-missing *db*)]
    (println pid link id))
  (println "done")
  (println "-----------"))

(defn check-unbalanced []
  (println)
  (check-unbalanced-plants)
  (println "-----------")
  (check-unbalanced-genera)
  (println "-----------")
  (check-unbalanced-authors)
  (println "-----------")
  (check-unbalanced-pubs)
  (println "-----------"))

(defn check-parents []
  (println)
  (check-for-parents *db*)
  (println "-----------"))
  
(defn check-grex []
  (println)
  (check-for-grex *db*)
  (println "-----------"))

(defn ppc []
  (println "Start" (java.util.Date.))
  (println "plantilus generation...\n")
  (let [timer (tmr/timer "full-processing-time")]
    (tmr/time! timer 
               (duplicates-report)
               (check-unbalanced)
               (precheck-missing)
               ;; now real work
               (process-plant-csv)
               (check-parents)
               (check-for-grex *db*))
    (println " done " (report-msecs (tmr/mean timer))))
  (println "running sh for pdbdiff and apdbmiss")
  (clojure.java.shell/sh "/Users/miner/bin/apdbmiss")
  (clojure.java.shell/sh "/Users/miner/bin/apdbdiff")
  (println "ppc done")
  (println "End" (java.util.Date.)))


(defn pgens [plantdb]
  (process-genera-csv plantdb))


;; called by lein run or if you try to execute a jar
(defn -main []
  (println "plantilus doesn't use a -main yet"))


(defn missing-genera []
  (check-for-missing-genera *db*))


(defn tenpar
  ([]
     (tenpar :id :common))
  ([& transforms]
     (take 10 (parse-plantdb-csv plants-csv-file (apply juxt transforms)))))


(defn ptrans
  ([transform limit]
     (println (java.util.Date.))
     (doseq [plant (take limit (parse-plantdb-csv plants-csv-file transform))]
       (println plant))
     (println (java.util.Date.)))
  ([transform]
     (println (java.util.Date.))
     (doseq [plant (parse-plantdb-csv plants-csv-file transform)]
       (println plant))
     (println (java.util.Date.))) )

(defn ppp [] (ptrans identity))


(defn gind []
  (generate-index-page-for-sorted-plants (remove catesby-only? (:sorted *db*))))


(defn gcom []
  (generate-common-page-for-db *db*))



(defn dbfilt [f & keys]
  (let [results (filter f (:sorted *db*))]
    (if-let [ks (seq keys)]
      (map (apply juxt keys) results)
      results)))


(defn ppubs []
  (process-pub-csv))


(defn pauths []
  (process-authors-csv))


(defn uph []
  (update-html-resources))


(alter-var-root #'*db* (constantly (make-plantdb (concat (parse-plantdb-csv plants-csv-file)
                                                         (parse-plantdb-csv orchid-csv-file)
                                                         (parse-plantdb-csv protea-csv-file))
                                                 (genera-map-from-csv)
                                                 (authors-map-from-csv)
                                                 (pubs-map-from-csv))))


(println (count (:idmap *db*)) "plants")

(println "(in-ns 'plantilus.repl)")

#_ (doseq [pid (flatten (dbfilt :grex :id))] 
     (println "\nworking" pid) (pprint (grex-tree-pid *db* pid)))
