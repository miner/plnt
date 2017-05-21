(in-ns 'plantilus.core)

;;; 02/14/14  10:08 by miner -- probably obsolete

(defn gen-sample-db []
  (let [all-plants (parse-csv csv-file)
	all-plants-map (reduce (fn [pm p] (conj pm [(:id p) p])) {} all-plants)
	all-cults (gen-cults (keys all-plants-map))]
    {:pmap all-plants-map
     :cults all-cults
     :auth {}
     :url-auth "http://plantilus.com/nomenclature/list-of-plant-authors.html"
     }))


;; to set up the right resources for testing
;; cp -R ~/plants/plantilus/src/plantilus/template /tmp/plantdb
;; mkdir /tmp/plantdb/foo

(defn setup-test []
  (when-not (.exists tmp-dir)
    (clojure.java.shell/sh "cp" "-R" (str template-dir) (str tmp-dir))))


(defn pw []
  (setup-test)
  (let [plant tplant]
    (let [pid (:id plant)
	  info-file (format "file:///tmp/plantdb/%s/index.html" pid)]
      (.mkdirs (io/file tmp-dir pid))
      (with-open [wr (io/writer info-file :encoding "utf-8")]
	(binding [*out* wr]
	  (doseq [str (dbspecies tplant)]
	    (print str))))
      (let [tmpfile  (format "/tmp/plantdb/%s/index.html" pid)]
	(println "wrote " tmpfile)
	(clojure.java.shell/sh "open" tmpfile)))))


(defn ppp-look-for-specials []
  (with-open [rdr (io/reader csv-file :encoding "MacRoman")]
    (doseq [line (line-seq rdr)]
      (let [specials   (reduce (fn [ys x] (if (> (int x) 255) (conj ys (int x)) ys)) #{} line)]
	(when (seq specials)
	  (println)
	  (println line)
	  (println specials) )))))

