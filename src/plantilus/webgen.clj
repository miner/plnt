(ns plantilus.webgen
  (:require [plantilus.bento :as b]
            [plantilus.resources :refer :all]
            [plantilus.templates :refer :all]
            [clojure.data :as d]
            [plantilus.util :as util]
            [miner.ftp :as ftp]
            [me.raynes.fs :as fs]
            [digest :as digest]
            [metrics.timers :as tmr]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import java.io.FileInputStream
           (java.util.zip CheckedInputStream Checksum Adler32 CRC32)))


(def ^:dynamic *collator* (java.text.Collator/getInstance java.util.Locale/US))

(def ^:const bsize (* 4 1024))



(defn collated-compare [s1 s2]
  (.compare ^java.text.Collator *collator* s1 s2))

(defn collation-key [s]
  (.getCollationKey  ^java.text.Collator *collator* s))


(defn drop-extension [^String filename]
  (let [dot (.lastIndexOf filename ".")]
    (if (neg? dot) filename (subs filename 0 dot))))

(defn replace-extension [^String filename ^String ext]
  (str (drop-extension filename) ext))  


(defn write-to-utf8-file [file strs]
  (with-open [wr (io/writer file :encoding "utf-8")]
    (binding [*out* wr]
      (doseq [str strs]
	(print str)))))


(defn report-msecs [msecs]
  (cond (< msecs 10000) (str msecs " msec")
        (< msecs (* 20 10000)) (str (/ (quot msecs 10) 100.0) " sec")
        (< msecs (* 20 60 10000)) (str (/ (quot msecs 600) 100.0) " min")
        :else (str msecs " msec")))

(defn process-one-plant-images [plant]
  (let [pid (:id plant)
        files (:images plant)
        images-file (io/file plantdb-dir pid "images.html")]
    (when (:alpha plant)
      (print (:alpha plant))
      (flush))
    (when (pos? (count files))
      (with-open [wr (io/writer images-file :encoding "utf-8")]
        (binding [*out* wr]
          (doseq [str (dbimagethumbs plant files)]
            (print str))))
      (dotimes [index (count files)]
        ;; strip off the leading 0 or 1 to get the big file
        (let [f (get files index)
              base (subs f 1)
              hfile (io/file plantdb-dir pid (single-image-page-for-thumb f))]
          (write-to-utf8-file hfile (dbimagesingle plant base index files)))))))

;; FIXME should handle single image as a special case
(defn process-plant-images [plantdb]
  (assert (fs/exists? plantdb-dir))
  (let [plants (:sorted (or plantdb b/*db*))]
    (let [agents (map (fn [p] (send (agent p) process-one-plant-images)) plants)]
      (when-not (apply await-for (* 1000 60 15) agents)
        (throw (ex-info "image page generation timed out!"
                          {:agent-errors (filter agent-error agents)}))))))

(defn process-pub-csv []
  (let [pubs1 (b/parse-pubs-csv pub-csv-file)
	pubs (b/update-alpha (sort-by :sortby pubs1) :sortby :alpha)
        pub-file (io/file plantdb-dir "1nfo" "publications.html")]
      (io/make-parents pub-file)
      (write-to-utf8-file pub-file (dbpubs pubs (b/alpha-set pubs :alpha)))))

(defn process-authors-csv []
  (let [auths1 (b/parse-authors-csv author-csv-file)
	auths (b/update-alpha (sort-by #(collation-key (:shortauth %1)) auths1) :shortauth :alpha)
        auth-file (io/file plantdb-dir "1nfo" "authors.html")]
      (io/make-parents auth-file)
      (write-to-utf8-file auth-file (dbauthors auths (b/alpha-set auths :alpha)))))


(defn process-botreg-csv [plantdb]
  (binding [b/*db* plantdb]
    (let [pins1 (concat (b/parse-botreg-csv botreg-csv-file)
                        (b/parse-botreg-csv botreg-protea-csv-file)
                        (b/parse-botreg-csv botreg-orchid-csv-file))
          sortkey-fn #(:sort-name (b/plant-for-id (:id %)))
          pins (b/update-alpha (sort-by #(collation-key (sortkey-fn %)) pins1) sortkey-fn :alpha)
          pin-file (io/file plantdb-dir "3ook" "botreg.html")]
      (io/make-parents pin-file)
      (write-to-utf8-file pin-file (dbbotreg-index pins plantdb (b/alpha-set pins :alpha))))))

(defn process-botrepos-csv [plantdb]
  (binding [b/*db* plantdb]
    (let [pins1 (concat (b/parse-botrepos-csv botrepos-csv-file)
                        (b/parse-botrepos-csv botrepos-protea-csv-file)
                        (b/parse-botrepos-csv botrepos-orchid-csv-file))
          sortkey-fn #(:sort-name (b/plant-for-id (:id %)))
          pins (b/update-alpha (sort-by #(collation-key (sortkey-fn %)) pins1) sortkey-fn :alpha)
          pin-file (io/file plantdb-dir "3ook" "botrepos.html")]
      (io/make-parents pin-file)
      (write-to-utf8-file pin-file (dbbotrepos-index pins plantdb (b/alpha-set pins :alpha))))))

(defn process-jacqplrar-csv [plantdb]
  (binding [b/*db* plantdb]
    (let [pins1 (b/parse-jacqplrar-csv jacqplrar-csv-file)
          sortkey-fn #(:sort-name (b/plant-for-id (:id %)))
          pins (b/update-alpha (sort-by #(collation-key (sortkey-fn %)) pins1) sortkey-fn :alpha)
          pin-file (io/file plantdb-dir "3ook" "jacqplrar.html")]
      (io/make-parents pin-file)
      (write-to-utf8-file pin-file (dbjacqplrar-index pins plantdb (b/alpha-set pins :alpha))))))

(defn process-pinus-csv [plantdb]
  (binding [b/*db* plantdb]
    (let [pins1 (b/parse-pinus-csv pinus-csv-file)
          sortkey-fn #(:sort-name (b/plant-for-id (:id %)))
          pins (b/update-alpha (sort-by #(collation-key (sortkey-fn %)) pins1) sortkey-fn :alpha)
          pin-file (io/file plantdb-dir "3ook" "pinus.html")]
      (io/make-parents pin-file)
      (write-to-utf8-file pin-file (dbpinus-index pins plantdb (b/alpha-set pins :alpha))))))

(defn process-pinus-more [plantdb]
  (binding [b/*db* plantdb]
    ;; just copy the resource straight, no html gen
    (let [template (io/file template-dir "3ook" "pinus-more.html")
          myfile (io/file plantdb-dir "3ook" "pinus-more.html")]
      (fs/copy template myfile))))

(defn process-cultprot-csv [plantdb]
  (binding [b/*db* plantdb]
    (let [pins1 (b/parse-cultprot-csv cultprot-csv-file)
          sortkey-fn #(:sort-name (b/plant-for-id (:id %)))
          pins (b/update-alpha (sort-by #(collation-key (sortkey-fn %)) pins1) sortkey-fn :alpha)
          pin-file (io/file plantdb-dir "3ook" "cultprot.html")]
      (io/make-parents pin-file)
      (write-to-utf8-file pin-file (dbcultprot-index pins plantdb (b/alpha-set pins :alpha))))))

(defn process-cultprot-more [plantdb]
  (binding [b/*db* plantdb]
    ;; just copy the resource straight, no html gen
    (let [template (io/file template-dir "3ook" "cultprot-more.html")
        myfile (io/file plantdb-dir "3ook" "cultprot-more.html")]
      (fs/copy template myfile))))

(defn process-genera-csv [plantdb]
  (let [gens1 (b/parse-genera-csv genera-csv-file)
	gens (b/update-alpha (sort-by #(collation-key (:genus %1)) gens1) :genus :alpha)]
    (let [gen-file (io/file plantdb-dir "1nfo" "genera.html")]
      (io/make-parents gen-file)
      (write-to-utf8-file gen-file (dbgenera-index gens plantdb (b/alpha-set gens :alpha))))
    (doseq [gen gens]
      (let [gfile (io/file plantdb-dir (:genid gen) "index.html")]
        (fs/mkdirs (fs/parent gfile))
        (write-to-utf8-file gfile (dbgenera-single gen plantdb))))))

(defn process-catesby [plantdb]
  (let [catesby-file (io/file plantdb-dir "3ook" "catesby.html")
        cat-plants (filter :catref (:sorted plantdb))]
    (io/make-parents catesby-file)
    (write-to-utf8-file catesby-file (dbcatesby cat-plants (b/alpha-set cat-plants :catalpha)))))

(defn process-catesby-index [plantdb]
  (let [catesby-file (io/file plantdb-dir "3ook" "catesby-index.html")
        cat-plants (filter :catref (:sorted plantdb))
        cmap (into {} (map #(vector (:id %) (calc-catesby-code %)) cat-plants))]
    (io/make-parents catesby-file)
    (write-to-utf8-file catesby-file  (dbcatesby-index (sort-by #(cmap (:id %)) cat-plants)))))

(defn common-alpha-bimap [sorted-common-map]
  ;; common-map is a sorted-map of common name to ID so we already
  ;; have items in order by common name. First, we find the first
  ;; instance of each letter of the common names.  As the original map
  ;; is sorted, we can just run through it with reduce.  But we also
  ;; want the inverse map, for deciding if a ch key is mapped.
  ;; Because the domains are independent, we can keep both mappings
  ;; (ch -> common, and common -> ch) in a single map.  Very clever.
  (reduce (fn [atoc [common _]]
	    (let [c (first (str/upper-case (first common)))]
	      (if (get atoc c)
		atoc
		(assoc atoc c common common c))))
	  {}
	  sorted-common-map))


(defn generate-index-page-for-sorted-plants [plants-sorted]
  (let [index-file (io/file plantdb-dir "1nfo" "index.html")]
    (io/make-parents index-file)
    (write-to-utf8-file index-file (dbindex plants-sorted (b/alpha-set plants-sorted :alpha)))))

(defn generate-common-page-for-db [db]
  (let [common-file (io/file plantdb-dir "1nfo" "common.html")]
    (io/make-parents common-file)
    (write-to-utf8-file common-file (dbcommon (:commonmap db) (common-alpha-bimap (:commonmap db))))))


(defn process-sppl [plantdb]
  (binding [b/*db* plantdb]
    ;; just copy the resource straight, no html gen
    (let [sppl-template (io/file template-dir "3ook" "sppl.html")
          sppl-file (io/file plantdb-dir "3ook" "sppl.html")]
      (fs/copy sppl-template sppl-file))))


(defn genera-map-from-csv []
  (let [gens (b/parse-genera-csv genera-csv-file)]
    (util/map-by :genid #(select-keys %1 [:genus :genid :abbr]) gens)))

(defn authors-map-from-csv []
  (let [auths (b/parse-authors-csv author-csv-file)]
    (util/map-by :authid :shortauth auths)))

(defn pubs-map-from-csv []
  (let [pubs (b/parse-pubs-csv pub-csv-file)]
    (util/map-by :pubid :shortpub pubs)))

(declare check-for-missing-genera)



(defn preprocess-historical-images [plantdb thumbdir bigdir pc-code]
  (doseq [i (fs/glob (str thumbdir "/*.jpg"))]
    ;; captions are not supported in the Botr filenames, just PCODE.jpg or PCODE+ACCEPTED.jpg
    (let [^String base (fs/base-name i)
          bigi (io/file bigdir base)
          plus (.indexOf base "+")
          hyphen (.indexOf base "-")
          dot (.lastIndexOf base ".")
          endID (if (neg? hyphen) dot hyphen)
          ^String id (if (neg? plus) (subs base 0 endID) (subs base 0 plus))
          accepted (when (pos? plus) (subs base (inc plus) endID))
          pc (.lastIndexOf base "_PC_")
          credit (if (pos? pc) (subs base (+ pc 4) dot) pc-code)
          caption (when (pos? hyphen)
                    (if (pos? pc)
                      (subs base (inc hyphen) pc)
                      (subs base (inc hyphen) dot)))
          to-base (str id "-" caption "_PC_" credit ".jpg")
          accepted-base (when accepted (str accepted "-" (or caption id) "_PC_" credit ".jpg"))
          to-accepted (when accepted (io/file plantdb accepted (str "2" accepted-base)))
          to (io/file plantdb id (str "3" to-base))
          big-to (io/file plantdb id to-base)
          big-to-accepted (when accepted (io/file plantdb accepted accepted-base))]
      (when (.endsWith id ".jpg" ) (println "Warning badly named photo: " i))
      (io/make-parents to)
      (fs/copy i to)
      (when to-accepted
        (io/make-parents to-accepted)
        (fs/copy i to-accepted))
      (fs/copy bigi big-to)
      (when big-to-accepted (fs/copy bigi big-to-accepted)))))



(defn update-html-resources []
  (fs/mkdirs plantdb-dir)
  (fs/copy-dir (io/file template-dir "resources") (io/file plantdb-dir))
)

(defn info-base [basename]
  ;; basename AbelChin-Leave_and_Flowers_$$$$.jpg
  ;; returns AbelChin-Leave_and_Flowers.jpg
  (let [info-marker "_$$$$.jpg"
        base (subs basename 0 (- (count basename) (count info-marker)))]
    (str base ".jpg")))

(defn multiples-by
  "Returns the set of multiple items in coll that match according to the classifier function f."
  [f coll]
  (let [groups (group-by f coll)]
    (into {} (filter (fn [[k v]] (> (count v) 1)) groups))))

(defn multiple-info-photos [dir]
  ;; returns map of pid to basenames for duplicate pids
  (let [infos (map fs/base-name (fs/glob (str dir "/*_$$$$.jpg")))
        idf (fn [base] (first (str/split base #"-")))]
    (multiples-by idf infos)))

(defn check-for-multiple-info-photos []
  (let [mults (multiple-info-photos (io/file pict-dir "imagebig"))]
    (when (seq mults)
      (println)
      (println "Multiple info files:")
      (doseq [[pid files] mults]
        (println pid)
        (doseq [fname files]
          (print "  ")
          (println fname)))
      (println))))

;; see also bento/duplicates

;; Takes the dirs from Lisa and makes one images dir with subdirs per PLANTID
(defn preprocess-images []
  (println "pre-processing images...")
  (check-for-multiple-info-photos)
  (let [image-big (io/file pict-dir "imagebig")
	image-thumb (io/file pict-dir "imagethumb")
	;; info-big (io/file pict-dir "infobig")
	;; info-thumb (io/file pict-dir "infothumb")
	botr-big (io/file pict-dir "BotRegLarge")
	botr-thumb (io/file pict-dir "BotRegThumb")
	brep-big (io/file pict-dir "botREPLarge")
	brep-thumb (io/file pict-dir "botREPsmall")
	plrar-big (io/file pict-dir "jacqplrarLarge")
	plrar-thumb (io/file pict-dir "jacqplrarSmall")
	edwards-big (io/file pict-dir "EdwardsLarge")
	edwards-thumb (io/file pict-dir "EdwardsSmall")
        catesby-big (io/file pict-dir "catesbybig")
        catesby-thumb (io/file pict-dir "catesbythumb")
        botmag1-big (io/file pict-dir "botmag1large")
        botmag1-thumb (io/file pict-dir "botmag1small")
        botmag2-big (io/file pict-dir "botmag2large")
        botmag2-thumb (io/file pict-dir "botmag2small")
        parad-big (io/file pict-dir "paradlarge")
        parad-thumb (io/file pict-dir "paradsmall")
        pinus-big (io/file pict-dir "pinuslarge")
        pinus-thumb (io/file pict-dir "pinussmall")
        dicticon-big (io/file pict-dir "dicticonorchlarge")
        dicticon-thumb (io/file pict-dir "dicticonorchsmall")
        orchalbum-big (io/file pict-dir "OrchidAlbumLarge")
        orchalbum-thumb (io/file pict-dir "OrchidAlbumSmall")
	plantdb (io/file data-dir "plantdb")]
    (update-html-resources)
    ;; copy all images into subdirs by PLANTID
    (doseq [i (fs/glob (str image-big "/*_$$$$.jpg"))]
      (let [^String base (fs/base-name i)
	    pieces (str/split base #"-")
	    ^String id (first pieces)
	    to (io/file plantdb id (info-base base))]
	(when-not (= (count pieces) 2) (println "Warning badly named photo, check hyphens: " i))
	(when-not (.endsWith base ".jpg" ) (println "Warning badly named photo, check extension: " i))
        (when-not (zero? (mod (count id) 4)) (println "Warning bad id in photo: " i))
        (when (fs/exists? (io/file image-big (info-base base)))
          (println "Warning: two similarly named files exist for " (info-base base)))
	(io/make-parents to)
	(fs/copy i to)))
    (doseq [i (fs/glob (str image-big "/*.jpg"))]
      (let [^String base (fs/base-name i)
	    pieces (str/split base #"-")
	    ^String id (first pieces)
	    to (io/file plantdb id base)]
	(when-not (= (count pieces) 2) (println "Warning badly named photo, check hyphens: " i))
	(when-not (.endsWith base ".jpg" ) (println "Warning badly named photo, check extension: " i))
        (when-not (zero? (mod (count id) 4)) (println "Warning bad id in photo: " i))
        (when-not (.endsWith base "_$$$$.jpg")
          (io/make-parents to)
          (fs/copy i to))))
    ;; info thumbnails prepend "0"
    (doseq [i (fs/glob (str image-thumb "/*_$$$$.jpg"))]
      (let [base (fs/base-name i)
	    id (first (str/split base #"-"))
	    to (io/file plantdb id (str 0 (info-base base)))]
	(io/make-parents to)
	(fs/copy i to)))
    ;; image thumbnails prepend "1"
    (doseq [i (fs/glob (str image-thumb "/*.jpg"))]
      (let [base (fs/base-name i)
	    id (first (str/split base #"-"))
	    to (io/file plantdb id (str "1" base))]
        (when-not (.endsWith base "_$$$$.jpg")
          (io/make-parents to)
          (fs/copy i to))))

    ;; FIXME to-base needs to be unique between botreg and botrep, and now botmag1, etc

    (preprocess-historical-images plantdb botr-thumb botr-big "BotR")
    ;; FIXME -- maybe 3 and 2 still work here as long as accepted-base is different
    ;; botREP are similar to botreg
    (preprocess-historical-images plantdb brep-thumb brep-big "BRep")
    (preprocess-historical-images plantdb plrar-thumb plrar-big "plrar")
    (preprocess-historical-images plantdb edwards-thumb edwards-big "Edwards")
    ;; botmags are similar to botreg
    (preprocess-historical-images plantdb botmag1-thumb botmag1-big "Botmag1")
    (preprocess-historical-images plantdb botmag2-thumb botmag2-big "Botmag2")
    (preprocess-historical-images plantdb parad-thumb parad-big "Parad")
    (preprocess-historical-images plantdb pinus-thumb pinus-big "DP")
    (preprocess-historical-images plantdb dicticon-thumb dicticon-big "DictIcon")
    (preprocess-historical-images plantdb orchalbum-thumb orchalbum-big "OrchidAlbum")

    ;; catesby thumbs are prepended with "4"
    (doseq [i (fs/glob (str catesby-thumb "/*.jpg"))]
      ;; captions are not supported in the Catesby filenames, same as BotReg, but no + ref either
      (let [^String base (fs/base-name i)
	    bigi (io/file catesby-big base)
	    dot (.lastIndexOf base ".")
	    ^String id (subs base 0 dot)
	    to-base (str id "-_PC_BCat.jpg")
	    to (io/file plantdb id (str "4" to-base))
	    big-to (io/file plantdb id to-base)]
	(when (.endsWith id ".jpg" ) (println "Warning badly named photo: " i))
	(io/make-parents to)
	(fs/copy i to)
	(fs/copy bigi big-to) ))
    )
  (println "images ready"))


(defn process-plant-info [plant]
  (let [pid (:id plant)]
    (when (:alpha plant)
      (print (:alpha plant))
      (flush))
    (let [info-file (io/file plantdb-dir pid "index.html")]
      (io/make-parents info-file)
      (write-to-utf8-file info-file (dbspecies plant)))))

(defn generate-all-plant-info [plants-sorted]
  (let [agents-info (map (fn [p] (send (agent p) process-plant-info)) plants-sorted)]
    (when-not (apply await-for (* 1000 60 3) agents-info)
      (throw (ex-info "info generation timed out!" 
                      {:agent-errors (filter agent-error agents-info)})))))

;; FIXME all the post-processing on plants should be done in one place
;; FIXME started on the path of caching image filenames but I haven't integrated into the whole process
(defn process-plant-csv []
  ;; used to skip images if plantdb-dir was there, but that's buggy now
  ;;  (when-not (fs/writeable? plantdb-dir)
  (preprocess-images)
  (assert (fs/exists? plantdb-dir))
  (update-html-resources)
  (let [db (b/make-plantdb (concat (b/parse-plantdb-csv plants-csv-file)
                                   (b/parse-plantdb-csv orchid-csv-file)
                                   (b/parse-plantdb-csv protea-csv-file))
                         (genera-map-from-csv)
                         (authors-map-from-csv)
                         (pubs-map-from-csv))
	plants-sorted (remove catesby-only? (:sorted db))]
    (binding [b/*db* db]
      (generate-index-page-for-sorted-plants plants-sorted)
      (generate-common-page-for-db db)

      (print "creating plant info files...")
      (let [timer (tmr/timer "info-processing-time")]
        (tmr/time! timer (generate-all-plant-info plants-sorted))
        (println " done " (report-msecs (tmr/mean timer))))

      (print "processing images... ")
      (let [timer (tmr/timer "image-processing-time")]
        (tmr/time! timer (process-plant-images db))
        (println " done " (report-msecs (tmr/mean timer))))

      (print "other stuff...")
      (process-genera-csv db)
      (process-pub-csv)
      (process-authors-csv)
      (process-botreg-csv db)
      (process-botrepos-csv db)
      (process-jacqplrar-csv db)
      (process-catesby db)
      (process-catesby-index db)
      (process-sppl db)
      (process-pinus-csv db)
      (process-pinus-more db)
      (process-cultprot-csv db)
      (process-cultprot-more db)
      (println " done")
      (println "checking for missing genera...")
      (check-for-missing-genera db)
      (println " done")
      (count plants-sorted))))





;;; new stuff to find file updates

(defn extension [file]
  (when file
    (let [base (fs/base-name file)
	  dot (.lastIndexOf ^String base ".")]
      (when (pos? dot)
	(subs base (inc dot))))))


(defn jpeg? [file]
  (= (extension file) "jpg"))

;; weak, but good enough for our app
(defn jpeg-hash [file]
  (fs/size file))


;; if you pass in a chksum object, you should .reset it before re-using unless you want
;; a running total checksum
;; default Adler32
(defn checksum
  ([file] (checksum file (byte-array bsize)))
  ([file buffer] (checksum file buffer (Adler32.)))
  ([file buffer ^Checksum chksum]
     (let [file (io/as-file file)]
       (when (fs/readable? file)
         (with-open [input (FileInputStream. file)
                     cis (CheckedInputStream. input chksum)]
          ;; read whole file to calculate checksum
          (while (not (neg? (.read cis buffer))))
          (.getValue (.getChecksum cis)))))))

;; make the checksum fn on the fly to manage the buffer memory allocation
;; SEM FIXME: there's an issue with multithreading since these resources are shared
;; without protection.  Maybe it's really better to call checksum directly.  Should
;; be thread local but that's a pain to program.  (Of course, bettter than breakage!)
(defn mkchecksum []
  (let [adler (Adler32.)
        buffer (byte-array bsize)]
    (fn [file]
      (.reset adler)
      (checksum file buffer adler))))
    

;; no  :encoding "UTF-8" because I don't want to confuse jpegs
(defn md5 [file]
  (when (fs/readable? file)
    (digest/md5 (io/as-file file))))

(defn sha1 [file-or-url]
  ;; missing file should return nil
  (let [file (io/as-file file-or-url)]
    (when (fs/readable? file)
      (digest/sha-1 file))))

;; Can be very fast for jpegs, but not completely reliable to use size as hash for jpegs
;; On the other hand, for my app this works well enough
(defn my-hash [file]
  (if (jpeg? file)
    (jpeg-hash file)
    (checksum file)))

;; FIXME too loose with str versus file, unix centric with the inc
(defn relative-path-as-strings [full-path prefix-dir]
  (let [full (str full-path)
	pref (str prefix-dir)]
    (cond (= full pref) ""
	  (.startsWith full pref) (subs full (inc (count pref)))
	  :else nil)))

(defn relative-path [full-path prefix-dir]
  (loop [fulls (seq (fs/split (str full-path)))
	 prefs (seq (fs/split (str prefix-dir)))]
    (if (seq prefs)
      (when (= (first prefs) (first fulls))
	(recur (rest fulls) (rest prefs)))
      (apply util/fs-join fulls))))

(defn needs-update [hash-fn cdir base croot oroot]
  ;(println "base = " base (type base))
  (when-not (.startsWith ^String base ".")
    (let [relative-dir (relative-path cdir croot)
	  web-file (io/file oroot relative-dir base)
	  pdb-file (io/file cdir base)
	  pdb-hash (hash-fn pdb-file)
	  web-hash (hash-fn web-file)]
      ;(println "checking for update: " pdb-file)
      (when (not= pdb-hash web-hash)
	(if (str/blank? relative-dir)
	  base
	  (util/fs-join relative-dir base))))))

(defn missing-dir [cdir subdir croot oroot]
  ;(println "subdir = " subdir (type subdir))
  (let [relative-dir (relative-path cdir croot)
	web-dir (io/file oroot relative-dir subdir)
	pdb-dir (io/file cdir subdir)]
    ;(println "checking for dir update: " pdb-dir)
    (when-not (fs/readable? web-dir)
      (if (str/blank? relative-dir)
        subdir
        (util/fs-join relative-dir subdir)))))

;;; NEEDS TESTING and gen of upd-dir

;; regular fs/walk always returns nil, func is called for side-effect

;; my version returns the seq, which uses some memory
(defn my-mapwalk-collecting-results [path func]
  "Walk over directory structure. Calls 'func' with [root dirs files], which should return a seq.  Results are collected with mapcat."
  (io! "Should not walk directories in a transaction.")
  (doall (mapcat #(apply func %) (fs/iterate-dir path))))


(defn push-web-update [ftp-url current-plant-dir previous-plant-dir]
  (let [pdb current-plant-dir
	web previous-plant-dir
        hash-fn my-hash]
    (ftp/with-ftp [client ftp-url]
      (dorun (fs/walk (fn [root dirs files]
                        (doseq [d dirs]
                          (when-let [up-dir (missing-dir root d pdb web)]
                            (ftp/client-mkdirs client up-dir)
                            (println "ftp mkdirs " up-dir)))
                        (doseq [f files]
                          (when-let [up-file (needs-update hash-fn root f pdb web)]
                            (let [src-file (io/file current-plant-dir up-file)]
                              (ftp/client-set-file-type client (ftp/guess-file-type src-file))
                              (ftp/client-put client src-file up-file)
                              (println "ftp put "  (str src-file) "\n   to " up-file)
                              ))))
                      pdb)))))

(defn rename1 [path]
  (when (fs/exists? path)
    (let [path1 (io/file (fs/parent path) (str (fs/base-name path) "1"))]
      (when (fs/exists? path1)
        (loop [n 2]
          (let [pathn (io/file (fs/parent path) (str (fs/base-name path) "-" n))]
            (if (fs/exists? pathn)
              (recur (inc n))
              (fs/rename path1 pathn)))))
      (fs/rename path path1))))

(defn run-web-update []
  (when-not (fs/readable? plantdb-dir)
    (throw (IllegalStateException. (str "Missing plantdb-dir: " plantdb-dir))))
  (when-not (fs/readable? web-plantdb-dir)
    (throw (IllegalStateException. (str "Missing web-plantdb-dir: " web-plantdb-dir))))
  (println (str (java.util.Date.)))
  (push-web-update plantdb-url plantdb-dir web-plantdb-dir)
  (rename1 web-plantdb-dir)
  (fs/rename plantdb-dir web-plantdb-dir)
  (println (str (java.util.Date.))))

(defn dry-run-push-web-update [ftp-url current-plant-dir previous-plant-dir]
  ;(println ftp-url current-plant-dir previous-plant-dir)
  (let [pdb current-plant-dir
	web previous-plant-dir
        hash-fn my-hash]
    (dorun (fs/walk (fn [root dirs files]
                      ;(println "looking at: " root)
                      (doseq [d dirs]
                        (when-let [up-dir (missing-dir root d pdb web)]
                          (println "dry-run would: ftp mkdirs " up-dir)))
                      (doseq [f files]
                        (when-let [up-file (needs-update hash-fn root f pdb web)]
                          (let [src-file (io/file current-plant-dir up-file)
                                guess (ftp/guess-file-type src-file)]
                            (println "dry-run would: ftp put "  (str src-file) "\n   to " up-file
                                     " as " guess)
                            ))))
                    pdb))))

(defn dry-run-web-update []
  (when-not (fs/readable? plantdb-dir)
    (throw (IllegalStateException. (str "Missing plantdb-dir: " plantdb-dir))))
  (when-not (fs/readable? web-plantdb-dir)
    (throw (IllegalStateException. (str "Missing web-plantdb-dir: " web-plantdb-dir))))
  (dry-run-push-web-update plantdb-url plantdb-dir web-plantdb-dir)
  (println "done"))


;; SEM -- `ack GENERA` finds bad links.  This finds db inconsistency.
;; Run this on command for Lisa. 
;; Probably should filter out "Zzzz" since that was specially used for Catesby stuff, not really a genus.
(defn check-for-missing-genera [plantdb]
  (let [genset (into #{} (map :genid (b/parse-genera-csv genera-csv-file)))
        genmap (util/mapmap :genus (:genmap plantdb))
        missing-map (sort-by second (remove #(contains? genset (first %)) genmap))]
    (when (not-empty missing-map)
      (println "** Missing Genera **")
      (doseq [[gid genus] missing-map] (print gid) (println "," genus))
      (println))))

(defn check-for-parents [plantdb]
  (let [idmap (:idmap plantdb)
        parent-ids (set (map :parent-id (remove :incomplete (vals idmap))))
        missing (remove (fn [parid] (or (nil? parid) (< (count parid) 8) (get idmap parid))) 
                             parent-ids)]
    (when (seq missing)
      (println "** Missing Parents **")
      (doseq [pid (sort missing)]
        (println pid)))))

(defn check-for-grex [plantdb]
  (let [plants (:sorted plantdb)
        missing (filter (fn [p] (and (:grex p) (not (:cultivar p)) (not (b/synonym? p))
                                     (not (and (:pollen-pid p) (:seed-pid p)))))
                        plants)]
    (when (seq missing)
      (println "** Missing Grex Parents **")
      (doseq [p missing]
        (println ((juxt :id :seed-pid :pollen-pid) p))))))


;; One-time deal to clean up images.html files with no useful images.  We no longer
;; generate them so it won't be a problem going forward.
(defn delete-empty-images-on-web [ftp-url plants]
  (ftp/with-ftp [client ftp-url]
    (doseq [p (filter #(zero? (count (:images %))) plants)]
      (let [img-file (str (io/file (:id p) "images.html"))]
        (println "ftp/client-delete" img-file)
        (ftp/client-delete client img-file)))))
