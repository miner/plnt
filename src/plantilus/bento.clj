(ns plantilus.bento
  (:refer-clojure)
  (:use plantilus.resources)
  (:require [clojure.data.csv :as csv]
            ;; [clojure.core.reducers :as r]
            [me.raynes.fs :as fs]
            [clojure.data :as d]
            [plantilus.util :as util]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;;; Converting plant images to web pages using enlive

(defonce ^:dynamic *db* {})

;; for error reporting while parsing
(def ^:dynamic *parsing* nil)

(defn nth0 [v] (nth v 0))

(defn nth1 [v] (nth v 1))

(def ^:const bento-plants-descriptor
  [["Date Created" :date-created]
   ["Date Modified" :date-modified]
   ["PlantID" :id]
   ["X" :x]
   ["Genus" :genus]
   ["Species" :species]
   ["var." :var]
   ["hybrid" :hybrid]
   ["subsp." :subsp]
   ["Special" :special]
   ["Cultivar" :cultivar]
   ["Cultivar2" :cultivar2]
   ["Patents" :patents]
   ["Attribution" :attribution]
   ["Common Name" :common]
   ["Common Index 1" :comind1]
   ["Common Index 2" :comind2]
   ["Common Index 3" :comind3]
   ["Family" :family]
   ["Category" :category]
   ["Zones" :zones]
   ["Size" :size]
   ["Growth Rate" :growth]
   ["Status" :status]
   ["Description" :description]
   ["Cultivars" :cultivars]
   ["History" :history]
   ["My Experience" :experience]
   ["Supplier" :supplier-1]
   ["ExpLoc" :exploc]
   ["Purchase Date 1" :purchase-date-1]
   ["Purchase Size 1" :purchase-size-1]
   ["Supplier 2" :supplier-2]
   ["Purchase Date 2" :purchase-date-2]
   ["Purchase Size 2" :purchase-size-2]
   ["Location" :location]
   ["Text Ready" :text-ready]
   #_ ["Photo DB" :photo-db]
   #_ ["Photo Main" :photo-main]
   ["Photo List" :photo-list]
   ["PhotoName" :photoname]
   ["PhotoCredit" :photocredit]
   ["Accepted Status" :accstatus]
   ["Accepted" :accepted]
   #_ ["Record Type" :record-type]
   ["BR Ref" :brref]
   ["BR Common" :brcommon]
   ["CatOther" :catother]
   ["CatRef" :catref]
   ["CatDesc" :catdesc]
   ["CatLoc" :catloc]
   ["CatName" :catname]
   ["Sun" :sun]
   ["Filtered Sun" :filtered-sun]
   ["Part Shade" :part-shade]
   ["Shade" :shade]
   ["Protected" :protected]
   ["Winter In" :winter-in]
   ["Flw White" :flw-white]
   ["Flw Yellow" :flw-yellow]
   ["Flw Orange" :flw-orange]
   ["Flw Red" :flw-red]
   ["Flw Pink" :flw-pink]
   ["Flw Purple" :flw-purple]
   ["Flw Blue" :flw-blue]
   ["Flw Green" :flw-green]
   ["Flw Brown" :flw-brown]
   ["Flw Beige" :flw-beige]
   ["Autumn Color" :autumn-color]
   ["Berry" :berry]
   ["Columnar" :columnar]
   ["Flower" :flower]
   ["Foliage" :foliage]
   ["Fragrance" :fragrance]
   ["Screen" :screen]
   ["Bloom Spring" :bloom-spring]
   ["Bloom Summer" :bloom-summer]
   ["Bloom Autumn" :bloom-autumn]
   ["Bloom Winter" :bloom-winter]
   ["Variegated" :variegated]
   ["Dirr" :dirr]
   ["Mobot" :mobot]
   ["Hillier" :hiller]
   ["PDN" :pdn]
   ["Data" :data]
   ["Arm" :arm]
   ["Photos needed" :photos-needed]
   ["Nickname" :nickname]
   ["Location 1" :location1]
   ["Location 2" :location2]
   ["Bot. Rep. Ref." :bot-rep-ref]
   ["Bot. Rep. Common" :bot-rep-common]
   ["Bot. Rep. Info" :bot-rep-info]
   ["Bot. Mag. Ref." :bot-mag-ref]
   ["Bot. Mag. Common" :bot-mag-common]
   ["Bot. Mag. Info." :bot-mag-info]
   ["Incomplete" :incomplete]
   ["Pseudofamily" :pseudo-family]
   ["Butt" :butt]
   ["Silver Leaves" :silver-leaves]
   ["PLRAR Ref" :plrar-ref]
   ["Grex" :grex]
   ["Seed parent" :seed-pid]
   ["Pollen parent" :pollen-pid]
   ["Hybridizing characteristics" :hybridizing]
   ["Location verified" :location-verified]
   ])

(def ^:const bento-plants-header-names (mapv nth0 bento-plants-descriptor))

(def ^:const plant-keys (mapv nth1 bento-plants-descriptor))




(def ^:const bento-authors-descriptor
  [["Date Created" :date-created]
   ["Date Modified" :date-modified]
   ["AuthorID" :authid]
   ["Short" :shortauth]
   ["Long" :longauth]
   ["Alternatives" :altauth]
   ["Country" :country]
   ["Other" :other]])

(def ^:const bento-authors-header-names (mapv nth0 bento-authors-descriptor))

(def ^:const author-keys (mapv nth1 bento-authors-descriptor))


(def ^:const bento-genera-descriptor
  [["Date Created" :date-created]
   ["Date Modified" :date-modified]
   ["GenusID" :genid]
   ["X" :x]
   ["Name" :genus]
   ["Author" :author]
   ["Common Name" :common]
   ["Published" :pub]
   ["Other" :other]
   ["Family" :family]
   ["Status" :gstatus]
   ["Group" :group]
   ["Stearn" :stearn]
   ["Abbr" :abbr]])

(def ^:const bento-genera-header-names (mapv nth0 bento-genera-descriptor))

(def ^:const genera-keys (mapv nth1 bento-genera-descriptor))


(def ^:const bento-pubs-descriptor
  [["Date Created" :date-created]
   ["Date Modified" :date-modified]
   ["PubID" :pubid]
   ["Short" :shortpub]
   ["Author" :author]
   ["Dates" :date]
   ["Index" :sortby]
   ["Long" :longpub]
   ["Other" :other]])


(def ^:const bento-pubs-header-names (mapv nth0 bento-pubs-descriptor))

(def ^:const pub-keys (mapv nth1 bento-pubs-descriptor))


(def ^:const bento-pinus-descriptor 
  [["PlantID" :id] 
   ["Pinus Ref" :pinusref] 
   ["Pinus Common" :common]])

(def ^:const bento-pinus-header-names (mapv nth0 bento-pinus-descriptor))

(def ^:const pinus-keys (mapv nth1 bento-pinus-descriptor))

(def ^:const bento-botreg-descriptor
  [["PlantID" :id] 
   ["BR Ref" :brref] 
   ["BR Common" :common]])

(def ^:const bento-botreg-header-names (mapv nth0 bento-botreg-descriptor))

(def ^:const botreg-keys (mapv nth1 bento-botreg-descriptor))


(def ^:const bento-botrepos-descriptor
  [["PlantID" :id] 
   ["Bot. Rep. Ref." :brref] 
   ["Bot. Rep. Common" :common]])

(def ^:const bento-botrepos-header-names (mapv nth0 bento-botrepos-descriptor))

(def ^:const botrepos-keys (mapv nth1 bento-botrepos-descriptor))


(def ^:const bento-jacqplar-descriptor
  [["PlantID" :id] 
   ["PLRAR Ref" :plrarref]])

(def ^:const bento-jacqplrar-header-names (mapv nth0 bento-jacqplar-descriptor))

(def ^:const jacqplrar-keys (mapv nth1 bento-jacqplar-descriptor))


(def ^:const bento-cultprot-descriptor 
  [["PlantID" :id] 
   ["Cult. Prot. Ref." :cultprotref] 
   ["Cult. Common" :common]])

(def ^:const bento-cultprot-header-names (mapv nth0 bento-cultprot-descriptor))

(def ^:const cultprot-keys (mapv nth1 bento-cultprot-descriptor))


(def ^:const catesby-keys [:catref
                           :catdesc
                           :catloc
                           :catdist
                           :catother
                           ])


;; all booleans
(def exposure-keys [:sun :filtered-sun :part-shade :shade :protected :winter-in])

(def flower-color-keys [:flw-white :flw-yellow :flw-orange :flw-red :flw-pink
                          :flw-purple :flw-blue :flw-green :flw-brown :flw-beige])

(def season-keys [:bloom-spring :bloom-summer :bloom-autumn :bloom-winter])

(def source-keys  [:dirr :mobot :hiller :pdn :data :arm])

(def boolean-keys (set (conj (concat exposure-keys flower-color-keys season-keys source-keys) :variegated :incomplete)))

(def ignored-keys #{
                    :date-created
                    :date-modified
                    :supplier-1
                    :exploc
                    :purchase-date-1
                    :purchase-size-1
                    :supplier-2
                    :purchase-date-2
                    :purchase-size-2
                    :location
                    :text-ready
                    :photo-db
                    :photo-main
                    :photo-list
                    :photoname
                    :photocredit
                    :record-type
                    :pinus-ref
                    :pinus-common
                    :pinus-quotes
                    :autumn-color
                    :berry 
                    :columnar 
                    :flower 
                    :foliage
                    :fragrance
                    :screen
                    :stearn
                    :status
                    :group
                    :photos-needed
                    :nickname
                    :location1
                    :location2
                    :bot-rep-ref
                    :bot-rep-common
                    :bot-rep-info
                    :bot-mag-ref
                    :bot-mag-common
                    :bot-mag-info
                    :cult-prot-ref
                    :pseudo-family
                    :butt
                    :silver-leaves
                    :plrar-ref
                   })





;; SEM not actually used
(def required-plant-keys (remove ignored-keys plant-keys))


;; for now it's convenient if all the keys are unique across all records, at least their parse rules should be based on the kword

;; new field :accepted is the accepted ID for the current name
;; (applies when :accstatus = "Synonym") but :accepted is not
;; guaranteed to be there, also applies when there is a :brref

;; :accstatus can be "NA" "Accepted" "Unresolved" or "Synonym" or "Catesby Only" and "Fake" (new)
;; Fake means the id is there just to avoid an error for a missing parent.  Never publish anything
;; that is "Fake".
;; :comind1 is short for "common name, indexed", for building an index of common names back to scientific

;; "Catesby Only" means the plant entry exists only for the Catesby stuff.  The "real" ID is in :accepted
;; (like "Synonym"), but this plant should not get a plant page at all.  The picture should be in the Catesby
;; pages, and the link should go to the :accepted name instead of the source ID.


;; each item should have a single char or nil for its alphakey (typically :alpha)
(defn alpha-set [items alphakey]
  (reduce (fn [aset item] (if-let [alpha (get item alphakey)] (conj aset alpha) aset)) #{} items))

(comment
;; SEM alpha-set is faster than reducers attempted below
(defn ralpha-set [items alphakey]
  (into #{} (r/remove nil? (r/map (fn [item] (get item alphakey)) items))))

(defn r2alpha-set [items alphakey]
  (disj (into #{} (r/map (fn [item] (get item alphakey)) items)) nil))

)


(defn first-word [s]
  (let [sp (.indexOf ^String s " ")]
    (if (neg? sp) s (subs s 0 sp))))
        
;; use h/html-snippet to get an html thingy like &copy; inserted as a value

(defn last-char [^String s]
  (.charAt s (dec (.length s))))
  
(defn cultivar-string [^String s]
  (cond (str/blank? s) nil
	;; really only care about (TM) and (R) chars, but this works for anything special (beyond A-Z, a-z)
	(> (int (last-char s)) (int \z)) s
	:else (str \' s \')))

(def author-info {})

(defn author-abbrev [author-id]
  (get author-info author-id author-id))

(defn qstr [& strs]
  (str \" (apply str strs) \"))


;; SEM FIXME all references to *db* could have variant that takes a plantdb, but maybe not worth it
(defn plant-for-id
  ([id] (plant-for-id *db* id))
  ([db id] (get-in db [:idmap id])))

(defn accepted-plant-for-id
  ([id] (accepted-plant-for-id *db* id))
  ([db id]
     (let [plant (get-in db [:idmap id])
        acc-id (:accepted plant)]
       (if acc-id
         (get-in db [:idmap acc-id])
         plant))))


(defn synonym? [plant]
  (= (:accstatus plant) "Synonym"))

(defn synonym-id? [pid]
  (synonym? (plant-for-id pid)))


(defn genera-for-id 
  ([id] (genera-for-id *db* id))
  ([db id] (get-in db [:genmap id])))


(defn latin-name [plant]
  (str/join " " (remove nil? (list (:x plant)
                                   (:genus plant)
                                   (:special plant)
                                   (:grex plant)
                                   (:species plant)
                                   (if-let [h (:hybrid plant)] (if (= h "x") "x" (str "x " h)))
                                   (if-let [v (:var plant)] (if (neg? (.indexOf ^String v ".")) 
                                                              (str "var. " v) v))
                                   (if-let [v (:subsp plant)] (str "subsp. " v))
                                   (if-let [v (:cultivar plant)] (cultivar-string v))
                                   (if-let [v (:cultivar2 plant)] (str "(" (cultivar-string v) ")"))
                                   ))))

(defn genus-id [plant]
  (subs (:id plant) 0 4))

(defn genus-abbreviation [plant]
  (when-let [gen (genera-for-id (genus-id plant))]
    (or (:abbr gen) (:genus gen))))


(defn orchid-name [plant]
  ;; like latin-name but tweaked for orchid display
  (if-not plant
    "unknown"
    (str/join " " (remove nil? (list (:x plant)
                                     (genus-abbreviation plant)
                                     (:special plant)
                                     (:grex plant)
                                     (:species plant)
                                     (if-let [h (:hybrid plant)] (if (= h "x") "x" (str "x " h)))
                                     (if-let [v (:var plant)] (if (neg? (.indexOf ^String v ".")) 
                                                                (str "var. " v) v))
                                     (if-let [v (:subsp plant)] (str "subsp. " v))
                                     (if-let [v (:cultivar plant)] (cultivar-string v))
                                     (if-let [v (:cultivar2 plant)] (str "(" (cultivar-string v) ")"))
                                   )))))
   

(defn ptrademark [s key]
  (when-not (str/blank? s)
    (-> s
	(str/replace " (TM)" (str \u2122))
	(str/replace " (R)" (str \u00AE)))))

(defn pfix-quote
  "Replace MacRoman-encoded smart-quotes with dumb ASCII quotes; also trims space"
  [s key]
  (str/escape (str/trim s) {\u2019 \' \u2018 \' \u201c \" \u201d \"}))

(defn pignore [s key]
  nil)

(defn prequired [s key]
  (if (str/blank? s)
    (throw (ex-info (str "Value required; empty string not allowed for " key) 
                    {:key key :parsing *parsing*}))
    ;;  (println (str "WARNING: Value required; empty string not allowed for " key))
    (pfix-quote s key)))

(defn pid4 [s key]
  (let [val   (prequired s key)]
    (when-not (zero? (mod (count val) 4))
      (throw (ex-info (str "Plant-ID length should be multiple of four for key: " key 
                           ", val= " s)
                      {:key key :val s :parsing *parsing*})))
    val))

(defn pid4opt [s key]
  (when-not (str/blank? s)
    (when-not (zero? (mod (count s) 4))
      (throw (ex-info (str "Plant-ID length should be multiple of four for key: " key
                          ", val= " s)
                      {:key key :val s :parsing *parsing*})))
    s))

(defn pbool [s key]
  (boolean (and s (case (str/lower-case s) ("" "0" "false" "nil" "null") false true))))


(defn pformatted
  "Specially formatted text converts for HTML encoding"
  [s key]
  (when-not (str/blank? s)
    (-> s (ptrademark key) (pfix-quote key))))


;; FileMaker embedded ^K instead of CR
(defn pmultiformatted-old
  "Specially formatted text converts to multiple paragraphs strings for HTML encoding"
  [s key]
  (when-not (str/blank? s)
    (let [vvv (vec (remove empty? (map #(pformatted %1 key) (str/split s #"\u000B"))))]
      (not-empty vvv))))

;; Bento embeds CR rather than ^K
(defn pmultiformatted
  "Specially formatted text converts to multiple paragraphs strings for HTML encoding"
  [s key]
  (when-not (str/blank? s)
    (let [vvv (vec (remove empty? (map #(pformatted %1 key) (str/split s #"\n"))))]
      (not-empty vvv))))


;; Not used for Bento
;; FM uses ^K \u000B instead of returns in fields
(defn pmulti [s key]
  (when-not (str/blank? s)
    (str/split s #"\u000B")))

(defn pvec
  "Parse string into a vector of values"
  [s key]
  (vec (pmulti s key)))

(def plant-key-to-fn {:id pid4
		      :genus prequired
		      :description pmultiformatted
		      :exposure pmulti
		      :cultivars pmultiformatted
		      :history pmultiformatted
		      :experience pmultiformatted
		      :hybridizing pmultiformatted
		      ;; keys for other records
		      :sortby prequired
		      :common ptrademark
		      :comind1 ptrademark
		      :comind2 ptrademark
		      :comind3 ptrademark
		      :brhist pmultiformatted
		      :brdesc pmultiformatted
		      :brother pmultiformatted
                      :catdesc pmultiformatted
                      :catother pmultiformatted
                      :catloc pmultiformatted
                      :catname pmultiformatted
                      :seed-pid pid4opt
                      :pollen-pid pid4opt
                      :accepted pid4opt
		      })


                          
;; default gets pformatted


(defn key-fn [key]
  (cond (contains? boolean-keys key) pbool
        (contains? ignored-keys key) pignore
        :else (or (get plant-key-to-fn key) pformatted)))

(defn parsed-item [raw-val key]
  ((key-fn key) raw-val key))

(defn parsed-fields
  "pvec vector of plant field values; keys vector of keys"
  [pvec keys]
  {:pre [(== (count pvec) (count keys))]}
  (binding [*parsing* pvec]
    (into {} (map (fn [k v] (when-let [pv (parsed-item v k)]
                              (vector k pv)))
                  keys pvec))))



(def exposure-names
  (reduce (fn [mp k]
            (let [name-words (str/split (name k) #"\-")]
              (assoc mp k (str/join " " (map str/capitalize name-words)))))
          {}
          exposure-keys))
  
;; could be cached
(defn exposure [plant]
  ;; rebuild old-style seq of strings from new-style booleans
  (map exposure-names (filter (fn [k] (k plant)) exposure-keys)))


(defn hybrid-except-x [plant]
  (if-let [hy (:hybrid plant)]
    (when-not (= hy "x") hy)))

;; SEM FIXME: what about grex?
(defn sort-name [plant]
  (str/lower-case (str/join " " (remove str/blank? (map #(%1 plant) 
                                                        [:genus :grex :species :var :subsp hybrid-except-x :cultivar])))))



(defn plant-record [pvec keys]
  (let [plant (parsed-fields pvec keys)
	^String id (:id plant)
        plant (into plant {:parent-id (subs id 0 (- (.length id) 4))
                           :sort-name (sort-name plant) })]
    (when (and (:grex plant) (not (:cultivar plant)) (not (synonym? plant)))
      (when-not (and (:seed-pid plant) (:pollen-pid plant))
        #_ (throw (IllegalStateException. 
                (str "Plant " id " looks like an orchid with bad parents")))
        (println (str "BAD Plant " id " looks like an orchid with bad parents"))
        ))
    plant))

;; basic match up vector of values with keys, no calculated values
(defn simple-record [pvec keys]
  {:pre [(== (count pvec) (count keys))]}
  (let [rec (parsed-fields pvec keys)]
    rec))

;; in theory, we might have some calculated fields
(defn pub-record [pvec keys]
  (simple-record pvec keys))

(defn author-record [pvec keys]
  (simple-record pvec keys))

(defn genera-record [pvec keys]
  (simple-record pvec keys))

(defn pinus-record [pvec keys]
  (simple-record pvec keys))

(defn botreg-record [pvec keys]
  (simple-record pvec keys))

(defn botrepos-record [pvec keys]
  (simple-record pvec keys))

(defn jacqplrar-record [pvec keys]
  (simple-record pvec keys))

(defn cultprot-record [pvec keys]
  (simple-record pvec keys))


(defn sort-ids
  "Sorts IDs by the corresponding plant's :sort-order (based on lexical :sort-name)"
  [pids plant-map]
  (sort-by #(get-in plant-map [% :sort-order]) pids))
  
(defn sorted-plant-id-vectors [plants]
  ;; map of [ID N]
  (into {} (map-indexed #(vector (second %2) %1) (sort-by first (map #(vector (:sort-name %) (:id %)) plants)))))

(defn extras-indexed [extras]
  (apply concat (keep-indexed (fn [i val] (when val [i val])) extras)))

(defn check-headers [headers expected filename]
  ;; debug (doseq [h headers] (println h))
  (when-not (= headers expected)
    (let [[extras missing _] (d/diff headers expected)]
      (print "Header diff for" filename "\n  missing = ")
      (prn (extras-indexed missing))
      (print "  extras = ")
      (prn (extras-indexed extras))
      (throw (ex-info (str "Bento header field names don't match for " filename ".")
                      {:extras extras
                       :missing missing})))))



;; encoding was defaulting to MacRoman, now always UTF-8

(defn parse-csv-file
  [fname expected-headers record-fn record-keys]
  (with-open [rdr (io/reader fname :encoding "UTF-8")]
    (let [lines (csv/read-csv rdr)
          header (first lines)]
      (check-headers header expected-headers fname)
      (doall (map #(record-fn %1 record-keys) (rest lines))))))

;; allows a transform function to hack the plant record
(defn parse-plantdb-csv
  ([fname] (parse-plantdb-csv fname identity))

  ([fname transform]
     (assert (= (count bento-plants-header-names) (count plant-keys)))
     (with-open [rdr (io/reader fname :encoding "UTF-8")]
       (let [lines (csv/read-csv rdr)
             header (first lines)]
       (check-headers header bento-plants-header-names fname)
       (doall (map #(transform (plant-record %1 plant-keys)) (rest lines)))))))



(defn parse-pubs-csv
  [fname]
  (parse-csv-file fname bento-pubs-header-names pub-record pub-keys))

(defn parse-authors-csv
  [fname]
    (parse-csv-file fname bento-authors-header-names author-record author-keys))

(defn parse-genera-csv
  [fname]
    (parse-csv-file fname bento-genera-header-names genera-record genera-keys))

(defn parse-pinus-csv
  [fname]
  (parse-csv-file fname bento-pinus-header-names pinus-record pinus-keys))

(defn parse-botreg-csv
  [fname]
  (parse-csv-file fname bento-botreg-header-names botreg-record botreg-keys))

(defn parse-botrepos-csv
  [fname]
  (parse-csv-file fname bento-botrepos-header-names botrepos-record botrepos-keys))

(defn parse-jacqplrar-csv
  [fname]
  (parse-csv-file fname bento-jacqplrar-header-names jacqplrar-record jacqplrar-keys))

(defn parse-cultprot-csv
  [fname]
  (parse-csv-file fname bento-cultprot-header-names cultprot-record cultprot-keys))


(defn pid-for-genus [plant]
  (when plant
    (subs (:id plant) 0 4)))

;; nil if it's already a simple species
(defn pid-for-species [plant]
  (when plant
    (let [^String pid (:id plant)]
      (when (> (.length pid) 8)
	(subs pid 0 8)))))


(defn cult-parent-id [^String pid]
  (when (> (.length pid) 8)
    (subs pid 0 (- (.length pid) 4))))

(defn gen-cults [pids]
  (reduce (fn [cmap pid]
	    (if-let [parent (cult-parent-id pid)]
	      (let [cs (get cmap parent [])]
		(assoc cmap parent (conj cs pid)))
	      cmap))
	  {}
	  pids))

;; allows parent to be a genus unlike cultivar calculation
(defn simple-parent-id [^String pid]
  (when (> (.length pid) 4)
    (subs pid 0 (- (.length pid) 4))))

(defn gen-children-ids [ids]
  (reduce (fn [cmap id]
	    (if-let [parid (simple-parent-id id)]
	      (let [subs (get cmap parid [])]
		(assoc cmap parid (conj subs id)))
	      cmap))
	  {}
	  ids))

(defn parent 
  ([plant] (parent *db* plant))
  ([db plant]
     (if-let [parent-id (cult-parent-id (:id plant))]
       (get-in db [:idmap parent-id]))))


(defn id-of-cultivar? 
  ([id] (id-of-cultivar? *db* id))
  ([db id]
     (get-in db [:idmap id :cultivar])))

;; other-cultivars is strict about actually being a cultivar (no varieties!)
;; and this plant has to be a cultivar itself to have "others"
(defn other-cultivar-ids 
  ([plant] (other-cultivar-ids *db* plant))
  ([db plant]
     (when (:cultivar plant)
       (let [pid (:id plant)]
         (if-let [parentid (cult-parent-id pid)]
           (if-let [sibling-ids (get-in db [:childmap parentid])]
             (filter #(and (not= pid %1) (id-of-cultivar? %1)) sibling-ids)))))))

(defn other-cultivar-ids-having-images [plant]
  (filter :images (other-cultivar-ids plant)))

;; Based only on the ID patterns, also includes varieties that aren't strictly cultivars.
(defn children-ids-for-parent-id 
  ([id] (children-ids-for-parent-id *db* id))
  ([db id] (get-in db [:childmap id])))

(defn children-ids [plant]
  (children-ids-for-parent-id (:id plant)))

(defn cultivar-ids [plant]
  (filter id-of-cultivar? (children-ids plant)))

(defn subtaxa-ids [plant]
  (remove id-of-cultivar? (children-ids plant)))

(defn relative-ids [plant]
  (when (and (not (synonym? plant)) (or (not (:cultivar plant)) (not (parent plant))))
    (remove synonym-id? (remove #{(:id plant)} (children-ids-for-parent-id (:parent-id plant)) ))))

(defn calc-catesby-code [plant]
  (when-let [^String cref (:catref plant)]
    (let [a12 (cond (.startsWith cref "Volume I ") "1"
                    (.startsWith cref "Volume II ") "2"
                    (.startsWith cref "Appendix ") "A"
                    :else "Z")
          plate-split (str/split cref #" plate ")
          plate-tail (str/split (second plate-split) #"\W")
          platenum (first plate-tail)
          plate (if (= (count platenum) 1) (str "0" platenum) platenum)
          junk (apply str (rest plate-tail))]
      (str a12 plate " " junk))))
      

(defn magic-double-plant-id? [^String pid]
  ;; AbcdAbcd (double first four chars) is a special notation for a hybrid without species
  (and (= (.length pid) 8) (= (subs pid 0 4) (subs pid 4 8))))

(defn genera-list-included-plant-id [plantdb pid]
  (let [plant (get-in plantdb [:idmap pid])]
    (when (not= "NA" (:accstatus plant))
      pid)))

(defn filtered-species [plantdb genid]
  (let [children (get-in plantdb [:childmap genid])]
    (filter (fn [pid] (genera-list-included-plant-id plantdb pid)) children)))

(defn gen-subtaxa-ids [gen plantdb]
  (filtered-species plantdb (:genid gen)))

(defn catesby-only? [plant]
  (= (:accstatus plant) "Catesby Only"))

(defn catesby-id [plant]
  (if (catesby-only? plant) 
    (:accepted plant)
    (:id plant)))

(defn find-info-thumb-file [id]
  (first (fs/glob (util/fs-join plantdb-dir id "[0234]*.jpg"))))

(defn find-image-thumb-files [id]
  (fs/glob (util/fs-join plantdb-dir id "[01234]*.jpg")))

(defn find-image-files [id]
  (fs/glob (util/fs-join plantdb-dir id (str id "-*.jpg"))))


;; from contrib seq.clj
(defn find-first
  "Returns the first item of coll for which (pred item) returns logical true.
  Consumes sequences up to the first match, will consume the entire sequence
  and return nil if no match is found."
  [pred coll]
  (first (filter pred coll)))


(defn find-images [plant]
  (let [image-files (find-image-thumb-files (:id plant))]
    (if-let [image-files (seq image-files)]
      (let [bases (map fs/base-name image-files)
	    p1 (assoc plant :images (vec bases))
	    brimg (when (:brref plant) (find-first #(= \3 (first %)) bases))
	    p2 (if brimg (assoc p1 :brimage brimg) p1)
            catimg (when (:catref plant) (find-first #(= \4 (first %)) bases))
	    p3 (if catimg (assoc p2 :catimage catimg) p2)]
        p3)
      plant)))



(defn assoc-images [plants]
  (doall (map find-images plants)))


(defn duplicates
  "Returns the set of duplicates in coll."
  [coll]
  (loop [seen #{} dups #{} coll coll]
    (if-let [cs (seq coll)]
      (let [item (first cs)]
        (if (contains? seen item)
          (recur seen (conj dups item) (rest cs))
          (recur (conj seen item) dups (rest cs))))
      (not-empty dups))))


(defn check-for-duplicates [f loaded-plants]
  (duplicates (map f loaded-plants)))

(let [capA (int \A) capZ (int \Z)]
  (defn ascii-key [word]
    (let [ch (first (str/upper-case (or (first word) \space)))]
      (if (<= capA (int ch) capZ)
        ch
        \space))))
    

;; sortedlist must be presorted by sortkey, sortkey should return a string,
;; each record/map gets new alphakey (typically :alpha) if it starts that letter (first "A", etc.)

(defn update-filtered-alpha [sortedlist sortkey filter-fn alphakey]
  (loop [alphanum (int \space) items sortedlist result []]
    ;; loop is exited by end of items (first items is nil)
    (if-let [item (first items)]
      (let [ch (if (filter-fn item) (ascii-key (sortkey item)) \space)
	    chnum (int ch)]
	(if (> chnum alphanum)
	  (recur chnum (rest items) (conj result (assoc item alphakey ch)))
	  (recur alphanum (rest items) (conj result item))))
      result)))

(defn update-alpha [sortedlist sortkey alphakey]
  ;; use identity as the filter-fn
  (update-filtered-alpha sortedlist sortkey identity alphakey))


(defn fake? [plant]
  (= (:accstatus plant) "Fake"))

(defn ignored-for-sorting? [plant]
  (or (:incomplete plant)
      (catesby-only? plant)
      (fake? plant)))

(defn ignored-as-incomplete? [plant]
  (or (:incomplete plant)
      (fake? plant)))

;; :incomplete plants should be filtered out of :sorted, but left in for :idmap

(defn gen-id [plant]
  (subs (:id plant) 0 4))

(defn extract-genera-info [plant]
  {:genid (gen-id plant)
   :genus (:genus plant)})

(defn make-plantdb [loaded-plants genera-map auth-map pub-map]
  (let [plants1 (assoc-images loaded-plants)
        plants-sorted1 (update-filtered-alpha (sort-by :sort-name plants1) :sort-name
                                              (complement ignored-for-sorting?)
                                              :alpha)
	plants-sorted2 (update-filtered-alpha plants-sorted1 :sort-name :brref :bralpha)
	plants-sorted-with-incompletes (update-filtered-alpha plants-sorted2 :sort-name :catref :catalpha)
        plants-sorted (remove ignored-as-incomplete? plants-sorted-with-incompletes)
        incompletes (filter ignored-as-incomplete? plants-sorted-with-incompletes)
	genera-by-id (merge-with merge
                      (reduce #(assoc %1 (gen-id %2) (extract-genera-info %2)) {} plants-sorted)
                      genera-map)
	plants-by-id (reduce #(assoc %1 (:id %2) %2) {} plants-sorted)
        incompletes-by-id (reduce #(assoc %1 (:id %2) %2) {} incompletes)
	child-map (gen-children-ids (keys plants-by-id))
	id-order-vecs (sorted-plant-id-vectors plants-sorted)
	id-map (merge-with #(conj %1 [:sort-order %2]) plants-by-id id-order-vecs)
	common-map (into (sorted-map)
			    (mapcat (fn [p] (map vector
					  (remove nil? (list (:comind1 p) (:comind2 p) (:comind3 p))) (repeat (:id p))))
				    (remove catesby-only? plants-sorted)))   
        ]
    ;; "Zzzz" is a fake genus used for extra Catesby entries only
    {:idmap (merge incompletes-by-id id-map)
     :genmap (dissoc genera-by-id "Zzzz")
     :authmap auth-map
     :pubmap pub-map
     :sorted plants-sorted
     :commonmap common-map
     :childmap (into {} (map #(vector (first %) (sort-ids (second %) id-map)) child-map))}))


(defn throw-balanced [full-string ch expecting]
  (if expecting
    (throw (IllegalArgumentException. (str "Unbalanced: '" full-string "', saw '" ch "', but expecting '" expecting "'")))
    (throw (IllegalArgumentException. (str "Unbalanced: '" full-string "', unbalanced '" ch "'")))))

(defn throw-if-unbalanced [^String s]
  (let [leftover (reduce (fn [stack ch]
            (case ch
              \[ (conj stack \])
              \{ (conj stack \})
              \( (conj stack \))
              \" (if (= (peek stack) \") (pop stack) (conj stack \"))
              \) (if (= (peek stack) \)) (pop stack) (throw-balanced s ch (peek stack)))
              \} (if (= (peek stack) \}) (pop stack) (throw-balanced s ch (peek stack)))
              \] (if (= (peek stack) \]) (pop stack) (throw-balanced s ch (peek stack)))
              stack))
          []
          s)]
    (when-not (empty? leftover)
      (throw (IllegalArgumentException. (str "Unbalanced: '" s "', missing '" (apply str leftover) "'"))))))

(defn unbalanced [labelv s]
  (try
    (throw-if-unbalanced s)
    (catch IllegalArgumentException e (str (apply str labelv) " - " (.getMessage e)))))

(def bal-plant-keys (filter (fn [k] (let [kfn (key-fn k)] (or (= kfn pformatted) (= kfn pmultiformatted))))
                            (remove #{:size} plant-keys)))

(defn unbalanced-plant [plant]
  (keep #(unbalanced (vector (:id plant) %) (get plant %))
        bal-plant-keys))

(defn check-unbalanced-plants []
  (println "Unbalanced Plants")
  (doseq [x (mapcat unbalanced-plant (concat (parse-plantdb-csv plants-csv-file)
                                             (parse-plantdb-csv orchid-csv-file)
                                             (parse-plantdb-csv protea-csv-file)))]
    ;;(when-not (.startsWith x "ProtParl:zones ")
      (println x)
      (println)))
;;)

(def bal-author-keys (filter (fn [k] (let [kfn (key-fn k)] (or (= kfn pformatted) (= kfn pmultiformatted))))
                           author-keys))

(defn unbalanced-author [author]
  (keep #(unbalanced (vector (:authid author) %) (get author %))
        bal-author-keys))

(defn check-unbalanced-authors []
  (println "Unbalanced Authors")
  (doseq [x (mapcat unbalanced-author (parse-authors-csv author-csv-file))]
  ;;  (when-not (.startsWith x "WLS:longauth ")
      (println x)
      (println)))
;;)

       
(def bal-genera-keys (filter (fn [k] (let [kfn (key-fn k)] (or (= kfn pformatted) (= kfn pmultiformatted))))
                           genera-keys))

(defn unbalanced-genera [genera]
  (keep #(unbalanced (vector (:genid genera) %) (get genera %))
        bal-genera-keys))

(defn check-unbalanced-genera []
  (println "Unbalanced Genera")
  (doseq [x (mapcat unbalanced-genera (parse-genera-csv genera-csv-file))]
    (println x)
    (println)))


(def bal-pub-keys (filter (fn [k] (let [kfn (key-fn k)] (or (= kfn pformatted) (= kfn pmultiformatted))))
                           pub-keys))

(defn unbalanced-pub [pub]
  (keep #(unbalanced (vector (:pubid pub) %) (get pub %))
        bal-pub-keys))

(defn check-unbalanced-pubs []
  (println "Unbalanced Pubs")
  (doseq [x (mapcat unbalanced-pub (parse-pubs-csv pub-csv-file))]
    (println x)
    (println)))


;; Look for duplicate IDs in all the files
(defn check-dups []
  (check-for-duplicates :id (concat (parse-plantdb-csv plants-csv-file)
                                    (parse-plantdb-csv orchid-csv-file)
                                    (parse-plantdb-csv protea-csv-file))))

(defn check-authors-dups []
  (check-for-duplicates :authid (parse-authors-csv author-csv-file)))

(defn check-gen-dups []
  (check-for-duplicates :genid (parse-genera-csv genera-csv-file)))

(defn check-pub-dups []
  (check-for-duplicates :pubid (parse-pubs-csv pub-csv-file)))



;; check-link is similar to link-content from templates.clj
;; but just checking for missing
;; check-link return true if found, false if missing
(defmulti check-link (fn [ltype id] ltype))

(defmethod check-link :default [ltype id]
  (plant-for-id id))

(defmethod check-link "pub" [ltype id]
   (get-in *db* [:pubmap id]))

(defmethod check-link "br" [ltype id]
  (plant-for-id id))

(defmethod check-link "cat" [ltype id]
  (plant-for-id id))

(defmethod check-link "bib" [ltype id]
  true)

(defmethod check-link "#" [ltype id]
  id)

(defmethod check-link "author" [ltype id]
  (get-in *db* [:authmap id]))

(defmethod check-link "http" [ltype id]
  id)

(defmethod check-link "xhttp" [ltype id]
  id)

(defmethod check-link "quote" [ltype id]
  true)

(defmethod check-link "plant" [ltype ^String id]
  (if (= (.length id) 4)
    (or (= id "Zzzz")
        (get-in *db* [:genmap id :genus]))
    (plant-for-id id)))

(defmethod check-link "images" [ltype id]
  (plant-for-id id))


(defn check-link-id [link-type id]
  (when-not (check-link link-type id)
    [link-type id]))

(defn check-link-from-regex [[link-type id]]
  (check-link-id link-type id))

(defn check-required-link [link-type id]
  (let [missing (check-link-id link-type id)]
    (when missing
      (list missing))))

(defn check-optional-link [link-type id]
  (when id
    (check-required-link link-type id)))

(defn check-optional-id [id]
  (check-optional-link "plant" id))

(defn check-for-missing-links [s]
  (when-not (str/blank? s)
    (let [pregex #"\[(\w+)://([^] ]+) *(?:[^]]*)\]"
          links (map (fn [[_ link-type id]] [link-type id]) (re-seq pregex s))]
      (filter check-link-from-regex links))))

(defn check-plant-for-missing [p]
  (map #(conj % (:id p))
  (concat
   (mapcat check-for-missing-links 
           (flatten ((juxt :description :experience :history :attribution :catdesc :bot-rep-info :bot-mag-info) p)))
   (check-optional-id (:seed-pid p))
   (check-optional-id (:pollen-pid p))
   )))

(defn check-genera-for-missing [gen]
  (map #(conj % (str "GENERA-" (:genid gen))) (check-required-link "author" (:author gen))))

;; author file doesn't need missing check
;;(defn check-author-for-missing [auth]

;; need to write a separate pub check since db :pubmap doesn't keep everything
(defn check-pub-for-missing [pub]
  (map #(conj % (str "PUB-" (:pubid pub))) (check-required-link "author" (:author pub))))


(defn check-for-all-missing [db]
  (let [*db* db]
    (mapcat check-plant-for-missing (:sorted db))))

