(ns plantilus.templates
  (:refer-clojure)
  (:use plantilus.resources)
  (:require [net.cgrand.enlive-html :as h]
            [plantilus.bento :as b]
	    [plantilus.util :as util]
	    [me.raynes.fs :as fs]
	    [clojure.set :as set]
	    [clojure.java.io :as io]
	    [clojure.string :as str]))



;; looking at an enlive node
(defn get-attr [node attr]
  (get (node :attrs) attr))

;; FIXME predeclared
(declare latin-name-node)
(declare nb-latin-name-node)

;; convenience for dealing with enlive nodes
(defn flattened [& nodes]
  (h/flatten-nodes-coll nodes))

(defn make-node
  "Make a node (map) wrapping a tag (like :span) an optional map of attributes around another node"
  ;; attrs is a map
  ;; hacked from h/wrap
  ([tag node] (make-node tag nil node))
  ([tag attrs node] (array-map :tag tag :attrs attrs :content (h/as-nodes node))))

(defn first-word [^String s]
  (let [sp (.indexOf s " ")]
    (if (neg? sp) s (subs s 0 sp))))
        
;; use h/html-snippet to get an html thingy like &copy; inserted as a value

(defn last-char [^String s]
  (.charAt s (dec (.length s))))
  
(defn cultivar-string [^String s]
  (cond (str/blank? s) nil
	;; really only care about (TM) and (R) chars, but this works for anything special (beyond A-Z, a-z)
	(> (int (last-char s)) (int \z)) s
	:else (str \' s \')))



(defmulti link-attrs (fn [ltype id] ltype))
(defmulti link-href (fn [ltype id] ltype))
(defmulti link-content (fn [ltype id] ltype))

(defmethod link-attrs :default [ltype id]
  {:class ltype :href (link-href ltype id)})

(defmethod link-attrs "#" [ltype id]
  {:class "xref" :href (link-href ltype id)})

(defmethod link-attrs "xhttp" [ltype id]
  {:class ltype :href (link-href ltype id) :target "_blank"})

(defmethod link-href :default [ltype id]
  (str "../" id "/"))

(defmethod link-content :default [ltype id]
  id)

(defmethod link-href "br" [ltype id]
  (str "../3ook/botreg.html" "#" id))

(defmethod link-href "brep" [ltype id]
  (str "../3ook/botrep.html" "#" id))

(defmethod link-href "plrar" [ltype id]
  (str "../3ook/jacqplrar.html" "#" id))

(defmethod link-href "cat" [ltype id]
  (str "../3ook/catesby.html" "#" id))

(defmethod link-href "bib" [ltype id]
  (str "http://plantilus.com/nomenclature/bibliography.html" "#" id))

(defmethod link-href "pub" [ltype id]
  (str "../1nfo/publications.html" "#" id))

(defmethod link-href "author" [ltype id]
  (str "../1nfo/authors.html" "#" id))

(defmethod link-href "http" [ltype id]
  (str "http://" id))

(defmethod link-href "xhttp" [ltype id]
  (str "http://" id))

(defn genera-relative-href [genid]
  (str "../1nfo/genera.html#" genid))

;; used for internal xrefs on genera page
(defn genera-relative-href-fragment [genid]
  (str "#" genid))

(defmethod link-href "plant" [ltype ^String id]
  (str "../" id "/index.html"))

(defmethod link-href "images" [ltype id]
  (str "../" id "/images.html"))

(defmethod link-href "#" [ltype id]
   (str "#" id))

;; FIXME -- most of the link-content should be doing a lookup
(defmethod link-content "pub" [ltype id]
  (get-in b/*db* [:pubmap id] (str "MISSING PUB-" id)))

(defmethod link-content "br" [ltype id]
  (or (flattened "Bot. Reg. " (latin-name-node (b/plant-for-id id))) (str "MISSING BR-" id)))

(defmethod link-content "cat" [ltype id]
  (or (flattened "Catesby " (:catname (b/plant-for-id id))) (str "MISSING CAT-" id)))

(defmethod link-content "bib" [ltype id]
  (str "MISSING BIB-" id))

(defmethod link-content "#" [ltype id]
  id)

(defmethod link-content "author" [ltype id]
  (get-in b/*db* [:authmap id] (str "MISSING AUTHOR-" id)))

(defmethod link-content "http" [ltype id]
  (str "http://" id))

(defmethod link-content "xhttp" [ltype id]
  (str "http://" id))

;; should move to bento
(defn genera-name-for-id [id]
  (if (= id "Zzzz")
    "unknown"
    (get-in b/*db* [:genmap id :genus] (str "MISSING GENERA-" id))))

(defn genera-node [id]
  (make-node :i (genera-name-for-id id)))

(defmethod link-content "plant" [ltype ^String id]
  (if (= (.length id) 4)
    (genera-node id)
    (or (latin-name-node (b/plant-for-id id)) (str "MISSING " id))))

(defmethod link-content "images" [ltype id]
  (or (latin-name-node (b/plant-for-id id)) (str "MISSING IMAGE-" id)))

(defn latin-name [plant]
  (str/join " " (remove nil? (list (:x plant)
                                   (:genus plant)
                                   (:special plant)
                                   (:grex plant)
                                   (:species plant)
                                   (if-let [h (:hybrid plant)] (if (= h "x") "x" (str "x " h)))
                                   (if-let [^String v (:var plant)] (if (neg? (.indexOf v ".")) 
                                                              (str "var. " v) v))
                                   (if-let [v (:subsp plant)] (str "subsp. " v))
                                   (if-let [v (:cultivar plant)] (cultivar-string v))
                                   (if-let [v (:cultivar2 plant)] (str "(" (cultivar-string v) ")"))
                                   ))))

(defn content? [x]
  (cond (string? x) (not (str/blank? x))
        (coll? x) (seq x)
        :else x))

(defn make-link-node [link-type id content]
  (make-node :a (link-attrs link-type id) (if (content? content) content (link-content link-type id))))

;; ignore the first item in the vector and the junk at the end, makes it easier to deal with the output from regex parsing
(defn make-link-node-from-regex [[ignore-full link-type id content & ignore]]
  (make-link-node link-type id content))

(defn plant-link-node [id]
  (make-link-node "plant" id nil))

(defn strip-links-node [nodes]
  (cond (nil? nodes) nil
	(string? nodes) nodes
	(and (map? nodes) (= (:tag nodes) :a)) (strip-links-node (:content nodes))
	(map? nodes) (make-node (:tag nodes) (:attrs nodes) (strip-links-node (:content nodes)))
	(coll? nodes) (flattened (map strip-links-node nodes))))

(defn short-plant-link-node [id]
  (let [plant (b/plant-for-id id)
        node (make-link-node "plant" id (nb-latin-name-node (b/plant-for-id id)))]
    (if (:incomplete plant)
      (strip-links-node node)
      node)))

(defn cultivar-images-link-node [id]
  (let [plant (b/plant-for-id id)]
    (when (and (:cultivar plant) (seq (:images plant)))
      (make-link-node "images" id (cultivar-string (:cultivar plant))))))

(defn subtaxa-images-link-node [id]
  (let [plant (b/plant-for-id id)]
    (when (and (not (:cultivar plant)) (seq (:images plant)))
      (make-link-node "images" id nil))))

(defn apply-node [f node]
  (cond (nil? node) nil
	(string? node) (f node)
	(map? node) {:tag (:tag node)
		     :attrs (:attrs node)
		     :content (h/flatten-nodes-coll (apply-node f (:content node)))}
	;; else it is a collection
	:default (h/flatmap #(apply-node f %) node)))

(defn nodify-node [node & nodify-transforms]
  (h/flatten-nodes-coll (reduce #(apply-node %2 %1) node nodify-transforms)))

;; re-seq will return all the matches in vectors [full-match group1 ... groupN].
;; We set the regex groups to pull out the link-type (group 1), the id (group 2) and the optional content (group 3).
;; The regex is a bit tricky.
(defn nodify-links [s]
  (let [pregex #"\[(\w+)://([^] ]+) *([^]]*)\]"
	plain-nodes (str/split s pregex)]
      (let [links (re-seq pregex s)]
	(util/interleave-all plain-nodes (map make-link-node-from-regex links)))))

;; support [#link foo bar] as internal link to #link within page
;; note: the link type is just plain #
(defn nodify-hash-links [s]
  (let [pregex #"\[(#)(\w+) *([^]]*)\]"
	plain-nodes (str/split s pregex)]
      (let [links (re-seq pregex s)]
	(util/interleave-all plain-nodes (map make-link-node-from-regex links)))))

(defn nodify-quote [s]
  (let [pregex #"\[quote: ([^]]+)\]"
	plain-nodes (str/split s pregex)]
      (let [qtexts (re-seq pregex s)]
	(util/interleave-all plain-nodes (map #(make-node :q (second %)) qtexts)))))

(defn nodify-block-quote [s]
  (let [pregex #"\[blockquote: ([^]]+)\]"
	plain-nodes (str/split s pregex)]
      (let [qtexts (re-seq pregex s)]
	(util/interleave-all plain-nodes (map #(make-node :blockquote (second %)) qtexts)))))

(defn nodify-braces [s]
  (let [pregex #"\{([^}]*)\}"
	plain-nodes (str/split s pregex)]
    (let [itexts (re-seq pregex s)]
      (if (zero? (count itexts))
	(first plain-nodes)
	(util/interleave-all plain-nodes (map #(make-node :em (second %)) itexts))))))
    

;; from enlive-tutorial
(defmulti parse-int type)
(defmethod parse-int java.lang.Integer [n] n)
(defmethod parse-int java.lang.String [s] (Integer/parseInt s))

(defmacro maybe-substitute
  ([expr] `(if-let [x# ~expr] (h/substitute x#) identity))
  ([expr & exprs] `(maybe-substitute (or ~expr ~@exprs))))

(defmacro maybe-content
  ([expr] `(if-let [x# ~expr] (h/content x#) identity))
  ([expr & exprs] `(maybe-content (or ~expr ~@exprs))))

(defmacro maybe-append
  ([expr] `(when-let [x# ~expr] (when-not (empty? x#) (h/append x#))))
  ([expr & exprs] `(maybe-append (or ~expr ~@exprs))))

(defn relative-plant-url [pid]
  (str "../" pid "/"))

(defn hybrid-node [plant]
  (let [hyb (:hybrid plant)]
    (when hyb
      (if (= hyb "x")
	 "x"
	 ["x " (make-node :i hyb)]))))

(defn var-node [plant]
  (let [^String vvv (:var plant)]
    (when vvv
      (let [dot (.indexOf vvv ". ")]
        (if (neg? dot)
          ["var. " (make-node :i vvv)]
          [(subs vvv 0 (+ dot 2)) (make-node :i (subs vvv (+ dot 2)))]))))) 

(defn subsp-node [plant]
  (let [vvv (:subsp plant)]
    (when vvv
      ["subsp. " (make-node :i vvv)])))

(defn cultivar-node [plant]
  (if-let [v1 (:cultivar plant)]
    (if-let [v2 (:cultivar2 plant)]
      (str (cultivar-string v1) " (" (cultivar-string v2) ")")
      (str (cultivar-string v1)))))      
  
(defn x-node [plant]
  (when (:x plant) "X"))

(defn latin-name-node [plant]
  (when plant
    (vec (flatten (interpose " " (remove nil? (list (x-node plant)
				      (make-node :i (:genus plant))
				      (:special plant)
                                      (:grex plant)
                                      (when (:species plant)
                                        (make-node :i (:species plant)))
				      (hybrid-node plant)
				      (var-node plant)
				      (subsp-node plant)
				      (cultivar-node plant))))))))

(def nbsp (first (h/html-snippet "&nbsp;")))

(defn nb-latin-name-node [plant]
  (when plant
    (let [segments (interpose nbsp (remove nil? (list (x-node plant)
                                               (make-node :i (:genus plant))
                                               (:special plant)
                                               (:grex plant)
                                               (when (:species plant)
                                                 (make-node :i (:species plant))))))
          extras (interpose " " (remove nil? (list (hybrid-node plant)
                                    (var-node plant)
                                    (subsp-node plant)
                                    (cultivar-node plant)))) ]
      (vec (flatten (concat segments (list " ") extras))))))


;; FIXME pre-declared for temporary convenience
(declare formatted-node)

(declare deref-author)

(defn author-node [plant]
  (formatted-node (deref-author (:attribution plant))))


(defn author-no-link-node [plant]
  (let [node (author-node plant)]
    (strip-links-node node)))

;; multiple links, first to plant, then to authors
(defn latin-name-author-node [plant]
  (vec (flatten (list (latin-name-node plant) " " (author-node plant)))))


;; link just to plant, not separate author link
(defn latin-name-link-author-node [plant]
  (if (:incomplete plant)
    ;; no link if incomplete
    (flattened (latin-name-node plant) " " (author-no-link-node plant))
    (make-node :a {:class "plant" :href (str "../" (:id plant) "/index.html")}
               (flattened (latin-name-node plant) " " (author-no-link-node plant)))))


(defn parent-link-node [plant]
  (if-let [parent-id (:id (b/parent plant))]
    (plant-link-node parent-id)))

(defn genus-link-node [plant]
  (if-let [gid (b/pid-for-genus plant)]
    (plant-link-node gid)))

;; SEM was
;;    (make-node :a {:class "plant" :href (genera-relative-href gid)} (make-node :i (:genus plant)))))

(defn species-link-node [plant]
  (if-let [spid (b/pid-for-species plant)]
    (plant-link-node spid)))

(defn cultivar-nodes [plant]
  (let [cs (b/cultivar-ids plant)]
    (map plant-link-node cs)))


(defn other-cultivar-nodes [plant]
  (map plant-link-node (b/other-cultivar-ids plant)))

(defn subtaxa-nodes [plant]
  (let [cs (b/subtaxa-ids plant)]
    (map plant-link-node cs)))


(defn relative-nodes [plant]
  (map #(latin-name-link-author-node (b/plant-for-id %)) (b/relative-ids plant)))

(defn image-height-width [file]
  (with-open [r (io/input-stream file)]
    (let [image (javax.imageio.ImageIO/read r)]
      [(.getHeight image) (.getWidth image)])))

(defn plant-image-height-width [id basename]
  (image-height-width (io/file plantdb-dir id basename)))
  

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
      
          
;; all the catesbybig and thumb should have BCat photo credit

;; Photo Credit is marked by _PC_Foo_bar at the end of the image filename; it should go into a separate <p>
;; If the PC value matches this map, then substitute the fancy info
(def magic-photo-credits
  { "Add1" "[pub://Addi Addisonia] 4: 54 (1919) at [xhttp://biodiversitylibrary.org/page/4509518 Biodiversity Heritage Library contributed by New York Botanic Garden]"

"FlDe" "Flora von Deutschland \u00D6sterreich und der Schweiz. 1885, Gera, Germany: Thom\u00E9 Courtesy of Kurt Stueber: [xhttp://www.biolib.de/ www.biblio.de]"

"BotR" "[xhttp://biodiversitylibrary.org/page/8261 From Bot. Reg. Courtesy Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

"BRep" "[xhttp://www.biodiversitylibrary.org/page/34966821 From Bot. Repos. Courtesy Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

"DP" "Original from Descr. Pinus courtesy of [xhttp://biodiversitylibrary.org/page/31681134 Biodiversity Heritage Library - (provided by Missouri Botanical Garden)] modified to provide labels."

"Starr" "[xhttp://www.hear.org/starr/ Plants of Hawaii]"

"PlAs" "[pub://PlAs Pl. Asiat. Rar.] - [xhttp://biodiversitylibrary.org/page/449433 Courtesy Biodiversity
Heritage Library - Provided by Missouri Botanical Garden]"

"Coniferophytus" "Coniferophytus from the [xhttp://forums.gardenweb.com/forums/load/conif/msg0915583418453.html Conifers Forum]"

"BCat" "[xhttp://biodiversitylibrary.org/page/10900100 Courtesy Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

"Shaw" "Courtesy of [xhttp://shawssunshinegardens.com/ Shaw's Sunsine Gardens]"

"Malene_Thyssen" "Courtesy of [xhttp://commons.wikimedia.org/wiki/User:Malene Malene Thyssen]"

"Royalbroil"   "Courtesy of [xhttp://commons.wikimedia.org/wiki/User:Royalbroil Royalbroil]"

"Parad" "[xhttp://www.biodiversitylibrary.org/page/36898142 From. Parad. Lond. Courtesy Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

"Botmag1"  "[xhttp://www.biodiversitylibrary.org/page/468613 From Bot. Mag. Courtesy Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

"Botmag2"  "[xhttp://www.biodiversitylibrary.org/page/470454 From Bot. Mag. Courtesy Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

"PLRAR" "[xhttp://www.biodiversitylibrary.org/page/271710 From Pl. Rar. Hort. Schoenbr. Courtesy Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

"Edwards" "[xhttp://www.biodiversitylibrary.org/page/238431 From Edward's Botanical Register.  Courtesy Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

"DictIcon" "[xhttp://biodiversitylibrary.org/page/38620883 From Dict. Icon. Orchid. Courtesy
Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

"OrchidAlbum" "[xhttp://biodiversitylibrary.org/page/35713772 From Orchid Album. Courtesy Biodiversity Heritage Library - Provided by Missouri Botanical Garden]"

  })

;; key is a photo-credit code
(def magic-caption-prefix
  {"BotR" "Bot. Reg."
   "BCat" "Catesby"
   "BRep" "Bot. Rep."
   "Parad" "Parad. Lond."
   "Botmag1" "Bot. Mag."
   "Botmag2" "Bot. Mag."})


(defn simple-photo-credit-from-basename [basename]
  (let [parts (str/split basename #"_PC_")
	ending (second parts)]
    (when ending
      (subs ending 0 (- (.length ^String ending) (.length ".jpg"))))))

(defn photo-credit-from-basename [basename]
  (let [simple (simple-photo-credit-from-basename basename)]
    (if simple
      (or (get magic-photo-credits simple) (str/replace simple  "_" " "))
      "Lisa Miner")))

;; only returns a value if it's one of the special notation photo credits
(defn magic-photo-credit-from-basename [basename]
  (let [simple (simple-photo-credit-from-basename basename)]
    (when simple
      (get magic-photo-credits simple))))


(defn plant-id-from-basename [^String basename]
   (let [hyphen (.indexOf basename "-")]
     (if (<= (int \0) (int (first basename)) (int \4))
       (subs basename 1 hyphen)
       (subs basename 0 hyphen) )))
    

(defn caption-from-basename
  ([^String basename]
     (caption-from-basename basename (plant-id-from-basename basename)))

  ([^String basename ^String id]
     ;; extra is offset for possible leading 0 or 1, plus hypen
     (let [parts (str/split basename #"_PC_")
	   ^String base (first parts)
	   extra (if (<= (int \0) (int (first base)) (int \4)) 2  1)
	   caption (str/replace (subs base (+ extra (.length id))
				      (if (= 1 (count parts))
					(- (.length base) (.length ".jpg"))
					(.length base)))
                                "_" " ")]
       caption)))


(defn alt-from-basename
  ([^String basename]
     (alt-from-basename basename (plant-id-from-basename basename)))

  ([^String basename ^String id]
     ;; extra is offset for possible leading 0 or 1, plus hypen
     (let [parts (str/split basename #"_PC_")
	   ^String base (first parts)
           prefix (get magic-caption-prefix (simple-photo-credit-from-basename basename))
	   extra (if (<= (int \0) (int (first base)) (int \4)) 2  1)
	   caption (str/replace (subs base (+ extra (.length id))
				      (if (= 1 (count parts))
					(- (.length base) (.length ".jpg"))
					(.length base)))
                                       "_" " ")
           syn-plant (b/plant-for-id (first-word caption))]
       ;; special encoding of a synonymn plant id as the first word of the caption
       (cond (and syn-plant prefix)  (str prefix " " (latin-name syn-plant))
             syn-plant (latin-name syn-plant)
             (empty? caption) prefix
             :else caption))))




(defn single-image-page-for-thumb [^String thumb]
  (when thumb
    (let [hyphen (.lastIndexOf thumb "-")
	  pc (.lastIndexOf thumb "_PC_")
          dot (.lastIndexOf thumb ".")
          credit (when (pos? pc) (subs thumb (+ pc (count "_PC_")) dot))
	  end (if (neg? pc) dot pc)
	  base (not-empty (subs thumb (inc hyphen) end))]
      ;; special test for magic-caption-prefix to avoid conflicts between botreg and botrep name encoding
      (cond (and base (get magic-caption-prefix credit)) (str base "-" credit ".html")
            base (str base ".html")
            credit (str credit ".html")
            :else "picture.html")
      )))

(defn find-single-info-file [id]
  (single-image-page-for-thumb (b/find-info-thumb-file id)))

(defn relative-single-image-page-for-thumb [^String thumb]
  (when-let [base (single-image-page-for-thumb thumb)]
    (let [id (plant-id-from-basename thumb)]
      (str "../" id "/" base))))
  

;; FIXME add link for big image page (TBA)
(defn thumb-image-node [full-path-image-file]
    (when full-path-image-file
      (let [[height width] (image-height-width full-path-image-file)
	    base (fs/base-name full-path-image-file)
	    caption (alt-from-basename base)]
	(make-node :img
		   {:src base
		    :height (str height) :width (str width)
		    :alt caption}
		   nil))))

(defn image-node [full-path-image-file]
  (when full-path-image-file
    (let [[height width] (image-height-width full-path-image-file)
	  base (fs/base-name full-path-image-file)
	  caption (alt-from-basename base)]
      (make-node :img
		 {:src base
		  :height (str height) :width (str width)
		  :alt caption}
		 nil))))


(defn info-image [plant]
  ;; name should start with "0" but we don't check here
  (first (:images plant)))

(defn info-thumb-node [plant]
  (let [id (:id plant)
	base (info-image plant)]
    (when base
      (let [[height width] (plant-image-height-width id base)
	    caption (alt-from-basename base id)]
	(make-node :img
		   {:src base
		    :height (str height) :width (str width)
		    :alt caption}
		   nil)))))


;;SEM FIXME: not so sure about this.  What if both :brref and :catref?
;; Probably should look at _PC_ or image to decide

(defn caption-node [base-image-file]
  (when base-image-file
    (let [caption (caption-from-basename base-image-file)
          pc (simple-photo-credit-from-basename base-image-file)
          prefix (get magic-caption-prefix pc)]
      (if (str/blank? caption)
        (let [plant (b/plant-for-id (plant-id-from-basename base-image-file))
              brref (:brref plant)
              catref (:catref plant)]
          (cond 
           (and brref (= pc "BotR")) (make-node :p  {:class "caption"} (formatted-node (str prefix " " brref)))
           (and catref (= pc "BCat")) (make-node :p  {:class "caption"} (formatted-node (str prefix " " catref)))
           :else (make-node :p  {:class "caption"} prefix)))
        (let [syn-plant (b/plant-for-id (first-word caption))
              brref (:brref syn-plant)]
          ;; special trick of naming the syn plant id as the caption
          (cond (and syn-plant brref (= pc "BotR"))
                (make-node :p  {:class "caption"}
                           (flattened (formatted-node (str (get magic-caption-prefix "BotR") " " brref)) " "
                                      (plant-link-node (:id syn-plant)) " (= syn.)"))
                syn-plant
                (make-node :p  {:class "caption"}
                           (flattened (plant-link-node (:id syn-plant)) " (= syn.)"))
                :else
                (make-node :p  {:class "caption"} caption)))))))




(defn info-thumb-caption [plant]
  (let [id (:id plant)
	info-image-base (info-image plant)]
    (when info-image-base
      (caption-node info-image-base))))

(defn thumb-caption-node [base-image-file]
  (caption-node base-image-file))


(defn info-thumb-magic-photo-credit [plant]
  (let [info-image-base (info-image plant)]
    (when info-image-base
      (let [credit (magic-photo-credit-from-basename info-image-base)]
	(when credit
	  (make-node :p  {:class "credit"} (formatted-node (str "{Photo credit:} " credit))))))))

;; FIXME should only use basename
(defn thumb-magic-photo-credit-node [image-file]
  (when image-file
    (let [base (fs/base-name image-file)
	  credit (magic-photo-credit-from-basename base)]
      (when credit
	(make-node :p {:class "credit"} (formatted-node (str "{Photo credit:} " credit)))))))

;; FIXME should only use basename
(defn photo-credit-node [image-file]
  (when image-file
    (let [base (fs/base-name image-file)
	  credit (photo-credit-from-basename base)]
      (when credit
	(make-node :p {:class "credit"} (formatted-node (str "{Photo credit:} " credit)))))))

(defn formatted-node [s]
  (when-not (str/blank? s)
    (let [nnn (nodify-node s nodify-hash-links nodify-links nodify-block-quote nodify-quote nodify-braces)]
      (flattened nnn))))

(defn formatted-multi-paragraph-node [ps]
  (when (seq ps)
    (map #(make-node :p (formatted-node %)) ps)))


(def html-dash-separator (first (h/html-snippet "&nbsp;&nbsp;&ndash;&nbsp;&nbsp;")))


;; FIXME debugging

(def ^:dynamic *debug-out* *out*)



(defn deref-author [auth]
  ;; allows mix of either coded auth ref or formatted field
  (if-let [lookup (get-in b/*db* [:authmap auth])]
    (str "[author://" auth " " lookup "]")
    auth))

(defn author-lookup [obj-author]
  ;; allows mix of either coded auth ref or formatted field
  (when-let [auth (:author obj-author)]
    (deref-author auth)))


(defn nodify-pub [pub]
  (let [alpha (:alpha pub)
	alpha-node (if alpha (make-node :a {:id alpha} " ") "")]
    (flattened (make-node :dt {:id (:pubid pub)}
			  (if (:author pub)
			    (flattened alpha-node (:shortpub pub) html-dash-separator (:author pub) html-dash-separator (formatted-node (:date pub)))
			    (flattened alpha-node (:shortpub pub) html-dash-separator
                                       (formatted-node (:date pub)))))
               "\n"
	       (make-node :dd  (make-node :i (:longpub pub)))
               "\n"
	       (if (:other pub)
		 (make-node :dd (flattened " " (formatted-node (:other pub))))
		 "")
               "\n")))

(defn nodify-publist [publist]
  (make-node :dl {:id "publications"} (vec (flatten (map nodify-pub publist)))))



(defn nodify-author [author]
  )

(defn nodify-authorlist [authorist]
  )




;; nil tells enlive to hide node; identity leaves node unchanged
(defn only-if
  "Enlive Transform that hides the node unless the val is logically true."
  [val]
  (when val identity))

(defn content-when
  "Enlive Transform that sets the content of the node unless the val is false, in which case, it hides the node"
  [val]
  (when val (h/content val)))

(defn set-attr-when [attr val]
  (if val
    (h/set-attr attr val)
    (h/remove-attr attr)))


;; templates are in the resources dir
;; FIXME too much repetitve work for cultivars and other cults; need a cache

(h/deftemplate dbspecies "template/Plant/dbspecies.html"  [plant]
  [:title] (h/content (str (latin-name plant) " - Plantilus.com"))
  ;[:div.section :> :h3] (h/content (latin-name plant))
  [:div.label :> :h2] (h/content (latin-name-author-node plant))
  [:div.label :> :h3] (h/content (:common plant))
  [:div.sideinfo :a.more] (only-if (> (count (:images plant)) 1))
  [:div.banner :div.section :a.igallery] (only-if (> (count (:images plant)) 1))
  [:div.sideinfo :div.imageframe :a.thumb] (set-attr-when :href (single-image-page-for-thumb (info-image plant)))
  [:div.sideinfo :div.imageframe :a :img] (h/substitute (info-thumb-node plant))
  [:div.sideinfo :div.imageframe :p.caption] (h/substitute (info-thumb-caption plant))
  [:div.sideinfo :div.imageframe :p.credit] (h/substitute (info-thumb-magic-photo-credit plant))
  [:p#ppatents] (maybe-append (:patents plant))
  [:p#pfamily] (maybe-append (:family plant))
  [:p#pgenus] (maybe-append (genus-link-node plant))
  [:p#pparent] (maybe-append (parent-link-node plant))
  [:p#pcategory] (maybe-append (:category plant))
  [:p#pzones] (maybe-append (:zones plant))
  [:p#psize] (maybe-append (:size plant))
  [:p#pgrowth] (maybe-append (:growth plant))
  [:p#pexposure] (maybe-append (interpose ", " (b/exposure plant)))
  [:p#pdesclabel] (only-if (:description plant))
  [:div#pdescription] (h/content (formatted-multi-paragraph-node (:description plant)))
  [:p#pcultlabel] (only-if (or (:cultivars plant) (seq (cultivar-nodes plant))))
  [:div#pcultivars] (h/content (formatted-multi-paragraph-node (:cultivars plant)))
  [:ul#pcultlist :li] (h/clone-for [c (cultivar-nodes plant)] (h/content c))
  [:p#psubtlabel] (only-if (seq (b/subtaxa-ids plant)))
  [:ul#psubtaxalist :li] (h/clone-for [sub (subtaxa-nodes plant)] (h/content sub))
  [:p#phistlabel] (only-if (:history plant))
  [:div#phistory] (h/content (formatted-multi-paragraph-node (:history plant)))
  [:p#pothercultlabel] (only-if (seq (b/other-cultivar-ids plant)))
  [:ul#pothercultlist :li] (h/clone-for [c (other-cultivar-nodes plant)] (h/content c))
  [:p#prellabel] (only-if (seq (b/relative-ids plant)))
  [:ul#prelativeslist :li] (h/clone-for [rel (relative-nodes plant)] (h/content rel))
  [:p#pexplabel] (only-if (:experience plant))
  [:div#pexperience] (h/content (formatted-multi-paragraph-node (:experience plant)))
  [:p#phyblabel] (only-if (:hybridizing plant))
  [:div#phybridizing] (h/content (formatted-multi-paragraph-node (:hybridizing plant))))

(defn alt-author-name-or-other [author]
  (let [other (formatted-node (:other author))]
    (cond (and other (:altauth author)) (flattened (:altauth author) "; info: " other)
	  other (flattened "info: " other)
	  :else (:altauth author))))

(h/deftemplate dbauthors "template/1nfo/authors.html"  [authorlist alphaset]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphaset alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.authors :table :tr.AuthorID]
  (h/clone-for [author authorlist]
	       [:td#Alpha] (h/do-> (set-attr-when :id (:alpha author))
				   (h/content (:shortauth author)))
	       [:td#Name] (h/do-> (h/set-attr :id (:authid author)) (h/content (:longauth author)))
	       [:td#AltName] (h/do-> (h/remove-attr :id)(h/content (alt-author-name-or-other author)))
	       [:td#Country] (h/do-> (h/remove-attr :id)(h/content (:country author)))
	       h/this-node (h/remove-attr :class))
  )


(h/deftemplate dbpubs "template/1nfo/publications.html"  [publist alphaset]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphaset alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.publications :dl] (h/substitute (nodify-publist publist)))


(defn common-or-synonym-link-node [plant]
  (if-let [accID (:accepted plant)]
    (flattened "(synonym of " (plant-link-node accID) ")")
    (:common plant)))



(h/deftemplate dbindex "template/1nfo/index.html"  [plantlist alphaset]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphaset alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.pindex :table :tr.plantindex]
  (h/clone-for [plant plantlist]
	       h/this-node (h/do-> (h/set-attr :id (:id plant)) (h/remove-attr :class))
	       [:td.wimages :a] (when (info-image plant) (h/set-attr :href (str "../" (:id plant) "/images.html")))
	       [:td.latin] (h/do-> (set-attr-when :id (:alpha plant))
				  (h/content (latin-name-link-author-node plant)))
	       [:td.common] (h/do-> (h/remove-attr :class) (h/content (common-or-synonym-link-node plant))))
  )


;; common-map has [common-indexed-name PLANT-ID] entries
(h/deftemplate dbcommon "template/1nfo/common.html"  [common-map common-alpha-bimap]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (get common-alpha-bimap alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.pindex :table :tr.plantindex]
  (h/clone-for [[common pid] common-map]
	       [:td.wimages :a] (when (info-image (b/plant-for-id pid)) (h/set-attr :href (str "../" pid "/images.html")))
	       [:td.latin] (h/content (latin-name-link-author-node (b/plant-for-id pid)))
	       [:td.common] (h/do-> (h/content common) (set-attr-when :id (get common-alpha-bimap common)))
  	       h/this-node (h/remove-attr :class))
  )


(def spacer (h/html-snippet " &nbsp;&nbsp;&nbsp;"))

(defn genera-list-node [genus]
  (flattened 
   (if-let [alpha (:alpha genus)] (make-node :a {:id alpha} ""))
   (if-let [x (:x genus)] (str x " ")) 
   (plant-link-node (:genus genus)) " "
   (strip-links-node (formatted-node (author-lookup genus)))
   (when (:common genus) (vector spacer (:common genus)))))

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

(defn genera-species-line-item-node [plant]
  (flattened (latin-name-link-author-node plant)
	     (condp = (:accstatus plant)
		 "Synonym" " (= synonym)"
		 "Unresolved" " (= unresolved)"
		  nil)))

(h/deftemplate dbgenera-index "template/1nfo/genera.html"  [genlist plantdb alphaset]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphaset alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.pindex :table :tr.plantindex]
  (h/clone-for [gen genlist]
	       h/this-node (h/do-> (h/set-attr :id (:genid gen)) (h/remove-attr :class))
	       [:td.genus] (h/do-> (set-attr-when :id (:alpha gen))
                                   (h/content (plant-link-node (:genid gen))))
	       [:td.author] (h/content (strip-links-node (formatted-node (author-lookup gen))))
	       [:td.gstatus] (h/do-> (h/remove-attr :class) 
                                     (h/content (formatted-node (:gstatus gen))))
	       [:td.common] (h/do-> (h/remove-attr :class) (h/content (:common gen)))))


(defn latin-name-for-gen [gen]
  (make-node :i (:genus gen)))

(h/deftemplate dbgenera-single "template/Plant/genera-single.html"  [gen plantdb]
  [:title] (h/content (str (:genus gen) " - Plantilus.com"))
  [:#pgenauth] (h/content (flattened (latin-name-for-gen gen) " " (formatted-node (author-lookup gen))))
  [:#pcommon] (maybe-append (:common gen))
  [:#pfamily] (maybe-append (:family gen))
  [:#pother] (maybe-append (formatted-node (:other gen)))
  [:#ppub] (maybe-append (formatted-node (:pub gen)))
  [:#psubtlabel] (only-if (seq (filtered-species plantdb (:genid gen))))
  [:ul#psubtaxalist :li] (h/clone-for [plantid (filtered-species plantdb (:genid gen))]
                                      (h/content (genera-species-line-item-node (get-in plantdb [:idmap plantid]))))
  )

  


(h/deftemplate dbimagethumbs "template/Plant/images.html" [plant image-files]
  [:title] (h/content (str "Images of " (latin-name plant) " - Plantilus.com"))
  [:div.label :> :h2] (h/content (latin-name-author-node plant))
  [:div.label :> :h3] (h/content (:common plant))
  [:div.imageframe :div]  (h/clone-for [imagefile (seq image-files)]
				      [:img] (h/substitute (thumb-image-node (io/file plantdb-dir (:id plant) imagefile)))
				      [:a] (h/set-attr :href (single-image-page-for-thumb imagefile))
				      [:p.caption] (h/substitute (thumb-caption-node (fs/base-name imagefile)))
				      [:p.credit] (h/substitute (thumb-magic-photo-credit-node imagefile)))
  [:div.sideinfo :ul.othercults :li] (h/clone-for [opid (b/other-cultivar-ids-having-images plant)]
						  h/this-node (h/content (cultivar-images-link-node opid)))
  [:div.sideinfo :h4.othercults] (only-if (seq (b/other-cultivar-ids-having-images plant)))
  [:div.sideinfo :ul.subtaxa :li] (h/clone-for [spid (b/subtaxa-ids plant)]
					       h/this-node (h/content (subtaxa-images-link-node spid)))
  [:div.sideinfo :h4.subtaxa] (only-if (seq (b/subtaxa-ids plant)))
  )


(defn botreg-image-node [plant]
  (when-let [botreg-img (:brimage plant)]
    (let [imgurl (str "../" (:id plant) "/" botreg-img)]
      (make-node :a {:href (relative-single-image-page-for-thumb botreg-img)}
		 (make-node :img {:src imgurl :alt (str "Bot. Reg. " (:brref plant))} nil)))))

(comment
  ;; Obsolete
(h/deftemplate dbbotreg "template/3ook/botreg.html" [sorted-botregplants alphaset]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphaset alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.botreg :table :tr]
  (h/clone-for [plant (seq sorted-botregplants)]
	       h/this-node (set-attr-when :id (:bralpha plant))
	       [:td.botregpict :div.botreg :a] (h/substitute (botreg-image-node plant))
	       [:td.botregpict :div.botreg :p.caption] (h/substitute (thumb-caption-node (:brimage plant)))
	       [:td.botregpict :div.botreg :p.credit] (h/substitute (thumb-magic-photo-credit-node (:brimage plant)))
	       
	       [:td.botreg :p.pbrlatin]  (h/do-> (h/append (latin-name-link-author-node plant))
						 (h/set-attr :id (:id plant)))
	       [:td.botreg :p.pbrcurname]  (h/append (latin-name-link-author-node (b/plant-for-id (or (:accepted plant) (:id plant)))))
	       [:td.botreg :p.pbrcommon]  (h/append (:brcommon plant))
	       [:td.botreg :p.pbrref]  (h/append (:brref plant))
	       [:td.botreg :p.pbrdesclabel]  (only-if (:brdesc plant))
	       [:td.botreg :div.pbrdesc]  (h/content (formatted-multi-paragraph-node (:brdesc plant)))
	       [:td.botreg :p.pbrhistlabel]  (only-if (:brhist plant))
	       [:td.botreg :div.pbrhist]  (h/content (formatted-multi-paragraph-node (:brhist plant)))
	       [:td.botreg :p.pbrotherlabel]  (only-if (seq (:brother plant)))
	       [:td.botreg :div.pbrothernames] (h/content (formatted-multi-paragraph-node (:brother plant)))))
)


(h/deftemplate dbbotreg-index "template/3ook/botreg.html" [sorted-brs plantdb alphas]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphas alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.pindex :table :tr.plantindex]
  (h/clone-for [pin sorted-brs]
	       h/this-node (set-attr-when :id (:alpha pin))
               ;; use latin-name without author for the botreg name
	       [:td.brname] (h/do-> (h/content (short-plant-link-node (:id pin)))
                                    (h/remove-attr :class)
                                    ;(h/set-attr :class "nowrap")
                                    (h/set-attr :id (:id pin)))
	       [:td.brref] (h/do-> (h/content (formatted-node (:brref pin)))
                                   (h/remove-attr :class))
	       [:td.common] (h/content (formatted-node (:common pin)))
	       [:td.current] (h/content (latin-name-link-author-node (b/accepted-plant-for-id (:id pin))))))



(defn catesby-image-node [plant]
  (when-let [catesby-img (:catimage plant)]
    (let [imgurl (str "../" (:id plant) "/" catesby-img)]
      (make-node :a {:href (relative-single-image-page-for-thumb catesby-img)}
		 (make-node :img {:src imgurl :alt (str "Catesby " (:catref plant))} nil)))))


(defn catesby-only? [plant]
  (= (:accstatus plant) "Catesby Only"))

(defn catesby-id [plant]
  (if (catesby-only? plant) 
    (:accepted plant)
    (:id plant)))

(defn catesby-latin-name-link-author-node [plant]
  (if-let [cid (catesby-id plant)] 
    (latin-name-link-author-node (b/plant-for-id cid))
    (latin-name-link-author-node plant)))




(h/deftemplate dbcatesby "template/3ook/catesby.html" [sorted-catesby-plants alphaset]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphaset alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  ;; reusing div.botreg for formatting
  [:div.botreg :table :tr]
  (h/clone-for [plant (seq sorted-catesby-plants)]
	       h/this-node (set-attr-when :id (:catalpha plant))
	       [:td.catesbypict :div.catesby :a] (h/substitute (catesby-image-node plant))
	       [:td.catesbypict :div.catesby :p.caption] (h/substitute (thumb-caption-node (:catimage plant)))
	       [:td.catesbypict :div.catesby :p.credit] (h/substitute (thumb-magic-photo-credit-node (:catimage plant)))
               ;; use actual id, not catesby-id for incoming links
	       [:td.catesby :p.pclatin]  (h/do-> (h/append (catesby-latin-name-link-author-node plant))
						 (h/set-attr :id (:id plant)))
	       [:td.catesby :p.pccommon]  (h/append (:common plant))
               [:td.catesby :p.pcnamelabel]  (only-if (:catname plant))
	       [:td.catesby :div.pcname]  (h/content (formatted-multi-paragraph-node (:catname plant)))
	       [:td.catesby :p.pcref]  (h/append (:catref plant))
               [:td.catesby :p.pcloclabel]  (only-if (:catloc plant))
               [:td.catesby :div.pcloc]  (h/content (formatted-multi-paragraph-node (:catloc plant)))
	       [:td.catesby :p.pcdesclabel]  (only-if (:catdesc plant))
	       [:td.catesby :div.pcdesc]  (h/content (formatted-multi-paragraph-node (:catdesc plant)))
               [:td.catesby :p.pcotherlabel]  (only-if (:catother plant))
	       [:td.catesby :div.pcother] (h/content (formatted-multi-paragraph-node (:catother plant)))
))

(defn catesby-ref-node [plant]
  ;; use the regular :id, not the (catesby-id p) to link into the catesby page
  (make-node :a {:class "plant" :href (link-href "cat" (:id plant))} (:catref plant)))
  

(h/deftemplate dbcatesby-index "template/3ook/catesby-index.html" [catesby-plants-by-index]
  [:div.pindex :table :tr.plantindex]
  (h/clone-for [plant catesby-plants-by-index]
	       [:td.latin] (h/content (catesby-latin-name-link-author-node plant))
	       [:td.plate] (h/content (catesby-ref-node plant))
  	       h/this-node (h/remove-attr :class))
  )




(defn inc-wrap [index max]
  (let [next (inc index)]
    (if (>= next max) 0 next)))

(defn dec-wrap [index max]
  (dec (if (zero? index) max index)))

(defn next-image [index others]
  (let [cnt (count others)]
    (when (>= cnt 2)
      (single-image-page-for-thumb (get others (inc-wrap index cnt))))))

(defn previous-image [index others]
  (let [cnt (count others)]
    (when (>= cnt 2)
      (single-image-page-for-thumb (get others (dec-wrap index cnt))))))


(h/deftemplate dbimagesingle "template/Plant/single-image.html" [plant image-file index other-image-files]
  [:title] (h/content (str (alt-from-basename image-file) " - " (latin-name plant) " - Plantilus.com"))
  [:a.inext] (set-attr-when :href (next-image index other-image-files))
  [:a.iprev] (set-attr-when :href (previous-image index other-image-files))
  [:a.igallery] (only-if (> (count other-image-files) 1))
  [:a.iprev] (only-if (> (count other-image-files) 1))
  [:a.inext] (only-if (> (count other-image-files) 1))
  [:div.label :> :h2] (h/content (latin-name-author-node plant))
  [:div.label :> :h3] (h/content (:common plant))
  [:div.imageframe :div.single :img] (h/substitute (image-node (io/file plantdb-dir (:id plant) image-file)))
  [:div.imageframe :div.single :p.caption] (h/substitute (caption-node image-file))
  [:div.imageframe :div.single :p.credit] (h/substitute (photo-credit-node image-file))
  )


;; SEM FIX ME -- remove class attrs that are only used for enlive targetting

(h/deftemplate dbcultprot-index "template/3ook/cultprot.html" [sorted-pins plantdb alphas]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphas alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.pindex :table :tr.plantindex]
  (h/clone-for [pin sorted-pins]
	       h/this-node (set-attr-when :id (:alpha pin))
               ;; use latin-name without author for the pinus name
	       [:td.cultprot] (h/do-> (h/content (short-plant-link-node (:id pin)))
                                      (h/remove-attr :class)
                                      ;(h/set-attr :class "nowrap")
                                      (h/set-attr :id (:id pin)))
	       [:td.cultprotref] (h/do-> (h/content (formatted-node (str "p. " (:cultprotref pin))))
                                         (h/remove-attr :class))
	       [:td.common] (h/content (formatted-node (:common pin)))
	       [:td.current] (h/content (latin-name-link-author-node (b/accepted-plant-for-id (:id pin))))))

(h/deftemplate dbbotrepos-index "template/3ook/botrepos.html" [sorted-brs plantdb alphas]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphas alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.pindex :table :tr.plantindex]
  (h/clone-for [pin sorted-brs]
	       h/this-node (set-attr-when :id (:alpha pin))
               ;; use latin-name without author for the botreg name
	       [:td.brname] (h/do-> (h/content (short-plant-link-node (:id pin)))
                                    (h/remove-attr :class)
                                    ;(h/set-attr :class "nowrap")
                                    (h/set-attr :id (:id pin)))
	       [:td.brref] (h/do-> (h/content (formatted-node (:brref pin)))
                                   (h/remove-attr :class))
	       [:td.common] (h/content (formatted-node (:common pin)))
	       [:td.current] (h/content (latin-name-link-author-node (b/accepted-plant-for-id (:id pin))))))


(h/deftemplate dbjacqplrar-index "template/3ook/jacqplrar.html" [sorted-brs plantdb alphas]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphas alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.pindex :table :tr.plantindex]
  (h/clone-for [pin sorted-brs]
	       h/this-node (set-attr-when :id (:alpha pin))
               ;; use latin-name without author for the botreg name
	       [:td.brname] (h/do-> (h/content (short-plant-link-node (:id pin)))
                                    (h/remove-attr :class)
                                    ;(h/set-attr :class "nowrap")
                                    (h/set-attr :id (:id pin)))
	       [:td.brref] (h/do-> (h/content (formatted-node (:plrarref pin)))
                                   (h/remove-attr :class))
	       [:td.current] (h/content (latin-name-link-author-node (b/accepted-plant-for-id (:id pin))))))


(h/deftemplate dbpinus-index "template/3ook/pinus.html" [sorted-pins plantdb alphas]
  [:div.jumpto :a] (fn [match] (let [alpha (second (get-attr match :href))]
				 (if (contains? alphas alpha)
				   match
				   (make-node :span {:class "disabled"} (str alpha)))))
  [:div.pindex :table :tr.plantindex]
  (h/clone-for [pin sorted-pins]
	       h/this-node (set-attr-when :id (:alpha pin))
               ;; use latin-name without author for the pinus name
	       [:td.pinus] (h/do-> (h/content (short-plant-link-node (:id pin)))
                                   (h/remove-attr :class)
                                   ;(h/set-attr :class "nowrap")
                                   (h/set-attr :id (:id pin)))
	       [:td.pinusref] (h/do-> (h/content (formatted-node (:pinusref pin)))
                                      (h/remove-attr :class))
	       [:td.common] (h/content (formatted-node (:common pin)))
	       [:td.current] (h/content (latin-name-link-author-node (b/accepted-plant-for-id (:id pin))))))

