(ns plantilus.grex
  (:require [plantilus.webgen :as w]
            [plantilus.bento :as b]
            [plantilus.util :as util]
            [rhizome.viz :as rv]))

(defn gtree [plantdb ancestor-pids plant]
  (cond (not plant) nil
        (ancestor-pids (:id plant)) (throw (ex-info "Parental cycle" {:cut-off (:id plant)
                                                                      :ancestors ancestor-pids}))
        :else (let [seed (b/plant-for-id plantdb (:seed-pid plant))
                    pollen (b/plant-for-id plantdb (:pollen-pid plant))
                    ancestor-pids (conj ancestor-pids (:id plant))]
                ;(println "looking at... " (:id plant))
                (if (or seed pollen)
                  [(:id plant) (gtree plantdb ancestor-pids seed) 
                   (gtree plantdb ancestor-pids pollen)]
                  (:id plant)))))

(defn gtree-pid [plantdb pid]
  (gtree plantdb #{} (b/plant-for-id plantdb pid)))

(defn terminals [gt]
  (let [ts (fn ts [gt frac]
             (if (vector? gt) 
               (merge-with + (ts (nth gt 1) (/ frac 2)) (ts (nth gt 2) (/ frac 2)))
               {gt frac}))]
    (ts gt 1)))

(defn minv [va vb]
  (if (== -1 (compare va vb))
    va
    vb))

;; naddr is a vector address [] for root, [1 2] second parent of first parent
;; minv gives preference to closest to root for "first" appearance
(defn first-appearance [gtree]
  (let [fs (fn fs [gt naddr]
             (if (vector? gt) 
               (merge-with minv {(nth gt 0) naddr} 
                           (fs (nth gt 1) (conj naddr 1))
                           (fs (nth gt 2) (conj naddr 2)))
               {gt naddr}))]
    (fs gtree [])))


(defn prune [gtree]
  (let [fa (first-appearance gtree)
        ps (fn ps [gt naddr]
              (if (string? gt)
                gt
                (let [[n left right] gt]
                  (if (= (get fa n) naddr) 
                    [n (ps left (conj naddr 1)) (ps right (conj naddr 2))]
                    [n "*"]))))]
    (ps gtree [])))
 

#_ (doseq [pid (flatten (dbfilt :grex :id))] 
     (println "\nworking" pid) (pprint (gtree-pid *db* pid)))

(defn gtv-orig [plantdb pid]
  (let [p (b/plant-for-id plantdb pid)
        lookup (partial b/plant-for-id plantdb)
        children (juxt (comp lookup :seed-pid) (comp lookup :pollen-pid))
        branch? (some-fn :seed-pid :pollen-pid)]
    (rv/view-tree branch? children p
                  :vertical? false
                  :options {:fontsize 6
                            :fontcolor :blue
                           ;; :splines :ortho 
                            :ranksep :equally
                            :rankdir :LR
                            :rank :source
                            :fixedsize true}
                  :edge->descriptor 
                  (fn [a b] {:color :red
                             ;; :samehead true
                             :arrowhead :none
                             :headport :w
                             :tailport :e})
                  :node->descriptor 
                  (fn [p] {:label (b/orchid-name p)
                           :fontcolor :blue
                           :fontsize 10

                           ;; :height 15
                           ;; :width 50
                           ;; :fixedsize true

                           :URL (str "http://plantilus.com/plantdb/" (:id p) "/")
                           :color :yellow
                           :shape :box}))))

(defn pruned? [node]
  (and (vector? node) (= "*" (nth node 1))))

(defn orchid-name [node plantdb]
  (let [pid (if (vector? node) (nth node 0) node)
        oname (b/orchid-name (b/plant-for-id plantdb pid))]
    (if (pruned? node)
      (str oname " *")
      oname)))



(defn get-plant [node plantdb]
  (let [pid (if (vector? node) (nth node 0) node)]
    (b/plant-for-id plantdb pid)))


(defn gtree-view [plantdb pid]
  (let [ptree (prune (gtree-pid plantdb pid))
        lookup (partial b/plant-for-id plantdb)
        children (juxt #(nth % 1) #(nth % 2))
        branch? #(and (vector? %) (== (count %) 3))]
    (rv/view-tree branch? children ptree
                  :vertical? false
                  :options {:fontsize 6
                            :fontcolor :blue
                           ;; :splines :ortho 
                            :ranksep :equally
                            :rankdir :LR
                            :rank :source
                            :fixedsize true}
                  :edge->descriptor 
                  (fn [a b] {:color :red
                             ;; :samehead true
                             :arrowhead :none
                             :headport :w
                             :tailport :e})
                  :node->descriptor 
                  (fn [p] {:label (orchid-name p plantdb)
                           :fontcolor :blue
                           :fontsize 10

                           ;; :height 15
                           ;; :width 50
                           ;; :fixedsize true

                           :URL (str "http://plantilus.com/plantdb/" 
                                     (:id (get-plant p plantdb)) "/")
                           :color :yellow
                           :shape :box}))))

(defn gtv 
  ([p] (gtv b/*db* p))
  ([plantdb p] (gtree-view plantdb (if (string? p) p (:id p)))))

;; :none :record :box :circle :plaintext (none)

;; for a quick test on reload                  
#_ (gtv "RlczYell")


#_ (reduce conj {} (for [pid (map first (dbfilt :grex :id))] (vector pid (g/terminals (g/gtree-pid *db* pid)))))
