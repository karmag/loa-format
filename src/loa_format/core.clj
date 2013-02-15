(ns loa-format.core
  (:use clojure.pprint
        clojure.data.zip.xml)
  (:require clojure.string
            clojure.zip
            clojure.xml)
  (:import java.io.File))

(defn- unlines [& lines]
  (->> (remove nil? lines)
       (interpose \newline)
       (apply str)))

(defn- to-ascii [string]
  (clojure.string/escape
   string
   {\u00ae ""
    \u00c6 "AE"
    \u00e0 "a"
    \u00e1 "a"
    \u00e2 "a"
    \u00e9 "e"
    \u00ed "i"
    \u00f6 "o"
    \u00fa "u"
    \u00fb "u"
    \u2014 "-"
    \u2018 "'"
    \u2019 "'"}))

(defn- capitalize [string]
  (when-not (empty? string)
    (str (clojure.string/upper-case (str (first string)))
         (.substring string 1))))

;;--------------------------------------------------
;; xml transformation

(defn- format-rule [rule-xml]
  {:no       (xml1-> rule-xml (attr :no))
   :text     (to-ascii (xml1-> rule-xml text))
   :reminder (xml1-> rule-xml (attr :reminder))})

(defn- format-type [type-xml]
  {:name (xml1-> type-xml text)
   :type (xml1-> type-xml (attr :type))})

(defn- format-card [card-xml]
  {:name     (to-ascii (xml1-> card-xml :name text))
   :cost     (xml1-> card-xml :cost text)
   :color    (xml1-> card-xml :color text)
   :pow      (xml1-> card-xml :pow text)
   :tgh      (xml1-> card-xml :tgh text)
   :typelist (xml-> card-xml :typelist :type format-type)
   :rulelist (xml-> card-xml :rulelist :rule format-rule)
   :multi (when-let [multi-xml (xml1-> card-xml :multi)]
            (format-card multi-xml))})

(defn- format-setinfo [set-xml]
  {:code         (xml1-> set-xml :code text)
   :name         (xml1-> set-xml :name text)
   :release-date (xml1-> set-xml :release-date text)})

(defn- format-instance [inst-xml]
  {:set    (xml1-> inst-xml :set text)
   :rarity (xml1-> inst-xml :rarity text)})

(defn- format-meta [meta-xml]
  {:card      (to-ascii (xml1-> meta-xml (attr :name)))
   :instances (xml-> meta-xml :instance format-instance)})

;;--------------------------------------------------
;; data reading

(defn- get-xml [directory]
  (reduce (fn [m filename]
            (let [xml (-> (File. directory (str filename ".xml"))
                          clojure.xml/parse
                          clojure.zip/xml-zip)]
              (assoc m (keyword filename) xml)))
          nil
          ["cards" "setinfo" "meta"]))

(defn- format-xml [data]
  (-> data
      (update-in [:cards] xml-> :card format-card)
      (update-in [:cards] (partial sort-by :name))
      (update-in [:setinfo] xml-> :set format-setinfo)
      (update-in [:setinfo] (fn [coll]
                              (->> (sort-by :release-date coll)
                                   (map-indexed #(assoc %2 :order %1))
                                   (reduce #(assoc %1 (:name %2) %2) nil))))
      (update-in [:meta] xml-> :card format-meta)
      (update-in [:meta] (partial reduce
                                  #(assoc %1 (:card %2) (:instances %2))
                                  nil))))

;;--------------------------------------------------
;; output

(defn- combine-protection-text [strs]
  (->> strs
       (partition-by #(if (.startsWith % "Protection from ")
                        :protection
                        (gensym)))
       (map (fn [items]
              (if (= 1 (count items))
                (first items)
                (let [prot-from (map #(.substring % 11) items)]
                  (str "Protection "
                       (case (count prot-from)
                         2 (apply format "%s and %s" prot-from)
                         3 (apply format "%s, %s and %s" prot-from)))))))))

(defn- combine-rule-text [strs]
  (->> (remove nil? strs)
       combine-protection-text
       (map (fn [string]
              (str (clojure.string/lower-case (first string))
                   (.substring string 1))))
       (clojure.string/join ", ")
       capitalize))

(defn- combine-rules [rules]
  (map (fn [combo]
         (if (= 1 (count combo))
           (first combo)
           {:text (combine-rule-text (map :text combo))
            :reminder (combine-rule-text (map :reminder combo))}))
       (partition-by :no rules)))

(defn- str-rules [rules]
  (->> (combine-rules rules)
       (map (fn [{:keys [text reminder]}]
              (apply str text (when-not (empty? reminder)
                                (str (when text " ")
                                     "(" reminder ")")))))
       (apply unlines)))

(defn- str-typeline [types]
  (let [[pre post] (split-with #(#{"super" "card"} (:type %))
                               types)
        pre (clojure.string/join " " (map :name pre))
        post (clojure.string/join " " (map :name post))]
    (if (empty? post)
      pre
      (str pre " - " post))))

(defn- str-card-part [card]
  (unlines (:name card)
           (:cost card)
           (when (:color card)
             (format "(%s)" (:color card)))
           (str-typeline (:typelist card))
           (when (:pow card)
             (format "%s/%s" (:pow card) (:tgh card)))
           (when-not (empty? (:rulelist card))
             (str-rules (:rulelist card)))))

(defn- get-set-rarity
  [card setinfo meta]
  (let [instances (sort-by (fn [{set-name :set}]
                             (get-in setinfo [set-name :order]))
                           (get meta (:name card)))]
    (map (fn [{set-name :set, rarity :rarity}]
           (str (get-in setinfo [set-name :code]) " " rarity))
         instances)))

(defn- str-card-meta [card setinfo meta]
  (->> (get-set-rarity card setinfo meta)
       (partition-by identity)
       (map (fn [[item & items]]
              (if (pos? (count items))
                (format "%s (x%d)" item (inc (count items)))
                item)))
       (clojure.string/join ", ")))

(defn- str-card [card setinfo meta]
  (unlines (str-card-part card)
           (when (:multi card)
             (unlines "----"
                      (str-card-part (:multi card))))
           (str-card-meta card setinfo meta)))

(defn- str-setinfo [setinfo]
  (apply unlines
         (map (fn [{:keys [code name release-date]}]
                (format "%-6s  %-10s  %s"
                        (or code "")
                        (or release-date "")
                        (or name "")))
              (sort-by :code (vals setinfo)))))

;;--------------------------------------------------
;; main

(defn- write-file [output-path cards setinfo meta]
  (with-open [f (clojure.java.io/writer output-path)]
    (.write f (str-setinfo setinfo))
    (.write f "\n\n")
    (doseq [c cards]
      (.write f (str-card c setinfo meta))
      (.write f "\n\n"))))

(defn -main [& [directory output-path & _]]
  (let [{:keys [cards setinfo meta] :as data}
        (format-xml (get-xml directory))]
    (write-file output-path cards setinfo meta)))
