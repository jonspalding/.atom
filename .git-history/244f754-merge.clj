(ns am.data-receptionist.models.locations-index.update.merge
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [components-bootstrap.components.es :as es]
            [am.data-receptionist.models.locations-index.polyline :as polyline]
            [meridian.clj-jts :as jts]
            [clojure.walk :refer [postwalk]]
            [schema.core :as s]
            [clojure.pprint :refer [print-table]]
            [am.data-receptionist.models.locations-index.update.changeset :as changeset]
            [am.data-receptionist.models.locations-index.update.es-interceptor :refer [make-es-location-storage]]))

(defn- fix-linear-rings [shape]
  (postwalk (fn [x] (if (nil? (s/check am.data.schemas.geo/GeoJsonLinearRing x))
                      (conj x (first x))
                      x))
            shape))

(defn- decode-polyline [p]
  (->> p
       polyline/decode
       (map #(clojure.set/rename-keys % {:latitude  :lat
                                         :longitude :lon}))
       vec
       (conj [])
       am.data.geo/->geo-json
       fix-linear-rings))

;(defn- encode-polyline [p]
;  (->> p
;       am.data.geo/geo-json->
;       (map (fn [x] (map #(clojure.set/rename-keys % {:lat :latitude
;                                                      :lon :longitude}) x)))
;       (map polyline/encode)))

(def ^:private jts-types {"polygon"      :Polygon
                          "multipolygon" :MultiPolygon})


(defn is-name-unique-in-hierarchy? [es name]
  (let [hits  (-> (es/search es "locations" "location" {:fields ["name"],
                                                        :query {
                                                          :filtered {
                                                            :query {
                                                              :match {
                                                                :name name
                                                              }
                                                            }
                                                            :filter {
                                                              :and {
                                                                :filters [
                                                                  {
                                                                    :exists {
                                                                      :field "name_suggest"
                                                                    }
                                                                  }
                                                                ]
                                                              }
                                                            }
                                                          }
                                                        }
                                                      })
                  :hits
                  :hits)
        exact-matches (->> hits
                           (filter #(= name (-> % :fields :name first))))]
    (= 1 (count exact-matches))))

(defn calculate-action [rm-polygon am-polygon intersects rm-area am-area rm-only am-only unique-name]
  (let [diff (- (* (/ 100 am-area) rm-area) 100)]
    (cond (not intersects)
          :no-intersect

          (or (< diff -50)
              (> diff 100))
          (if unique-name :merge :manual-review)

          (.contains rm-polygon am-polygon)
          :merge

          (.contains am-polygon rm-polygon)
          :no-op

          (and rm-only am-only)
          :merge

          :else
          :no-action-found)))

(defn merge-location [es location]
  (if-let [es-location (if (empty? (:id location))
                           (-> (es/search es "locations" "location" {:query {:term {:seo_id (:seo-id location)}}})
                               :hits
                               :hits
                               first)
                           (es/get es "locations" "location" (:id location)))]
    (do
      (let [rm-coords (decode-polyline (or (:polygon location)
                                           (:simplified-polygon location)))
            rm-polygon (.buffer (jts/polygon rm-coords) 0)
            am-geo-shape (-> es-location :_source :geo_shape)
            am-polygon (jts/map->jts (-> am-geo-shape
                                         (update-in [:type] #(get jts-types %))
                                         (update-in [:coordinates] fix-linear-rings)))
            intersects (.intersects am-polygon rm-polygon)
            rm-area (.getArea rm-polygon)
            am-area (.getArea am-polygon)
            merged (.union am-polygon rm-polygon)
            merged-area (.getArea merged)
            intersection (.intersection am-polygon rm-polygon)
            intersection-area (.getArea intersection)
            rm-only (.difference rm-polygon am-polygon)
            rm-only-area (.getArea rm-only)
            am-only (.difference am-polygon rm-polygon)
            am-only-area (.getArea am-only)
            unique-name (is-name-unique-in-hierarchy? es (-> es-location :_source :name))
            action (calculate-action rm-polygon am-polygon intersects rm-area am-area rm-only am-only unique-name)]
        {:id             (:_id es-location)
         :am-geo-shape   am-geo-shape
         :seo-id         (:seo-id location)
         :level          (-> es-location :_source :level)
         :intersects     intersects
         :rm-area        (format "%.5f" rm-area)
         :rm-polygon     rm-polygon
         :am-area        (format "%.5f" am-area)
         :am-polygon     am-polygon
         :merged-area    (format "%.5f" merged-area)
         :merged-polygon merged
         :area-diff      (format "%.2f%%" (- (* (/ 100 am-area) rm-area) 100))
         :intersection   intersection
         :intersection-area (format "%.5f" intersection-area)
         :rm-only        rm-only
         :rm-only-area   (format "%.5f" rm-only-area)
         :am-only        am-only
         :am-only-area   (format "%.5f" am-only-area)
         :action         action}))
      (select-keys location [:id :seo-id])))

(def ^:private header [:seo-id :centroid :simplified-polygon :polygon :id :url :msl :parent :canonical])

(defn- ->geo-json-feature [name color poly]
  {:type       "Feature"
   :properties {:name  name
                :color color}
   :geometry   (jts/->shape poly)})

(defn- ->geo-json-feature-collection [poly-map]
  {:type     "FeatureCollection"
   :features [(->geo-json-feature "AM" "blue" (:am-polygon poly-map))
              (->geo-json-feature "RM" "green" (:rm-polygon poly-map))]})

(defn- location-presenter-html [location]
  (format "<tr class=\"location\" data-geo-json='%s'>
            <td class=\"id\">%s</td>
            <td class=\"seo-id\">%s</td>
            <td class=\"level\">%d</td>
            <td class=\"intersects\">%b</td>
            <td class=\"rm-area\">%s</td>
            <td class=\"am-area\">%s</td>
            <td class=\"area-diff\">%s</td>
            <td class=\"merged-area\">%s</td>
            <td class=\"intersection-area\">%s</td>
            <td class=\"rm-only-area\">%s</td>
            <td class=\"am-only-area\">%s</td>
           </tr>\n"
          (cheshire.core/encode (->geo-json-feature-collection location))
          (:id location) (:seo-id location) (:level location) (:intersects location)
          (:rm-area location) (:am-area location) (:area-diff location)
          (:merged-area location) (:intersection-area location) (:rm-only-area location)
          (:am-only-area location)))

(defn- write-file [es name locations]
  (println (count locations))
  (let [html-header (slurp "header.html")
        html-footer (slurp "footer.html")
        filename (str "rm-vs-am-" name ".html")]
    (with-open [out-file (io/writer filename)]
      (println "Writing to " filename)
      (.write out-file html-header)
      (doall
        (map #(.write out-file (location-presenter-html %)) locations))
      (.write out-file html-footer))))

(defn build-html [es file]
  (with-open [csv-file (io/reader file)]
    (->> (drop 1 (doall (csv/read-csv csv-file)))
         (map #(merge-location es (zipmap header %)))
         (group-by :action)
         (mapv (fn [[action locations]]
                  (println action ":" (count locations))
                  (write-file es (name action) locations))))))

(defn make-attribute-set-change [action-field es id field value old-value]
  {:action :set-attribute
   :id id
   :field action-field
   :old-value old-value
   :new-value value})

(defn ->change [es location]
  ; (println "location" location)
  (make-attribute-set-change :geo-shape
                             (make-es-location-storage es)
                             (:id location)
                             :geo_shape
                             (-> location
                                 :merged-polygon
                                 jts/->shape
                                 (select-keys [:type :coordinates]))
                             (:am-geo-shape location)))

(defn ->changeset [es locations]
  (map (partial ->change es) locations))

(defn build-changeset [es file outfile]
  (with-open [csv-file (io/reader file)]
    (let [cs (->> (drop 1 (doall (csv/read-csv csv-file)))
                  (map #(merge-location es (zipmap header %)))
                  (filter #(= :merge (:action %)))
                  (->changeset es))]
      (changeset/save-changeset cs outfile))))
