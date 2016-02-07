(ns ewen.prismic-kit.core
  (:require [clj-http.client :as http-client]
            [clojure.set :refer [rename-keys]]
            [slingshot.slingshot :refer [try+ throw+]]
            [clojure.pprint :refer [pp pprint]])
  (:import [java.time.format DateTimeFormatter]
           [java.time LocalDate LocalDateTime]))

(def date-formatter (DateTimeFormatter/ofPattern "yyy-MM-dd"))
(def timestamp-formatter (DateTimeFormatter/ofPattern "yyy-MM-dd'T'HH:mm:ssZ"))

(defn span-with-offset [span offset]
  (-> (update-in span [:start] - offset)
      (update-in [:end] - offset)))

(defn split-by-main-span [text spans]
  (loop [start 0
         text-offset 0
         current-text (StringBuilder.)
         spans spans
         current-spans []
         splitted-texts []
         main-span nil]
    (if-let [{span-start :start span-end :end span-type :type :as span}
             (first spans)]
      (cond (and (not main-span)
                 (not= "hyperlink" span-type)
                 (not= "label" span-type))
            (recur span-end
                   text-offset
                   (.append current-text (subs text start span-end))
                   (rest spans)
                   (conj current-spans (span-with-offset span text-offset))
                   splitted-texts
                   nil)
            (and (not main-span)
                 (or (= "hyperlink" span-type) (= "label" span-type)))
            (let [link-text (StringBuilder.
                             (subs text span-start span-end))
                  current-text (.append current-text
                                        (subs text start span-start))]
              (recur span-end
                     span-start
                     link-text
                     (rest spans)
                     []
                     (conj splitted-texts
                           {:text (.toString current-text)
                            :spans current-spans})
                     span))
            (and (:end main-span)
                 (or (= "hyperlink" span-type) (= "label" span-type)))
            (throw (Exception. "Error while processing spans"))
            (and (:end main-span)
                 (not= "hyperlink" span-type)
                 (not= "label" span-type))
            (if (>= span-start (:end main-span))
              (recur span-end
                     (:end main-span)
                     (StringBuilder. (subs text (:end main-span) span-end))
                     (rest spans)
                     [(span-with-offset span (:end main-span))]
                     (conj splitted-texts
                           (assoc main-span
                                  :text (.toString current-text)
                                  :spans current-spans))
                     nil)
              (recur start
                     text-offset
                     current-text
                     (rest spans)
                     (conj current-spans
                           (span-with-offset span text-offset))
                     splitted-texts
                     main-span)))
      (conj splitted-texts (assoc main-span
                                  :text (.toString
                                         (.append current-text
                                                  (subs text start
                                                        (count text))))
                                  :spans current-spans)))))

(defn next-spans [spans]
  (loop [spans spans
         next-spans []]
    (let [span (first spans)]
      (cond (not span)
            [next-spans spans]
            (empty? next-spans)
            (recur (next spans) (conj next-spans span))
            (= (:start span) (:start (peek next-spans)))
            (recur (next spans) (conj next-spans span))
            :else [next-spans spans]))))

(defn spans-as-html [text spans]
  (let [{:keys [start end type]} (first spans)]
    (if (empty? (rest spans))
      (format "<%s>%s</%s>"
              type (subs text start end) type)
      (format "<%s>%s</%s>"
              type (spans-as-html text (rest spans)) type))))

(defn text-as-html* [text spans]
  (loop [start 0
         spans spans
         content (StringBuilder.)]
    (let [[n-spans rest-spans] (next-spans spans)]
      (if (empty? n-spans)
        (.toString (.append content (subs text start (count text))))
        (recur (:end (first n-spans))
               rest-spans
               (doto content
                 (.append (subs text start (:start (first n-spans))))
                 (.append (spans-as-html text n-spans))))))))

(defn document-link-as-html [{:keys [slug] :as data} link-resolver]
  (format "<a href=\"%s\">%s</a>"
          (if link-resolver (link-resolver data) "#")
          slug))

(defn hyperlink-as-html [text spans data link-resolver]
  (case (:type data)
    "Link.web" (format "<a href=\"%s\">%s</a>"
                       (:url (:value data))
                       (text-as-html* text spans))
    "Link.document" (format "<a href=\"%s\">%s</a>"
                            (if link-resolver
                              (let [{:keys [document is-broken]}
                                    (:value data)]
                                (link-resolver
                                 (assoc document :is-broken is-broken)))
                              "#")
                            (text-as-html* text spans))
    "Link.image" (format "<a href=\"%s\">%s</a>"
                         (-> data :value :image :url)
                         (text-as-html* text spans))
    "Link.file" (format "<a href=\"%s\">%s</a>"
                        (-> data :value :file :url)
                        (text-as-html* text spans))
    (throw (Exception. (str "Invalid link type: %s" (:type data))))))

(defn label-span-as-html [text spans data]
  (format "<span class=\"%s\">%s</span>"
          (:label data)
          (text-as-html* text spans)))

(defn text-as-html
  ([text spans]
   (text-as-html text spans nil))
  ([text spans link-resolver]
   (let [splitted-text (split-by-main-span text spans)]
     (apply str (map
                 (fn [{:keys [text spans type data]}]
                   (cond (= "hyperlink" type)
                         (hyperlink-as-html text spans data link-resolver)
                         (= "label" type)
                         (label-span-as-html text spans data)
                     :else (text-as-html* text spans)))
                 splitted-text)))))

(comment

  (pprint (text-as-html* "text text ff link e"
                            [{:start 5, :end 9, :type "strong"}
                             {:start 5, :end 9, :type "em"}
                             {:start 11, :end 13, :type "em"}]))

  (pprint (split-by-main-span "text text ff link e ddd"
                               [{:start 5, :end 6, :type "strong"}
                                {:start 6, :end 8, :type "strong"}
                                {:start 6, :end 8, :type "em"}
                                {:start 8, :end 9, :type "strong"}
                                {:start 10, :end 11, :type "em"}
                                {:start 11, :end 13, :type "em"}
                                {:start 11, :end 13, :type "strong"}
                                {:start 13,
                                 :end 17,
                                 :type "hyperlink",
                                 :data
                                 {:type "Link.web", :value {:url "http://www.google.fr"}}}
                                {:start 13, :end 17, :type "em"}
                                {:start 13, :end 17, :type "strong"}
                                {:start 18, :end 21, :type "strong"}]))

  (pprint (split-by-main-span "P2ggg"
                              [{:start 0, :end 1, :type "strong"}
                               {:start 1, :end 2, :type "strong"}
                               {:start 1, :end 2, :type "em"}
                               {:start 2, :end 5, :type "label", :data {:label "author"}}
                               {:start 2, :end 4, :type "strong"}
                               {:start 2, :end 4, :type "em"}
                               {:start 4, :end 5, :type "em"}]))

  (pprint (split-by-main-span "P2ggg"
                              [{:start 4,
                                :end 5,
                                :type "hyperlink",
                                :data
                                {:type "Link.file",
                                 :value
                                 {:file
                                  {:name "test.txt",
                                   :kind "document",
                                   :url
                                   "https://blogtemplateegr.cdn.prismic.io/blogtemplateegr%2F90263740-6c37-4fb9-be7e-f271963396e2_test.txt",
                                   :size "5"}}}}]))


  (text-as-html "text text ff link e ddd"
                [{:start 5, :end 6, :type "strong"}
                 {:start 6, :end 8, :type "strong"}
                 {:start 6, :end 8, :type "em"}
                 {:start 8, :end 9, :type "strong"}
                 {:start 10, :end 11, :type "em"}
                 {:start 11, :end 13, :type "em"}
                 {:start 11, :end 13, :type "strong"}
                 {:start 13,
                  :end 17,
                  :type "hyperlink",
                  :data
                  {:type "Link.document",
                   :value
                   {:document
                    {:id "Vq51byQAAEMxi9qn",
                     :type "document1",
                     :tags [],
                     :slug "titre1",
                     :uid "titre1"},
                    :isBroken false}}}
                 {:start 13, :end 17, :type "em"}
                 {:start 13, :end 17, :type "strong"}
                 {:start 17, :end 21, :type "strong"}]
                (fn [x] (prn x) "e"))
  )

(declare as-html)

(defn opt-attrs
  ([label direction]
   (let [opt-attrs (if label (format " class=\"%s\" " label) " ")]
     (if direction
       (format "%sdir=\"%s\" " opt-attrs direction)
       opt-attrs)))
  ([label direction other-class]
   (if (nil? other-class)
     (opt-attrs label direction)
     (let [opt-attrs (if label
                       (format " class=\"%s %s\" " other-class label)
                       (format " class=\"%s\" " other-class))]
       (if direction
         (format "%sdir=\"%s\" " opt-attrs direction)
         opt-attrs)))))

(defn paragraph-as-html
  ([fragment]
   (paragraph-as-html fragment nil))
  ([{:keys [text spans label direction]} link-resolver]
   (format "<p%s>%s</p>"
           (opt-attrs label direction)
           (if link-resolver
             (text-as-html text spans link-resolver)
             (text-as-html text spans)))))

(defn preformatted-as-html
  ([fragment]
   (preformatted-as-html fragment nil))
  ([{:keys [text spans label direction]} link-resolver]
   (format "<pre%s>%s</pre>"
           (opt-attrs label direction)
           (if link-resolver
             (text-as-html text spans link-resolver)
             (text-as-html text spans)))))

(defn heading-as-html
  ([fragment level]
   (heading-as-html fragment nil))
  ([{:keys [text spans label direction]} level link-resolver]
   (format "<h%s%s>%s</h%s>"
           level
           (opt-attrs label direction)
           (if link-resolver
             (text-as-html text spans link-resolver)
             (text-as-html text spans))
           level)))

(defn geopoint-as-html [{:keys [latitude longitude]}]
  (format "<div class=\"geopoint\"><span class=\"latitude\">%s</span><span class=\"longitude\">%s</span></div>" latitude longitude))

(defn image-view-as-html [{:keys [url alt dimensions
                                  label direction linkTo]}
                          link-resolver]
  (let [view-html
        (format "<img src=\"%s\" alt=\"%s\" width=\"%s\" height=\"%s\"/>"
                url alt (:width dimensions) (:height dimensions))
        link (when linkTo
               (case (:type linkTo)
                 "Link.image" (-> linkTo :value :image :url)
                 "Link.web" (-> linkTo :value :url)
                 "Link.file" (-> linkTo :value :file :url)
                 "Link.document" (if link-resolver
                                   (let [{:keys [document is-broken]}
                                         (:value linkTo)]
                                     (link-resolver
                                      (assoc
                                       document :is-broken is-broken)))
                                   "#")))
        link-html
        (if linkTo
          (format "<a href=\"%s\">%s</a>" link view-html)
          view-html)]
    (if (or label direction)
      (format "<p%s>%s</p>"
              (opt-attrs label direction "block-img") link-html)
      link-html)))

(defn embed-as-html [{:keys [html type embed_url provider_name
                             label direction]}]
  (format "<div%sdata-oembed=\"%s\" data-oembed-type=\"%s\" data-oembed-provider=\"%s\">%s</div>"
          (opt-attrs label direction) embed_url type provider_name html))

(defn list-item-as-html [{:keys [text spans label direction]} link-resolver]
  (let [list-html (StringBuilder.)]
    (.append list-html (format "<li%s>" (opt-attrs label direction)))
    (.append list-html (text-as-html text spans link-resolver))
    (.append list-html "</li>")
    (.toString list-html)))

(defn list-as-html [items link-resolver]
  (let [list-html (StringBuilder.)]
    (.append list-html "<ul>")
    (doseq [item items]
      (.append list-html (as-html item link-resolver)))
    (.append list-html "</ul>")
    (.toString list-html)))

(defn o-list-as-html [items link-resolver]
  (let [list-html (StringBuilder.)]
    (.append list-html "<ol>")
    (doseq [item items]
      (.append list-html (as-html item link-resolver)))
    (.append list-html "</ol>")
    (.toString list-html)))

(defn structured-text-as-html
  [structured-text link-resolver]
  (let [structured-text-html (StringBuilder.)]
    (doseq [subfield structured-text]
      (.append structured-text-html (as-html subfield link-resolver)))
    (.toString structured-text-html)))

(defn group-as-html
  ([group]
   (group-as-html group nil))
  ([group link-resolver]
   (let [group-html (StringBuilder.)]
     (doseq [group-item group]
       (.append group-html (as-html group-item link-resolver)))
     (.toString group-html))))

(defn slice-as-html
  ([slice]
   (slice-as-html slice nil))
  ([slice link-resolver]
   (let [slice-html (StringBuilder.)]
     (doseq [slice-item slice]
       (let [slice-type (:slice_type (meta slice-item))
             slice-label (:slice_label (meta slice-item))]
         (.append slice-html (format "<div%s data-slicetype=\"%s\">"
                                     (opt-attrs slice-label nil "slice")
                                     slice-type))
         (.append slice-html (as-html slice-item link-resolver))
         (.append slice-html "</div>")))
     (.toString slice-html))))

(defn document-as-html
  ([document]
   (document-as-html document nil))
  ([document link-resolver]
   (let [document-html (StringBuilder.)]
     (doseq [[field-name field] document]
       (.append document-html (as-html field link-resolver)))
     (.toString document-html))))

(defn as-html
  ([fragment]
   (as-html fragment nil))
  ([fragment link-resolver]
   (let [{type ::type} (meta fragment)]
     (case type
       ::document (document-as-html fragment link-resolver)
       ::structured-text (structured-text-as-html fragment link-resolver)
       ::text (format "<span class=\"text\">%s</span>"
                      (text-as-html* (:text fragment) (:spans fragment)))
       ::number (format "<span class=\"number\">%s</span>"
                        (:value fragment))
       ::color (format "<span class=\"color\">%s</span>" (:value fragment))
       ::date (format "<time>%s</time>" (:value fragment))
       ::range (format "<span class=\"number\">%s</span>" (:value fragment))
       ::select (format "<span class=\"text\">%s</span>" (:value fragment))
       ::timestamp (format "<time>%s</time>" (:value fragment))
       ::geopoint (geopoint-as-html fragment)
       ::image (image-view-as-html (:main fragment) link-resolver)
       ::embed (embed-as-html fragment)
       ::link-document (document-link-as-html fragment link-resolver)
       ::link-web (format "<a href=\"%s\">%s</a>"
                          (:url fragment) (:url fragment))
       ::link-image (format "<img src=\"%s\" alt=\"%s\"/>"
                            (:url fragment) (:alt fragment))
       ::link-file (format "<a href=\"%s\">%s</a>"
                           (:url fragment) (:name fragment))
       ::group (group-as-html fragment link-resolver)
       ;;group-items and documents have the same structure
       ::group-item (document-as-html fragment link-resolver)
       ::slice (slice-as-html fragment link-resolver)
       ::paragraph (paragraph-as-html fragment link-resolver)
       ::heading1 (heading-as-html fragment 1 link-resolver)
       ::heading2 (heading-as-html fragment 2 link-resolver)
       ::heading3 (heading-as-html fragment 3 link-resolver)
       ::heading4 (heading-as-html fragment 4 link-resolver)
       ::heading5 (heading-as-html fragment 5 link-resolver)
       ::heading6 (heading-as-html fragment 6 link-resolver)
       ::o-list (o-list-as-html fragment link-resolver)
       ::o-list-item (list-item-as-html fragment link-resolver)
       ::list (list-as-html fragment link-resolver)
       ::list-item (list-item-as-html fragment link-resolver)
       ::image-view (image-view-as-html fragment link-resolver)
       ::preformatted (preformatted-as-html fragment)
       (throw (Exception.
               (str "Unexpected type while generating HTML: " type)))))))

(defn structured-text-add-list-item
  [{path ::path :as context} state {:keys [type] :as value}]
  (let [maybe-list (peek state)
        maybe-list-type (::type (meta maybe-list))
        [list-type item-type]
        (cond (= "list-item" type) [::list ::list-item]
              (= "o-list-item" type) [::o-list ::o-list-item]
              :else
              (throw
               (Exception.
                (str
                 "Error while parsing list items. Unexpected type: "
                 type))))
        value (dissoc value :type)]
    (cond
      (or (and (= ::list-item item-type) (= maybe-list-type ::list))
          (and (= ::o-list-item item-type) (= maybe-list-type ::o-list)))
      (let [path (conj path (count maybe-list))
            context (assoc context ::type item-type ::path path)]
        (->> (conj maybe-list (with-meta value context))
             (conj (pop state))))
      :else
      (let [context (assoc context ::type item-type ::path (conj path 0))]
        (conj state (with-meta [(with-meta value context)]
                      (assoc context ::type list-type)))))))

(defn parse-view [context [view-name view]]
  (let [context (update-in context [::path] conj view-name)]
    [view-name (with-meta view (assoc context ::type ::image-view))]))

(defn parse-image [context {:keys [views] :as image}]
  (let [image (-> (dissoc image :views)
                  (merge views))
        parse-view (partial parse-view context)]
    (with-meta (into {} (map parse-view image))
      (assoc context ::type ::image))))

(defn structured-text-reducer
  [{document ::document path ::path :as context}
   state {:keys [type] :as value}]
  (let [index (count state)
        {path ::path :as context} (update-in context [::path] conj index)
        state
        (case type
          "o-list-item" (let [structured-text-add-list-item
                              (partial
                               structured-text-add-list-item context)]
                          (structured-text-add-list-item state value))
          "list-item" (let [structured-text-add-list-item
                            (partial
                             structured-text-add-list-item context)]
                        (structured-text-add-list-item state value))
          "embed" (conj state
                        (-> (:oembed value)
                            (merge (dissoc value :type :oembed))
                            (with-meta (assoc context ::type ::embed))))
          "image" (->> (parse-image context {:main (dissoc value :type)})
                       (conj state))
          "preformatted" (->> (with-meta (dissoc value :type)
                                (assoc context ::type ::preformatted))
                              (conj state))
          "paragraph" (->> (with-meta (dissoc value :type)
                             (assoc context ::type ::paragraph))
                           (conj state))
          "heading1" (->> (with-meta (dissoc value :type)
                            (assoc context ::type ::heading1))
                          (conj state))
          "heading2" (->> (with-meta (dissoc value :type)
                            (assoc context ::type ::heading2))
                          (conj state))
          "heading3" (->> (with-meta (dissoc value :type)
                            (assoc context ::type ::heading3))
                          (conj state))
          "heading4" (->> (with-meta (dissoc value :type)
                            (assoc context ::type ::heading4))
                          (conj state))
          "heading5" (->> (with-meta (dissoc value :type)
                            (assoc context ::type ::heading5))
                          (conj state))
          "heading6" (->> (with-meta (dissoc value :type)
                            (assoc context ::type ::heading6))
                          (conj state))
          (throw
           (str "Error while parsing StructuredText. Unknown type: "
                type)))]
    (conj (pop state) (peek state))))

(defn parse-structured-text [context value]
  (-> (partial structured-text-reducer context)
      (reduce [] value)
      (with-meta (assoc context ::type ::structured-text))))

(declare parse-field*)
(declare parse-field)

(defn parse-group-item [context index value]
  (let [context (update-in context [::path] conj index)
        parse-field (partial parse-field context)]
    (-> (into {} (map parse-field value))
        (with-meta (assoc context ::type ::group-item)))))

(defn parse-group [context value]
  (let [parse-group-item (partial parse-group-item context)]
    (-> (into [] (map-indexed parse-group-item value))
        (with-meta (assoc context ::type ::group)))))

(defn parse-slice-item [context index {:keys [value] :as slice-item}]
  (let [context (update-in context [::path] conj index)
        parse-field* (partial parse-field* context)
        other-meta (dissoc slice-item :type :value)]
    (-> (parse-field* value)
        (vary-meta merge other-meta))))

(defn parse-slice [context value]
  (let [parse-slice-item (partial parse-slice-item context)]
    (-> (into [] (map-indexed parse-slice-item value))
        (with-meta (assoc context ::type ::slice)))))

(defn parse-field* [context {:keys [type value] :as field}]
  (case type
    "StructuredText" (parse-structured-text context value)
    "Image" (parse-image context value)
    "Embed" (-> (:oembed value)
                (merge (dissoc value :type :oembed))
                (with-meta (assoc context ::type ::embed)))
    "Text" (with-meta {:text value :spans []}
             (assoc context ::type ::text))
    "Number" (with-meta {:value value}
               (assoc context ::type ::number))
    "Color" (with-meta {:value value}
              (assoc context ::type ::color))
    "Date" (with-meta {:value (LocalDate/parse value date-formatter)}
             (assoc context ::type ::date))
    "Range" (with-meta {:value value}
              (assoc context ::type ::range))
    "Select" (with-meta {:value value}
               (assoc context ::type ::select))
    "Timestamp" (with-meta {:value (LocalDateTime/parse
                                    value timestamp-formatter)}
                  (assoc context ::type ::timestamp))
    "GeoPoint" (with-meta value
                 (assoc context ::type ::geopoint))
    "Link.document" (-> (:document value)
                        (merge (dissoc value :document))
                        (rename-keys {:isBroken :is-broken})
                        (with-meta
                          (assoc context ::type ::link-document)))
    "Link.web" (with-meta value (assoc context ::type ::link-web))
    "Link.image" (with-meta (:image value)
                   (assoc context ::type ::link-image))
    "Link.file" (with-meta (:file value)
                  (assoc context ::type ::link-file))
    "Group" (parse-group context value)
    "SliceZone" (parse-slice context value)
    (throw
     (Exception. (str "Invalid prismic field type: " type)))))

(defn parse-field [{path ::path :as context}
                   [field-name {:keys [type value] :as field}]]
  (let [{path ::path :as context}
        (update-in context [::path] conj field-name)
        field (parse-field* context field)]
    [field-name field]))

(defn parse-document [{:keys [type data] :as document}]
  (let [document (get data (keyword type))
        context (-> (dissoc document :data)
                    (assoc ::type ::document))
        parse-field (partial parse-field {::document document
                                          ::path []})]
    (-> (into {} (map parse-field document))
        (with-meta context))))

(comment

  (parse-document
   {:id "Vq51byQAAEMxi9qn",
   :uid "titre1",
   :type "document1",
   :href
   "https://blogtemplateegr.cdn.prismic.io/api/documents/search?ref=Vreu4CgAAD4A0DJY&q=%5B%5B%3Ad+%3D+at%28document.id%2C+%22Vq51byQAAEMxi9qn%22%29+%5D%5D",
   :tags [],
   :slugs ["titre1"],
   :linked_documents
   [{:id "Vq51byQAAEMxi9qn",
     :tags [],
     :type "document1",
     :slug "titre1"}
    {:id "Vq51byQAAEMxi9qn",
     :tags [],
     :type "document1",
     :slug "titre1"}
    {:id "Vq51byQAAEMxi9qn",
     :tags [],
     :type "document1",
     :slug "titre1"}],
   :data
   {:document1
    {:content2
     {:type "Image",
      :value
      {:main
       {:url
        "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
        :alt "",
        :copyright "",
        :dimensions {:width 1500, :height 500}},
       :views
       {:medium
        {:url
         "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
         :alt "",
         :copyright "",
         :dimensions {:width 800, :height 250}},
        :icon
        {:url
         "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
         :alt "",
         :copyright "",
         :dimensions {:width 250, :height 250}}}}},
     :timestamp1
     {:type "Timestamp", :value "2016-03-02T23:00:00+0000"},
     :location1
     {:type "GeoPoint",
      :value {:latitude 66.0893642704709, :longitude -0.703125}},
     :embed1
     {:type "Embed",
      :value
      {:oembed
       {:author_url "https://www.youtube.com/user/malakoffmederic",
        :thumbnail_height 360,
        :thumbnail_url
        "https://i.ytimg.com/vi/H5N-xL2STKs/hqdefault.jpg",
        :width 480,
        :type "video",
        :embed_url "https://youtu.be/H5N-xL2STKs",
        :title "Campagne Malakoff Médéric 2014",
        :provider_name "YouTube",
        :author_name "Malakoff Médéric",
        :thumbnail_width 480,
        :version "1.0",
        :provider_url "https://www.youtube.com/",
        :height 270,
        :html
        "<iframe width=\"480\" height=\"270\" src=\"https://www.youtube.com/embed/H5N-xL2STKs?feature=oembed\" frameborder=\"0\" allowfullscreen></iframe>"}}},
     :content1
     {:type "StructuredText",
      :value
      [{:type "paragraph",
        :text "text text ff link e ddd",
        :spans
        [{:start 5, :end 6, :type "strong"}
         {:start 6, :end 8, :type "strong"}
         {:start 6, :end 8, :type "em"}
         {:start 8, :end 9, :type "strong"}
         {:start 10, :end 11, :type "em"}
         {:start 11, :end 13, :type "em"}
         {:start 11, :end 13, :type "strong"}
         {:start 13,
          :end 17,
          :type "hyperlink",
          :data
          {:type "Link.document",
           :value
           {:document
            {:id "Vq51byQAAEMxi9qn",
             :type "document1",
             :tags [],
             :slug "titre1",
             :uid "titre1"},
            :isBroken false}}}
         {:start 13, :end 17, :type "em"}
         {:start 13, :end 17, :type "strong"}
         {:start 17, :end 21, :type "strong"}],
        :direction "rtl"}
       {:type "o-list-item", :text "fff", :spans []}
       {:type "o-list-item", :text "dffd", :spans []}
       {:type "list-item", :text "ss", :spans []}
       {:type "list-item", :text "ddd", :spans []}
       {:type "preformatted",
        :text "hhhhddddddd",
        :spans [],
        :label "ingredient",
        :direction "rtl"}
       {:type "embed",
        :oembed
        {:author_url "https://www.youtube.com/user/malakoffmederic",
         :thumbnail_height 360,
         :thumbnail_url
         "https://i.ytimg.com/vi/H5N-xL2STKs/hqdefault.jpg",
         :width 480,
         :type "video",
         :embed_url "https://youtu.be/H5N-xL2STKs",
         :title "Campagne Malakoff Médéric 2014",
         :provider_name "YouTube",
         :author_name "Malakoff Médéric",
         :thumbnail_width 480,
         :version "1.0",
         :provider_url "https://www.youtube.com/",
         :height 270,
         :html
         "<iframe width=\"480\" height=\"270\" src=\"https://www.youtube.com/embed/H5N-xL2STKs?feature=oembed\" frameborder=\"0\" allowfullscreen></iframe>"},
        :label "author",
        :direction "rtl"}
       {:type "image",
        :url
        "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
        :alt "",
        :copyright "",
        :dimensions {:width 960, :height 800},
        :linkTo
        {:type "Link.image",
         :value
         {:image
          {:name "delete-button.png",
           :kind "image",
           :url
           "https://blogtemplateegr.cdn.prismic.io/blogtemplateegr%2F80358f59-e73d-4f26-8896-485b9fc98b01_delete-button.png",
           :size "1300",
           :height "30",
           :width "30"}}},
        :label "author",
        :direction "rtl"}
       {:type "paragraph",
        :text "P2ggg",
        :spans
        [{:start 0, :end 1, :type "strong"}
         {:start 1, :end 2, :type "strong"}
         {:start 1, :end 2, :type "em"}
         {:start 2, :end 5, :type "label", :data {:label "author"}}
         {:start 2, :end 4, :type "strong"}
         {:start 2, :end 4, :type "em"}
         {:start 4, :end 5, :type "em"}]}]},
     :tt {:type "Text", :value "hhh"},
     :gallery
     {:type "Group",
      :value
      [{:picture
        {:type "Image",
         :value
         {:main
          {:url
           "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
           :alt "",
           :copyright "",
           :dimensions {:width 960, :height 800}},
          :views
          {:regular
           {:url
            "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
            :alt "",
            :copyright "",
            :dimensions {:width 800, :height 500}},
           :icon
           {:url
            "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
            :alt "",
            :copyright "",
            :dimensions {:width 50, :height 50}}}}},
        :caption {:type "Text", :value "caption"}}
       {:picture
        {:type "Image",
         :value
         {:main
          {:url
           "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
           :alt "",
           :copyright "",
           :dimensions {:width 960, :height 800}},
          :views
          {:regular
           {:url
            "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
            :alt "",
            :copyright "",
            :dimensions {:width 800, :height 500}},
           :icon
           {:url
            "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
            :alt "",
            :copyright "",
            :dimensions {:width 50, :height 50}}}}},
        :caption {:type "Text", :value "caption2"}}]},
     :select1 {:type "Select", :value "opt1"},
     :range1 {:type "Range", :value "19"},
     :link1
     {:type "Link.document",
      :value
      {:document
       {:id "Vq51byQAAEMxi9qn",
        :type "document1",
        :tags [],
        :slug "titre1",
        :uid "titre1"},
       :isBroken false}},
     :title1
     {:type "StructuredText",
      :value
      [{:type "heading1", :text "Titre1", :spans [], :label "quote"}]},
     :body
     {:type "SliceZone",
      :value
      [{:type "Slice",
        :slice_type "featured-items",
        :slice_label "full-featured",
        :value
        {:type "Group",
         :value
         [{:illustration
           {:type "Image",
            :value
            {:main
             {:url
              "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
              :alt "",
              :copyright "",
              :dimensions {:width 1200, :height 1200}},
             :views
             {:icon
              {:url
               "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
               :alt "",
               :copyright "",
               :dimensions {:width 300, :height 300}}}}},
           :read-more
           {:type "Link.document",
            :value
            {:document
             {:id "Vq51byQAAEMxi9qn",
              :type "document1",
              :tags [],
              :slug "titre1",
              :uid "titre1"},
             :isBroken false}}}]}}]},
     :color1 {:type "Color", :value "#583535"},
     :date1 {:type "Date", :value "2016-02-02"},
     :number1 {:type "Number", :value 12.0}}}})

  (as-html *1)


  )

(defn api [{:keys [url token]}]
  (let [{:keys [status body] :as resp}
        (http-client/get url
                         {:as :json
                          :query-params {"access_token" token}})]
    (if (= 200 status)
      body
      (throw+ resp "Received HTTP status %s" status))))

(defn master-ref [{:keys [url token]}]
  (let [{:keys [status body] :as resp}
        (http-client/get url
                         {:as :json
                          :query-params {"access_token" token}})]
    (if (= 200 status)
      (-> (filter :isMasterRef (:refs body))
          first :ref)
      (throw+ resp "Received HTTP status %s" status))))

(defn search
  ([options] (search options nil))
  ([{:keys [url token ref]} q]
   (let [{:keys [status body] :as resp}
         (http-client/get (str url "/documents/search")
                          {:as :json
                           :query-params (merge {"access_token" token
                                                 "ref" ref}
                                                (when q {"q" q}))})]
     (if (= 200 status)
       body
       (throw+ resp "Received HTTP status %s" status)))))

(comment

  (def options (atom {:url "https://blogtemplateegr.cdn.prismic.io/api"
                      :token "MC5WcTV2YkNRQUFLeFNpN1p5.WGJZZ17vv73vv71yPe-_ve-_ve-_vUEcGu-_vWTvv73vv73vv73vv71M77-9ee-_vVhu77-9cu-_ve-_ve-_vQ"}))
  (swap! options assoc :ref (master-ref @options))

  (refs api)

  (pprint (api options))

  (xml-parser (.openStream (io/resource "test.html")))

  )
