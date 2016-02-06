(ns ewen.prismic-kit.core
  (:require [clj-http.client :as http-client]
            [clojure.set :refer [rename-keys]]
            [slingshot.slingshot :refer [try+ throw+]]
            [clojure.pprint :refer [pp pprint]]))

(defn span-with-offset [span offset]
  (-> (update-in span [:start] - offset)
      (update-in [:end] - offset)))

(defn split-by-hyperlinks [text spans]
  (loop [start 0
         text-offset 0
         current-text (StringBuilder.)
         spans spans
         current-spans []
         splitted-texts []
         hyperlink-end nil
         hyperlink-data nil]
    (if-let [{span-start :start span-end :end span-type :type :as span}
             (first spans)]
      (cond (and (not hyperlink-end) (not= "hyperlink" span-type))
            (recur span-end
                   text-offset
                   (.append current-text (subs text start span-end))
                   (rest spans)
                   (conj current-spans (span-with-offset span text-offset))
                   splitted-texts
                   nil nil)
            (and (not hyperlink-end) (= "hyperlink" span-type))
            (let [link-text (StringBuilder.
                             (subs text span-start span-end))]
              (recur span-end
                     span-start
                     link-text
                     (rest spans)
                     []
                     (conj splitted-texts
                           {:text (.toString current-text)
                            :spans current-spans
                            :is-hyperlink false})
                     span-end
                     (:data span)))
            (and hyperlink-end (= "hyperlink" span-type))
            (throw (Exception. "Error while processing spans"))
            (and hyperlink-end (not= "hyperlink" span-type))
            (if (>= span-start hyperlink-end)
              (recur span-end
                     hyperlink-end
                     (StringBuilder. (subs text hyperlink-end span-end))
                     (rest spans)
                     [(span-with-offset span hyperlink-end)]
                     (conj splitted-texts
                           {:text (.toString current-text)
                            :spans current-spans
                            :data hyperlink-data
                            :is-hyperlink true})
                     nil nil)
              (recur start
                     text-offset
                     current-text
                     (rest spans)
                     (conj current-spans
                           (span-with-offset span text-offset))
                     splitted-texts
                     hyperlink-end
                     hyperlink-data)))
      (conj splitted-texts {:text (.toString
                                   (.append current-text
                                            (subs text start
                                                  (count text))))
                            :spans current-spans
                            :is-hyperlink (if hyperlink-end
                                            true false)}))))

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

(defn document-link-as-html [data link-resolver]
  (format "<a href=\"%s\"></a>"
          (if link-resolver (link-resolver data) "#")))

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
    (throw (Exception. (str "Invalid link type: %s" (:type data))))))

(defn text-as-html
  ([text spans]
   (text-as-html text spans nil))
  ([text spans link-resolver]
   (let [splitted-text (split-by-hyperlinks text spans)]
     (apply str (map
                 (fn [{:keys [text spans is-hyperlink data]}]
                   (if is-hyperlink
                     (hyperlink-as-html text spans data link-resolver)
                     (text-as-html* text spans)))
                 splitted-text)))))

(comment

  (pprint (text-as-html* "text text ff link e"
                            [{:start 5, :end 9, :type "strong"}
                             {:start 5, :end 9, :type "em"}
                             {:start 11, :end 13, :type "em"}]))

  (pprint (split-by-hyperlinks "text text ff link e ddd"
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

(defn paragraph-as-html
  ([fragment]
   (paragraph-as-html fragment nil))
  ([{:keys [text spans]} link-resolver]
   (format "<p>%s</p>" (if link-resolver
                         (text-as-html text spans link-resolver)
                         (text-as-html text spans)))))
(defn heading1-as-html
  ([fragment]
   (heading1-as-html fragment nil))
  ([{:keys [text spans]} link-resolver]
   (format "<h1>%s</h1>" (if link-resolver
                           (text-as-html text spans link-resolver)
                           (text-as-html text spans)))))
(defn heading2-as-html
  ([fragment]
   (heading2-as-html fragment nil))
  ([{:keys [text spans]} link-resolver]
   (format "<h2>%s</h2>" (if link-resolver
                           (text-as-html text spans link-resolver)
                           (text-as-html text spans)))))
(defn heading3-as-html
  ([fragment]
   (heading3-as-html fragment nil))
  ([{:keys [text spans]} link-resolver]
   (format "<h3>%s</h3>" (if link-resolver
                           (text-as-html text spans link-resolver)
                           (text-as-html text spans)))))
(defn heading4-as-html
  ([fragment]
   (heading4-as-html fragment nil))
  ([{:keys [text spans]} link-resolver]
   (format "<h4>%s</h4>" (if link-resolver
                           (text-as-html text spans link-resolver)
                           (text-as-html text spans)))))
(defn heading5-as-html
  ([fragment]
   (heading5-as-html fragment nil))
  ([{:keys [text spans]} link-resolver]
   (format "<h5>%s</h5>" (if link-resolver
                           (text-as-html text spans link-resolver)
                           (text-as-html text spans)))))
(defn heading6-as-html
  ([fragment]
   (heading6-as-html fragment nil))
  ([{:keys [text spans]} link-resolver]
   (format "<h6>%s</h6>" (if link-resolver
                           (text-as-html text spans link-resolver)
                           (text-as-html text spans)))))

(defn image-view-as-html [{:keys [url alt]}]
  (format "<img src=\"%s\" alt=\"%s\">" url alt))

(defn list-item-as-html [{:keys [text spans]} link-resolver]
  (let [list-html (StringBuilder.)]
    (.append list-html "<li>")
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
       ::text (text-as-html* (:text fragment) (:spans fragment))
       ::image (image-view-as-html (:main fragment))
       ::embed (:html fragment)
       ::link-document (document-link-as-html fragment link-resolver)
       ::link-web (format "<a href=\"%s\"></a>" (:url fragment))
       ::link-image (format "<a href=\"%s\"></a>" (:url fragment))
       ::group nil
       ::paragraph (paragraph-as-html fragment link-resolver)
       ::heading1 (heading1-as-html fragment link-resolver)
       ::heading2 (heading2-as-html fragment link-resolver)
       ::heading3 (heading3-as-html fragment link-resolver)
       ::heading4 (heading4-as-html fragment link-resolver)
       ::heading5 (heading5-as-html fragment link-resolver)
       ::heading6 (heading6-as-html fragment link-resolver)
       ::o-list (o-list-as-html fragment link-resolver)
       ::o-list-item (list-item-as-html fragment link-resolver)
       ::list (list-as-html fragment link-resolver)
       ::list-item (list-item-as-html fragment link-resolver)
       ::image-view (image-view-as-html fragment)
       (throw (Exception.
               (str "Unexpected type while generating HTML: " type)))))))

(defn structured-text-add-list-item
  [{:keys [path] :as context} state {:keys [type] :as value}]
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

(defn parse-view [{:keys [path] :as context} [view-name view]]
  (let [path (conj path view-name)]
    [view-name
     (with-meta view (assoc context ::type ::image-view ::path path))]))

(defn parse-image [context image]
  (let [parse-view (partial parse-view context)]
    (with-meta (into {} (map parse-view image))
      {::type ::image})))

(defn structured-text-reducer
  [{:keys [document path] :as context} state {:keys [type] :as value}]
  (let [index (count state)
        path (conj path index)
        structured-text-add-list-item
        (partial structured-text-add-list-item (assoc context :path path))
        state
        (case type
          "o-list-item" (structured-text-add-list-item state value)
          "list-item" (structured-text-add-list-item state value)
          "embed" (->> (with-meta (:oembed value) {::type ::embed})
                       (conj state))
          "image" (->> (parse-image context {:main (dissoc value :type)})
                       (conj state))
          "paragraph" (->> (with-meta (dissoc value :type)
                             {::type ::paragraph})
                           (conj state))
          "heading1" (->> (with-meta (dissoc value :type)
                            {::type ::heading1})
                          (conj state))
          "heading2" (->> (with-meta (dissoc value :type)
                            {::type ::heading2})
                          (conj state))
          "heading3" (->> (with-meta (dissoc value :type)
                            {::type ::heading3})
                          (conj state))
          "heading4" (->> (with-meta (dissoc value :type)
                            {::type ::heading4})
                          (conj state))
          "heading5" (->> (with-meta (dissoc value :type)
                            {::type ::heading5})
                          (conj state))
          "heading6" (->> (with-meta (dissoc value :type)
                            {::type ::heading6})
                          (conj state))
          (throw
           (str "Error while parsing StructuredText. Unknown type: "
                type)))]
    (conj (pop state)
          (vary-meta (peek state) assoc ::document document ::path path))))

(defn parse-structured-text [context value]
  (-> (partial structured-text-reducer context)
      (reduce [] value)
      (with-meta {::type ::structured-text})))

(defn parse-field [{:keys [document path] :as context}
                   [field-name {:keys [type value] :as field}]]
  (let [path (conj path field-name)
        field (case type
                "StructuredText" (parse-structured-text
                                  (assoc context ::path path) value)
                "Image" (parse-image context value)
                "Embed" (with-meta (:oembed value) {::type ::embed})
                "Text" (with-meta {:text value :spans []}
                         {::type ::text})
                "Link.document" (-> (:document value)
                                    (merge (dissoc value :document))
                                    (rename-keys {:isBroken :is-broken})
                                    (with-meta {::type ::link-document}))
                "Link.web" (with-meta value {::type ::link-web})
                "Link.image" (with-meta (:image value)
                               {::type ::link-image})
                "Group" (with-meta [] {::type ::group})
                (throw
                 (Exception. (str "Invalid prismic field type: " type))))]
    [field-name (vary-meta field assoc ::document document ::path path)]))

(defn parse-document [{:keys [type data] :as document}]
  (let [document (get data (keyword type))
        context (-> (dissoc document :data)
                  (assoc ::type ::document))
        parse-field (partial parse-field {:document document})]
    (-> (into {} (map parse-field document))
        (with-meta context))))

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

  (parse-document
   {:id "Vq51byQAAEMxi9qn",
    :uid "titre1",
    :type "document1",
    :href
    "https://blogtemplateegr.cdn.prismic.io/api/documents/search?ref=VrYAvigAAEAAxiJO&q=%5B%5B%3Ad+%3D+at%28document.id%2C+%22Vq51byQAAEMxi9qn%22%29+%5D%5D",
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
      :slug "titre1"}],
    :data
    {:document1
     {:tt {:type "Text", :value "hhh"},
      :title1
      {:type "StructuredText",
       :value [{:type "heading1", :text "Titre1", :spans []}]},
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
          "<iframe width=\"480\" height=\"270\" src=\"https://www.youtube.com/embed/H5N-xL2STKs?feature=oembed\" frameborder=\"0\" allowfullscreen></iframe>"}}
        {:type "image",
         :url
         "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
         :alt "",
         :copyright "",
         :dimensions {:width 960, :height 800}}
        {:type "paragraph", :text "P2", :spans []}]},
      :content2
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
         :caption {:type "Text", :value "caption"}}]}}}})

  (as-html *1)


  )

(comment

  (def options (atom {:url "https://blogtemplateegr.cdn.prismic.io/api"
                      :token "MC5WcTV2YkNRQUFLeFNpN1p5.WGJZZ17vv73vv71yPe-_ve-_ve-_vUEcGu-_vWTvv73vv73vv73vv71M77-9ee-_vVhu77-9cu-_ve-_ve-_vQ"}))
  (swap! options assoc :ref (master-ref @options))

  (refs api)

  (pprint (api options))

  (xml-parser (.openStream (io/resource "test.html")))

  )
