(ns ewen.prismic-kit.core
  (:require [ewen.prismic-kit.fragment :refer
             [Html fragment-as-html ->List ->OList map->Paragraph
              map->Heading1 map->Heading2 map->Heading3 map->Heading4
              map->Heading5 map->Heading6 map->Embed ->Image
              map->DocumentLink ->StructuredText map->Image ->Text
              map->WebLink map->ImageLink map->Document]]
            [clj-http.client :as http-client]
            [clojure.set :refer [rename-keys]]
            [slingshot.slingshot :refer [try+ throw+]]
            [clojure.pprint :refer [pp pprint]])
  (:import [ewen.prismic_kit.fragment
            Paragraph Heading1 Heading2 Heading3 Heading4 Heading5 Heading6
            Embed Image List OList StructuredText Text DocumentLink
            WebLink ImageLink Document]))

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

(defn image-as-html [{{url :url alt :alt} :main}]
  (format "<img src=\"%s\" alt=\"%s\">" url alt))

(defn list-as-html [{:keys [items]}]
  (let [list-html (StringBuilder.)]
    (.append list-html "<ul>")
    (doseq [{:keys [text spans]} items]
      (.append list-html "<li>")
      (.append list-html (text-as-html text spans))
      (.append list-html "</li>"))
    (.append list-html "</ul>")
    (.toString list-html)))

(defn olist-as-html [{:keys [items]}]
  (let [list-html (StringBuilder.)]
    (.append list-html "<ol>")
    (doseq [{:keys [text spans]} items]
      (.append list-html "<li>")
      (.append list-html (text-as-html text spans))
      (.append list-html "</li>"))
    (.append list-html "</ol>")
    (.toString list-html)))

(defn structured-text-as-html*
  ([fragment]
   (structured-text-as-html* fragment nil))
  ([fragment link-resolver]
   (if (satisfies? Html fragment)
     (if link-resolver
       (fragment-as-html fragment link-resolver)
       (fragment-as-html fragment))
     (condp instance? fragment
       Paragraph (paragraph-as-html fragment link-resolver)
       Heading1 (heading1-as-html fragment)
       Heading2 (heading2-as-html fragment)
       Heading3 (heading3-as-html fragment)
       Heading4 (heading4-as-html fragment)
       Heading5 (heading5-as-html fragment)
       Heading6 (heading6-as-html fragment)
       Embed (:html fragment)
       Image (image-as-html fragment)
       List (list-as-html fragment)
       OList (olist-as-html fragment)
       (throw (Exception.
               (str "Unexpected type while generation HTML: "
                    (type fragment))))))))

(defmulti structured-text-as-html
  (fn [document-type field-name field & args]
    [document-type field-name (type field)]))

(defmethod structured-text-as-html :default
  ([document-type field-name field]
   (structured-text-as-html* field))
  ([document-type field-name field link-resolver]
   (structured-text-as-html* field link-resolver)))

(defn structured-text-field-as-html
  [{:keys [value] :as structured-text} link-resolver]
  (let [structured-text-html (StringBuilder.)]
    (doseq [val value]
      (.append structured-text-html (as-html val link-resolver)))
    (.toString structured-text-html)))

(defn field-as-html*
  ([field]
   (field-as-html* field nil))
  ([field link-resolver]
   (if (satisfies? Html field)
     (if link-resolver
       (fragment-as-html field link-resolver)
       (fragment-as-html field))
     (condp instance? field
       StructuredText (structured-text-field-as-html field link-resolver)
       Image (image-as-html field)
       Embed (:html field)
       Text (format "<p>%s</p>" (:value field))
       DocumentLink (document-link-as-html field link-resolver)
       WebLink (format "<a href=\"%s\"></a>" (:url field))
       ImageLink (format "<a href=\"%s\"></a>" (:url field))
       (throw (Exception.
               (str "Unexpected type while generating html: "
                    (type field))))))))

(defmulti field-as-html (fn [document-type field-name field & args]
                          [document-type field-name]))

(defmethod field-as-html :default
  ([document-type field-name field]
   (field-as-html* field))
  ([document-type field-name field link-resolver]
   (field-as-html* field link-resolver)))

(defn document-as-html*
  ([document]
   (document-as-html* document nil))
  ([{:keys [type data] :as document} link-resolver]
   (if (satisfies? Html document)
     (if link-resolver
       (fragment-as-html document link-resolver)
       (fragment-as-html document))
     (let [document-html (StringBuilder.)]
       (doseq [[field-name field] data]
         (.append document-html (as-html field link-resolver)))
       (.toString document-html)))))

(defmulti document-as-html (fn [document-type document & args]
                             document-type))

(defmethod document-as-html :default
  ([document-type document]
   (document-as-html* document))
  ([document-type document link-resolver]
   (document-as-html* document link-resolver)))

(defn as-html
  ([fragment]
   (as-html fragment nil))
  ([fragment link-resolver]
   (let [{:keys [document-type field-name in-structured-text?]} fragment]
     (condp instance? fragment
       Document (document-as-html document-type fragment link-resolver)
       StructuredText (field-as-html
                       document-type field-name
                       fragment link-resolver)
       Text (field-as-html document-type field-name fragment)
       DocumentLink (field-as-html
                     document-type field-name fragment link-resolver)
       WebLink (field-as-html document-type field-name fragment)
       ImageLink (field-as-html document-type field-name fragment)
       Image (if in-structured-text?
               (structured-text-as-html
                document-type field-name fragment link-resolver)
               (field-as-html document-type field-name fragment))
       Embed (if in-structured-text?
               (structured-text-as-html
                document-type field-name fragment link-resolver)
               (field-as-html document-type field-name fragment))
       Paragraph (structured-text-as-html
                  document-type field-name fragment link-resolver)
       Heading1 (structured-text-as-html
                 document-type field-name fragment)
       Heading2 (structured-text-as-html
                 document-type field-name fragment)
       Heading3 (structured-text-as-html
                 document-type field-name fragment)
       Heading4 (structured-text-as-html
                 document-type field-name fragment)
       Heading5 (structured-text-as-html
                 document-type field-name fragment)
       Heading6 (structured-text-as-html
                 document-type field-name fragment)
       List (structured-text-as-html
             document-type field-name fragment)
       OList (structured-text-as-html
              document-type field-name fragment)
       (throw (Exception.
               (str "Unexpected type while generation HTML: "
                    (type fragment))))))))

(defn structured-text-add-list-item [state {:keys [type] :as value}]
  (let [maybe-list (peek state)]
    (cond
      (or (and (= "list-item" type) (instance? List maybe-list))
          (and (= "o-list-item" type) (instance? OList maybe-list)))
      (conj (pop state)
            (update-in maybe-list [:items] conj value))
      (= "list-item" type)
      (conj state (->List [value]))
      (= "o-list-item" type)
      (conj state (->OList [value]))
      :else
      (throw
       (Exception.
        (str "Error while parsing StructuredText list item. Unknown type: "
             type))))))

(defn structured-text-reducer
  [document-type field-name state {:keys [type text spans] :as value}]
  (let [state
        (case type
          "o-list-item" (structured-text-add-list-item state value)
          "list-item" (structured-text-add-list-item state value)
          "embed" (conj state (-> (:oembed value)
                                  (rename-keys {:type :embed_type})
                                  (merge (dissoc value :oembed))
                                  map->Embed))
          "image" (conj state (->Image value))
          "paragraph" (conj state (map->Paragraph value))
          "heading1" (conj state (map->Heading1 value))
          "heading2" (conj state (map->Heading2 value))
          "heading3" (conj state (map->Heading3 value))
          "heading4" (conj state (map->Heading4 value))
          "heading5" (conj state (map->Heading5 value))
          "heading6" (conj state (map->Heading6 value))
          (throw
           (str "Error while parsing StructuredText. Unknown type: "
                type)))]
    (conj (pop state) (assoc (peek state)
                             :in-structured-text? true
                             :document-type document-type
                             :field-name field-name))))

(defn parse-structured-text [document-type field-name value]
  (-> (partial structured-text-reducer document-type field-name)
      (reduce [] value)
      ->StructuredText))

(defn parse-field [document-type [field-name {:keys [type value]}]]
  (let [field (case type
                "StructuredText" (parse-structured-text
                                  document-type field-name value)
                "Image" (map->Image value)
                "Embed" (map->Embed (:oembed value))
                "Text" (->Text value)
                "Link.document" (-> (:document value)
                                    (merge (dissoc value :document))
                                    (rename-keys {:isBroken :is-broken})
                                    map->DocumentLink)
                "Link.web" (map->WebLink value)
                "Link.image" (map->ImageLink (:image value))
                (throw
                 (Exception. (str "Invalid prismic field type: " type))))]
    [field-name (assoc field
                       :document-type document-type
                       :field-name field-name)]))

(defn parse-document [{:keys [type] :as document}]
  (let [parse-field (partial parse-field type)]
    (map->Document
     (update-in document [:data]
                #(into {} (map parse-field (get % (keyword type))))))))

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
   "https://blogtemplateegr.cdn.prismic.io/api/documents/search?ref=VrUggSgAADwAwOh7&q=%5B%5B%3Ad+%3D+at%28document.id%2C+%22Vq51byQAAEMxi9qn%22%29+%5D%5D",
   :tags [],
   :slugs ["titre1"],
   :linked_documents
   [{:id "Vq51byQAAEMxi9qn",
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
     {:type "Link.image",
      :value
      {:image
       {:name "star.gif",
        :kind "image",
        :url
        "https://wroomdev.s3.amazonaws.com/tutoblanktemplate%2F97109f41-140e-4dc9-a2c8-96fb10f14051_star.gif",
        :size "627291",
        :height "800",
        :width "960"}}},
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
        "<iframe width=\"480\" height=\"270\" src=\"https://www.youtube.com/embed/H5N-xL2STKs?feature=oembed\" frameborder=\"0\" allowfullscreen></iframe>"}}}}}})

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
