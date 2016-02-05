(ns ewen.prismic-kit.fragment)

(defprotocol Html
  (fragment-as-html [this] [this link-resolver]))

(defrecord Paragraph [text spans])
(defrecord Heading1 [text spans])
(defrecord Heading2 [text spans])
(defrecord Heading3 [text spans])
(defrecord Heading4 [text spans])
(defrecord Heading5 [text spans])
(defrecord Heading6 [text spans])
(defrecord Embed [html])
;; Todo check all embed types
(defrecord Image [main])
(defrecord List [items])
(defrecord OList [items])
(defrecord StructuredText [value])
(defrecord Text [value])
(defrecord DocumentLink [id type is-broken])
(defrecord WebLink [url])
(defrecord ImageLink [url])
(defrecord Document [id type data])
