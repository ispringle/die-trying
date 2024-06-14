

(defmacro with-page ((&key title) &body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :name "viewport" :content "width=device-width,initial-scale=1")
       (:title ,title)
       (:link :rel "icon"
              :href "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22><text y=%2228%22 font-size=%2232%22>💀</text></svg>")
       (:link :rel "preconnect"
              :href "https://fonts.googleapis.com")
       (:link :rel "preconnect"
              :href "https://fonts.gstatic.com"
              :crossorigin t)
       (:link :rel "stylesheet"
              :href "https://fonts.googleapis.com/css2?family=Oldenburg&display=swap")
       (:link :rel "stylesheet"
              :href "assets/style.css")
       (:meta :name "description"
              :content "Get rich or..."))
      (:meta :charset "utf-8")
      (:body ,@body))))
