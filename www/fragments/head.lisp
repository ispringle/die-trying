(in-package #:die-trying)

(lambda (&key (title "die trying") (description "Get rich or...") image url 
              summary excerpt date author &allow-other-keys)
  "Dynamic head fragment that generates meta tags and optionally social media tags.
   Accepts all page metadata as keyword arguments."
  (spinneret:with-html-string
    (:meta :name "viewport" :content "width=device-width, initial-scale=1")
    (:title title)
    (:link :rel "icon"
           :href "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%2275%22 x=%2250%22 font-size=%2270%22 text-anchor=%22middle%22>💀</text></svg>")
    (:link :rel "preconnect" :href "https://fonts.googleapis.com")
    (:link :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin "")
    (:link :href "https://fonts.googleapis.com/css2?family=Oldenburg&display=swap" :rel "stylesheet")
    (:link :rel "stylesheet" :href "/assets/style.css")
    (:meta :name "description" :content (or description "Get rich or..."))
    (:meta :charset "utf-8")
    ;; Generate social media meta tags if image/url provided
    (when (or image url)
      (:raw (generate-social-meta 
             :title title 
             :description (or description "Get rich or...") 
             :image image 
             :url url)))))

