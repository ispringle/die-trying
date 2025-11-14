(in-package #:die-trying)

(spinneret:with-html-string
  (:doctype)
  (:html :lang "en"
    (:head)
    (:body
      (:dt-header)
      (:main
        (:dt-roll :class "h-feed"
          ;; Dynamically generate blog roll from stream and post directories
          (let* ((stream-posts (die-trying:get-all-posts "www/stream/"))
                 (full-posts (die-trying:get-all-posts "www/post/"))
                 ;; Combine all posts
                 (all-posts (append stream-posts full-posts))
                 ;; Sort by date (newest first)
                 (sorted-posts (sort all-posts #'string> 
                                     :key (lambda (p) 
                                            (or (getf (die-trying:get-post-metadata p) :date) "")))))
            (dolist (post sorted-posts)
              (let* ((metadata (get-post-metadata post))
                     (tag (getf metadata :tag))
                     (title (getf metadata :title))
                     (excerpt (getf metadata :excerpt))
                     (date-pretty (getf metadata :date-pretty))
                     (is-stream (search "stream" (namestring post)))
                     (url (if is-stream 
                              "#"  ; Stream posts are inline only
                              (format nil "/post/~a.html" tag))))
                (if is-stream
                    ;; Stream posts: just show the custom element inline
                    (:raw (format nil "<~a></~a>" tag tag))
                    ;; Full posts: generate article card with link
                    (progn
                      (:div
                        (:a :href url
                          (:article :class "h-entry"
                            (:h2 :class "p-name" title)
                            (:p :class "p-summary" excerpt)))
                        (:span :class "date" date-pretty)))))))
          ;; Manually add lorem-ipsum at the bottom
          (:div
            (:a :href "/lorem-ipsum.html"
              (:article
                (:p "There's nothing else to see, but you can check out my lorem ipsum page..."))))
          ;; Include the styles
          (:style
           (:raw "
          article {
            background: #d9efbe;
            border: solid black 1px;
            box-shadow: -6.9px 6.9px rgba(0, 0, 0, 0.5);
            padding: 1em 1em;
            max-width: 669px;
          }
          a {
            text-decoration: none;
            color: unset;
          }
          dt-roll > div {
            position: relative;
          }
          div .date {
            font-size: 0.6em;
            position: absolute;
            left: 600px;
            width: max-content;
            border: solid black 1px;
            box-shadow: -6.9px 6.9px rgba(0, 0, 0, 0.5);
            padding: 1.5em 1em;
            margin: -1em 0;
            background: #d9efbe;
          }"))))
      (:dt-footer
        (:h-card)
        (:span "CC BY-NC-SA"))
      (:dt-background))))

