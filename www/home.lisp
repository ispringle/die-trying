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
                            (:span :class "date" date-pretty)
                            (:h2 :class "p-name" title)
                            (:p :class "p-summary" excerpt)))))))))
          ;; Manually add lorem-ipsum at the bottom
          (:div
            (:a :href "/lorem-ipsum.html"
              (:article
                (:p "There's nothing else to see, but you can check out my lorem ipsum page..."))))
          ;; Include the styles
          (:style
           (:raw "
          article {
            background: #fff;
            border: 4px solid #000;
            box-shadow: 4px 0 0 rgba(255, 0, 0, 0.6), -4px 0 0 rgba(0, 255, 255, 0.6);
            padding: 1em 1em;
            max-width: 100%;
            transition: transform 0.2s ease, box-shadow 0.2s ease;
          }
          a {
            text-decoration: none;
            color: unset;
          }
          dt-roll > div {
            position: relative;
          }
          dt-roll > div:hover article {
            transform: translate(-2px, -2px);
            box-shadow: 6px 0 0 rgba(255, 0, 0, 0.7), -6px 0 0 rgba(0, 255, 255, 0.7);
          }
          article .date {
            display: inline-block;
            font-size: 0.7em;
            font-family: 'Courier New', monospace;
            background: #000;
            color: #fff;
            padding: 0.3em 0.6em;
            margin-bottom: 0.5em;
            text-transform: uppercase;
            letter-spacing: 1px;
          }
          article h2 {
            margin-top: 0.5em;
          }
          @media (max-width: 1024px) {
            article {
              max-width: 100%;
            }
          }"))))
      (:dt-footer
        (:h-card)
        (:span "CC BY-NC-SA"))
      (:dt-background))))
