(in-package www)
(define-element dt-background (style)
  (dt-background :style style
    (template :shadowrootmode "open" 
              (script)
              (div)
              (div)
              (div))))
