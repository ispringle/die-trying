;;; Utils
(defun mapc-walk-directory (fn dir)
  "Walk a directory, `dir'` and run `fn' on all subdirectories and files."
  (dolist (entry (cl-fad:list-directory dir))
    (when (cl-fad:directory-pathname-p entry)
      (mapc-walk-directory fn entry))
    (funcall fn entry)))

(defun vector-empty-p (vec)
  (unless (vector-to-list vec) t))


(defun copy-file (path &optional (out (get-out-path path)))
  (unless (cl-fad:directory-exists-p out)
    (cl-fad:copy-file path (get-out-path path) :overwrite t)))

;;;
(defun categorize-file (path)
  (if (not (equal (pathname-type path) "html"))
      'asset
      (let ((dom (lquery:$ (initialize path))))
        (if (vector-empty-p (lquery:$ dom "html"))
            'fragment
            'template))))

(defun path-to-dom (path)
  (lquery:$ (initialize path)))

(defun build-fragment-objects (paths)
  (mapcar (lambda (path)
            (let ((dom (path-to-dom path))
                  (tag (pathname-name path)))
              (list :dom dom :tag tag)))
          paths))

(defun find-dependencies (fragments)
  (let (updated-fragments)
    (mapcar (lambda (dom-plist)
              (let ((dependencies nil)
                    (dom (getf dom-plist ':dom)))
                (mapcar (lambda (fragment)
                          (let ((tag (getf fragment ':tag)))
                            (when (not (vector-empty-p (lquery:$ dom tag)))
                              (setf dependencies (cons tag dependencies)))))
                        fragments)
                (setf updated-fragments
                      (cons (list :tag (getf dom-plist ':tag)
                                  :dom dom
                                  :deps dependencies)
                            updated-fragments))))
            fragments)
    updated-fragments))

(defun expand (node fragments)
  (mapcar (lambda (fragment)
            (let ((tag (getf fragment ':tag))
                  (el (getf fragment ':dom)))
              (lquery:$ node tag (append el))))
          fragments)
  node)

;;; Processors
(defun process-input-files (dir)
  (let (templates fragments)
    (mapc-walk-directory (lambda (path)
                           (let ((category (categorize-file path)))
                             (cond ((equal category 'template) (setf templates (cons path templates)))
                                   ((equal category 'fragment) (setf fragments (cons path fragments)))
                                   (t (copy-file path)))))
                         dir)
    (list templates fragments)))

(defun process-templates (templates fragments)
  templates)

(defun process-fragments (fragment-paths)
  (let* ((fragments (build-fragment-objects fragment-paths))
         (expanded-fragments nil))
    (mapcar (lambda (fragment)
              (setf expanded-fragments
                    (cons (list :tag (getf fragment ':tag)
                                :dom (expand (getf fragment ':dom) fragments))
                          expanded-fragments)))
            fragments)
    expanded-fragments))

(defun write-templates (templates base-dir)
  templates)

(defun main (in out)
  (let* ((x (process-input-files in))
         (fragments (process-fragments (cadr x)))
         (templates (process-templates (car x) fragments)))
    (write-templates templates out)
    ))

(main "www/" "out/")
