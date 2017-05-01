(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :closure-html)
(ql:quickload :cxml-stp)

(defparameter *base-url* "http://www.jombelajarjava.com")

(defun serialize-to-file (file node)
  (with-open-file (output-file (ensure-directories-exist file)
                               :direction :output
                               :if-exists :supersede)
    (stp:serialize node (chtml:make-character-stream-sink output-file))))

(defun define-check (symbol-and-args)
  (destructuring-bind (symbol tag &optional attr attr-val) symbol-and-args
    `(,symbol (node)
              (and (typep node 'stp:element)
                   (equal (stp:local-name node) ,tag)
                   ,(if (and attr attr-val)
                        `(equal (stp:attribute-value node ,attr) ,attr-val)
                        t)))))

(defmacro with-predicate (list-of-symbol-and-args &body body)
  `(flet ,(mapcar #'define-check list-of-symbol-and-args)
     ,@body))

(defun find-menu (url)
  (with-predicate ((menu-container-p "ul" "id" "menu-tutorial-menu"))
    (let* ((response  (drakma:http-request url))
           (document  (chtml:parse response (stp:make-builder))))
      (first (stp:filter-recursively #'menu-container-p document)))))

(defun scrape-menu (menu-node)
  (serialize-to-file "out/menu.html" menu-node))

(defun parse-links-in-menu (menu-node)
  (with-predicate ((sub-menu-p "ul" "class" "sub-menu"))
    (let* ((sub-menus (stp:filter-recursively #'sub-menu-p menu-node))
           (menu-list (alexandria:flatten (mapcar #'stp:list-children sub-menus))))
      (mapcar (lambda (li)
                (stp:attribute-value (stp:first-child li) "href"))
              menu-list))))

(defun save-image (url)
  (let ((filename (concatenate 'string
                               (reverse (loop for x across (reverse url)
                                              while (not (equal x #\/))
                                              collect x))))
        (image    (drakma:http-request url)))
    (with-open-file (output-file (ensure-directories-exist
                                  (format nil "out/images/~a" filename))
                                 :direction :output
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
      (write-sequence image output-file))))

(defun scrape-content (link)
  (with-predicate ((contentp "div" "class" "entry-content")
                   (imagep   "img"))
    (let* ((filename   (remove #\/ (subseq link (length *base-url*))))
           (page       (drakma:http-request link))
           (document   (chtml:parse page (stp:make-builder)))
           (content    (first (stp:filter-recursively #'contentp document)))
           (images     (stp:filter-recursively #'imagep content))
           (images-url (mapcar (lambda (img)
                                 (stp:attribute-value img "src"))
                               images)))
      (mapcar #'save-image images-url)
      (serialize-to-file (format nil "out/~a.html" filename) content))))

(defun scrape-all-contents (links)
  (dolist (link links)
    (scrape-content link)))

(defun scrape-all ()
  (let* ((menu  (find-menu *base-url*))
         (links (parse-links-in-menu menu)))
    (scrape-menu menu)
    (scrape-all-contents links)))
