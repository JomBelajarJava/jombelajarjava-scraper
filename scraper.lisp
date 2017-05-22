(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :closure-html)
(ql:quickload :cxml-stp)
(ql:quickload :quri)

(defparameter *base-url* "http://www.jombelajarjava.com")

(defun serialize-to-file (file node)
  "Serialize the cxml node to file."
  (with-open-file (output-file (ensure-directories-exist file)
                               :direction :output
                               :if-exists :supersede)
    (stp:serialize node (chtml:make-character-stream-sink output-file))))

(defun define-check (symbol-and-args)
  "Define how to do the checking for the node given the symbol and its args."
  (destructuring-bind (symbol tag &optional attr attr-val) symbol-and-args
    `(,symbol (node)
              (and (typep node 'stp:element)
                   (equal (stp:local-name node) ,tag)
                   ,(if (and attr attr-val)
                        `(equal (stp:attribute-value node ,attr) ,attr-val)
                        t)))))

(defmacro with-predicate (list-of-symbol-and-args &body body)
  "Macro to create a predicate to check if node is of which element."
  `(flet ,(mapcar #'define-check list-of-symbol-and-args)
     ,@body))

(defun find-menu (url)
  "Return a node containing menu container."
  (with-predicate ((menu-container-p "ul" "id" "menu-tutorial-menu"))
    (let* ((response  (drakma:http-request url))
           (document  (chtml:parse response (stp:make-builder))))
      (first (stp:filter-recursively #'menu-container-p document)))))

(defun scrape-menu (menu-node)
  "Serialize menu-node to file 'out/menu.html'."
  (serialize-to-file "out/menu.html" menu-node))

(defun parse-links-in-menu (menu-node)
  "Return a list of links in menu-node."
  (with-predicate ((sub-menu-p "ul" "class" "sub-menu"))
    (let* ((sub-menus (stp:filter-recursively #'sub-menu-p menu-node))
           (menu-list (alexandria:flatten (mapcar #'stp:list-children sub-menus))))
      (mapcar (lambda (li)
                (stp:attribute-value (stp:first-child li) "href"))
              menu-list))))

(defun save-image (url)
  "Save image to a file using decoded name given the url."
  (let* ((image-name (concatenate 'string
                                  (reverse (loop for x across (reverse url)
                                                 while (not (equal x #\/))
                                                 collect x))))
         (filename   (quri:url-decode (quri:url-decode (quri:url-decode image-name))))
         (image      (drakma:http-request url)))
    (with-open-file (output-file (ensure-directories-exist
                                  (format nil "out/images/~a" filename))
                                 :direction :output
                                 :if-exists :supersede
                                 :element-type '(unsigned-byte 8))
      (write-sequence image output-file))))

(defun save-all-images (urls)
  "Save all images given a list of urls."
  (dolist (url urls)
    (save-image url)))

(defun scrape-content (link)
  "Scrape the content of the page from the link. Also save all the images
  in that content."
  (with-predicate ((contentp "div" "class" "entry-content")
                   (imagep   "img"))
    (let* ((filename   (remove #\/ (subseq link (length *base-url*))))
           (page       (drakma:http-request link))
           (document   (chtml:parse page (stp:make-builder)))
           (content    (first (stp:filter-recursively #'contentp document)))
           (images     (stp:filter-recursively #'imagep content))
           (image-urls (mapcar (lambda (img)
                                 (stp:attribute-value img "src"))
                               images)))
      (serialize-to-file (format nil "out/~a.html" filename) content)
      (save-all-images image-urls))))

(defun scrape-all-contents (links)
  "Scrape the content of all pages from a list of links."
  (dolist (link links)
    (scrape-content link)))

(defun scrape-all ()
  "Scrape everything."
  (let* ((menu  (find-menu *base-url*))
         (links (parse-links-in-menu menu)))
    (scrape-menu menu)
    (scrape-all-contents links)))
