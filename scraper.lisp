(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :closure-html)
(ql:quickload :cxml-stp)

(defparameter *base-url* "http://www.jombelajarjava.com")

(defun serialize-to-file (file node)
  (with-open-file (*output-file* (ensure-directories-exist file)
                                 :direction :output
                                 :if-exists :supersede)
    (stp:serialize node (chtml:make-character-stream-sink *output-file*))))

(defmacro with-predicate (symbol tag attr attr-val &body body)
  `(flet ((,symbol (node)
            (and (typep node 'stp:element)
                 (equal (stp:local-name node) ,tag)
                 (equal (stp:attribute-value node ,attr) ,attr-val))))
     ,@body))

(defun find-menu (url)
  (with-predicate menu-container-p "ul" "id" "menu-tutorial-menu"
    (let* ((response  (drakma:http-request url))
           (document  (chtml:parse response (stp:make-builder))))
      (first (stp:filter-recursively #'menu-container-p document)))))

(defun scrape-menu (menu-node)
  (serialize-to-file "out/menu.html" menu-node))

(defun parse-links-in-menu (menu-node)
  (with-predicate sub-menu-p "ul" "class" "sub-menu"
    (let* ((sub-menus (stp:filter-recursively #'sub-menu-p menu-node))
           (menu-list (alexandria:flatten (mapcar #'stp:list-children sub-menus))))
      (mapcar (lambda (li)
                (stp:attribute-value (stp:first-child li) "href"))
              menu-list))))

(defun scrape-content (link)
  (with-predicate contentp "div" "class" "entry-content"
    (let* ((filename (remove #\/ (subseq link (length *base-url*))))
           (page     (drakma:http-request link))
           (document (chtml:parse page (stp:make-builder)))
           (content  (first (stp:filter-recursively #'contentp document))))
      (serialize-to-file (format nil "out/~a.html" filename) content))))

(defun scrape-all-contents (links)
  (dolist (link links)
    (scrape-content link)))

(defun scrape-all ()
  (let* ((menu  (find-menu *base-url*))
         (links (parse-links-in-menu menu)))
    (scrape-menu menu)
    (scrape-all-contents links)))
