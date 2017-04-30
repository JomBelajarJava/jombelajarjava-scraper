(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :closure-html)
(ql:quickload :cxml-stp)

(defparameter *base-url* "http://www.jombelajarjava.com")

(defun sub-menu-p (node)
  (and (typep node 'stp:element)
       (equal (stp:local-name node) "ul")
       (equal (stp:attribute-value node "class") "sub-menu")))

(defun parse-link (node)
  (stp:attribute-value (stp:first-child node) "href"))

(defun parse-links-in-menu ()
  (let* ((homepage  (drakma:http-request *base-url*))
         (document  (chtml:parse homepage (stp:make-builder)))
         (sub-menus (stp:filter-recursively #'sub-menu-p document))
         (menu-list (alexandria:flatten (mapcar #'stp:list-children sub-menus))))
    (mapcar #'parse-link menu-list)))

(defun contentp (node)
  (and (typep node 'stp:element)
       (equal (stp:local-name node) "div")
       (equal (stp:attribute-value node "class") "entry-content")))

(defun scrape-content (link)
  (let* ((filename (remove #\/ (subseq link (length *base-url*))))
         (page     (drakma:http-request link))
         (document (chtml:parse page (stp:make-builder)))
         (content  (first (stp:filter-recursively #'contentp document))))
    (with-open-file (*output-file* (ensure-directories-exist
                                    (format nil "out/~a.html" filename))
                                   :direction :output
                                   :if-exists :supersede)
      (stp:serialize content (chtml:make-character-stream-sink *output-file*)))))

(defun scrape-all-content ()
  (dolist (link (parse-links-in-menu))
    (scrape-content link)))
