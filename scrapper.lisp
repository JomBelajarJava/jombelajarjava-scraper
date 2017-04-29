(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :closure-html)
(ql:quickload :cxml-stp)

(defun sub-menu-p (node)
  (and (typep node 'stp:element)
       (string= (stp:local-name node) "ul")
       (string= (stp:attribute-value node "class") "sub-menu")))

(defun parse-link (node)
  (stp:attribute-value (stp:first-child node) "href"))

(defun parse-links-in-menu ()
  (let* ((homepage  (drakma:http-request "http://www.jombelajarjava.com"))
         (document  (chtml:parse homepage (stp:make-builder)))
         (menu-list (alexandria:flatten (mapcar #'stp:list-children
                                                (stp:filter-recursively #'sub-menu-p document)))))
    (mapcar #'parse-link menu-list)))
