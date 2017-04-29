(ql:quickload :alexandria)
(ql:quickload :drakma)
(ql:quickload :closure-html)
(ql:quickload :cxml-stp)

(defun sub-menu-p (node)
  (and (typep node 'stp:element)
       (equal (stp:local-name node) "ul")
       (equal (stp:attribute-value node "class") "sub-menu")))

(defun parse-link (node)
  (stp:attribute-value (stp:first-child node) "href"))

(defun parse-links-in-menu ()
  (let* ((homepage  (drakma:http-request "http://www.jombelajarjava.com"))
         (document  (chtml:parse homepage (stp:make-builder)))
         (sub-menus (stp:filter-recursively #'sub-menu-p document))
         (menu-list (alexandria:flatten (mapcar #'stp:list-children sub-menus))))
    (mapcar #'parse-link menu-list)))
