(ql:quickload :drakma)
(ql:quickload :quri)
(ql:quickload :cl-html5-parser)
(ql:quickload :css-selectors-simple-tree)
(ql:quickload :xmls)

(defparameter *base-url* "http://www.jombelajarjava.com")

(defun write-html (node filename)
  "Write the node html content to specified file."
  (with-open-file (output-file (ensure-directories-exist
                                (format nil "out/~a.html" filename))
                               :direction :output
                               :if-exists :supersede)
    (xmls:write-xml (html5-parser:transform-html5-dom :xmls node) output-file)))

(defun write-image (image filename)
  "Save image to file."
  (with-open-file (output-file (ensure-directories-exist
                                (format nil "out/images/~a" filename))
                               :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (write-sequence image output-file)))

(defun scrape-html (nodes file)
  "Unwrap nodes and write their content to file. Did this because
  css-selectors always returns a list even for single element."
  (mapcar (lambda (node) (write-html node file)) nodes))

(defun parse-links (menu)
  "Parse href attribute value from menu. Only takes the link in sub-menu."
  (let* ((anchors (css-selectors:query ".sub-menu a" menu)))
    (mapcar (lambda (a) (html5-parser:element-attribute a "href")) anchors)))

(defun take-image-name (link)
  "Take image name from link. For example, https://www.google.com/image.png
  will return image.png."
  (concatenate 'string (reverse (loop for x across (reverse link)
                                      while (not (equal x #\/))
                                      collect x))))

(defun save-image (link)
  "Save image to a file using decoded name from a link."
  (let ((image-name (take-image-name link)))
    (when (notany (lambda (name) (string= name image-name))
                  '("facebook.png" "twitter.png" "google.png"))
      (let* ((filename   (quri:url-decode (quri:url-decode (quri:url-decode image-name))))
             (image      (drakma:http-request link)))
        (format t "Saving image from ~a...~%" link)
        (write-image image filename)))))

(defun save-images (links)
  "Save all images from a list of links."
  (dolist (link links)
    (save-image link)))

(defun scrape-content (link)
  "Scrape the content article from the link. Also save all images in
  the article."
  (let* ((filename    (remove #\/ (subseq link (length *base-url*))))
         (document    (drakma:http-request link))
         (html        (html5-parser:parse-html5 document))
         (article     (css-selectors:query "article" html))
         (images      (css-selectors:query "img" article))
         (image-links (mapcar (lambda (img)
                                (html5-parser:element-attribute img "src"))
                              images)))
    (format t "Scraping ~a...~%" link)
    (scrape-html article filename)
    (save-images image-links)))

(defun scrape-contents (links)
  "Scrape the contents from a list of links."
  (dolist (link links)
    (scrape-content link)))

(defun scrape-all ()
  "Scrape everything."
  (let* ((document (drakma:http-request *base-url*))
         (html     (html5-parser:parse-html5 document))
         (menu     (css-selectors:query "#menu-tutorial-menu" html))
         (links    (parse-links menu)))
    (format t "Scraping menu...~%")
    (scrape-html menu "menu")
    (scrape-contents links)))
