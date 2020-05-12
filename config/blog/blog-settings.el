;;; blog-settings.el --- My site: http://caisah.info

;;; Commentary:
;;  Blog config

;;; Code:

(defun blog-get-string-from-file (path)
  "Read contents of the file PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun blog-strip-path (str)
  "Format the relative path in STR."
  (replace-regexp-in-string "\\.\\." "\." str))

(defconst blog-root-dir "~/Documents/caisah.info/src")
(defconst blog-blog-dir (concat blog-root-dir "/blog"))
(defconst blog-img-dir (concat blog-root-dir "/img"))
(defconst blog-css-dir (concat blog-root-dir "/css"))
(defconst blog-pub-root-dir "~/Documents/caisah.info/public")
(defconst blog-pub-blog-dir (concat blog-pub-root-dir "/blog"))
(defconst blog-pub-img-dir (concat blog-pub-root-dir "/img"))
(defconst blog-pub-css-dir (concat blog-pub-root-dir "/css"))
(defconst blog-head-string
  (blog-get-string-from-file "~/.emacs.d/config/blog/head.html"))
(defconst blog-header-string
  (blog-get-string-from-file "~/.emacs.d/config/blog/header.html"))
(defconst blog-footer-string
  (blog-get-string-from-file "~/.emacs.d/config/blog/footer.html"))

;; Set all the details for the org website
(setq org-publish-project-alist
      `(("root"
         :base-directory ,blog-root-dir
         :base-extension "org"
         :publishing-directory ,blog-pub-root-dir
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-head ,(blog-strip-path blog-head-string)
         :html-preamble ,(blog-strip-path blog-header-string)
         :html-postamble ,(blog-strip-path blog-footer-string)
         :recursive nil)

        ("blog"
         :base-directory ,blog-blog-dir
         :base-extension "org"
         :publishing-directory ,blog-pub-blog-dir
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-head ,blog-head-string
         :html-preamble ,blog-header-string
         :html-postamble ,blog-footer-string)

        ("img"
         :base-directory ,blog-img-dir
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory ,blog-pub-img-dir
         :publishing-function org-publish-attachment)

        ("css"
         :base-directory ,blog-css-dir
         :base-extension "css\\|el"
         :publishing-directory ,blog-pub-css-dir
         :publishing-function org-publish-attachment)

        ("website" :components ("root" "blog" "img" "css"))))


(provide 'blog-settings)
;;; blog-settings ends here
