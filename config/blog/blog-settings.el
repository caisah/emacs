;;; blog-settings.el --- My site: http://caisah.info

;;; Commentary:
;;  Blog config

;;; Code:
(defun my-get-string-from-file (path)
  "Read contents of the file PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun format-path (str)
  "Format the relative path in STR."
  (replace-regexp-in-string "\\.\\." "\." str))

(format-path "img class=\"logo in-header\" src=\"../img/lambda.gif\" />")

(setq org-publish-project-alist
      `(("root"
         :base-directory "~/Documents/caisah.info/src"
         :base-extension "org"
         :publishing-directory "~/Documents/caisah.info/public"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-head ,(format-path (my-get-string-from-file "./head.html"))
         :html-preamble ,(format-path (my-get-string-from-file "./header.html"))
         :html-postamble ,(format-path (my-get-string-from-file "./footer.html"))
         :recursive nil)

        ("blog"
         :base-directory "~/Documents/caisah.info/src/blog"
         :base-extension "org"
         :publishing-directory "~/Documents/caisah.info/public/blog"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-head ,(my-get-string-from-file "./head.html")
         :html-preamble ,(my-get-string-from-file "./header.html")
         :html-postamble ,(my-get-string-from-file "./footer.html"))

        ("img"
         :base-directory "~/Documents/caisah.info/src/img"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "~/Documents/caisah.info/public/img"
         :publishing-function org-publish-attachment)

        ("css"
         :base-directory "~/Documents/caisah.info/src/css"
         :base-extension "css\\|el"
         :publishing-directory "~/Documents/caisah.info/public/css"
         :publishing-function org-publish-attachment)

        ("website" :components ("root" "images" "css"))))


(provide 'blog-settings)
;;; blog-settings ends here
