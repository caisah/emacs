;;; blog-settings.el --- My site: http://caisah.info

;;; Commentary:
;;  Blog config

(setq org-publish-project-alist
      '(("org"
         :base-directory "~/Documents/caisah.info/org"
         :base-extension "org"
         :publishing-directory "~/Documents/caisah.info"
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\"
                       href=\"css/styles.css\" type=\"text/css\"/>"
         :html-preamble t)

        ("img"
         :base-directory "~/Documents/caisah.info/org/img"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "~/Documents/caisah.info/img"
         :publishing-function org-publish-attachment)

        ("css"
         :base-directory "~/Documents/caisah.info/org/css"
         :base-extension "css\\|el"
         :publishing-directory "~/Documents/caisah.info/img"
         :publishing-function org-publish-attachment)
        ("website" :components ("org" "images" "css"))))


(provide 'blog-settings)
;;; blog-settings ends here
