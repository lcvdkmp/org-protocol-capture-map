;;; org-protocol-capture-map/org-protocol-capture-map.el -*- lexical-binding: t; -*-

(require 'org-protocol)


(defun org-protocol-capture-map--pandoc-parse (content)
  "Convert CONTENT from html to org using pandoc."
  (with-temp-buffer
    (insert content)
    (if (not (zerop (call-process-region
                     (point-min) (point-max)
                     "pandoc" t t nil "-f" "html" "-t" "org" "--wrap=none")))
        (message "Pandoc failed: %s" (buffer-string))
        ;; Pandoc succeeded
        (progn
          ;; Pandoc creates org properties for HTML attributes (such ass id, class).
          ;; Strip these, as we are only interested in text
          (dolist (prop (org-buffer-property-keys))
            (org-delete-property-globally prop))
          (buffer-string)
          ))))

(defun org-protocol-capture-map--apply-plist-map (plist &optional ftitle fbody)
  "Map PLIST's `:title' and `:body' using FTITLE and FBODY respectively.
FTITLE and FBODY should be functions taking a string, and producing a string."
  (when ftitle (plist-put plist :title (ftitle plist-get plist :title)))
  (when fbody (plist-put plist :body (funcall fbody (plist-get plist :body)))))

(defun org-protocol-capture-map--apply-body-html-map (plist)
  (org-protocol-capture-map--apply-plist-map plist nil #'org-protocol-capture-map--pandoc-parse))

(defun org-protocol-capture-map--capture-html (data)
  "Capture input using `org-protocol-capture', but map `:body' to html using `org-protocol-capture-map--pandoc-parse'."
  (advice-add 'org-protocol-parse-parameters :filter-return #'org-protocol-capture-map--apply-body-html-map)
  (org-protocol-capture data)
  (advice-remove 'org-protocol-parse-parameters #'org-protocol-capture-map--apply-body-html-map))


(add-to-list 'org-protocol-protocol-alist
             '("capture-html"
               :protocol "capture-html"
               :function org-protocol-capture-map--capture-html
               :kill-client t))


(provide 'org-protocol-capture-map)
