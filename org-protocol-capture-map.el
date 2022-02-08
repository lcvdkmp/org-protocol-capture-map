;;; org-protocol-capture-map/org-protocol-capture-map.el -*- lexical-binding: t; -*-

(require 'org-protocol)
(require 'cl-lib)

(defcustom org-protocol-capture-map-strip-properties t
  "Whether to strip pandocs generated properties from body."
  :group 'org-protocol-capture-map :type 'boolean)

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
          (when org-protocol-capture-map-strip-properties 
            (dolist (prop (org-buffer-property-keys))
              (org-delete-property-globally prop)))
          (buffer-string)))))

(cl-defmacro org-protocol-capture-map-add (module-name &key protocol function kill-client ftitle fbody)
  "Construct a org-protocol with aditional title and body maps.
for MODULE-NAME, PROTOCOL, FUNCTION, and KILL-ClIENT see the definition of `org-protocol-protocol-alist'.
Map the parsed title and body using FTITLE and FBODY respectively.
FTITLE and FBODY should be functions taking a string, and producing a string."
 (let ((ma (intern (concat "org-protocol-capture-map--" module-name "-advice")))
       (cf (intern (concat "org-protocol-capture-map--" module-name "-hijack")))
       ;; execute protocol function when defined, otherwise fallback to org-protocol-capture
       (fun (or (symbol-function function) 'org-protocol-capture)))
   `(if (or ,ftitle ,fbody)
        (progn
          (defun ,ma (plist)
            (when ,ftitle (plist-put plist :title (funcall ,ftitle (plist-get plist :title))))
            (when ,fbody (plist-put plist :body (funcall ,fbody (plist-get plist :body)))))

          ;; construe a function that adds ,ma as advice around protocol function call
          (defun ,cf (data)
            (advice-add 'org-protocol-parse-parameters
                        :filter-return ',ma)
            (funcall ,fun data)
            (advice-remove 'org-protocol-parse-parameters ',ma))

          ;; construct the protocol, injecting our own function into the protocol
          (add-to-list 'org-protocol-protocol-alist
                       '(,module-name :function ,cf :protocol ,protocol :kill-client ,kill-client)))
      (add-to-list 'org-protocol-protocol-alist
                   '(,module-name :function ,fun :protocol ,protocol :kill-client ,kill-client)))))


;; interface
(org-protocol-capture-map-add "capture-html"
                              :protocol "capture-html"
                              :kill-client t
                              :fbody #'org-protocol-capture-map--pandoc-parse)

(provide 'org-protocol-capture-map)
