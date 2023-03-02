*from subtree, including max-level heading from point*

-   :: org export - Include subtree header in filename when exporting subtree from org-mode - Emacs Stack Exchange

```
(defun qz/org-export-headline (&optional backend async subtreep visible-only body-only ext-plist)
  "Export the current Org headline using BACKEND.

The available backends are the ones of `org-export-backends' and
'pdf.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without surrounding template.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings."
  (interactive)
  (let* ((backend (unless backend
                    (intern
                     (completing-read "Available backends: "
                                      (append org-export-backends '(pdf slack))))))
         (headline (car (last (org-get-outline-path t))))
         (headline-alnum (replace-regexp-in-string "[^[:alnum:]-_]" "-" headline))
         (file-prefix (file-name-sans-extension (buffer-file-name)))
         (filename (format "%s-%s.%s" file-prefix headline-alnum
                           (cl-case backend
                             ('pdf "tex")
                             ('slack "md")
                             (t backend)))))
    (save-restriction
      (org-narrow-to-subtree)
      (kill-new (s-join " -> " (org-get-outline-path t nil)))
      (org-export-to-file
          (if (eq backend 'pdf) 'latex backend)
          filename async subtreep visible-only body-only ext-plist
          (when (eq backend 'pdf)
            (lambda (file) (org-latex-compile file))))
      (widen))
    (with-temp-buffer
      (insert-file-contents filename)
      (kill-new (buffer-string)))))
```

- *{C-c C-M-e} :: qz/org-export-headline*

```
(define-key org-mode-map (kbd "C-c C-M-e") 'qz/org-export-headline)
```

