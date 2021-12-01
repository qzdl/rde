(define-module (qzdl emacs)
  #:exports (init-el))

(define init-el
  '( ;; NOWEB CONF START
    (with-eval-after-load 'pdf-view
      (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode))
    lpmtq
    (require 'hyperbole)
    (with-eval-after-load 'org
      ;; NOWEB ORG START
      (with-eval-after-load 'org-roam
        ;; NOWEB ROAM START
        (setq qz/org-roam-dailies-filespec "private-%<%Y-%m-%d>.org"
        (setq org-roam-dailies-capture-templates
              `(("d" "default" entry
                 ,(s-join "\n" '("* [%<%H:%M>] %?"
                                 "CREATED: <%<%Y-%m-%d %H:%M>>"
                                 "FROM: %a"))
                 :if-new (file+head+olp
                          ,qz/org-roam-dailies-filespec
                          ,(s-join "\n" '("#+title: <%<%Y-%m-%d>>"
                                          "#+filetags: daily private project" ""
                                          "%(qz/today-dateref)" ""
                                          "* today, I will"))
                          ("journal")))))
        ;; NOWEB ROAM END
        )
      ;; NOWEB ORG END
      )
    ;; NOWEB CUSTOM START
    
    ;; NOWEB CUSTOM END
     ;; NOWEB CONF END
    ))
