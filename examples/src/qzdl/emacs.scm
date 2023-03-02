(define-module (qz emacs)
  #:use-module (rde serializers elisp)
  #:export (init-el))

(define init-el
  (elisp-serialize
   '(

     ;; NOWEB GENERAL START
     (defmacro qz/advice- (target-fn state advice-fn)
       (let* ((s-advice (lambda (e)
                          (intern (format "qz/advice-%s--%s--%s"
                                          e target-fn advice-fn))))
              (enable (funcall s-advice 'enable))
              (disable (funcall s-advice 'disable)))
         `(progn
            (defun ,enable ()
              (interactive)
              (advice-add ',target-fn ,state ',advice-fn))
     
            (defun ,(funcall s-advice 'disable) ()
              (interactive)
              (advice-remove ',target-fn ',advice-fn))
     
            (,enable)
            (list ',enable ',disable))))
     (defun cons->table (body &optional &key cols tail-fn)
       "a transformation helper for org-babel, which has defaults
     to parse robustly the proper-list[1] over the simple cons[2]
     
     body      *values you wish to transform*: a list; cons, proper,
               a-, etc.
     :cols     *column headers for the results*: wrap the result in
               ((co1 col2) . (hline . (..res..)); as such, they will
               be made in addition to any headers and/or `hlines'
               applied by `org-babel' (esp. those from `:colnames').
     :tail-fn  *control the parsing of each entry of `body'*:
               by default, `cdr' because for a simple `cons' '(a . b),
               cdr will yield 'b -> (cdr '(a . b)).  If operating on
               some `list' '(a b), then the analog for `'b' is `cadr'
               -> (cadr '(a b)) -> `'b'
     
     [1] proper-list: '(a b)   ; '(a . (b . nil))
     [2] simple-cons: '(a . b) ; '(a . b)"
       (let ((res (mapcar (lambda (c)
                            (list (car c)
                                  (funcall (or tail-fn 'cdr) c)))
                          body)))
         (if cols
             (cons cols (cons 'hline res))
           res)))
     
     ;;; e.g  {C-n C-SPC M-e C-p C-x C-;}
     ;; (cons->table
     ;;  '((56 . "/home/samuel/life/roam/20210420T114708Z-newstore.org")
     ;;    (11 . "/home/samuel/life/roam/20210813T161035Z-kubernetes.org")
     ;;    (10 . "/home/samuel/life/roam/20200515T151822Z-postgresql.org"))
     ;;  :cols '(count file))
     (defun qz/ensure-list (s)
       (if (listp s)
           s
         (list s)))
     (defvar qz/debug 0 "debugging assists")
     
     (defmacro qz/debug- (&rest body)
       (if qz/debug
           `(progn ,@body)))
     
     (qz/debug- (message "yo"))
     (require 's)
     
     (setq qz/newstore-envs '(sandbox staging production)
           qz/newstore-env-current nil
           qz/newstore-envs-abbrev '((sandbox . x) (staging . s) (production . p))
           qz/newstore-tenant-current nil
           qz/newstore-tenants '("dodici" "windsor"
                                 "boardriders" "marine-layer"
                                 "frankandoak" "vince"))
     
     (defun qz/newstore-choose-env (&optional env)
       (interactive)
       (message "qz/newstore-env-current: %s"
                (setq qz/newstore-env-current
                      (or env (completing-read "env: " qz/newstore-envs))))
       (qz/restclient-choose-env qz/newstore-env-current)
       (qz/es-choose-url nil nil qz/newstore-env-current))
     
     (defun qz/newstore-choose-tenant (&optional tenant)
       (interactive)
       (message "qz/newstore-tenant-current: %s"
                (setq qz/newstore-tenant-current
                      (or tenant (completing-read "tenant: " qz/newstore-tenants))))
       (qz/restclient-choose-tenant qz/newstore-tenant-current))
     
     (defun qz/newstore-auth-current ()
       (message "qz/newstore-auth-cache: <for qz/newstore-env-current: %s>"
                qz/newstore-env-current)
       (setq qz/newstore-auth-cache
             (qz/newstore-auth qz/newstore-env-current)))
     
     (defun qz/newstore-auth (env)
       "get the auth (password) associated with
     a given `env' from `qz/newstore/envs'
     
     to populate, just fill a `pass' entry like so echo mypass | pass
       insert -e newstore/production"
       (s-trim (shell-command-to-string
                (format "pass newstore/%s" env))))
     
     ;; (defun qz/newstore-quick-auth ()
     ;;   (interactive)
     ;;   (qz/newstore-choose-tenant)
     ;;   (qz/newstore-choose-env)
     ;;   (org-sbe "newstore-token"))
     (defun qz/shell-command-to-list-of-strings (command)
       (remove "" (s-split "\n" (shell-command-to-string command))))
     (defun qz/revert-buffer-no-confirm ()
       "Revert buffer without confirmation."
       (interactive)
       (revert-buffer :ignore-auto :noconfirm))
     ;; NOWEB GENERAL END

     ;; NOWEB CONF START
     ;; NOWEB KBD START
     (define-key global-map (kbd "C-c C-x C-j") 'org-clock-goto)
     (define-key global-map (kbd "C-c !") 'org-time-stamp-inactive)
     (define-key global-map (kbd "C-M-j") 'delete-indentation)
     (define-key global-map (kbd "C-M-y") 'consult-yank-from-kill-ring)
     (define-key global-map (kbd "C-x C-M-f") 'consult-recent-file)
     ;;(custom-set-variables
     ;; '(org-disputed-keys '([(shift o)] . [(meta shift o)])))
     
     (defun qz/newline-above ()
       (interactive)
       (save-excursion
         (beginning-of-line)
         (newline))
       (indent-according-to-mode))
     
     (define-key global-map (kbd "C-z") 'qz/newline-above)
     ;;(define-key global-map (kbd "C-o") 'open-line)
     ;;
     ;;(org-remap org-mode-map
     ;;           'open-line 'org-open-line)
     
     (define-key global-map (kbd "M-s-h") 'windmove-swap-states-left)
     (define-key global-map (kbd "M-s-j") 'windmove-swap-states-down)
     (define-key global-map (kbd "M-s-k") 'windmove-swap-states-up)
     (define-key global-map (kbd "M-s-l") 'windmove-swap-states-right)
     (define-key global-map (kbd "H-M-s-h") 'windmove-swap-states-left)
     (define-key global-map (kbd "H-M-s-j") 'windmove-swap-states-down)
     (define-key global-map (kbd "H-M-s-k") 'windmove-swap-states-up)
     (define-key global-map (kbd "H-M-s-l") 'windmove-swap-states-right)
     (define-key global-map (kbd "H-s-h") 'windmove-left)
     (define-key global-map (kbd "s-h")   'windmove-left)
     (define-key global-map (kbd "H-s-j") 'windmove-down)
     (define-key global-map (kbd "s-j")   'windmove-down)
     (define-key global-map (kbd "H-s-k") 'windmove-up)
     (define-key global-map (kbd "s-k")   'windmove-up)
     (define-key global-map (kbd "H-s-l") 'windmove-right)
     (define-key global-map (kbd "s-l")   'windmove-right)
     ;; Activate occur easily inside isearch
     
     (define-key isearch-mode-map (kbd "C-o")
                 (lambda () (interactive)
                   (let ((case-fold-search isearch-case-fold-search))
                     (occur (if isearch-regexp
                                isearch-string
                              (regexp-quote isearch-string))))))
     (define-key isearch-mode-map (kbd "M-o")
                 (lambda () (interactive)
                   (let ((case-fold-search isearch-case-fold-search))
                     (consult-line (if isearch-regexp
                                       isearch-string
                                     (regexp-quote isearch-string))))))
     (global-set-key (kbd "C-s") 'isearch-forward-regexp)
     (global-set-key (kbd "C-r") 'isearch-backward-regexp)
     (global-set-key (kbd "C-M-s") 'isearch-forward)
     (global-set-key (kbd "C-M-r") 'isearch-backward)
     (define-key global-map (kbd "s-\\") 'org-store-link)
     (define-key rde-toggle-map (kbd "d")   'toggle-debug-on-error)
     
     (define-key rde-toggle-map (kbd "h c") 'highlight-changes-mode)
     (define-key rde-toggle-map (kbd "h C") 'global-highlight-changes-mode)
     
     (define-key rde-toggle-map (kbd "h i") 'highlight-indent-guides-mode)
     
     ;; a bit sus, but maybe equivalent to toggling off hi-lock-mode
     (define-key rde-toggle-map (kbd "h p") 'unhighlight-regexp)
     (define-key rde-toggle-map (kbd "h P") 'global-hi-lock-mode)
     
     ;; for the incessant observers demanding more than {M-g M-g}
     (define-key rde-toggle-map (kbd "l")   'linum-mode)
     (define-key rde-toggle-map (kbd "L")   'global-linum-mode)
     (define-key global-map (kbd "M-s L") 'consult-line-multi)
     (define-key global-map (kbd "C-c n j") 'org-roam-dailies-capture-today)
     (define-key global-map (kbd "C-c n J") 'org-roam-dailies-goto-today)
     
     (define-key global-map (kbd "C-c n C-r") 'org-roam-refile)
     (define-key global-map (kbd "C-c n r") 'org-roam-node-random)
     
     (define-key global-map (kbd "C-c n a r") 'org-roam-ref-add)
     (define-key global-map (kbd "C-c n a t") 'org-roam-tag-add)
     (define-key global-map (kbd "C-c n a a") 'org-roam-alias-add)
     
     (defun qz/consult-notes ()
       (interactive)
       (let ((default-directory org-roam-directory))
         (call-interactively 'consult-ripgrep)))
     
     (define-key global-map (kbd "C-c n s") 'qz/consult-notes)
     ;; NOWEB KBD END
     (defun qz/dwim-fold ()
       (interactive)
       (let* ((is-or-derives (lambda (mode)
                               (or (eq major-mode mode)
                                   (derived-mode-p major-mode mode))))
              (fold-fn
               (cond ((funcall is-or-derives 'org-mode) 'qz/org-fold)
                     ((funcall is-or-derives 'magit-diff-mode) 'magit-section-cycle-diffs)
                     (t (message "no dwim path configured, honey")))))
         (when (symbolp fold-fn)
           (call-interactively fold-fn))))
     
     (define-key global-map (kbd "s-TAB") 'qz/dwim-fold)
     ;; NOWEB CUSTOM START
     (custom-set-variables
      '(org-imenu-depth 99))
     ;; NOWEB CUSTOM END
     (defvar qz/aws-env nil
       "the aws login configuration, managed through saml2aws
     
     to manipulate, run
     $ saml2aws login -a PROFILE_ALIAS
     
     files of note
     `$HOME/.aws/'
     `$HOME/.saml2aws'")
     (defun qz/choose-aws-env (&optional env)
       (interactive)
       (setq qz/aws-env
             (or env (completing-read
                      "aws-env: "
                      (->> (shell-command-to-string
                            "cat ~/.saml2aws | grep '^name' | cut -d'=' -f2")
                           (s-split "\n")
                           (remove "")))))
       (async-shell-command
        (format "saml2aws login -a %s && echo all good yo || echo uh-oh" qz/aws-env)
        "*aws*"
        "*error - aws*"))
     (defvar qz/kubectl-context nil
       "the operating kubernetes context.
     
     to check, at a shell, run:
     `$ kubectl config get-contexts -o name'
     or
     `$ kubectl config current-context")
     (defun qz/choose-kubectl-context (ctx)
       (interactive)
       (setq qz/kubectl-context
             (or ctx (completing-read "k8s ctx: "
                                      (qz/shell-command-to-list-of-strings
                                       "kubectl config get-contexts -o name"))))
       (async-shell-command (format "kubectl config use-context %s"
                                    qz/kubectl-context)
                            "*kubectl*"))
     
     ;; optional; quality of life improvement to bury kubectl buffer
     (add-to-list 'display-buffer-alist '("*kubectl*" display-buffer-no-window))
     (defun qz/read-tab ()
       (interactive)
       (let* ((file (s-trim (shell-command-to-string "python3 $HOME/life/scratch/tabs.py")))
              (jd   (mapcar (lambda (o) (cons (gethash "title" o) o))
                            (cl-sort ;; accessed ASC
                             (with-temp-buffer
                               (insert-file-contents file)
                               (json-parse-buffer))
                             'lt :key (lambda (o)
                                        (gethash "accessed" o)))))
              (choice (cdr (assoc
                            (consult--read
                             (mapcar 'car jd)
                             :prompt "choose tab: "
                             :default (car (seq-take jd 1))
                             :sort nil)
                            jd))))
         (insert
          (cl-destructuring-bind (title url)
              (list (gethash "title" choice)
                    (gethash "url" choice))
            (cl-case major-mode
              (org-mode (org-make-link-string url title))
              (t (format "%s :: %s" title url)))))))
     
     (define-key global-map (kbd "C-c C-s-l") 'qz/read-tab)
     (defun eos/narrow-or-widen-dwim (p)
       "Widen if buffer is narrowed, narrow-dwim otherwise.
     Dwim means: region, org-src-block, org-subtree, or
     defun, whichever applies first. Narrowing to
     org-src-block actually calls `org-edit-src-code'.
     
     With prefix P, don't widen, just narrow even if buffer
     is already narrowed."
       (interactive "P")
       (declare (interactive-only))
       (cond ((and (buffer-narrowed-p) (not p)) (widen))
             ((region-active-p)
              (narrow-to-region (region-beginning)
                                (region-end)))
             ((derived-mode-p 'org-mode)
              ;; `org-edit-src-code' is not a real narrowing
              ;; command. Remove this first conditional if
              ;; you don't want it.
              (cond ((ignore-errors (org-edit-src-code) t)
                     (delete-other-windows))
                    ((ignore-errors (org-narrow-to-block) t))
                    (t (org-narrow-to-subtree))))
             ((derived-mode-p 'latex-mode)
              (LaTeX-narrow-to-environment))
             (t (narrow-to-defun))))
     
     (define-key global-map (kbd "C-x C-n") 'eos/narrow-or-widen-dwim)
     (defun qz/yq-interactively ()
       "haha yaml loophole"
       (interactive)
       (let ((jq-interactive-command "yq"))
         (call-interactively 'jq-interactively)))
     (defun qz/insert-gpl ()
       "Insert the short brief of GNU GPL v3."
       (interactive)
       (save-mark-and-excursion
         (push-mark)
         (insert "
     <one line to give the program's name and a brief idea of what it does.>
     Copyright (C) <year>  <name of author>
     
     This program is free software: you can redistribute it and/or modify
     it under the terms of the GNU General Public License as published by
     the Free Software Foundation, either version 3 of the License, or
     (at your option) any later version.
     
     This program is distributed in the hope that it will be useful,
     but WITHOUT ANY WARRANTY; without even the implied warranty of
     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
     GNU General Public License for more details.
     
     You should have received a copy of the GNU General Public License
     along with this program. If not, see <http://www.gnu.org/licenses/>.")
         (comment-region (mark) (point))))
     
     (custom-set-variables
      '(sqlind-indentation-offsets-alist
        '((syntax-error sqlind-report-sytax-error)
          (in-string sqlind-report-runaway-string)
     
     
          (comment-continuation sqlind-indent-comment-continuation)
          (comment-start sqlind-indent-comment-start)
          (toplevel 0)
          (in-block +)
          (in-begin-block +)
          (block-start 0)
          (block-end 0)
          (declare-statement +)
          (package ++)
          (package-body 0)
          (create-statement +)
          (defun-start +)
          (labeled-statement-start 0)
          (statement-continuation +)
          (nested-statement-open sqlind-use-anchor-indentation +)
          (nested-statement-continuation sqlind-use-previous-line-indentation)
          (nested-statement-close sqlind-use-anchor-indentation)
          (with-clause sqlind-use-anchor-indentation)
          (with-clause-cte +)
          (with-clause-cte-cont ++)
          (case-clause 0)
          (case-clause-item sqlind-use-anchor-indentation +)
          (case-clause-item-cont sqlind-right-justify-clause)
          (select-clause sqlind-right-justify-clause)
          (select-column sqlind-indent-select-column)
          (select-column-continuation sqlind-indent-select-column +)
          ;; ((default . ++) (kinda . +) ( . sqlind-use-anchor-indentation))
          (select-join-condition ++) ; this should wrap
          (select-table sqlind-indent-select-table)
          (select-table-continuation sqlind-indent-select-table +)
          (in-select-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (insert-clause sqlind-right-justify-clause)
          (in-insert-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (delete-clause sqlind-right-justify-clause)
          (in-delete-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (update-clause sqlind-right-justify-clause)
          (in-update-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator))))
     (custom-set-variables
      '(sqlind-default-indentation-offsets-alist
        '((syntax-error sqlind-report-sytax-error)
          (in-string sqlind-report-runaway-string)
          (comment-continuation sqlind-indent-comment-continuation)
          (comment-start sqlind-indent-comment-start)
          (toplevel 0)
          (in-block +)
          (in-begin-block +)
          (block-start 0)
          (block-end 0)
          (declare-statement +)
          (package ++)
          (package-body 0)
          (create-statement +)
          (defun-start +)
          (labeled-statement-start 0)
          (statement-continuation +)
          (nested-statement-open sqlind-use-anchor-indentation +)
          (nested-statement-continuation sqlind-use-previous-line-indentation)
          (nested-statement-close sqlind-use-anchor-indentation)
          (with-clause sqlind-use-anchor-indentation)
          (with-clause-cte +)
          (with-clause-cte-cont ++)
          (case-clause 0)
          (case-clause-item sqlind-use-anchor-indentation +)
          (case-clause-item-cont sqlind-right-justify-clause)
          (select-clause sqlind-right-justify-clause)
          (select-column sqlind-indent-select-column)
          (select-column-continuation sqlind-indent-select-column +)
          (select-join-condition -- --)
          (select-table sqlind-indent-select-table)
          (select-table-continuation sqlind-indent-select-table +)
          (in-select-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (insert-clause sqlind-right-justify-clause)
          (in-insert-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (delete-clause sqlind-right-justify-clause)
          (in-delete-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
          (update-clause sqlind-right-justify-clause)
          (in-update-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator))))
     (defun qz/add-pdb-py-debug ()
       "add debug code and move line down"
       (interactive)
       (back-to-indentation)
       (insert "import pdb; pdb.set_trace();\n"))
     (custom-set-variables '(python-indent-offset 4))
     ;; NOWEB GOLANG START
     (with-eval-after-load 'go-mode
       (setq gofmt-command "golines")
       (add-hook 'go-mode-hook
                 (lambda () (add-hook 'before-save-hook
                                      'gofmt-before-save
                                      nil 'local)))
       )
     ;; NOWEB GOLANG END
     ;; NOWEB ES START
     (with-eval-after-load 'restclient
       (defun qz/es-choose-url (&optional url backend env)
         (interactive)
         (and qz/debug (message "DEBUG qz/es-choose-url: %s"
                                (list url backend env)))
         (let* ((backend (qz/es-choose-backend backend))
                (url (or url
                         (and backend env
                              (qz/es-choose-env env)
                              (format qz/newstore-es-string backend env)))))
           (message "es-default-url: %s"
                    (setq es-default-url
                          (or url (completing-read
                                   "es-url: " qz/newstore-es-urls)))))
         es-default-url)
       
       (defun qz/es-choose-backend (&optional backend)
         (interactive)
         (and qz/debug (message "DEBUG qz/es-choose-backend: %s" backend))
         (message "qz/newstore-es-backend-current: %s"
                  (setq qz/newstore-es-backend-current
                        (or backend (completing-read "es-backend: " qz/newstore-es-backends))))
         qz/newstore-es-backend-current)
       
       (defun qz/es-choose-env (&optional env)
         (interactive)
         (and qz/debug (message "DEBUG qz/es-choose-env: %s" env))
         (message "qz/newstore-es-env-current: %s"
                  (setq qz/newstore-es-env-current
                        (or env (completing-read "es-env: " qz/newstore-envs))))
         qz/newstore-es-env-current)
       
       (defun qz/test-es-ui (&optional url backend env)
         (setq qz/newstore-es-env-current nil
               qz/newstore-es-backend-current nil)
         (funcall-interactively 'qz/es-choose-url url backend env)
         (list
          qz/newstore-es-env-current
          qz/newstore-es-backend-current
          es-default-url))
       
       ;;(qz/test-es-ui)              ;; prompt, noset
       ;;(qz/test-es-ui nil)          ;; prompt, noset
       ;;(qz/test-es-ui nil nil)      ;; prompt, noset
       ;;(qz/test-es-ui nil nil nil)  ;; prompt, noset
       ;;(qz/test-es-ui nil 'kibana 'production)    ;; noprompt, set
       
       (defun qz/es-choose-cookie-headers ()
         "TODO"
         (interactive)
         (message
          "es-default-headers: %s"
          (setq es-default-headers `(("Content-Type" . "application/json; charset=UTF-8")
                                     ("Cookie" . ,(format "ACCEZZIOCOOKIE=%s"
                                                          (read-from-minibuffer "es cookie: ")))))))
       (setq es-default-url "https://elasticsearch-production.newstore.luminatesec.com"
             es-current-url es-default-url
             es-default-headers nil
             es-always-pretty-print t
             es-default-headers
             `(("Content-Type" . "application/json; charset=UTF-8")
               ("Cookie" . ,(format "ACCEZZIOCOOKIE=%s"
                                    "11fdbe68-b0f3-4dd0-9894-f97afe3662dc"))))
       
       (setq qz/newstore-es-string "https://%s-%s.newstore.luminatesec.com"
             qz/newstore-es-backends '(kibana elasticsearch)
             qz/newstore-es-backend-current nil
             qz/newstore-es-env-current nil
             qz/newstore-es-urls (cl-loop for env in qz/newstore-envs
                                          append (cl-loop for es-backend in qz/newstore-es-backends
                                                          collect (format qz/newstore-es-string es-backend env))))
       )
     ;; NOWEB ES END
     ;; NOWEB RESTCLIENT START
     (with-eval-after-load 'restclient
       (defvar qz/restclient-env nil)
       
       (defun qz/restclient-choose-env (&optional env)
         (interactive)
         (message "qz/restclient-env: %s"
                  (setq qz/restclient-env
                        (cdr (assoc (intern (or env
                                                (completing-read "restclient-env: " qz/newstore-envs)))
                                    qz/newstore-envs-abbrev))))
         qz/restclient-env)
       (defvar qz/restclient-tenant nil)
       
       (defun qz/restclient-choose-tenant (&optional tenant)
         (interactive)
         (message "qz/restclient-tenant: %s"
                  (setq qz/restclient-tenant
                        (or tenant (completing-read
                                    "restclient-tenant: " qz/newstore-tenants))))
         qz/restclient-tenant)
       (defvar qz/restclient-token nil)
       (defvar qz/restclient-token-field 'access_token)
       
       (defun qz/restclient-hook ()
         "Update token from a request."
         ;; url is visible while the hook is running.
         (let ((result))
           (save-excursion
             (cond
              ((string-suffix-p "/token" url)
               (condition-case nil
                   (progn
                     (setq result (cdr (assoc qz/restclient-token-field (json-read))))
                     (when (stringp result)
                       (progn
                         (setq qz/restclient-token result)
                         (message (concat "stored token: " qz/restclient-token)))))
                 (error (message "That wasn't cleanly handled."))))))))
       
       (add-hook 'restclient-response-loaded-hook 'qz/restclient-hook)
       (provide 'restclient-hooks)
       )
     ;; NOWEB RESTCLIENT END
     (setq magit-bind-magit-project-status t)
     (with-eval-after-load 'project
       (with-eval-after-load 'magit
         ;; Only more recent versions of project.el have `project-prefix-map' and
         ;; `project-switch-commands', though project.el is available in Emacs 25.
         (when (and magit-bind-magit-project-status
                    (boundp 'project-prefix-map))
           (unless ;; Only modify if it hasn't already been modified.
               (equal project-switch-commands
                      (eval (car (get 'project-switch-commands 'standard-value))
                            t))
             (message "qz: setting magit-project-status, but project-switch-commands has been changed already"))
           (progn
             (define-key project-prefix-map "m" 'magit-project-status)
             (add-to-list 'project-switch-commands '(magit-project-status "Magit") t)))))
     ;; NOWEB EMBARK START
     (define-key global-map (kbd "C-.") 'embark-act)
     (with-eval-after-load 'embark
       
       )
     ;; NOWEB EMBARK END
     ;; NOWEB CONSULT START
     (with-eval-after-load 'consult
       (require 's)
       
       (defun qz/consult-ripgrep-files (files)
         (let* ((consult-ripgrep-args (concat consult-ripgrep-args " -L"))
                (rg-dir "/tmp/null"))
           (f-delete rg-dir t)
           (mkdir rg-dir t)
           (mapcar (lambda (f)
                     (f-symlink (expand-file-name f)
                                (format "%s/%s-%s"
                                        rg-dir (gensym) (s-replace "/" "-" f))))
                   files)
           (consult-ripgrep rg-dir)))
       (defun qz/consult-ripgrep-bookmark ()
         (interactive)
         (let ((files (mapcar (lambda (b) (cdr (assoc 'filename b)))
                              bookmark-alist)))
           (qz/consult-ripgrep-files files)))
       
       (define-key global-map (kbd "C-c b s") 'qz/consult-ripgrep-bookmark)
       (define-key global-map (kbd "C-x C-M-SPC") 'consult-global-mark)
       (with-eval-after-load 'project
         (define-key project-prefix-map (kbd "M-g") 'consult-ripgrep)
         (add-to-list 'project-switch-commands '(consult-ripgrep "ripgrep") t))
       (mapcar (lambda (bind)
                 (define-key global-map (kbd (car bind)) (cadr bind)))
               '(("C-x b" consult-buffer)))
       )
     ;; NOWEB CONSULT END
     (with-eval-after-load 'pdf-view
       (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode))
     (defun pj-line-width () 650)
     ;;(require 'hyperbole) ;; TODO klink
     (define-key global-map (kbd "C-<down-mouse-2>") 'hkey-either)
     (define-key global-map (kbd "M-<return>") 'hkey-either)
     (global-hl-todo-mode 1)
     (global-hi-lock-mode 1)
     (defun qz/ensure-alias (alias &optional node)
       (let ((node (or node  (org-roam-node-at-point 'assert))))
         (save-excursion
           (with-current-buffer (find-file-noselect (org-roam-node-file node))
             (goto-char (org-roam-node-point node))
             (org-roam-alias-add alias)))))
     (defun qz/should-be-private-p (file)
       (with-current-buffer (or (find-buffer-visiting file)
                                (find-file-noselect file))
         (qz/is-private-p)))
     
     (defun qz/is-file-private ()
       (interactive)
       (message (concat "should " (f-this-file) " be private..? "
                        (or (and (qz/should-be-private-p (f-this-file)) "yes") "no"))))
     (defun qz/is-daily-p (&optional node &rest _)
       (if-let ((title (qz/node-title node)))
           (string-match-p qz/daily-title-regexp title)))
     (defun qz/is-project-p (&optional node &rest _)
       (or (qz/file-has-todo-p node _)
           (qz/is-daily-p node _)))
     (defun qz/is-person-p (&optional node &rest _)
       (qz/has-link-to-p (qz/title->roam-id "person")
                         (and node (org-roam-node-p node)
                              (org-roam-node-id node))))
     (defun qz/file-has-todo-p (&optional node &rest _)
       "Return non-nil if current buffer has any todo entry.
     
     TODO entries marked as done are ignored, meaning the this
     function returns nil if current buffer contains only completed
     tasks.
     
     (1) parse the buffer using org-element-parse-buffer. It
       returns an abstract syntax tree of the current Org buffer. But
       since we care only about headings, we ask it to return only them
       by passing a GRANULARITY parameter - 'headline. This makes
       things faster.
     
     (2) Then we extract information about TODO keyword from
       headline AST, which contains a property we are interested in -
       :todo-type, which returns the type of TODO keyword according to
       org-todo-keywords - 'done, 'todo or nil (when keyword is not
       present).
     
     (3) Now all we have to do is to check if the buffer list contains
       at least one keyword with 'todo type. We could use seq=find on
       the result of org-element-map, but it turns out that it provides
       an optional first-match argument that can be used for our needs."
       (save-excursion
         (with-current-buffer (or (and node (org-roam-node-p node)
                                       (find-file-noselect (org-roam-node-file node)))
                                  (current-buffer))
           (org-with-wide-buffer
            (org-element-map                          ; (2)
                (org-element-parse-buffer 'headline) ; (1)
                'headline
              (lambda (h)
                (eq (org-element-property :todo-type h)
                    'todo))
              nil 'first-match)))))                     ; (3)
     
     (defun qz/has-link-p (src dst)
       "undirected connection exists, from `src' to `dst'"
       (org-roam-db-query
        [:select [source dest]
                 :from links
                 :where (or (and (= dest $s1) (= source $s2))
                            (and (= dest $s2) (= source $s1)))]
        src dst))
     
     (defun qz/node-has-link-p (src dst)
       (qz/has-link-p (org-roam-node-id src)
                      (org-roam-node-id dst)))
     (setq qz/transitive-query "
     with recursive cte (id, degree) as (
     
       select n.id, 0 as degree
       from nodes n
       where n.id = $s1
     
       union all
     
       select distinct
         source as id, c.degree + 1 as degree
       from links l
       join cte c on l.dest = c.id
       where degree <= 1 -- therefore, yield 1st degree transitivity
     
     )
     select distinct id, degree
     from cte
     --where cte.id = $s2
     --where degree = 2
     order by degree desc
     ")
     
     (defun qz/transitive-links (dst)
       (org-roam-db-query qz/transitive-query dst))
     
     (defun qz/has-transitive-link-p (dst &optional src)
       (if-let* ((nap (or src (org-roam-node-at-point)))
                 (src (or src (org-roam-node-id nap))))
           (seq-contains
            (seq-map 'car
                     (org-roam-db-query qz/transitive-query dst src))
            src)
         ))
     
     ;;(seq-contains '(1 2 4) 1)
     
     (defun qz/node-has-transitive-link-p (dst &optional src)
       (qz/has-transitive-link (org-roam-node-id dst) src))
     
     ;;x(qz/has-transitive-link-p (qz/title->roam-id "emacs"))
     (defun qz/org-roam-migrate-jobs ()
       (interactive )
       (dolist (file (org-roam--list-all-files))
         (with-current-buffer (or (find-buffer-visiting file)
                                  (find-file-noselect file))
           (message "%s visiting" file)
           (qz/dispatch-hook)
           (save-buffer))))
     
                                             ;(qz/org-roam-migrate-jobs)
     
     (defun qz/file-created-as-timestamp (&optional file)
       (when-let* ((file (or file (buffer-file-name (current-buffer))))
                   (ffile (f-base file))
                   (p (string-match "-" ffile))
                   (s (substring ffile 0 p)))
         (cond
          ((= p 16) (let* ((td (timezone-parse-date s))
                           (tt (timezone-parse-time (elt td 3))))
                      (format "[%s %s]"
                              (s-join "-" (cl-subseq td nil 3))
                              (s-join ":" tt))))
          ((= p 14) (let* ((yy (substring s 0 4))
                           (mm (substring s 4 6))
                           (dd (substring s 6 8))
                           (hh (substring s 8 10))
                           (MM (substring s 10 12))
                           (ss (substring s 12 14)))
                      (format "[%s-%s-%s %s:%s:%s]"
                              yy mm dd
                              hh MM ss))))))
     
     (defun qz/org-roam-node-updated-precedence (pt file)
       (list (org-roam-get-keyword "UPDATED")
             (let ((v (s-join " " (org-entry-get-multivalued-property pt "UPDATED"))))
               (when (not (string-empty-p v)) v))
             (format-time-string
              "[%Y-%m-%d %H:%M:%S]"
              (file-attribute-modification-time (file-attributes file)))))
     
     (defun qz/org-roam-node-created-precedence (pt file)
       (list (org-roam-get-keyword "CREATED")
             (let ((v (s-join " " (org-entry-get-multivalued-property pt "CREATED"))))
               (when (not (string-empty-p v)) v))
             (qz/file-created-as-timestamp file)))
     
     (defun qz/org-roam-node-date-precedence (node date-fn)
       (save-excursion
         (let* ((pt (org-roam-node-point node))
                (file (org-roam-node-file node)))
           (with-current-buffer (or (find-buffer-visiting file)
                                    (find-file-noselect file))
             (goto-char pt)
             ;;(message "qz: getting node updated: %s" (org-roam-node-title node))
             (org-with-wide-buffer
              (car (remove nil (funcall date-fn pt file))))))))
     
     (defun qz/org-roam-node-updated-date (node)
       (qz/org-roam-node-date-precedence
        node
        (lambda (pt file)
          (append (qz/org-roam-node-updated-precedence pt file)
                  (qz/org-roam-node-created-precedence pt file)))))
     
     (defun qz/org-roam-node-created-date (node)
       (qz/org-roam-node-date-precedence
        node
        '(lambda (pt file)
           (append (qz/org-roam-node-created-precedence pt file)
                   (reverse (qz/org-roam-node-updated-precedence pt file))))))
     
     ;;(cl-subseq [1 2 3] 1 2)
     ;;(car [1 2 3])
     ;;(or (s-join "a" nil) "b")
     
     ;; (let ((node (org-roam-node-from-title-or-alias "wine")))
     ;;   (list (qz/org-roam-node-updated-date node)
     ;;         (qz/org-roam-node-created-date node)))
     
     ;;(funcall (lambda (a b c) (message "%s %s %s" a b c)) 1 2 3)
     
     (defun qz/do-migration--created-updated ()
       (let ((org-mode-hook nil))
         (cl-loop for node in (seq-map 'car (org-roam-db-query [:select id :from nodes]))
                  ;;(org-roam-node-list) groups erroneously
                  collect
                  (let ((node (org-roam-populate (org-roam-node-create :id node))))
                    (format "%s,%s,%s,%s,%s"
                            ;;"%s:%s,%s,%s,%s" to show with point
                            (org-roam-node-id node)
                            ;;(org-roam-node-point node)
                            (qz/org-roam-node-updated-date node)
                            (qz/org-roam-node-created-date node)
                            (org-roam-node-title node))
                    (kill-buffer (get-file-buffer (org-roam-node-file node)))))))
     ;; NOWEB ORG START
     (message "pre org: %s" (shell-command-to-string "date"))
     (with-eval-after-load 'org
       (require 's)
       (message "mid org: %s" (shell-command-to-string "date"))
       (define-key org-mode-map (kbd "C-c C-j") 'consult-org-heading)
       (defvar qz/org-babel-indent-exclude-lang
         '("yaml")
         "org-babel languages to exclude from auto indent/format.")
       
       ;;(setq qz/org-babel-indent-exclude-lang nil)
       ;;(setq qz/debug t)
       
       (defun qz/org-babel-indent-block (beg end &rest args)
         (interactive "r")
         (and qz/debug (message "qz/org-babel-indent-block: BEG %s END %s ARGS %s" beg end args))
         (save-mark-and-excursion
           (when (and (funcall-interactively 'org-babel-mark-block)
                      (not (seq-contains-p
                            qz/org-babel-indent-exclude-lang
                            (car (car (cdr (car (org-babel-tangle-single-block 1 t))))))))
             (call-interactively 'indent-region))))
       
       (define-key org-mode-map
                   (kbd "C-c C-v C-\\") 'qz/org-babel-indent-block)
       
       ;; NOTE: blocks default
       ;;(add-to-list 'org-ctrl-c-ctrl-c-hook 'qz/org-babel-indent-block)
       ;;(setq org-ctrl-c-ctrl-c-hook nil)
       ;;
       ;; NOTE: not the right eval/exec fn for `{C-c C-c}'
       ;;(advice-add 'org-babel-eval :before 'qz/org-babel-indent-block)
       ;;(advice-remove 'org-babel-eval 'qz/org-babel-indent-block)
       ;;
       ;; conclusion: use `advice' so as not to block standard org-mode
       ;; `{C-c C-c}' behaviour like with `org-ctrl-c-ctrl-c-hook'
       
       (qz/advice- org-babel-execute-src-block :before qz/org-babel-indent-block)
       (defun qz/org-refresh-inline-images (&rest args)
         (org-toggle-inline-images t)
         (org-toggle-inline-images t))
       
       (qz/advice- org-babel-execute-src-block :after qz/org-refresh-inline-images)
       ;; (define-key org-mode-map (kbd "C-c C-M-i")
       ;;             (lambda ()
       ;;               "go to default opening mode -- see `org-startup-folded'"
       ;;               (interactive)
       ;;               (let ((org-startup-folded (if current-prefix-arg
       ;;                                             t
       ;;                                           org-startup-folded)))
       ;;                 (funcall-interactively 'org-global-cycle '(4)))))
       
       (defun qz/org-fold ()
         "go to default opening mode -- see `org-startup-folded'"
         (interactive)
         (let ((prefix-arg current-prefix-arg))
           (funcall-interactively 'org-content (or current-prefix-arg 9999))))
       
       
       ;; current subtree
       ;; given some tree
       
       "
       a
       aa
        aa
         ab
       ab
       ac
       aca
        cb
         ba
        cd
       ad
       "
       
       (define-key org-mode-map (kbd "C-c C-M-i") 'qz/org-fold)
       
       (setq org-babel-default-header-args:sql
             '((:engine . "postgres")
               (:dbport . 5432) ;; NEVER connect to an external on 5433 by default
               (:dbhost . "localhost")
               ))
       
       (setq org-babel-python-command "python3")
       (setq org-babel-default-header-args:jq
             '((:results . "output")
               (:compact . "no")
               (:wrap . "src json")))
       
       ;; NOWEB AGENDA START
       
       (with-eval-after-load 'org-agenda
         (message "AGENDA start")
         (defun qz/agenda-files-update (&rest _)
           "Update the value of `org-agenda-files' with relevant candidates"
           (interactive)
           (setq org-agenda-files (qz/files-agenda)
                 qz/agenda-daily-files (qz/agenda-daily-files-f)))
         (defun qz/agenda-files-update-clock (&rest _)
           "An optimisation for org-clock, which is SO SLOW.
          Returns a LIST of files that contain CLOCK, which reduces
         processing a lot"
           (interactive)
           (setq org-agenda-files (qz/clock-files)))
         (list
          ;; optimisation setup: setup subset of clock files
          (qz/advice- org-clock-resolve :before qz/agenda-files-update-clock)
          ;; optimisation teardown: restore full set of agenda-files
          (qz/advice- org-clock-resolve :after qz/agenda-files-update))
         (setq qz/daily-title-regexp ".?[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.?")
         
         (defun qz/agenda-daily-files-f ()
           (seq-filter (lambda (s) (string-match qz/daily-title-regexp s))
                       org-agenda-files))
         
         ;;(qz/agenda-daily-files-f)
         (defun qz/clock-files ()
           (split-string
            (shell-command-to-string
             "rg CLOCK ~/life/roam/ -c | grep -v 'org#' | awk -F '[,:]' '{print $1}'")))
         (defun qz/files-agenda ()
           (seq-uniq (append qz/org-agenda-files (qz/project-files))))
         (defun qz/project-files ()
           "Return a list of note files containing Project tag."
           (seq-map
            'car
            (org-roam-db-query
             '(:select :distinct file
                       :from tags
                       :inner :join nodes
                       :on (= tags:node_id nodes:id)
                       :where (= tags:tag "project")))))
         (defun qz/org-roam-private-files ()
           "Return a list of note files containing tag =private="
           (seq-map
            'car
            (org-roam-db-query
             [:select :distinct file
                      :from tags
                      :inner :join nodes
                      :on (= tags:node_id nodes:id)
                      :where (= tags:tag "private")])))
         ;; current (default) sorting strat
         '((agenda habit-down time-up priority-down category-keep)
           (todo priority-down category-keep)
           (tags priority-down category-keep)
           (search category-keep))
         
         
         (defun qz/agenda-todo-dailies ()
           "the most necessary simple invention in months.
         (as of [2022-01-19 Wed])
         
         get a list of `TODO' entries, from daily files, ordered by date (from filename/category) DESCENDING.
         
         - see `qz/agenda-daily-files-f' for the subset view of `org-agenda-files'
         - see `org-agenda-sorting-strategy' for sort permutations."
           (interactive)
           (let* ((org-agenda-files (qz/agenda-daily-files-f))
                  (org-agenda-sorting-strategy '(timestamp-down category-down)))
             (org-todo-list)))
         
         (define-key global-map (kbd "C-c n t") 'qz/agenda-todo-dailies)
         (defun qz/org-agenda-gtd ()
           (interactive)
           (org-agenda nil "g")
           (goto-char (point-min))
           (org-agenda-goto-today))
         
         ;; HACK safe agenda ops on load
         (when (not (boundp 'org-agenda-custom-commands))
           (setq org-agenda-custom-commands nil))
         
         (require 'org-roam)
         
         ;; (message "agenda: setting custom commands\n%s" org-agenda-custom-commands)
         
         (add-to-list
          'org-agenda-custom-commands
          `("g" "GTD"
            ((agenda "" ((org-agenda-span 'day) (org-deadline-warning-days 60)))
             (tags-todo "now"
                        ((org-agenda-overriding-header "\nnow\n")))
             (tags-todo "wip"
                        ((org-agenda-overriding-header "\nwip\n")))
             (todo "TODO"
                   ((org-agenda-overriding-header "\nto process\n")
                    (org-agenda-files '(,(format "%s/%s" org-roam-directory "inbox.org")))))
             (todo "TODO"
                   ((org-agenda-overriding-header "\ndaily inbox\n")
                    (org-agenda-files qz/agenda-daily-files)))
             (todo "TODO"
                   ((org-agenda-overriding-header "\nemails\n")
                    (org-agenda-files '(,(format "%s/%s" org-roam-directory "emails.org")))))
             (todo "TODO"
                   ((org-agenda-overriding-header "\none-off Tasks\n")
                    (org-agenda-files '(,(format "%s/%s" org-roam-directory "next.org")))))
             (todo "TODO"
                   ((org-agenda-overriding-header "\nto yak shave\n")
                    (org-agenda-files '(,(format "%s/%s" org-roam-directory "emacs.org"))))))))
         (add-to-list
          'org-agenda-custom-commands
          `("c" "create"
            ((agenda "" ((org-agenda-span 'day) (org-deadline-warning-days 60)))
             (tags-todo "diy+create+do+buy+make+wip"
                        ((org-agenda-overriding-header "wip")))
             (tags-todo "diy+create+do"
                        ((org-agenda-overriding-header "create")))
             (tags-todo "buy"
                        ((org-agenda-overriding-header "buy")))
             (tags-todo "make"
                        ((org-agenda-overriding-header "make"))))))
         (add-to-list
          'org-agenda-custom-commands
          `("w" "work"
            ((tags-todo "{work}+wip"
                        ((org-agenda-overriding-header "wip")
                         (org-tags-match-list-sublevels nil) ;; show subheadings!!!! inherited!!!!
                         ;; (org-agenda-hide-tags-regexp
                         ;;  (concat org-agenda-hide-tags-regexp "\\|work"))
                         ))
             (tags-todo "{work}"
                        ((org-agenda-overriding-header "work")))
             )))
         
         ;;(pp org-agenda-custom-commands)
         (add-to-list
          'org-agenda-custom-commands
          '("1" "Events" agenda "display deadlines and exclude scheduled"
            ((org-agenda-span 'year)
             (org-agenda-time-grid nil)
             (org-agenda-show-all-dates nil)
             (org-agenda-entry-types '(:deadline)) ;; this entry excludes :scheduled
             (org-deadline-warning-days 0) )))
         
         
         (defun zin/org-agenda-skip-tag (tag &optional others)
           "Skip all entries that correspond to TAG.
         
         If OTHERS is true, skip all entries that do not correspond to TAG."
           (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
                 (current-headline (or (and (org-at-heading-p)
                                            (point))
                                       (save-excursion (org-back-to-heading)))))
             (if others
                 (if (not (member tag (org-get-tags-at current-headline)))
                     next-headline
                   nil)
               (if (member tag (org-get-tags-at current-headline))
                   next-headline
                 nil))))
         
         
         (add-to-list
          'org-agenda-custom-commands
          '("0" "moving"
            ;; TODO: Add A priority to the top.
            ((agenda
              ""
              ((org-agenda-time-grid nil)
               (org-agenda-start-on-weekday nil)
               (org-agenda-start-day "+1d")
               (org-agenda-span 160)
               (org-agenda-show-all-dates nil)
               (org-agenda-time-grid nil)
               (org-agenda-show-future-repeats nil)
               (org-agenda-block-separator nil)
               (org-agenda-entry-types '(:deadline))
               (org-agenda-skip-function
                '(not
                  (zin/org-agenda-skip-tag "moving" 't)
                  ;; '(org-agenda-skip-entry-if 'notregexp":moving:") ; no love for inherited tags
                  ))
               (org-agenda-overriding-header "\nUpcoming deadlines (+160d)\n")))
             (agenda
              "*"
              ((org-agenda-block-separator nil)
               (org-agenda-span 160)
               (org-agenda-show-future-repeats nil)
               (org-agenda-skip-deadline-prewarning-if-scheduled t)
               (org-agenda-skip-function
                '(org-agenda-skip-entry-if 'notregexp":moving:"))
               (org-agenda-overriding-header "\nAgenda\n")))
             (tags-todo "moving"
                        ((org-agenda-block-separator nil)
                         (org-agenda-skip-function '(or (org-agenda-skip-if nil '(scheduled))))
                         (org-agenda-overriding-header "\nMoving Backlog\n"))))))
         (defvar qz/agenda-daily-files nil)
         (setq org-agenda-hide-tags-regexp "project")
         (defun qz/org-category (&optional len)
           (let* ((len (or len 25)))
             (->>
              (if buffer-file-name
                  (file-name-sans-extension (file-name-nondirectory buffer-file-name))
                "")
              (replace-regexp-in-string "private-" "")
              (replace-regexp-in-string
               ;; datetime from file, could do "[0-9]\\{6\\}T[0-9]\\{6\\}Z?-"
               (concat "[0-9][0-9][0-9][0-9]" "[0-9][0-9]" "[0-9][0-9]"
                       "T" "[0-9][0-9]" "[0-9][0-9]" "[0-9][0-9]" "Z-")
               "")
              (s-pad-right len " ")
              (s-truncate len))))
         
         ;;(qz/org-category)
         (let* ((agenda "  %(qz/org-category)%-12t% s")
                (other "%i%(qz/org-category 12)%l"))
           (setq org-agenda-prefix-format (list (cons 'agenda agenda)
                                                (cons 'todo other)
                                                (cons 'todo other)
                                                (cons 'todo other)
                                                (cons 'search other))))
         
         (defun vulpea-agenda-category (&optional len)
           "Get category of item at point for agenda.
         
         Category is defined by one of the following items:
         - CATEGORY property
         - TITLE keyword
         - TITLE property
         - filename without directory and extension
         
         When LEN is a number, resulting string is padded right with
         spaces and then truncated with ... on the right if result is
         longer than LEN.
         
         Usage example:
         
           (setq org-agenda-prefix-format
                 '((agenda . \" Emacs Configuration %?-12t %12s\")))
         
         Refer to `org-agenda-prefix-format' for more information."
           (let* ((file-name (when buffer-file-name
                               (file-name-sans-extension
                                (file-name-nondirectory buffer-file-name))))
                  (title (qz/node-title))
                  (category (org-get-category))
                  (result
                   (or (if (and title
                                (string-equal category file-name))
                           title
                         category)
                       "")))
             (if (numberp len)
                 (s-truncate len (s-pad-right len " " result))
               result)))
         (org-no-warnings (defvar date))
         (defun qz/org-lunar-phases ()
           "Show lunar phase in Agenda buffer."
           (require 'lunar)
           (let* ((phase-list (lunar-phase-list (nth 0 date)
                                                (nth 2 date)))
                  (phase (cl-find-if (lambda (phase)
                                       (equal (car phase) date))
                                     phase-list)))
             (when phase
               (setq ret (concat (lunar-phase-name (nth 2 phase)))))))
         ;; 🌑🌒🌓🌔🌕🌖🌗🌘🌙🌚🌛🌜
         (setq lunar-phase-names
               '("🌚 new moon" ; unicode symbol : 🌑 use full circle as fallback
                 "🌛 first quarter moon"
                 "🌝 full moon" ; unicode symbol: 🌕 use empty circle as fallback
                 "🌜 last quarter moon"))
         (setq calendar-latitude 52.5)  ; imprecise
         (setq calendar-longitude 13.4)
         (setq calendar-location-name "berlin")
         
         (autoload 'solar-sunrise-sunset "solar.el")
         (autoload 'solar-time-string "solar.el")
         (defun qz/diary-sunrise ()
           "Local time of sunrise as a diary entry.
         The diary entry can contain `%s' which will be replaced with
         `calendar-location-name'."
           (let ((l (solar-sunrise-sunset date)))
             (when (car l)
               (concat
                (if (string= entry "")
                    "🌄 sunrise"
                  (format entry (eval calendar-location-name))) " "
                (solar-time-string (caar l) nil)))))
         
         (defun qz/diary-sunset ()
           "Local time of sunset as a diary entry.
         The diary entry can contain `%s' which will be replaced with
         `calendar-location-name'."
           (let ((l (solar-sunrise-sunset date)))
             (when (cadr l)
               (concat
                (if (string= entry "")
                    "🌅 sunset"
                  (format entry (eval calendar-location-name))) " "
                (solar-time-string (caadr l) nil)))))
         )
       
       ;; NOWEB AGENDA END
       
       ;(require 'ob-async)
       (setq org-confirm-babel-evaluate nil)
       (setq org-structure-template-alist
             '(;; yp
               ("d"  . "definition")
               ("ee" . "example")
               ("es" . "src es")
               ("el" . "src emacs-lisp")
               ("q"  . "quote")
               ("sb" . "src shell")
               ("se" . "src emacs-lisp")
               ("sl" . "src scheme")
               ("sp" . "src sql :engine postgres")
               ("sr" . "src R")
               ("ss" . "src")
               ("jp" . "src jupyter-python")
               ("jr" . "src jupyter-R")
               ("r"  . "src restclient")))
       (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
          (lisp . t)
          ;;(jupyter . t)
          (python . t)
          (jq . t)
          ;;(ipython . t)
          (scheme . t)
          (sql . t)
          ;;(plant-uml . t)
          (shell . t)
          (sqlite . t)
          ;;(elasticsearch . t)j ;; TODO rde package es/rest
          ;;(restclient . t)
          (R . t)))
       (defun qz/org-babel-choose-block (&optional lob)
         "choose block, insert scaffold for args.
       
       might honestly be better to generate `yas' template when we load
       blocks with `qz/org-babel-do-lob-ingest-files', but I've never used
       yas so idk
       
       use a prefix arg to shortcut (org-table-get-constant \"bonk\"
       "
         (interactive)
         (message "prefix: %s" (list current-prefix-arg prefix-arg lob))
         (let ((lob (or lob
                        (intern (completing-read
                                 "lob: " (mapcar 'car org-babel-library-of-babel))))))
           (with-current-buffer (current-buffer)
             (end-of-line)
             (newline)
             (insert (format "#+name: call-%s\n#+call: %s(%s)"
                             lob lob (or (and current-prefix-arg
                                              "(org-table-get-constant \"bonk\")")
                                         "")))
       
             (when-let
                 ((args (remove
                         nil (cl-loop for a in (assoc lob org-babel-library-of-babel)
                                      append
                                      (when (listp a)
                                        (cl-loop for b in a
                                                 collect
                                                 (when (eq :var (car b)) (cdr b))))))))
               (message "%s" args)
               (insert (format "(%s)" (s-join ", " args)))))))
       
       ;;(qz/org-babel-choose-block 'newstore-get-order-by-type)
       (defun qz/lob-get-named-src-block-body (name)
         (cl-destructuring-bind
             (file . pt) (qz/lob-get-named-src-block name)
           (with-current-buffer (find-file-noselect file)
             (save-excursion
               (goto-char pt)
               (org-babel-expand-src-block)))))
       
       ;;(apply 'format "hey %s %s %s" (list 1 2 4))
       
       (defun qz/named (name &rest args)
         "shorthand wrapper of `qz/lob-get-named-src-block-body', for clearer header args"
         (apply 'format (qz/lob-get-named-src-block-body name) args))
       
       (defun qz/lob-get-named-src-block (name)
         (message "checking name: %s" name)
         (cl-block named    ; thank u cltl, thank u 1980s, thank u guy steele
           (save-excursion  ; check current-buffer
             (when (not (org-babel-goto-named-src-block name))
               (cl-return-from named (cons (buffer-file-name) (point)))))
           (mapcar (lambda (f)
                     (with-current-buffer (find-file-noselect f)
                       (save-excursion
                         ;; it's odd that nil means "i found it"
                         (when (not (org-babel-goto-named-src-block name))
                           (cl-return-from named (cons f (point)))))))
                   (remove nil qz/org-babel-lob-ingest-files))))
       
       (defun qz/lob-goto-named-src-block (name)
         (interactive
          (list
           (completing-read "lob: " (mapcar 'car org-babel-library-of-babel))))
         (cl-destructuring-bind
             (file . pt) (qz/lob-get-named-src-block name)
           (find-file file)
           (goto-char pt)))
       (defun qz/lob-restclient-copy-curl-command (&optional name)
         "this one was a struggle"
         (interactive)
         (when-let ((name (or name (thing-at-point 'symbol))))
           (cl-destructuring-bind
               (file . pt) (qz/lob-get-named-src-block name)
             (save-excursion
               (with-current-buffer (find-file-noselect file)
                 (goto-char pt)
                 (next-line)
                 (let ((expanded (org-babel-expand-src-block)))
                   (message "expanded: %s" expanded)
                   (with-temp-buffer ;;(get-buffer-create "*restclient*") ;;TODO replace w temp
                     (restclient-mode)
                     (insert expanded)
                     (goto-char (point-min))
                     (restclient-jump-next)
                     (restclient-copy-curl-command))))))))
       (define-key org-babel-map (kbd "M-l") 'qz/org-babel-choose-block)
       (define-key org-babel-map (kbd "M-l") 'qz/org-babel-choose-block)
       (define-key org-babel-map (kbd "M-g") 'qz/lob-goto-named-src-block)
       (defun qz/org-babel-make-table-constants ()
         "exec from the top of a tree"
         (interactive)
         (let* ((hi-lock-auto-select-face t)
                (write-constants (equal '(4) current-prefix-arg))
                ;; above is 100x better when you patch `hi-lock-face-symbol-at-point'
                ;; with `(or (and hi-lock-auto-select-face (hi-lock-read-face-name)) 'hi-yellow)'
                (col '()))
           (save-mark-and-excursion
             (org-map-tree
              (lambda ()
                (when-let* ((s (org-get-heading))
                            (s (org-no-properties s))
                            (i (string-match "::" s))
                            (k (substring s 0 (- i 1)))
                            (v (substring s (+ 3 i))))
                  (message "key: %s" k)
                  (message "value: %s" v)
                  (setq col (cons (format "%s=%s" k v) col))
                  (funcall-interactively 'highlight-phrase v)
                  (message "applied highlight for '%s'" v)
                  )))
             (when write-constants
               (org-back-to-heading)
               (next-line)
               (newline)
               (previous-line)
               (insert (format "#+constants: %s" (s-join " " (reverse col))))))
           (message "col: %s" col)
           col))
       
       (define-key org-babel-map (kbd "M-d") 'qz/org-babel-make-table-constants)
       (defun qz/to-shell (command)
         (interactive) ;; TODO how to interactive bind to `command'??
         (with-current-buffer (vterm "*to-shell*")
           (mapc
            (lambda (c)
              (message c)
              (vterm-send-string c)
              (vterm-send-return))
            (qz/ensure-list command))))
       
       (defun qz/current-src-block ()
         (interactive)
         (s-split
          "[\n]"
          (kill-new (nth 6 (car ;; lspec
                            (cdr (car
                                  (save-excursion
                                    (when-let ((head (org-babel-where-is-src-block-head)))
                                      (goto-char head))
                                    (org-babel-tangle-single-block 1 t)))))))))
       
       (defun qz/shell-current-src-block ()
         (interactive)
         (when-let ((command (qz/current-src-block)))
           (qz/to-shell command)))
       
       (define-key org-babel-map (kbd "C-<return>") 'qz/shell-current-src-block)
       (defun qz/org-kill-expanded-src-block ()
         (interactive)
         (kill-new (org-babel-expand-src-block))
         (message "copied: %s" (s-truncate 25 (car kill-ring))))
       
       (define-key org-babel-map (kbd "M-w") 'qz/org-kill-expanded-src-block)
       (defun qz/org-babel--list->rows (name lst)
         (cons (list name)
               (cons 'hline (mapcar 'list lst))))
       (with-eval-after-load 'org-roam
         ;; NOWEB ROAM START
         (message "roam start")
         (setq qz/org-roam-dailies-filespec "private-%<%Y-%m-%d>.org")
       
         (defun qz/inspect-agenda-files ()
           `((org-files-list . ,(length (org-files-list)))
             ((org-agenda-files . ,(length (org-agenda-files)))
              ((qz/project-files . ,(length (qz/project-files)))
               (qz/agenda-daily-files-f . ,(length (qz/agenda-daily-files-f)))))))
         (defun qz/inspect-agenda-updates ()
           (mapcar (lambda (s) `(,s . (,(progn (funcall s)
                                               (qz/inspect-agenda-files)))))
                   '(qz/agenda-files-update qz/agenda-files-update-clock)))
         (setq qz/org-agenda-files
               (mapcar (lambda (f) (expand-file-name (format "%s/%s" org-roam-directory f)))
                       '("calendar-home.org" "calendar-work.org" "schedule.org")))
         (defvar qz/org-babel-lob-ingest-files
           (append (mapcar (lambda (s)
                             (when-let ((n (org-roam-node-from-title-or-alias s)))
                               (org-roam-node-file n)))
                           '("NewStore"
                             "kubernetes"
                             "postgres"
                             "es-mode"
                             "elisp"
                             "plantuml"
                             "GNU Guix"
                             "git"
                             ))
                   ;; .. other files
                   nil
                   ;; ..
                   )
           "files from which named `src' blocks should be loaded")
         
         (defun qz/org-babel-do-lob-ingest-files (&optional files)
           (interactive)
           (let ((r (mapcar (lambda (f) (cons (org-babel-lob-ingest f) f))
                            (append qz/org-babel-lob-ingest-files files))))
             ;;(message "%s" (pp r))
             r))
         
         (cons->table
          (qz/org-babel-do-lob-ingest-files))
         (unless (boundp 'org-agenda-directory)
           (setq org-agenda-directory nil))
         (defun qz/create-node ()
           "assumes point is at the desired headline"
           (interactive)
           (org-id-get-create)
           (org-delete-property "ROAM_EXCLUDE"))
         
         (defun qz/exclude-node ()
           "assumes point is at the desired headline -- unlikely to work for files"
           (org-set-property "ROAM_EXCLUDE" "t"))
         
         (define-key org-mode-map (kbd "C-c C-x i") 'qz/create-node)
         (define-key org-mode-map (kbd "C-c C-x i") 'qz/create-node)
         (defun qz/org-roam-capture-current ()
           (interactive)
           "Capture a task in agenda mode."
           (org-capture nil "I"))
         
         (defun qz/roam-capture-todo ()
           (interactive)
           "Capture a task in agenda mode."
           (cl-destructuring-bind (thing region)
               (qz/thing-at-point-or-region-and-region)
             (org-roam-capture- :goto t
                                :keys "n"
                                :node (org-roam-node-create :title thing)
                                :props `(:immediate-finish t :jump-to-captured nil
                                                           :region ,region     :insert-at ,(point-marker)
                                                           :finalize 'insert-link))
             (qz/capture-last-captured)))
         (defun qz/utc-timestamp ()
           (format-time-string "%Y%m%dT%H%M%SZ" (current-time) t))
         (setq qz/org-roam-capture-head "#+title: ${title}\n")
         (setq qz/capture-title-timestamp-roam "%(qz/utc-timestamp)-${slug}.org")
         
         (setq org-roam-capture-templates
               `(("d" "default" plain "%?"
                  :if-new (file+head ,qz/capture-title-timestamp-roam
                                     ,qz/org-roam-capture-head)
                  :unnarrowed t)
                 ("n" "empty" plain "%?"
                  :if-new (file+head ,qz/capture-title-timestamp-roam
                                     ,qz/org-roam-capture-head)
                  :immediate-finish t)
                 ))
         (setq org-roam-dailies-capture-templates
               `(("d" "default" entry
                  ,(s-join "\n" '("* [%<%H:%M:%S>] %?"
                                  ;;"CREATED: <%<%Y-%m-%d %H:%M>>"
                                  "- from :: %a"
                                  ;;"- from (point) :: %f"
                                  "- clocking :: %K" ; REVIEW does this solve tangents?
                                  ;; the region one is a bit tricky
                                  ;;"- region ::\n  #+begin_quote\n%(\"%i\")\n#+end_quote"
                                  ))
                  :if-new (file+head+olp
                           ,qz/org-roam-dailies-filespec
                           ,(s-join "\n" '("#+title: <%<%Y-%m-%d>>"
                                           "#+filetags: daily private project" "" ""
                                           "%(qz/today-dateref)" "" ""
                                           "* today, I will"))
                           ("journal")))))
         
         (setq qz/org-roam-dailies-capture-templates--tangent
               '("d" "default" entry
                 ,(s-join "\n" '("* TANGENT [%<%H:%M>] %?"
                                 ;;"CREATED: <%<%Y-%m-%d %H:%M>>"
                                 "- from :: %a"))
                 :if-new (file+head+olp
                          ,qz/org-roam-dailies-filespec
                          ,(s-join "\n" '("#+title: <%<%Y-%m-%d>>"
                                          "#+filetags: daily private project" ""
                                          "%(qz/today-dateref)" ""
                                          "* today, I will"
                                          "* journal"
                                          "* tangent"))
                          ("tangent"))))
         ;;; day lookup
         (defvar qz/day-lookup
           '((Mon . "[[id:d5ad0bac-e82b-43d0-960f-26eeb1daf91b][Monday]]")
             (Tue . "[[id:cb662cc6-bde2-4f9c-b3fa-62346c6df27a][Tuesday]]")
             (Wed . "[[id:411a8e5a-8d89-4886-b2ea-047a3970710a][Wednesday]]")
             (Thu . "[[id:659b9931-ae09-422b-8e91-1bf4cc58e94c][Thursday]]")
             (Fri . "[[id:b3255cd1-db37-4e07-99cf-5e60d52a2579][Friday]]")
             (Sat . "[[id:b63897c3-30cc-42eb-83b5-c8e372e5af9a][Saturday]]")
             (Sun . "[[id:2e28574b-4793-4c05-b83d-e36e9a77515b][Sunday]]"))
           "an index; get days from abbrev (assoc 'Mon qz/day-lookup)")
         (defvar qz/month-lookup
           '("[[id:b92355d7-110e-467c-b7a7-d02b2043af3f][January]]"
             "[[id:7e0af966-8d3e-4e88-b53f-d074902e175a][February]]"
             "[[id:f41751f8-a2a9-4b38-ba03-2ceec2fae4cc][March]]"
             "[[id:ae0ae458-2216-4178-8073-4a26f23747d9][April]]"
             "[[id:6a680100-e842-4257-819f-8cf6cbedddbc][May]]"
             "[[id:f811621c-1b37-43f7-9d01-52bdf9f27637][June]]"
             "[[id:a4d5c8fe-3910-4483-b59e-ce50cd6699a7][July]]"
             "[[id:94e9b0a7-6cd0-4104-821e-613876fe76e3][August]]"
             "[[id:f9ad8160-cae5-4195-a85f-0160710ce8dd][September]]"
             "[[id:da9f0d53-e3f7-4f72-bc1a-d060bc2d1745][October]]"
             "[[id:a4e3a97a-dac9-4bc6-a5e9-5949f707a6de][November]]"
             "[[id:f874ca1a-0d3f-4840-8340-511ed0ac286f][December]]")
           "an index; get days from abbrev (nth 0 qz/month-lookup)")
         (defun qz/today-dateref (&optional time)
           (cl-destructuring-bind (day nday month year)
               (split-string
                (format-time-string "%a:%d:%m:%Y" (or nil (current-time))) ":")
             (format "%s %s %s, %s"
                     (cdr (assoc (intern day) qz/day-lookup))
                     nday
                     (nth (- (string-to-number month) 1) qz/month-lookup)
                     (or (if-let ((node (org-roam-node-from-title-or-alias year)))
                             (org-link-make-string
                              (concat "id:" (org-roam-node-id node))
                              (org-roam-node-title node)))
                         year))))
         (defun qz/org-daily-tangent-capture ()
           (interactive)
           "Capture the inevitable tangent"
           (org-capture nil "t"))
         (defun qz/today-as-daily-file ()
           (format-time-string "private-%Y-%m-%d.org"))
         ;; [[file:~/.doom.d/config.org::*capture convenience functions][capture convenience functions]]
         (defun qz/current-roam-link ()
           "Get link to org-roam file with title"
           (interactive)
         
           (concat "* TODO "
                   (let ((n (qz/org-roam-node-at-point)))
                     (org-link-make-string
                      (concat "id:" (org-roam-node-id n))
                      (org-roam-node-title n)))))
         (defun qz/node-tags (&optional node)
           (or (and node (org-roam-node-tags node))
               (save-excursion
                 (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
                 (if (= (org-outline-level) 0)
                     (split-string-and-unquote (or (cadr (car (org-collect-keywords '("filetags")))) ""))
                   (org-get-tags)))))
         
         (defun qz/node-title (&optional node limit)
           (or (and node (org-roam-node-title node))
               (save-excursion
                 (goto-char (org-roam-node-point (org-roam-node-at-point 'assert)))
                 (if (= (org-outline-level) 0)
                     (cadr (car (org-collect-keywords '("title"))))
                   (substring-no-properties (org-get-heading t t t))))))
         (defun qz/title->roam-id (title)
           (org-roam-node-id (org-roam-node-from-title-or-alias title)))
         (defun qz/ensure-tag (tagstring tag)
           "Apply `org-roam-tag-add' for `tag' to `(OR node@pt NODE)'"
           (let ((ltag (-flatten (or (and (listp tag) tag)
                                     (list tag)))))
             (message "ensuring tag for %s" ltag)
             (org-roam-tag-add ltag)))
         
         (defun qz/org-roam--insert-timestamp (&rest args)
           (when (not (org-entry-get nil "CREATED"))
             (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]")))
           (qz/org-roam--updated-timestamp))
         
         (defun qz/org-roam--updated-timestamp (&rest args)
           "on the current-heading, and current-node"
           (interactive)
           (mapcar (lambda (pt)
                     (when pt
                       (org-entry-put
                        pt "UPDATED"
                        (format-time-string "[%Y-%m-%d %a %H:%M]"))))
                   (list (and (org-roam-node-at-point)
                              (org-roam-node-point (org-roam-node-at-point)))
                         (save-excursion
                           (org-back-to-heading-or-point-min)
                           (point)))))
         
         (add-hook 'org-roam-capture-new-node-hook 'qz/org-roam--insert-timestamp)
         (add-hook 'org-mode-hook (lambda ()
                                    (add-hook 'before-save-hook
                                              'qz/org-roam--updated-timestamp nil t)))
         (qz/advice- org-id-get-create :after qz/org-roam--insert-timestamp)
         (defun qz/hard-refresh-org-tags-in-buffer ()
           (interactive)
           (setq org-file-tags nil)      ; blast the cache
           (org-set-regexps-and-options) ; regen property detection regexp
           (org-get-tags))               ; write to cache
         (defun qz/title-to-tag (title &optional capitalize?)
           "Convert TITLE to tag."
           (if (equal "@" (cl-subseq title 0 1))
               title
             (concat "@" (s-replace " " ""
                                    (or (and capitalize?
                                             (capitalize title))
                                        title)))))
         (defun qz/org-roam-node-from-tag (tag)
           (seq-map
            'car
            (org-roam-db-query
             [:select :distinct file
                      :from tags
                      :inner :join nodes
                      :on (= tags:node_id nodes:id)
                      :where (= tags:tag tag)])))
         (defun qz/note-buffer-p (&optional node &rest _)
           "Return non-nil if the currently visited buffer is a note."
           (interactive)
           (or (org-roam-node-p node)
               (and buffer-file-name (org-roam-file-p buffer-file-name))))
         (defun qz/is-private-p (&optional node &rest _)
           (interactive)
           (let ((title (qz/node-title node)))
             (if (not title)
                 (and (message "unable to evaluate privateness; no title") nil) ; return false (not private)
               (or (string-match-p qz/daily-title-regexp title) ; daily
                   (string-match-p "meeting" title)             ; concerns a meeting
                   (qz/has-link-to-p                            ; concerns work
                    (mapcar 'qz/title->roam-id
                            (list "thinkproject" "NewStore")))))))
         (defun qz/has-links (node)
           "connections exist, for id of `node'"
           (org-roam-db-query
            [:select [source dest]
                     :from links
                     :where (or  (= dest $s1)
                                 (= source $s1))]
            node))
         
         (defun qz/node-has-links (node)
           "connections exist, for `node'"
           (qz/has-links (org-roam-node-id node)))
         (defun qz/has-link-to-p (dst &optional src)
           "directed connection exists, from `src' to `dst'"
           (if-let* ((nap (or src (org-roam-node-at-point)))
                     (src (or src (org-roam-node-id nap))))
               (org-roam-db-query
                [:select dest
                         :from links
                         :where (and (= source $s1)
                                     (IN dest $v2))]
                src (apply 'vector (qz/ensure-list dst)))))
         
         (defun qz/node-has-link-to-p (dst &optional src)
           (qz/has-link-to-p (org-roam-node-id dst)
                             (and dst (org-roam-node-id dst))))
         ;;; ref capture
         (setq org-roam-capture-ref-templates
               `(("r" "ref" plain
                  "\n#+begin_quote\n${body}\n#+end_quote\n%?"
                  :if-new (file+head ,qz/capture-title-timestamp-roam
                                     "#+title: ${title}\n")
                  :unnarrowed t)))
         (defun qz/roam-buffer-image-width ()
           (setq-local org-image-actual-width 150)
           (org-redisplay-inline-images))
         
         (add-hook 'org-roam-mode-hook 'qz/roam-buffer-image-width)
         (cons->table
          (add-to-list 'magit-section-initial-visibility-alist (cons 'org-roam-node-section 'hide)))
         ;; [[file:~/.doom.d/config.org::*templates][templates]]
         (setq org-capture-templates
               `(;; basic fire&forget
                 ("i" "inbox" entry
                  (file ,(concat org-roam-directory "/inbox.org"))
                  "* TODO %? \n\n - from :: %a")
         
                 ;; spanish language capturing
                 ("v" "vocab; spanish" entry
                  (file+headline ,(concat org-roam-directory "/spanish_language.org") "vocab, phrases")
                  ,(s-join "\n" '("** \"%?\" :es:"
                                  "- from :: %a" ""
                                  "*** :en:" "")))
         
                 ;; capture link to live `org-roam' thing
                 ("n" "now, as in NOW" entry (file ,(concat org-roam-directory "/wip.org"))
                  ,(s-join "\n" '("* TODO [#A1] %? "
                                  "DEADLINE: %T"
                                  "CREATED: %u")))
         
                 ;; fire directly into inbox from outside of emacs
                 ("c" "org-protocol-capture" entry (file ,(concat
                                                           org-roam-directory "/inbox.org"))
                  ,(s-join "\n" '("* TODO [[%:link][%:description]]" ""
                                  "#+begin_quote" ""
                                  "%i"
                                  "#+end_quote"))
                  :immediate-finish t)
         
                 ;; push last captured item into inbox
                 ("l" "last-capture" entry (file ,(concat org-roam-directory "/inbox.org"))
                  (function qz/inbox-last-captured)
                  :immediate-finish t)
         
                 ("I" "current-roam" entry (file ,(concat org-roam-directory "/inbox.org"))
                  (function qz/current-roam-link)
                  :immediate-finish t)
         
                 ("W" "weekly review" entry
                  (file+datetree ,(concat org-roam-directory "/reviews.org"))
                  ;; ... from template
                  (file ,(concat org-roam-directory "/templates/weekly_review.org")))
         
                 ("D" "daily review" entry
                  (file+datetree ,(concat org-roam-directory "/reviews.org"))
                  ;; ... from template
                  (file ,(concat org-roam-directory "/templates/daily_review.org")))
         
                 ("S" "screenshot" entry
                  (file ,(concat org-roam-directory "/screenshots.org"))
                  ,(s-join "\n" '("* screenshot: %?" ""
                                  "%(qz/screenshot-clip)"
                                  )))
                 ))
         
         
         (defun qz/screenshot-clip ()
           (interactive)
           (let ((default-directory (concat org-roam-directory "/images")))
             (s-replace "file:" (format "file:%s/" default-directory)
                        (with-temp-buffer
                          (let ((default-directory default-directory))
                            (org-mode)
                            (org-download-clipboard)
                            (buffer-string))))))
         
         
         
         
         ;; (setq org-screenshot-method ;;"/gnu/store/n4arghf8l3f6svv2xlxwnvw4jcwa48qk-sway-shot-output %s"
         ;;       "/gnu/store/am017g4gdhf45kvg5xkp3s2lhkbfdwzh-sway-shot-window-or-selection %s")
         
         
         ;; [[file:~/.doom.d/config.org::*capture templates][roam capture templates]]
         
         ;; NOWEB ROAM END
         )
       (require 'org-download)
       (defun qz/org-inbox-capture ()
         (interactive)
         "Capture a task in agenda mode."
         (org-capture nil "i"))
       (define-key org-mode-map (kbd "C-<return>")
                   'org-insert-heading-respect-content)
       (define-key org-mode-map (kbd "C-S-<return>")
                   'org-insert-heading)
       
       (defun org-insert-subheading-respect-content (arg)
         (interactive "P")
         (let ((org-insert-heading-respect-content t))
           (org-insert-subheading arg)))
       
       (define-key org-mode-map (kbd "C-M-<return>")
                   'org-insert-subheading-respect-content)
       (define-key org-mode-map (kbd "C-M-S-<return>")
                   'org-insert-subheading)
       
       (define-key org-mode-map (kbd "C-c C-M-<") 'org-do-promote)
       (define-key org-mode-map (kbd "C-c C-M->") 'org-do-demote)
       
       (define-key org-mode-map (kbd "C-c C-M-p")
                    (lambda ()
                      (interactive)
                      (make-marker)
                      (org-up-heading-or-point-min)))
       (setq org-agenda-columns-add-appointments-to-effort-sum t)
       (setq org-agenda-default-appointment-duration 30)
       (add-to-list 'org-global-properties
                    '("Effort_ALL" . "0 0:15 0:30 1:00 2:00 4:00 8:00"))
       (setq org-confirm-babel-evaluate nil)
       ;; [[file:~/.doom.d/config.org::*refile][refile]]
       (setq org-refile-targets '(("reading.org" :level . 0)
                                  ("emacs.org" :level . 0)
                                  ("watching.org" :level . 0)
                                  ("learning.org" :level . 0)
                                  ("inbox.org" :level . 0)
                                  ("sample.org" :level . 0)
                                  ("wip.org" :level . 0)))
       (setq org-log-refile 'note)
       (setq org-log-redeadline 'note)
       (setq org-log-reschedule 'note)
       (setq org-log-done 'note)
       ;;(setq org-startup-folded 'content)
       (setq org-tags-column -85)    ;; auto works better for olivetti
       ;; (setq org-tags-column 120) ;; for wide screens & no 80char limiting
       (setq org-tag-alist
             '(("@errand" . ?e)
               ("@work" . ?w)
               ("@home" . ?h)
               ("@blog" . ?B)
               (:newline)
               ("emacs" . ?E)
               ("wip" . ?W)
               ("CANCELLED" . ?c)
               (:newline)
               ("learning" . ?l)
               ("research" . ?r)
               (:newline)
               ("book" . ?b)
               ("article" . ?a)
               ("paper" . ?p)
               (:newline)
               ("talk" . ?t)
               ("film" . ?f)))
       
       ;;(cons->table org-tag-alist)
       (setq org-enforce-todo-dependencies t)
       (setq org-enforce-todo-checkbox-dependencies t)
       (defun qz/org-choose-current-attachment ()
         (let ((attach-dir (org-attach-dir)))
           (if attach-dir
               (let* ((file (pcase (org-attach-file-list attach-dir)
                              (`(,file) file)
                              (files (completing-read "Open attachment: "
                                                      (mapcar 'list files) nil t))))
                      (path (expand-file-name file attach-dir)))
                 path))))
       
       (defun qz/org-insert-current-attachment ()
         (interactive)
         (insert
          (format "[[file:./%s]]"
                  (dired-make-relative
                   (qz/org-choose-current-attachment)))))
       
       (define-key org-mode-map (kbd "C-c M-a") 'qz/org-insert-current-attachment)
       
       (defun qz/org-insert-last-stored-link (arg)
         "Insert the last link stored in `org-stored-links'."
         (interactive "p")
         (qz/org-insert-all-links arg "" "\n"))
       
       (defun qz/org-insert-all-links (arg &optional pre post)
         "Insert all links in `org-stored-links'.
       When a universal prefix, do not delete the links from `org-stored-links'.
       When `ARG' is a number, insert the last N link(s).
       `PRE' and `POST' are optional arguments to define a string to
       prepend or to append."
         (interactive "P")
         (let ((org-link-keep-stored-after-insertion (equal arg '(4)))
               (links (copy-sequence org-stored-links))
               (pr (or pre "- "))
               (po (or post "\n"))
               (cnt 1) l)
           (if (null org-stored-links)
               (message "No link to insert")
             (while (and (or (listp arg) (>= arg cnt))
                         (setq l (if (listp arg)
                                     (pop links)
                                   (pop org-stored-links))))
               (setq cnt (+ 1 cnt))
               (insert pr)
               (message "%s" `((:l ,l)
                               (:car-l ,(car l))
                               (:cadr-l ,(cadr l))
                               (:mod-l ,(car (last (s-split "/" (car l)))))))
               (org-insert-link
                nil (car l)
                (or (cadr l)
                    (qz/ol-file l)))
               (insert po)))))
       
       (define-key org-mode-map (kbd "C-c M-l") 'qz/org-insert-last-stored-link)
       (defun qz/ol-file (link)
         "transform file path into pretty ol-output
                   - respect projects; truncate prior path, keeping only basename
       "
         ;; (car (last (s-split "/" "file:~/sys/rde/goop.boop::pattern")))
         ;; == "goop.boop::pattern"
         ;; (message  "HELLO :: %s/%s" p (car (last (s-split "/" p))))
         (let* ((p (car link))
                (inner (mapcar
                        (lambda (s) (let ((ss (car s)))
                                      (and (s-contains? (f-base ss) p)
                                           (cons (f-base ss) ss))))
                        project--list))
                (suffix (s-join
                         " . "
                         ;; sort by length of path desc, taking the innermost subproj
                         ;; take first value of first result
                         (--tb (cl-sort (remove nil inner)
                                        (lambda (a b) (gt (length a) (length b)))
                                        :key 'cdr)
                               (mapcar 'car)
                               (reverse)))))
           (format "%s%s"
                   (or (and suffix (format "(%s)" suffix)) "")
                   (car (last (s-split "/" p))))))
       
       ;;(car org-stored-links)
       ;;(qz/ol-file (car org-stored-links))
       (defalias '--tb '->>)
       (defalias '--tf '->)
       (defalias 'gt '>)
       (defalias 'lt '<)
       (defun qz/create-excluded-ids-for-headlines-in-buffer ()
         "Add ID properties to all headlines in the current file which
       do not already have one."
         (interactive)
         (org-map-entries (lambda (&rest r)
                            (unless (org-id-get)
                              (org-id-get-create)
                              (org-set-property "ROAM_EXCLUDE" "t")))))
       
       
       (add-hook 'org-mode-hook
                 (lambda ()
                   (add-hook 'before-save-hook
                             'qz/create-excluded-ids-for-headlines-in-buffer nil 'local)))
       
       (setq org-id-link-to-org-use-id t)
       (setq org-image-actual-width 640)
       ;;(require 'org)
       
       (defun org-cycle-hide-drawers (state)
         "Re-hide all drawers after a visibility state change."
         (when (and (derived-mode-p 'org-mode)
                    (not (memq state '(overview folded contents))))
           (save-excursion
             (let* ((globalp (memq state '(contents all)))
                    (beg (if globalp
                             (point-min)
                           (point)))
                    (end (if globalp
                             (point-max)
                           (if (eq state 'children)
                               (save-excursion
                                 (outline-next-heading)
                                 (point))
                             (org-end-of-subtree t)))))
               (goto-char beg)
               (while (re-search-forward org-drawer-regexp end t)
                 (save-excursion
                   (beginning-of-line 1)
                   (when (looking-at org-drawer-regexp)
                     (let* ((start (1- (match-beginning 0)))
                            (limit
                             (save-excursion
                               (outline-next-heading)
                               (point)))
                            (msg (format
                                  (concat
                                   "org-cycle-hide-drawers:  "
                                   "`:END:`"
                                   " line missing at position %s")
                                  (1+ start))))
                       (if (re-search-forward "^[ \t]*:END:" limit t)
                           (outline-flag-region start (point-at-eol) t)
                         (user-error msg))))))))))
       
       (defun qz/org-align-tags ()
         (interactive)
         (org-align-tags 'yes-all-the-bloody-tags))
       (defun qz/org-sort-subtree ()
         (interactive)
         (save-mark-and-excursion
           (org-up-heading-or-point-min)
           (call-interactively 'org-sort)))
       
       (define-key org-mode-map (kbd "C-c C-M-6") 'qz/org-sort-subtree)
       (defun qz/org-md-nolink (link contents info)
         (format "%s" contents))
       
       (org-export-define-derived-backend 'my-md 'md
         :menu-entry
         '(?M "Export to Markdown without links" (lambda (a s v b) (org-md-export-to-markdown a s v)))
         :translate-alist '((link . qz/org-md-nolink)))
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
       (define-key org-mode-map (kbd "C-c C-M-e") 'qz/org-export-headline)
       (defun org-slack-headline (headline contents info)
         "Transcode HEADLINE element into Markdown format.
       CONTENTS is the headline contents.  INFO is a plist used as
       a communication channel."
         (unless (org-element-property :footnote-section-p headline)
           (let* ((level (org-export-get-relative-level headline info))
                  (title (org-export-data (org-element-property :title headline) info))
                  (todo (and (plist-get info :with-todo-keywords)
                             (let ((todo (org-element-property :todo-keyword
                                                               headline)))
                               (and todo (concat (org-export-data todo info) " ")))))
                  (tags (and (plist-get info :with-tags)
                             (let ((tag-list (org-export-get-tags headline info)))
                               (and tag-list
                                    (concat "     " (org-make-tag-string tag-list))))))
                  (priority
                   (and (plist-get info :with-priority)
                        (let ((char (org-element-property :priority headline)))
                          (and char (format "[#%c] " char)))))
                  ;; Headline text without tags.
                  (heading (concat todo priority title)))
             (format "%s*%s*\n\n%s"
                     (if (gt level 1)
                         (concat (s-repeat (- level 3) ;; starting flush
                                           "  ")
                                 "- ")
                       "")
                     title contents))))
       )
     (message "post org: %s" (shell-command-to-string "date"))
     ;; NOWEB ORG END
     (when (fboundp 'adaptive-wrap-prefix-mode)
       (add-hook 'org-mode-hook 'adaptive-wrap-prefix-mode))
     (setq org-tag-alist
           '(("@errand" . ?e)
             ("@work" . ?w)
             ("@home" . ?h)
             ("@blog" . ?B)
             (:newline)
             ("emacs" . ?E)
             ("task" . ?t)
             ("CANCELLED" . ?C)
             (:newline)
             ("learning" . ?l)
             ("research" . ?r)
             (:newline)
             ("book" . ?b)
             ("article" . ?a)
             ("paper" . ?p)
             ("talk" . ?t)
             ("film" . ?f)))
     (defun qz/get-mail ()
       (interactive)
       (async-shell-command "mbsync -Va && notmuch new"))
     (defun qz/rde-sanity ()
       (interactive)
       (async-shell-command
        (concat "cd $HOME/git/sys/rde"
                "&& guix repl -L . dev/sanity.scm")))
     (setq qz/emacs/config "~/git/sys/rde/rde/examples/abcdw/configs.org"
           qz/sh/tangle "make -C $HOME/git/sys/rde/rde/examples/abcdw tangle ")
     
     (defun qz/tangle ()
       (interactive)
       (async-shell-command
        (concat
         "make -C $HOME/git/sys/rde/rde/examples/abcdw tangle"
         " && echo 'tangle--ehg--di' | espeak --stdin")))
     
     (defun qz/reload-config-home ()
       (interactive)
       (async-shell-command
        (concat
         "make -C $HOME/git/sys/rde/rde/examples/abcdw home"
         "&& echo 'home--bal-ehg--di' | espeak --stdin ")))
     
     (defun qz/reload-config-system ()
       (interactive)
       (async-shell-command
        (concat
         "sudo -E make -C $HOME/git/sys/rde/rde/examples/abcdw system"
         "&& echo 'system--bal-ehg--di' | espeak --stdin")))
     
     (defun qz/reload-config-all ()
       (interactive)
       (async-shell-command
        (concat
         "   make -C $HOME/git/sys/rde rde/channels/update-locked"
         "&& make -C $HOME/git/sys/rde rde/channels/pull-locked"
         "&& make -C $HOME/git/sys/rde/rde/examples/abcdw all"
         "&& echo 'do the do, like ooo; pull & home & system bal-ehg--di'"
         "   | espeak --stdin")))
     
     (defun qz/reload-config-emacs ()
       (interactive)
       (load-file "~/.config/emacs/init.el"))
     
     (defun qz/reload-guix-pins ()
       (interactive)
       (async-shell-command
        (s-join " " '("cd $HOME/git/sys/rde"
                      "&& make rde/channels/update-locked"
                      "&& make rde/channels/pull-locked"))))
     
     (defun qz/guix-upgrade ()
       (interactive)
       (async-shell-command
        (s-join " "
                '("cd $HOME/git/sys/rde"
                  "&& make rde/channels/update-locked"
                  "&& make rde/channels/pull-locked"
                  "&& guix package -u"
                  "&& guix upgrade"
                  "&& make"))))
     (defun qz/sway-choose-output-res (&optional display res)
       (interactive)
       (let* ((cur (s-trim (shell-command-to-string
                            "swaymsg -t get_outputs | jq -r 'map( . | select(.focused == true) | .name) | first'")))
              (cmd (format "swaymsg 'output %s enable res %s'"
                           (or display
                               (completing-read "display: "
                                                '("DP-1" "DP-2"
                                                  "eDP-1"
                                                  "HDMI-1" "HDMI-2")
                                                nil t cur))
                           (or res
                               (completing-read "resolution: "
                                                '("1920x1080"
                                                  "5120x1440")
                                                nil t)))))
         (when (y-or-n-p (format "exec ~%s~?" cmd))
           (shell-command cmd))))
     
     (defun qz/bt-connect (mac)
       (interactive "saddress: ")
       (async-shell-command
        (format "bluetoothctl connect %s" mac)
        "*bluetoothctl*"))
     
     (defun qz/bt-airpods ()
       (interactive)
       (qz/bt-connect "44:F0:9E:51:52:7B"))
     
     (defun qz/bt-aiaiai ()
       (interactive)
       (qz/bt-connect "44:F0:9E:51:52:7B")
     
     
       )
     
     ;; (setq minibuffer-mode-hook nil)
     ;; (add-hook 'minibuffer-mode-hook 'olivetti-mode)
     
     (add-hook 'minibuffer-mode-hook
               (lambda ()
                 (setq-local olivetti-body-width 200)
                 (olivetti-mode)))
     
     ;; (require 'perfect-margin)
     
     ;; (perfect-margin-mode 1)
     ;; (setq perfect-margin-ignore-regexps nil
     ;;       perfect-margin-ignore-filters nil)
     (custom-set-variables
      '(cursor-type 'hbar))
     (setq outline-default-state 'outline-show-only-headings)
     (defun hi-lock-face-symbol-at-point ()
       "Highlight each instance of the symbol at point.
     Uses the next face from `hi-lock-face-defaults' without prompting,
     unless you use a prefix argument.
     Uses `find-tag-default-as-symbol-regexp' to retrieve the symbol at point.
     
     If REGEXP contains upper case characters (excluding those preceded by `\\')
     and `search-upper-case' is non-nil, the matching is case-sensitive.
     
     This uses Font lock mode if it is enabled; otherwise it uses overlays,
     in which case the highlighting will not update as you type.  The Font
     Lock mode is considered \"enabled\" in a buffer if its `major-mode'
     causes `font-lock-specified-p' to return non-nil, which means
     the major mode specifies support for Font Lock."
       (interactive)
       (let* ((regexp (hi-lock-regexp-okay
                       (find-tag-default-as-symbol-regexp)))
              (hi-lock-auto-select-face t)
              (face (hi-lock-read-face-name)))
         (or (facep face)
             (setq face (or (and hi-lock-auto-select-face (hi-lock-read-face-name))
                            'hi-yellow)))
         (unless hi-lock-mode (hi-lock-mode 1))
         (hi-lock-set-pattern
          regexp face nil nil
          (if (and case-fold-search search-upper-case)
              (isearch-no-upper-case-p regexp t)
            case-fold-search))))
     
     (with-eval-after-load 'highlight-indent-guides
       
       ;; highlight-indent-guides-character
       ;; 🐶
       ;; 🐩
       ;; |
       ;; default (char-to-string 9474)
       ;; default │
       
       (custom-set-variables '(highlight-indent-guides-method 'character)
                             `(highlight-indent-guides-character 9474)
                             '(highlight-indent-guides-auto-enabled nil)
                             '(highlight-indent-guides-responsive 'top))
       
       ;; I don't use this AT ALL so give some ridulous defaults
       (set-face-background 'highlight-indent-guides-odd-face "green")
       (set-face-background 'highlight-indent-guides-even-face "red")
       
       ;; this is the /only/ based highlight-indent guide variety
       (set-face-foreground 'highlight-indent-guides-character-face "#333")
       (let ((accent "#555"))
         (set-face-foreground 'highlight-indent-guides-stack-character-face accent)
         (set-face-foreground 'highlight-indent-guides-top-character-face accent))
       (defun qz/highlight-indent-toggle-responsive (&optional arg)
         (interactive)
         (let ((val (if (or (and arg (> 0 arg))
                            (and (not arg) highlight-indent-guides-responsive))
                        nil qz/highlight-indent-guides-responsive)))
           (custom-set-variables
            `(highlight-indent-guides-responsive ',val))
           (highlight-indent-guides-mode)
           val))
       (defvar qz/lazy-last -1)
       (defun qz/lazy-eyes (&optional arg)
         (interactive)
         (setq qz/lazy-last (if (> 0 qz/lazy-last) 1 -1))
         (qz/highlight-indent-toggle-responsive qz/lazy-last)
         (global-hl-line-mode qz/lazy-last))
       )
     
     (defvar qz/font-initial-size (face-attribute 'default :height))
     (defvar qz/resize-mini-windows-initial resize-mini-windows)
     (defvar qz/max-mini-window-height-initial max-mini-window-height)
     
     (defun qz/reset-visual-initial ()
       (interactive)
       (set-face-attribute 'default nil :height qz/font-initial-size)
       (setq resize-mini-windows    qz/resize-mini-windows-initial
             max-mini-window-height qz/max-mini-window-height-initial))
     (defun qz/font-big-80 ()
       (interactive)
       (set-face-attribute 'default nil :height 300)
       (setq resize-mini-windows t
             max-mini-window-height nil))
     (defvar qz/unsplash-tags nil)
     (defun qz/unsplash ()
       "yet another lazy shell-command wrapper; wallpaper edition"
       (interactive)
       (let ((tag (read-from-minibuffer
                   "unsplash tags: " (car qz/unsplash-tags))))
         (async-shell-command
          (format "TAGS='%s'
     mv \"$XDG_CACHE_HOME/wallpaper.png\" \"$XDG_CACHE_HOME/$(date +%%Y-%%m-%%d--%%H-%%M-%%S)-wallpaper.png\"
     curl -L \"https://source.unsplash.com/5120x1440?$TAGS\" -o \"$XDG_CACHE_HOME/wallpaper.png\"
     swaymsg output \"*\" background ~/.cache/wallpaper.png fill" tag))
         (setq qz/unsplash-tags (seq-uniq (cons tag qz/unsplash-tags)))))
     ;; NOWEB CONF END
     )))
