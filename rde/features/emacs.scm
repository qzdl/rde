(define-module (rde features emacs)
  #:use-module (rde features)
  #:use-module (rde features predicates)
  #:use-module (rde emacs packages)
  #:use-module (gnu home-services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services wm)
  #:use-module (gnu home-services xdg)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (guix gexp)
  #:use-module (guix packages)

  #:export (feature-emacs
	    feature-emacs-faces
	    feature-emacs-completion
	    feature-emacs-input-method
	    feature-emacs-project
	    feature-emacs-magit
	    feature-emacs-eshell
	    feature-emacs-org-mode
	    feature-emacs-org-roam
	    feature-emacs-message
	    feature-emacs-erc
	    feature-emacs-telega
            feature-emacs-transmission

            elisp-configuration-service
            emacs-xdg-service))

(define* (elisp-configuration-service
          name
          #:optional (elisp-expressions '())
          #:key (elisp-packages '()))
  (let* ((configure-package
	  (elisp-configuration-package
	   (string-append "configure-" (symbol->string name))
           elisp-expressions
           #:elisp-packages elisp-packages)))
    (simple-service
     (symbol-append 'emacs- name '-configurations)
     home-emacs-service-type
     (home-emacs-extension
      (elisp-packages (list configure-package))))))

(define* (emacs-xdg-service
          name xdg-name gexp
          #:key (default-for '()))
  (define file-name (string-append "emacs-" (symbol->string name)))
  (define file-file (file-append (program-file file-name gexp) " %u"))
  (define desktop-file (symbol-append 'emacs- name '.desktop))
  (simple-service
   (symbol-append 'emacs-xdg- name)
   home-xdg-mime-applications-service-type
   (home-xdg-mime-applications-configuration
    (default (map (lambda (m) (cons m desktop-file)) default-for))
    (desktop-entries
     (list
      (xdg-desktop-entry
       (file file-name)
       (name xdg-name)
       (config `((exec . ,file-file)))
       (type 'application)))))))



(define* (feature-emacs
	  #:key
	  (package emacs-next-pgtk-latest)
	  (emacs-server-mode? #t)
	  (additional-elisp-packages '()))
  "Setup and configure GNU Emacs."
  (ensure-pred boolean? emacs-server-mode?)
  (ensure-pred list-of-elisp-packages? additional-elisp-packages)
  (ensure-pred package? package)

  (define emacs-client (file-append package "/bin/emacsclient"))
  (define emacs-client-create-frame
    (program-file "emacs-client-create-frame"
		  #~(apply system*
			   #$(file-append package "/bin/emacsclient")
			   "--create-frame"
			   (cdr (command-line)))))
  (define emacs-client-no-wait
    (program-file "emacs-client-no-wait"
		  #~(apply system*
			   #$(file-append package "/bin/emacsclient")
			   "--no-wait"
			   (cdr (command-line)))))
  (define emacs-editor
    (program-file "emacs-editor"
		  #~(apply system*
			   #$(file-append package "/bin/emacs")
			   "--no-splash"
			   (cdr (command-line)))))

  (define (emacs-home-services config)
    "Returns home services related to GNU Emacs."
    (require-value 'full-name config)
    (require-value 'email config)
    (let* ((full-name (get-value 'full-name config))
	   (email     (get-value 'email config)))
      (list
       (service
	home-emacs-service-type
	(home-emacs-configuration
	 (package package)
	 (elisp-packages (cons* emacs-modus-themes additional-elisp-packages))
	 (server-mode? emacs-server-mode?)
	 (xdg-flavor? #t)
	 (init-el
	  `((setq user-full-name ,full-name)
	    (setq user-mail-address ,email)
	    ,#~""
	    (setq custom-file
		  (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
			  "/emacs/custom.el"))
	    (load custom-file t)
            ,#~""
            (defun rde/display-load-time ()
              (interactive)
              (message "rde emacs loaded in %s, C-h r i for search in emacs manual by topic. C-h C-a for welcome screen." (emacs-init-time)))

            ;; (setq inhibit-splash-screen t)
            (defun display-startup-echo-area-message ()
              (rde/display-load-time))
	    ,#~""
	    ;; (define-key global-map (kbd "M-/") 'hippie-expand)

	    (column-number-mode 1)
	    (save-place-mode 1)
	    (show-paren-mode 1)

	    (setq-default indent-tabs-mode nil)
	    (setq save-interprogram-paste-before-kill t)
	    (setq mouse-yank-at-point t)
	    (setq require-final-newline t)
            (add-hook 'prog-mode-hook
                      (lambda () (setq show-trailing-whitespace t)))

	    (load-theme 'modus-operandi t)))
	 (early-init-el
	  `(,(slurp-file-gexp (local-file "../emacs/early-init.el"))))
	 ;;; TODO: Rebuilding packages with emacs will be useful for
	 ;;; native-comp, but for some reason dash.el fails to build,
	 ;;; need to investigate the issue.
	 ;; (rebuild-elisp-packages? #t)
	 ))

       (simple-service 'emacs-set-default-editor
		       home-environment-variables-service-type
		       `(("ALTERNATE_EDITOR" . ,emacs-editor)
			 ("VISUAL" . ,emacs-client-no-wait)))
       (when (get-value 'sway config)
 	 (simple-service
	  'emacs-update-environment-variables-on-sway-start
	  home-sway-service-type
	  `((exec
	     ,(program-file
	       "update-emacs-env-variables"
	       #~(system*
		  #$emacs-client "--eval"
		  (string-append
                   "(mapcar (lambda (lst) (apply #'setenv lst)) '"
                   (let* ((port   ((@@ (ice-9 popen) open-input-pipe)
		                   (string-append "env")))
	                  (result ((@@ (ice-9 rdelim) read-delimited) "" port))
	                  (vars (map (lambda (x)
                                       (let ((si (string-index x #\=)))
                                         (list (string-take x si)
                                               (string-drop x (+ 1 si)))))
			             ((@@ (srfi srfi-1) remove)
			              string-null? (string-split
                                                    result #\newline)))))
	             (close-port port)
	             (format #f "~s" vars))
                   ")"))))
            (for_window "[title=\".* - Emacs Client\"]"
                        floating enable,
                        resize set 80 ppt 80 ppt)))))))

  (feature
   (name 'emacs)
   (values (append
	    `((emacs . #t))
	    (make-feature-values emacs-editor emacs-client
                                 emacs-client-create-frame
                                 emacs-client-no-wait
                                 emacs-server-mode?)))
   (home-services-getter emacs-home-services)))

(define (strip-emacs-name p)
  (let ((name (package-name p)))
    (string->symbol
     (if (string-prefix? "emacs-" name)
         (string-drop name (string-length "emacs-"))
         name))))

(define* (feature-emacs-input-method
	  #:key
	  (input-method "cyrillic-dvorak")
	  (input-method-package emacs-cyrillic-dvorak-im))
  "Configure input-method for GNU Emacs.  Allows to use other layouts
with emacs, whithout losing ability to use keybindings.  Supported
both Emacsy toggle-input-method (C-\\) and system layout switching by
utilizing reverse-im package."

  (define emacs-f-name 'input-method)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'mule
	 (require ',(strip-emacs-name input-method-package))
	 (setq default-input-method ,input-method)
	 (require 'reverse-im))
	(with-eval-after-load
	 'reverse-im
	 (setq reverse-im-input-methods ,input-method)
	 (reverse-im-mode 1)))
      #:elisp-packages (list emacs-reverse-im input-method-package))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-message
	  #:key
	  (smtp-server #f)
	  (smtp-port 587))
  "Configure email sending capabilities provided by @file{message.el}."
  (ensure-pred string? smtp-server)
  (ensure-pred integer? smtp-port)

  (define emacs-f-name 'message)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'message
	 (setq send-mail-function 'smtpmail-send-it)
	 (setq smtpmail-smtp-server ,smtp-server)
	 (setq smtpmail-smtp-service ,smtp-port)

	 (setq message-auto-save-directory
	       (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
		       "/emacs/mail-drafts")))))

     (emacs-xdg-service
      emacs-f-name
      "Emacs (Client) [mailto:]"
      #~(system*
         #$emacs-cmd "--eval"
	 (string-append
          "\
(progn
 (set-frame-name \"Reply to Email - Emacs Client\")
 (browse-url-mail \"" (cadr (command-line)) "\"))"))
      #:default-for '(x-scheme-handler/mailto))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-erc
	  #:key
	  ;; (emacs-client? #f)
	  (erc-server "irc.libera.chat")
	  (erc-port 6697)
	  (erc-nick #f)
	  (erc-autojoin-channels-alist '()))
  "Configure GNU Emacs IRC client."
  (ensure-pred string? erc-server)
  (ensure-pred integer? erc-port)
  (ensure-pred maybe-string? erc-nick)
  (ensure-pred list? erc-autojoin-channels-alist)

  (define emacs-f-name 'erc)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'erc
	 (setq erc-server ,erc-server)
	 (setq erc-port ,erc-port)
	 ,@(if erc-nick `((setq erc-nick ,erc-nick)) '())
	 (setq erc-autojoin-channels-alist
	       ',erc-autojoin-channels-alist)

	 (setq erc-fill-static-center 14)
	 (setq erc-fill-function 'erc-fill-static)
	 (setq erc-fill-column 86)

	 (setq erc-track-visibility nil)

	 (define-key erc-mode-map (kbd "s-b") 'erc-switch-to-buffer))))
     (emacs-xdg-service
      emacs-f-name
      "Emacs (Client) [IRC]"
      #~(system* #$emacs-cmd "--eval" "(erc-tls)"))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-telega)
  "Configure telega.el for GNU Emacs"
  (define emacs-f-name 'telega)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))
    (define xdg-gexp
      #~(system*
         #$emacs-cmd
         "--eval"
         (string-append
	  "(progn
(set-frame-name \"Telega - Emacs Client\")
(if (and (boundp 'telega--status) (equal telega--status \"Ready\"))
 (telega-browse-url \"" (car (cdr (command-line))) "\")"
 "
 (telega)
 (add-hook 'telega-ready-hook
  (lambda ()
   (telega-browse-url \"" (car (cdr (command-line))) "\")))"
   "))")))

    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'telega
         ;; FIXME: Implement proper switch buffer function
	 (define-key telega-chat-mode-map (kbd "s-b") 'telega-switch-buffer)
	 (define-key telega-root-mode-map (kbd "s-b") 'telega-switch-buffer)

         (setq telega-emoji-company-backend 'telega-company-emoji)
         (defun my-telega-chat-mode ()
           (set (make-local-variable 'company-backends)
                (append (list telega-emoji-company-backend
                              'telega-company-username
                              'telega-company-hashtag)
                        (when (telega-chat-bot-p telega-chatbuf--chat)
                          '(telega-company-botcmd))))
           (company-mode 1))
         (add-hook 'telega-chat-mode-hook 'my-telega-chat-mode)

	 (setq telega-completing-read-function completing-read-function)))
      #:elisp-packages (list emacs-telega))

     (emacs-xdg-service emacs-f-name "Emacs (Client) [tg:]" xdg-gexp
                        #:default-for '(x-scheme-handler/tg))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-eshell)
  "Configure org-mode for GNU Emacs."
  (define emacs-f-name 'eshell)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((define-key global-map (kbd "s-e") 'eshell)
        (with-eval-after-load
         'eshell
         (add-hook
          'eshell-hist-mode-hook
          (lambda ()
            (with-eval-after-load
             'consult
             (define-key eshell-hist-mode-map (kbd "M-r") 'consult-history))))
         (add-hook
          'eshell-mode-hook
          (lambda ()
            (setenv "PAGER" "")

            (eshell/alias "e" "find-file $1")
            (eshell/alias "ee" "find-file-other-window $1")
            (eshell/alias "d" "dired $1")
            (with-eval-after-load
             'magit (eshell/alias "gd" "magit-diff-unstaged"))

            (define-key eshell-mode-map (kbd "s-e") 'switch-to-prev-buffer))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-mode)
  "Configure org-mode for GNU Emacs."
  (define emacs-f-name 'org-mode)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
         'org
         (progn
	  (setq org-adapt-indentation nil)
	  (setq org-edit-src-content-indentation 0)
	  (setq org-startup-indented t))))
      #:elisp-packages (list emacs-org))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-magit)
  "Configure Magit for GNU Emacs."
  (define emacs-f-name 'magit)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      #:elisp-packages (list emacs-magit))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;; TODO: Move font record to apropriate module
(use-modules (rde features fontutils))

;; TODO: Can be useful to have different presets for different
;; environments.  For easier and faster switching.
(define* (feature-emacs-faces)
  "Configure faces for GNU Emacs."

  (define emacs-f-name 'faces)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'fonts config)
    (define font-monospace (get-value 'font-monospace config))
    (define font-sans      (get-value 'font-sans config))

    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'faces
	 (let* ((mono-fn ,(font-name font-monospace))
		(sans-fn ,(font-name font-sans))
		(mono (font-spec
		       :name ,(font-name font-monospace)
		       :size   ,(font-size font-monospace)
		       :weight ',(or (font-weight font-monospace) 'normal)))
		;; For people coming here years later, only
		;; face which can contain size or integer
		;; height is default, everything else should
		;; set only family or relative height
		;; (decimal value), the font-spec even
		;; without height/size shouldn't be used.
		;; Otherwise text-adjust and other stuff can
		;; be broken.
		(faces `((default ((t (:font ,mono))))
			 (fixed-pitch ((t (:family ,mono-fn))))
			 (button ((t (:inherit (fixed-pitch)))))
			 (variable-pitch ((t (:family ,sans-fn)))))))
	   (dolist (face faces)
		   (custom-set-faces face))

	   (dolist (face faces)
		   (put (car face) 'saved-face nil))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; TODO: Remove corfu, rename to minibuffer
(define* (feature-emacs-completion)
  "Configure completion system for GNU Emacs."
  (define emacs-f-name 'completion)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((with-eval-after-load
	 'minibuffer

	 (setq enable-recursive-minibuffers t)

	 (require 'orderless)
	 (require 'savehist)
	 (require 'vertico)
	 (require 'corfu)
	 (require 'marginalia)
	 (require 'embark)
	 (require 'consult))

	(with-eval-after-load
	 'orderless
	 (setq completion-styles '(orderless))
	 (setq completion-category-overrides
	       '((file (styles . (partial-completion))))))

	(with-eval-after-load
	 'savehist
	 (setq savehist-file
	       (concat (or (getenv "XDG_CACHE_HOME") "~/.cache")
		       "/emacs/history"))
	 (savehist-mode 1))

	(with-eval-after-load
	 'embark
	 (define-key global-map (kbd "s-.") 'embark-act))

	(with-eval-after-load
	 'consult
	 ;; TODO: Move to feature-emacs-buffers
	 (define-key global-map (kbd "s-w") 'kill-current-buffer)
	 (define-key global-map (kbd "s-o") 'other-window)
	 (define-key global-map (kbd "s-b") 'consult-buffer)
	 (define-key global-map (kbd "s-B") 'consult-buffer)

	 (define-key global-map (kbd "M-y") 'consult-yank-pop))

	(with-eval-after-load 'vertico (vertico-mode 1))
	(with-eval-after-load 'corfu (corfu-global-mode 1))
	(with-eval-after-load
	 'marginalia
	 ;; FIXME: Temporary disable annotations for describe-variables.
	 ;; See: <https://github.com/masm11/emacs/issues/104>
	 (setf (alist-get 'variable marginalia-annotator-registry)
	       '(none builtin marginalia-annotate-variable))
	 (marginalia-mode 1)))
      #:elisp-packages  (list emacs-orderless emacs-marginalia
		              emacs-vertico emacs-corfu
		              emacs-consult emacs-embark-next))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-project)
  "Configure project.el for GNU Emacs."

  (define emacs-f-name 'project)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      ;; TODO: https://github.com/muffinmad/emacs-ibuffer-project
      `((with-eval-after-load
	 'project
	 (with-eval-after-load
	  'consult
	  (setq consult-project-root-function
		(lambda ()
		  (when-let (project (project-current))
			    (car (project-roots project)))))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

;; TODO: rewrite to states
(define* (feature-emacs-org-roam
	  #:key
	  (org-roam-directory #f))
  "Configure org-roam for GNU Emacs."
  (define (not-boolean? x) (not (boolean? x)))
  (ensure-pred not-boolean? org-roam-directory)

  (define emacs-f-name 'org-roam)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (elisp-configuration-service
      emacs-f-name
      `((add-hook 'after-init-hook 'org-roam-mode)
	(with-eval-after-load
	 'org-roam
	 (define-key org-roam-mode-map (kbd "C-c n n") 'org-roam)
	 (define-key org-roam-mode-map (kbd "C-c n f") 'org-roam-find-file)
	 (define-key org-mode-map      (kbd "C-c n i") 'org-roam-insert)
	 (setq org-roam-directory ,org-roam-directory)))
      #:elisp-packages (list emacs-org-roam))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-transmission)
  "Configure transmission.el."

  (define emacs-f-name 'transmission)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (require-value 'emacs-client-create-frame config)
    (define emacs-cmd (get-value 'emacs-client-create-frame config))

    (list
     (elisp-configuration-service
      emacs-f-name
      `((define-key global-map (kbd "C-c T t") 'transmission))
      #:elisp-packages (list emacs-transmission))

     (emacs-xdg-service
      emacs-f-name "Emacs (Client) [magnet:]"
      #~(system*
         #$emacs-cmd "--eval"
	 (string-append "\
(progn
 (set-frame-name \"Transmission - Emacs Client\")
 (transmission)
 (delete-other-windows)
 (transmission-add \"" (cadr (command-line)) "\")
 (revert-buffer))"))
      #:default-for '(x-scheme-handler/magnet))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;; TODO: feature-emacs-monocole
;; TODO: feature-emacs-reasonable-keybindings
;; TODO: Fix env vars for emacs daemon
;; https://github.com/purcell/exec-path-from-shell
;; TODO: feature-emacs-epub https://depp.brause.cc/nov.el/
;; TODO: feature-series-tracker https://github.com/MaximeWack/seriesTracker
