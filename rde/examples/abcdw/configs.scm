(define-module (rde examples abcdw configs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features bluetooth)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features password-utils)
  #:use-module (rde features version-control)
  #:use-module (rde features fontutils)
  #:use-module (rde features terminals)
  #:use-module (rde features tmux)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features emacs)
  #:use-module (rde features linux)
  #:use-module (rde features bittorrent)
  #:use-module (rde features mail)
  #:use-module (rde features docker)

  ;; #:use-module (gnu services)
  ;; #:use-module (gnu services nix)

  #:use-module (rde packages)

  #:use-module (gnu services ssh)

  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)

  #:use-module (gnu home-services ssh)
  #:use-module (gnu packages)
  #:use-module (guix gexp)

  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))


;;; User-specific features

;; Initial user's password hash will be available in store, so it's
;; use this feature with care
;; (display (crypt "hi" "$6$abc"))

(define* (mail-acc id user #:optional (type 'gmail))
  "Make a simple mail-account with gmail type by default."
  (mail-account
   (id   id)
   (fqda user)
   (type type)))

(define* (mail-lst id fqda urls)
  "Make a simple mailing-list."
  (mailing-list
   (id   id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define %abcdw-features
  (list
   (feature-user-info
    #:user-name "samuel"
    #:full-name "Samuel Culpepper"
    #:email "samuel@samuelculpepper.com"
    ;; WARNING: This option can reduce the explorability by hiding
    ;; some helpful messages and parts of the interface for the sake
    ;; of minimalistic, less distractive and clean look.  Generally
    ;; it's not recommended to use it.
    #:emacs-advanced-user? #t)
    #:user-groups '("lp")) ;; TODO confluence of features -> groups

   (feature-gnupg
     #:gpg-primary-key "EE20E25391AAB9BB"
     #:gpg-smart-card? #f)
    ;; (feature-password-store
    ;;  #:remote-password-store-url "ssh://abcdw@olorin.lan/~/state/password-store")

   (feature-mail-settings
    #:mail-accounts
    (list (mail-account
           (id   'work)
           (fqda "sculpepper@newstore.com")
           (type 'gmail))
          (mail-account
           (id   'personal)
           (fqda "samuel@samuelculpepper.com")
           (type 'bravehost)))
    #:mailing-lists
    (list ;; (mail-lst 'rde-announce "~acbdw/rde-announce@lists.sr.ht"
          ;;           '("https://lists.sr.ht/~abcdw/rde-announce/export"))
          ;; (mail-lst 'rde-discuss "~acbdw/rde-discuss@lists.sr.ht"
          ;;           '("https://lists.sr.ht/~abcdw/rde-discuss"))
          ;; (mail-lst 'rde-devel "~acbdw/rde-devel@lists.sr.ht"
          ;;           '("https://lists.sr.ht/~abcdw/rde-devel"))
          (mail-lst 'guix-devel "guix-devel@gnu.org"
                    '("https://yhetil.org/guix-devel/0g"))
          (mail-lst 'guix-patches "guix-patches@gnu.org"
                    '("https://yhetil.org/guix-patches/1"))))
   (feature-keyboard
    #:keyboard-layout %thinkpad-layout)))

;;; TODO: Add documentation about starting guile repl
;;; TODO: feature-wallpapers https://wallhaven.cc/
;;; TODO: feature-icecat
;;; TODO: feature-bash?
;;; TODO: feature-battery
;; PipeWire/iwd:
;; https://github.com/J-Lentz/iwgtk
;; https://github.com/krevedkokun/guix-config/blob/master/system/yggdrasil.scm


;;; Generic features should be applicable for various hosts/users/etc

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

(define my-org-directory "~/life/")
(define my-notes-directory (string-append my-org-directory "/roam"))

(define my-extra-emacs-config
  '(;; TODO port back to literate
    (require 'hyperbole)

    (with-eval-after-load 'pdf-view
      (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode))

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

    (define-key global-map (kbd "s-h") 'windmove-left)
    (define-key global-map (kbd "s-j") 'windmove-down)
    (define-key global-map (kbd "s-k") 'windmove-up)
    (define-key global-map (kbd "s-l") 'windmove-right)

    (define-key global-map (kbd "M-s-h") 'windmove-swap-states-left)
    (define-key global-map (kbd "M-s-j") 'windmove-swap-states-down)
    (define-key global-map (kbd "M-s-k") 'windmove-swap-states-up)
    (define-key global-map (kbd "M-s-l") 'windmove-swap-states-right)

    (defun qz/get-mail ()
      (interactive)
      (async-shell-command "mbsync -Va && notmuch new"))

    (defun qz/reload-config-home ()
      (interactive)
      (async-shell-command
       (concat "cd $HOME/git/sys/rde/rde/examples/abcdw/ "
               "&& make ixy-home-reconfigure")))

    (defun qz/reload-config-system ()
      (interactive)
      (async-shell-command
       (concat "cd $HOME/git/sys/rde/rde/examples/abcdw/ "
               "&& sudo -E make ixy-system-reconfigure")))

    (defun qz/reload-config-emacs ()
      (interactive)
      (load-file "~/.config/emacs/init.el"))

    (define-key global-map (kbd "s-\\") 'org-store-link)

    (with-eval-after-load 'org-roam
     (setq org-confirm-babel-evaluate nil)

     ;; [[file:~/.doom.d/config.org::*refile][refile]]
     (setq org-refile-targets '(("next.org" :level . 0)
                                ("reading.org" :level . 0)
                                ("emacs.org" :level . 0)
                                ("watching.org" :level . 0)
                                ("learning.org" :level . 0)
                                ("inbox.org" :level . 0)
                                ("sample.org" :level . 0)
                                ("wip.org" :level . 0)))

     (setq qz/capture-title-timestamp "20210813T161035Z-${slug}")

     (defun qz/today-as-daily-file ()
       (format-time-string "private-%Y-%m-%d.org"))

     ;; [[file:~/.doom.d/config.org::*templates][templates]]
     (setq org-capture-templates
           `(("i" "inbox" entry
              (file ,(concat org-agenda-directory "/inbox.org"))
              "* TODO %? \nCREATED: %u\nFROM: %a")
             ;; spanish language capturing
             ("v" "vocab; spanish" entry
              (file+headline ,(concat org-roam-directory "/spanish_language.org") "vocab, phrases")
              ,(s-join "\n" '("** \"%?\" :es:"
                              "FROM: %a" ""
                              "*** :en:" "")))
             ;; capture link to live `org-roam' thing
             ("n" "now, as in NOW" entry (file ,(concat org-agenda-directory "/wip.org"))
              ,(s-join "\n" '("* TODO [#A1] %? "
                              "DEADLINE: %T"
                              "CREATED: %u")))
             ;; fire directly into inbox
             ("c" "org-protocol-capture" entry (file ,(concat org-agenda-directory "/inbox.org"))
              ,(s-join "\n" '("* TODO [[%:link][%:description]]"
                              "CREATED: %u" ""
                              "#+begin_quote" ""
                              "%i"
                              "#+end_quote"))
              :immediate-finish t)
             ;; push last captured item into inbox
             ("l" "last-capture" entry (file ,(concat org-agenda-directory "/inbox.org"))
              (function qz/inbox-last-captured)
              :immediate-finish t)
             ("I" "current-roam" entry (file ,(concat org-agenda-directory "inbox.org"))
              (function qz/current-roam-link)
              :immediate-finish t)
             ("t" "tangent" entry (file+headline ,(concat org-roam-dailies-directory
                                                          (qz/today-as-daily-file))
                                                 "tangent")
              ,(s-join "\n" '("* TANGENT [%<%H:%M>] %?"
                              "CREATED: <%<%Y-%m-%d %H:%M>>"
                              "FROM: %a")))
             ("w" "weekly review" entry
              (file+datetree ,(concat org-agenda-directory "reviews.org"))
              (file ,(concat org-agenda-directory "templates/weekly_review.org")))))

     ;; [[file:~/.doom.d/config.org::*capture convenience functions][capture convenience functions]]
     (defun qz/current-roam-link ()
       "Get link to org-roam file with title"
       (interactive)
       (concat "* TODO "
               (let ((n (qz/org-roam-node-at-point)))
                 (org-link-make-string
                  (concat "id:" (org-roam-node-id n))
                  (org-roam-node-title n)))))

     (defun qz/org-inbox-capture ()
       (interactive)
       "Capture a task in agenda mode."
       (org-capture nil "i"))

     (defun qz/org-daily-tangent-capture ()
       (interactive)
       "Capture the inevitable tangent"
       (org-capture nil "t"))

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

     ;; [[file:~/.doom.d/config.org::*capture templates][roam capture templates]]

     (setq qz/org-roam-capture-head "#+title: ${title}\n")
     (setq qz/capture-title-timestamp-roam "20210813T161035Z-${slug}.org")

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

    ;;; ref capture
     (setq org-roam-capture-ref-templates
           `(("r" "ref" plain
              "%?"
              :if-new (file+head ,qz/capture-title-timestamp-roam
                                 "#+title: ${title}\n")
              :unnarrowed t)))

    ;;; dailies
     (setq
      qz/org-roam-dailies-filespec "private-%<%Y-%m-%d>.org"
      org-roam-dailies-capture-templates
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
     )

    (require 'perfect-margin)
    (perfect-margin-mode 1)
    (setq perfect-margin-ignore-regexps nil
          perfect-margin-ignore-filters nil)

    (custom-set-variables
     '(cursor-type 'hbar))

        ;;;; END OF EXTRA CONFIG
    ))


(use-modules (gnu services)
             (gnu services databases)
             (gnu services desktop))

;;; WARNING: The order can be important for features extending
;;; services of other features.  Be careful changing it.
(define %main-features
  (list
   (feature-custom-services
    #:system-services
    (list
     ;; (service nix-service-type)
     )
    #:home-services
    (list
     ;; TODO: Remove it once upstreamed.
     ((@ (gnu services) simple-service)
      'make-guix-aware-of-guix-home-subcomand
      (@ (gnu home services) home-environment-variables-service-type)
      '(("GUILE_LOAD_PATH" .
         "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0\
:$GUILE_LOAD_PATH")
        ("GUILE_LOAD_COMPILED_PATH" .
         "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache\
:$GUILE_LOAD_COMPILED_PATH")
        ;; javascript sucks, npm sucks
        ;; https://github.com/npm/npm/issues/6675#issuecomment-250318382
        ;; https://github.com/npm/cli/issues/1451
        ;; https://github.com/pnpm/pnpm/issues/2574
        ;; https://github.com/rxrc/zshrc/blob/3ca83703da5bd93b015747835a8a0164160c9b83/env.zsh#L33-L928
        ("NPM_CONFIG_USERCONFIG" . "${XDG_CONFIG_HOME}/npm/config")
        ("NPM_CONFIG_CACHE" . "${XDG_CACHE_HOME}/npm")
        ("NPM_CONFIG_TMP" . "${XDG_RUNTIME_DIR}/npm")
        ("YARN_CACHE_FOLDER" . "${YARN_CACHE_FOLDER:-$XDG_CACHE_HOME/yarn}")
        ("NODE_REPL_HISTORY" . "${NODE_REPL_HISTORY:-$XDG_CACHE_HOME/node/repl_history}")
        ("NVM_DIR" . "${NVM_DIR:-$XDG_DATA_HOME/nvm}")
        ("BABEL_CACHE_PATH" . "${BABEL_CACHE_PATH:-$XDG_CACHE_HOME/babel/cache.json}")
        ;; XXX this reports multiple entries for PATH and doesn't use this one
        ;; ("PATH" . (string-join
        ;;            (list "$PATH"
        ;;                  "$HOME/local/bin"
        ;;                  "${XDG_CACHE_HOME}/npm/bin") ":"))
        ))
     ((@ (gnu services) simple-service)
      'extend-shell-profile
      (@ (gnu home-services shells) home-shell-profile-service-type)
      (list
       "PATH=\"$PATH:$HOME/local/bin:${XDG_CACHE_HOME}/npm/bin\""
       #~(string-append
          "alias superls="
          #$(file-append (@ (gnu packages base) coreutils) "/bin/ls")))))
    #:system-services
    (list (service openssh-service-type)
          (service postgresql-service-type)
          (service postgresql-role-service-type
                   (postgresql-role-configuration
                    (roles (list (postgresql-role
                                  (name "postgres")
                                  (create-database? #t))
                                 (postgresql-role
                                  (name "samuel")
                                  (create-database? #t))
                                 (postgresql-role
                                  (name "newstore")
                                  (create-database? #t))))))))


   (feature-base-services)
   (feature-desktop-services)
   (feature-docker)

   (feature-fonts)
   (feature-pipewire)
   (feature-backlight)

   (feature-alacritty
    #:config-file (local-file "./config/alacritty/alacritty.yml")
    )
   (feature-zsh
    #:extra-config
    '(;; XXX higher level category
      "alias ns='cd $HOME/git/ns'"
      "alias om='ns && cd om'"
      "alias omom='om && cd om'"
      "alias rt='ns && cd routing'"
      "alias sys='cd $HOME/git/sys'"

      ;; TIL https://unix.stackexchange.com/questions/225943/except-the-1st-argument
      "rgw() { d=$1; p=$2; argv[1,2]=(); rg $p $d $@; }"
      "alias rgg='rgw $HOME/git/'"
      "alias rgr='rgw $HOME/git/sys/rde'"
      "alias rgns='rgw $HOME/git/ns'"
      "alias rgom='rgw $HOME/git/ns/om'"
      "alias rgrt='rgw $HOME/git/ns/routing'"
      "alias rgsys='rgw $HOME/git/sys'"
      ))
   (feature-ssh
    #:ssh-configuration
    (home-ssh-configuration
     (default-options
      '((hostkey-algorithms . "+ssh-rsa")
        (pubkey-accepted-algorithms "+ssh-rsa")))
     (extra-config
      (list (ssh-host
             (host "bastion-sandbox")
             (options '((user . "ubuntu@bastion-sandbox")
                        (hostname . "bastion-sandbox.ssh.newstore.luminatesec.com")
                        (port . 22)
                        (identity-file . "~/.ssh/newstore-luminate.pem"))))
            (ssh-host
             (host "bastion-staging")
             (options '((user . "ubuntu@bastion-staging")
                        (hostname . "bastion-staging.ssh.newstore.luminatesec.com")
                        (port . 22)
                        (identity-file . "~/.ssh/newstore-luminate.pem"))))
            (ssh-host
             (host "bastion-production")
             (options '((user . "ubuntu@bastion-production")
                        (hostname . "bastion-production.ssh.newstore.luminatesec.com")
                        (port . 22)
                        (identity-file . "~/.ssh/newstore-luminate.pem"))))))))
   (feature-git)

   (feature-sway
    #:xwayland? #t
    #:opacity 0.88
    #:wallpaper "$HOME/.cache/wallpaper.png"
    #:extra-config
    `((include ,(local-file "./config/sway/config"))))

   (feature-sway-run-on-tty
    #:sway-tty-number 2)
   (feature-sway-screenshot)
   (feature-sway-statusbar)

   (feature-direnv)
   (feature-emacs
    #:additional-elisp-packages
    (append  ;; TODO if feature-emacs-PACKAGE exists, advise its use
     (pkgs "emacs-yasnippet" "emacs-elfeed" "emacs-hl-todo"
           "emacs-dimmer" "emacs-hyperbole" "emacs-org-fragtog"
           "emacs-yaml-mode" "emacs-plantuml-mode"
           ;; "emacs-org-autotangle"
           ))
    #:extra-config ;; this will be much tidier collected from literate config, with each elisp block as `:noweb'
    (append
      (list #~"(define-key key-translation-map [?\\C-x] [?\\C-u])\n"
            #~"(define-key key-translation-map [?\\C-u] [?\\C-x])\n")
      my-extra-emacs-config))

   (feature-emacs-appearance #:light? #f)
   (feature-emacs-faces)
   (feature-emacs-completion)
   (feature-emacs-project)
   (feature-emacs-perspective)
   (feature-emacs-input-methods)
   (feature-emacs-which-key)
   (feature-emacs-keycast)
   (feature-emacs-perfect-margin)

   (feature-emacs-dired)
   (feature-emacs-vterm)
   (feature-emacs-monocle)
   (feature-emacs-message)
   (feature-emacs-erc
    #:erc-nick "qzdl"
    #:erc-autojoin-channels-alist
    '(("irc.libera.chat" "#guix" "#emacs" "#tropin" "#systemcrafters")
      ("irc.oftc.net"    "#pipewire" "#wayland")))
   (feature-emacs-elpher)
   (feature-emacs-telega)
   (feature-emacs-pdf-tools)

   ;; TODO: Revisit <https://en.wikipedia.org/wiki/Git-annex>
   (feature-emacs-git)
   ;; TODO: <https://www.labri.fr/perso/nrougier/GTD/index.html#table-of-contents>
   (feature-emacs-org
    #:org-directory my-org-directory
    #:org-agenda-directory my-notes-directory)
   (feature-emacs-org-roam
    ;; TODO: Rewrite to states
    #:org-roam-directory my-notes-directory
    #:org-roam-dailies-directory
    (string-append my-notes-directory "/daily"))
   (feature-emacs-ref
    ;; TODO: Rewrite to states
    #:bibliography-paths
    (list (string-append my-org-directory "/tex.bib"))
    #:bibliography-notes
    (string-append my-org-directory "/bib.org")
    #:bibliography-directory my-notes-directory)

   (feature-emacs-es-mode)
   (feature-emacs-restclient)

   (feature-isync #:isync-verbose #t)
   (feature-l2md)
   (feature-msmtp
    #:msmtp-package msmtp-latest)
   (feature-notmuch
    #:notmuch-saved-searches
    (cons* '(:name "Work Inbox" :query "tag:work and tag:inbox"
             :count "tag:work and tag:inbox and tag:unread"
             :sort-order oldest-first :key "W")
           %rde-notmuch-saved-searches))

   (feature-transmission #:auto-start? #f)

   (feature-xdg
    #:xdg-user-directories-configuration
    (home-xdg-user-directories-configuration
     (music "$HOME/music")
     (videos "$HOME/vids")
     (pictures "$HOME/pics")
     (documents "$HOME/docs")
     (download "$HOME/dl")
     (desktop "$HOME")
     (publicshare "$HOME")
     (templates "$HOME")))
   (feature-bluetooth #:auto-enable? #t)
   (feature-base-packages
    #:home-packages
    (append
     (pkgs
      "alsa-utils" "mpv" "youtube-dl" "imv" "vim"
      "obs" "obs-wlrobs"
      "icecat"
      "ungoogled-chromium-wayland" "ublock-origin-chromium"
      "nyxt"
      "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-standard"
      "ripgrep" "curl" "make")))))

(define %laptop-features
  (list ))


;;; Hardware/host specific features

;; TODO: Switch from UUIDs to partition labels For better
;; reproducibilty and easier setup.  Grub doesn't support luks2 yet.

(define ixy-mapped-devices
  (list (mapped-device
         (source (uuid "cb453366-cc17-4742-ada1-91f7f569103f"))
         (target "sys-root")
         (type luks-device-mapping))))

(define ixy-file-systems
  (list (file-system
           (device (file-system-label "sys-root"))
           (mount-point "/")
           (type "ext4")
           (dependencies ixy-mapped-devices))
         (file-system
           (device "/dev/nvme0n1p1")
           (mount-point "/boot/efi")
           (type "vfat"))
         ))


;; (define ixy-file-systems
;;   (append
;;    (map (match-lambda
;; 	  ((subvol . mount-point)
;; 	   (file-system
;; 	     (type "btrfs")
;; 	     (device "/dev/mapper/enc")
;; 	     (mount-point mount-point)
;; 	     (options (format #f "subvol=~a" subvol))
;; 	     (dependencies ixy-mapped-devices))))
;; 	'((root . "/")
;; 	  (boot . "/boot")
;; 	  (gnu  . "/gnu")
;; 	  (home . "/home")
;; 	  (data . "/data")
;; 	  (log  . "/var/log")))
;;    (list
;;     (file-system
;;       (mount-point "/boot/efi")
;;       (type "vfat")
;;       (device (uuid "8C99-0704" 'fat32))))))

(use-modules
 (gnu packages linux)
 ((nongnu packages linux) #:prefix nongnu:)
 ((nongnu system linux-initrd) #:prefix nongnu-sys:))
(define %ixy-features
  (list
   (feature-host-info
    #:host-name "ixy"
    #:timezone  "Europe/Berlin")
   ;;; Allows to declare specific bootloader configuration,
   ;;; grub-efi-bootloader used by default
   ;; (feature-bootloader)
   ; os
   (feature-kernel
    #:kernel nongnu:linux
    #:kernel-arguments
    '("quiet" "ipv6.disable=1" "net.ifnames=0"
      "modprobe.blacklist=snd_hda_intel,snd_soc_skl")
    #:firmware (list nongnu:linux-firmware
                     nongnu:sof-firmware)
    #:initrd nongnu-sys:microcode-initrd
    #:kernel-loadable-modules (list v4l2loopback-linux-module))
   (feature-file-systems
    #:mapped-devices ixy-mapped-devices
    #:file-systems   ixy-file-systems)
   ;(feature-hidpi)
   ))


;;; rde-config and helpers for generating home-environment and
;;; operating-system records.

(define-public ixy-config
  (rde-config
   (features
    (append
     %abcdw-features
     %main-features
     %ixy-features))))

;; TODISCUSS: Make rde-config-os/he to be a feature instead of getter?
(define ixy-os
  (rde-config-operating-system ixy-config))
(define ixy-he
  (rde-config-home-environment ixy-config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("ixy-home" ixy-he)
      ("ixy-system" ixy-os)
      (_ ixy-he))))

;; (pretty-print-rde-config ixy-config)
;; (use-modules (gnu services)
;; 	     (gnu services base))
;; (display
;;  (filter (lambda (x)
;; 	   (eq? (service-kind x) console-font-service-type))
;; 	 (rde-config-system-services ixy-config)))

;; (use-modules (rde features))
;;  ((@@ (ice-9 pretty-print) pretty-print)
;;   (map feature-name (rde-config-features ixy-config)))

(dispatcher)
