(define-module (rde examples abcdw configs)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features bluetooth)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features system)
  #:use-module (rde features wm)
  #:use-module (rde features xdisorg)
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
  #:use-module (rde features video)
  #:use-module (rde features markup)
  ;; #:use-module (gnu services)
  #:use-module (rde features networking)
  #:use-module (gnu services)
  #:use-module (rde home services i2p)

  #:use-module (gnu system keyboard)

  #:use-module (nongnu packages nvidia)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (rde examples abcdw emacs)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu packages)
  #:use-module (rde packages)
  #:use-module (rde packages emacs)
  #:use-module (rde packages emacs-xyz)
  #:use-module (gnu packages fonts)
  #:use-module (guix gexp)
  #:use-module (guix inferior)
  #:use-module (guix channels)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
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

(define my-org-directory "~/life")
(define my-notes-directory
  (string-append my-org-directory "/roam"))

(pretty-print "pre-%abcdw-features")

(define gaming? #f)

(define %abcdw-features
  (remove
   unspecified?
   (list
    (feature-user-info
     #:emacs-advanced-user? #t
     #:user-name "samuel"
     #:full-name "Samuel Culpepper"
     #:email "samuel@samuelculpepper.com"
     #:user-groups '("lp")) ;; TODO confluence of features -> groups

    (feature-gnupg
     #:gpg-primary-key "EE20E25391AAB9BB"
     #:gpg-smart-card? #f)
    (feature-password-store)
    (feature-mail-settings
     #:mail-accounts
     (list (mail-account
            (id   'personal)
            (fqda "samuel@samuelculpepper.com")
            (type 'bravehost))
           (mail-account
            (id   'work)
            (fqda "sculpepper@newstore.com")
            (type 'gmail)))
     #:mailing-lists
     (list
      ;; https://public-inbox.org/README.html
      (mail-lst 'public-inbox-meta "meta@public-inbox.org"
                '("https://public-inbox.org/meta"
                  "nntps://news.public-inbox.org/inbox.comp.mail.public-inbox.meta"
                  "imaps://news.public-inbox.org/inbox.comp.mail.public-inbox.meta.0"))

      ;; source: https://mail.python.org/archives/list/speed@python.org/latest
      ;;  -> mbox: https://mail.python.org/archives/list/speed@python.org/export/speed@python.org-2022-02.mbox.gz?start=1970-01-01&end=2022-02-21
      ;; (mail-lst 'python-speed "speed@python.org"
      ;;           '("https://mail.python.org/mailman/listinfo/speed"
      ;;             "https://mail.python.org/archives/list/speed@python.org/"))

      ;; (mail-lst 'rde-announce "~acbdw/rde-announce@lists.sr.ht"
      ;;           '("https://lists.sr.ht/~abcdw/rde-announce/export"))
      ;; (mail-lst 'rde-discuss "~acbdw/rde-discuss@lists.sr.ht"
      ;;           '("https://lists.sr.ht/~abcdw/rde-discuss"))
      ;; (mail-lst 'rde-devel "~acbdw/rde-devel@lists.sr.ht"
      ;;           '("https://lists.sr.ht/~abcdw/rde-devel"))
      ;;; emacs
      (mail-lst 'emacs-org-mode "emacs-orgmode@gnu.org"
                '("https://yhetil.org/orgmode"))

      (mail-lst 'emacs-hyperbole "bug-hyperbole@gnu.org"
                '("https://lists.gnu.org/archive/mbox/bug-hyperbole"
                  "https://lists.gnu.org/archive/html/bug-hyperbole"))
      (mail-lst 'emacs-hyperbole-users "hyperbole-users@gnu.org"
                '("https://lists.gnu.org/archive/mbox/hyperbole-users"
                  "https://lists.gnu.org/archive/html/hyperbole-users"))

      (mail-lst 'guix-bugs "guix-bugs@gnu.org"
                '("https://yhetil.org/guix-bugs/0"))
      (mail-lst 'guix-devel "guix-devel@gnu.org"
                '("https://yhetil.org/guix-devel/0"))
      (mail-lst 'guix-patches "guix-patches@gnu.org"
                '("https://yhetil.org/guix-patches/1"))))

    (feature-keyboard
     ;; To get all available options, layouts and variants run:
     ;; cat `guix build xkeyboard-config`/share/X11/xkb/rules/evdev.lst
     #:keyboard-layout %thinkpad-layout))))

;;(map pretty-print %abcdw-features)

;;; TODO: feature-wallpapers https://wallhaven.cc/
;;; TODO: feature-icecat
;; PipeWire/iwd:
;; https://github.com/J-Lentz/iwgtk
;; https://github.com/krevedkokun/guix-config/blob/master/system/yggdrasil.scm


;;; Generic features should be applicable for various hosts/users/etc

(define* (pkgs #:rest lst)
  (map specification->package+output lst))

(define* (pkgs-vanilla #:rest lst)
  "Packages from guix channel."
  (define channel-guix
    (list (channel
           (name 'guix)
           (url "https://git.savannah.gnu.org/git/guix.git")
           (commit
            "2b6af630d61dd5b16424be55088de2b079e9fbaf"))))

  (define inferior (inferior-for-channels channel-guix))
  (define (get-inferior-pkg pkg-name)
    (car (lookup-inferior-packages inferior pkg-name)))

   (map get-inferior-pkg lst))

(use-modules (gnu services)
             (gnu services databases)
             (gnu services desktop))

(pretty-print "pre-%main-features")
;;; WARNING: The order can be important for features extending
;;; services of other features.  Be careful changing it.
(define %main-features
  (remove
   unspecified?
   (list
    ;;(feature-ssh-socks-proxy
    ;; #:host "204:cbf:3e07:e67a:424f:93bc:fc5c:b3dc")
    ;;(feature-i2pd
    ;; #:outproxy 'http://acetone.i2p:8888
    ;; ;; 'purokishi.i2p
    ;; #:less-anonymous? #t)

    (feature-custom-services
     #:feature-name-prefix 'ixy
     ;; #:system-services
     ;; (list
     ;;  (simple-service 'nvidia-udev-rule udev-service-type
     ;;                  (list nvidia-driver)))
     #:home-services
     (list
      ;; TODO: Remove it once upstreamed.
      ((@ (gnu services) simple-service)
       'make-guix-aware-of-guix-home-subcomand
       (@ (gnu home services) home-environment-variables-service-type)
       `(
         ;;; GRAPHICS
         ;;("LIBGL_DRIVERS_PATH" . (string-join (list "/gnu/store/bg8mrp0ply34c76xq1i8b4hgjyh6hi8k-nvidia-driver-495.46/lib/gbm"
         ;;                                           "/gnu/store/bg8mrp0ply34c76xq1i8b4hgjyh6hi8k-nvidia-driver-495.46/lib"
         ;;                                           "/gnu/store/bg8mrp0ply34c76xq1i8b4hgjyh6hi8k-nvidia-driver-495.46") ":"))
         ;;("LIBGL_DEBUG" . "verbose")
         ;;("G_MESSAGES_DEBUG" . "1")

         ;;("MESA_LOADER_DRIVER_OVERRIDE" . "nvidia") ;; no nvidia_dri
         ;;("MESA_LOADER_DRIVER_OVERRIDE" . "nvidia-drm") ;; no nvidia-drm_dri

         ;;("MESA_DEBUG" . "1")
         ;;("MESA_LOG_FILE" . "/tmp/mesa.log")

         ;; glfw patched?
         ;; https://github.com/bulletphysics/bullet3/issues/2595#issuecomment-588080665
         ;; ("MESA_GL_VERSION_OVERRIDE" . "3.4")
         ;;("MESA_GLSL_VERSION_OVERRIDE" . "340")

         ;;("GBM_BACKEND" . "nvidia-drm")
        ;;;; guix build --no-grafts -f /home/samuel/git/sys/nonguix/nongnu/packages/nvidia.scm | wl-copy
        ;;;; or
        ;;;; guix build nvidia-driver | wl-copy
         ;;,@(let ((driver-path "/gnu/store/cbj701jzy9dj6cv84ak0b151y9plb5sc-nvidia-driver-495.46"))
         ;;    `(("GBM_BACKENDS_PATH" . ,(string-join (list driver-path
         ;;                                                 (string-append driver-path "/lib")
         ;;                                                 (string-append driver-path "/lib/gbm")
         ;;                                                 "$PATH") ":"))
         ;;      ("VK_ICD_FILENAMES" . ,(string-append driver-path "/share/vulkan/icd.d/nvidia_icd.json"))
         ;;      ("LIBGL_DRIVERS_PATH" . ,(string-join (list driver-path
         ;;                                                  (string-append driver-path "/lib")
         ;;                                                  (string-append driver-path "/lib/gbm")
         ;;                                                  "$PATH") ":"))
         ;;      ;; https://github.com/NVIDIA/egl-wayland/issues/39#issuecomment-927288015
         ;;      ;; undocumented
         ;;      ;; might have an issue for containerised stuff, as set(uid|gid)
         ;;      ("__EGL_EXTERNAL_PLATFORM_CONFIG_DIRS" . ,(string-append driver-path "/share/egl/egl_external_platform.d"))
         ;;      ))
         ;;
         ;;("__GLX_VENDOR_LIBRARY_NAME" . "nvidia")
         ;;("WLR_NO_HARDWARE_CURSORS" . "1")
         ;;("WLR_DRM_NO_ATOMIC" . "1")
         ;; echo "/dev/dri/card$(udevadm info -a -n /dev/dri/card1 | grep boot_vga | rev | cut -c 2)"
         ;;("WLR_DRM_DEVICES" . "/dev/dri/card1")   ;; gpu only
         ;;("WLR_DRM_DEVICES" . "/dev/dri/card1") ;; cpu only
         ;;("WLR_DRM_DEVICES" . "/dev/dri/card0:/dev/dri/card1") ;; gpu:cpu

        ;;; GUILE
         ("GUILE_LOAD_PATH" .
          "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0\
:$GUILE_LOAD_PATH")
         ("GUILE_LOAD_COMPILED_PATH" .
          "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache\
:$GUILE_LOAD_COMPILED_PATH")

        ;;; JS/BABEL
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

        ;;; DEVELOPMENT
         ("GUIX_CHECKOUT" . "$HOME/git/sys/guix")
         ("GUIX_EXTRA_PROFILES" . "$HOME/.guix-extra-profiles")

        ;;; ETC
         ("GDK_BACKEND" . "wayland") ;; ... for clipboarding emasc
         ("PATH" . (string-join (list "$PATH"
                                      "$HOME/go/bin"
                                      "$HOME/.local/bin"
                                      "$HOME/.krew/bin"
                                      "${XDG_CACHE_HOME}/npm/bin")
                                ":"))))
      ;; ((@ (gnu services) simple-service)
      ;;  'extend-shell-profile
      ;;  (@ (gnu home-services shells) home-shell-profile-service-type)
      ;;  (list
      ;;   #~(string-append
      ;;      "alias superls="
      ;;      #$(file-append (@ (gnu packages base) coreutils) "/bin/ls"))))

      ;;; home jobs
      ;;
      ;; see logs at ~/.local/var/log/mcron.log
      ;;   tail --follow ~/.local/var/log/mcron.logtail --follow ~/.local/var/log/mcron.log
      ;;
      ;; see job spec at [[info:mcron#Guile Syntax][mcron#Guile Syntax]]
      ((@ (gnu services) simple-service)
       'home-jobs (@ (gnu home services mcron) home-mcron-service-type)
       (list
         ;;; job: commit my notes
        #~(job '(next-minute '(15))
               (lambda ()
                 (system* (string"(cd /home/samuel/life && git add . && git commit -m \"auto-commit | $(date -u)\") &>/tmp/commit-log"))
                 "notes-commit"))
         ;;; job: fulltext index the universe
        #~(job '(next-hour)
               (lambda ()
                 (system* (file-append (@@ (gnu packages search) recoll) "/bin/recollindex")))
               "recollindex")
         ;;; job: generate tags
        ;; ref :: https://guix.gnu.org/en/manual/devel/en/html_node/Scheduled-Job-Execution.html
        #~(job '(next-hour '(12 0)) ;; every 12 hours
               (string-append #$(@@ (gnu packages idutils) idutils) "/bin/mkid git")
               #:user "samuel"))))
     #:system-services
     (remove
      unspecified?
      (append
       (if gaming?
           (@@ (gnu services desktop) %desktop-services)
           '())
       (list
        ;;; cron jobs
        (simple-service
         'system-jobs (@ (gnu services mcron) mcron-service-type)
         ;; ref :: https://guix.gnu.org/en/manual/devel/en/html_node/Scheduled-Job-Execution.html
         (list
          ;; update locate database
          ;; ref :: https://guix.gnu.org/en/manual/devel/en/html_node/Scheduled-Job-Execution.html
          #~(job '(next-hour '(12 0)) ;; every 12 hours
                 (lambda ()
                   (execl (string-append #$(@ (gnu packages base) findutils) "/bin/updatedb")
                            "updatedb"
                            "--prunepaths=/tmp /var/tmp /gnu/store"))
                   "updatedb")))

        ;;; udev: nvidia
           (when gaming?
             (simple-service
              'nvidia-udev-rule udev-service-type
              (list nvidia-driver)))

        ;;; desktop manager: X11 gdm + nvidia
           (when #f
          (simple-service
           'gdm-xorg-conf gdm-service-type
           (gdm-configuration
            (xorg-configuration
             (xorg-configuration (keyboard-layout %thinkpad-layout)
                                 (modules (append
                                           (list nvidia-driver)
                                           %default-xorg-modules))
                                 (drivers (list "nvidia")))))))

        ;;; postgres: don't include if gaming
        (unless gaming?
          (service postgresql-service-type))
        (unless gaming?
          (service postgresql-role-service-type
                   (postgresql-role-configuration
                    (roles (list (postgresql-role
                                  (name "postgres")
                                  (create-database? #t))
                                 (postgresql-role
                                  (name "samuel")
                                  (permissions '(superuser))
                                  (create-database? #t))
                                 (postgresql-role
                                  (name "newstore")
                                  (create-database? #t)))))))

        ;;; ssh
        ;; TODO key up, remove password method
        (service openssh-service-type
                 (openssh-configuration
                  (password-authentication? #t)
                  ;; (authorised-keys
                  ;;  `(("hww" ,(local-file "hww.pub"))
                  ;;    ))
                  ))
        ))))

    (unless gaming? (feature-base-services))
    (unless gaming? (feature-desktop-services))

    (feature-backlight #:step 5)

    (feature-docker)

    (unless gaming? (feature-pipewire))

    (feature-fonts
     #:font-monospace (font "Iosevka" #:size 14 #:weight 'regular)
     ;; #:font-monospace (font "Fira Mono" #:size 14 #:weight 'semi-light)
     #:font-packages (list font-iosevka font-fira-mono))
    (feature-alacritty
     #:config-file (local-file "./config/alacritty/alacritty.yml")
     #:default-terminal? #f
     #:backup-terminal? #t
     #:software-rendering? #f)
    (feature-vterm)
    (feature-bash)
    (feature-direnv)
    (feature-zsh
     #:enable-zsh-autosuggestions? #t
     #:extra-zshrc
     (list ;; XXX higher level category
      ;; something which evals equiv to following for each promptline "PS1=\"[$(date -u '+%Y-%m-%d | %H:%M')] $PS1\""
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

      "alias gp='ls $GUIX_EXTRA_PROFILES'"
      "_gP() { export GUIX_PROFILE=$1 ; }"
      "alias gP='_gP'"
      "_gsP() { . $GUIX_EXTRA_PROFILES/$1/$1 ; }"
      "gsP=_gsP"
      ))
    (feature-ssh
     #:ssh-configuration
     (home-ssh-configuration
      (default-options
        '((hostkey-algorithms . "+ssh-rsa")
          (pubkey-accepted-algorithms "+ssh-rsa")))
      (extra-config
       (list (ssh-host
              (host "qz")
              ;; <(create ixy->qz key)>
              (options '((user . "samuel")
                         (hostname . "192.168.0.249")
                         (port . 22)
                         (identity-file . "~/.ssh/qzdl.pub"))))
             (ssh-host
              (host "ko")
              (options '((user . "root")
                         (hostname . "192.168.0.240")
                         (port . 2222)
                         (identity-file . "~/.ssh/ko.pub"))))
             (ssh-host
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

    ;; --- WM -----------------------------------------
    ;; --- i3   --------

    ;;(feature-i3)
    ;; (feature-i3-run-on-tty
    ;;  #:i3-tty-number 2
    ;;  ;;#:launch-args "> /tmp/i3.log"
    ;;  )

    ;; --- sway --------
    (unless gaming?
      (feature-sway
       ;; #:sway sway-latest ;; sway-last (transform-nvidia sway-latest)
       ;; #:sway (transform-nvidia sway-latest)
       ;; #:xdg-desktop-portal-wlr xdg-desktop-portal-wlr-latest
       ;; #:xdg-desktop-portal-wlr (transform-nvidia xdg-desktop-portal-wlr-latest)
       #:xwayland? #f
       #:opacity 0.9
       #:wallpaper "$HOME/.cache/wallpaper.png"
       #:extra-config
       `(;;(include ,(local-file "./config/sway/config"))
         ;; TODO sway: toggle opacity for WINDOW
         (,#~"output DP-2 res 5120x1440 bg ~/.cache/wallpaper.png fill")
         ;; TODO sway: wacom input rotation matrix
         (,#~"input \"*\" tool_mode \"*\" relative calibration_matrix 0.0 -1.0 1.0 1.0 0.0 0.0")
         ;; danke demis https://github.com/minikN/guix/blob/ca15b5a5954d50fe75e2b03f21afc019e002022b/config.scm#L173
         (for_window "[app_id=\"pavucontrol\"]" floating enable, border pixel)
         (for_window "[app_id=\"pinentry-qt\"]" floating enable, border pixel)
         (for_window "[title=\"Nightly - Sharing Indicator\"]" floating enable, border pixel)
         (bindsym $mod+Ctrl+o opacity set 1)
         (bindsym $mod+Ctrl+p opacity minus 0.1)
         (bindsym $mod+x exec $menu))))
    (unless gaming?
      (feature-sway-run-on-tty
       #:sway-tty-number 2
       ;;#:launch-args "--unsupported-gpu" ;; 1.7-rc1+ https://github.com/swaywm/sway/releases/tag/1.7-rc1
       ;;#:launch-args "--unsupported-gpu --debug &>/tmp/sway"
       ))
    (unless gaming?
      (feature-sway-tessen
       #:menu-arg "rofi"
       #:menu-package rofi-wayland))
    (unless gaming? (feature-sway-screenshot))
    ;; ----- sway: statusbar; superseded by waybar
    ;; (unless gaming?
    ;;   (feature-sway-statusbar
    ;;    #:use-global-fonts? #f))
    ;; -----
    (unless gaming?
      (feature-waybar
       #:waybar-modules
       (list
        (waybar-sway-workspaces)
        ;; (waybar-sway-window)
        (waybar-tray)
        (waybar-idle-inhibitor)
        ;; (waybar-temperature)
        ;; (waybar-sway-language)
        (waybar-volume #:intense? #f)
        (waybar-battery #:intense? #f)
        (waybar-clock))))

    ;; FIXME swayidle: external monitor resuming bug (probably gpu issue)e
    ;; https://github.com/swaywm/sway/issues/5759
    (unless gaming?
      (feature-swayidle))
    (unless gaming?
      (feature-swaylock
       #:swaylock (@ (gnu packages wm) swaylock-effects)
       ;; The blur of last-screen on lock screen is not privacy-friendly.
       ;; TODO use blurred wallpaper from $HOME/.cache/wallpaper.png
       #:extra-config '(;; (screenshots)
                        (effect-blur . 7x5)
                        (image . "~/.cache/wallpaper.png")
                        (scale . fill)
                        (clock))))
    (unless gaming?
      (feature-rofi
       #:theme (local-file "config/rofi/themes/base16-default-dark.rasi")))
    ;; ------------------------------------------------

    ;; --- EMACS --------------------------------------
    (feature-emacs
     ;;#:emacs emacs-next-pgtk-latest
     #:emacs (@ (gnu packages emacs) emacs-next-pgtk)
     ;; (if (string=? (or (getenv "BUILD_SUBMITTER") "") "git.sr.ht")
     ;;     (@ (gnu packages emacs) emacs-next-pgtk)
     ;;     emacs-next-pgtk-latest)

     #:extra-init-el
     (append
      (list #~"(define-key key-translation-map [?\\C-x] [?\\C-u])\n"
            #~"(define-key key-translation-map [?\\C-u] [?\\C-x])\n")
      init-el)
     #:additional-elisp-packages
     ;; TODO if feature-emacs-PACKAGE exists, advise its use
     (append
      (list emacs-consult-dir
            emacs-consult-eglot
            emacs-consult-recoll
            emacs-sql-indent
            emacs-code-review
            emacs-ob-go)
      (pkgs "emacs-elfeed"
            "emacs-hl-todo"
            "emacs-ytdl"
            "emacs-dimmer"
            "emacs-hyperbole"
            ;; "emacs-ement"
            "emacs-restart-emacs"
            "emacs-yaml-mode"
            "emacs-org-download"
            "emacs-org-edit-latex"
            "emacs-guix"
            "emacs-forge"
            "emacs-debbugs"
            "emacs-plantuml-mode"
            "emacs-ggtags"
            "emacs-highlight-indent-guides"

            "emacs-paredit"
            "emacs-ess"
            "emacs-ob-async"
            "emacs-org-fragtog"
            "emacs-org-super-agenda"
            "emacs-org-transclusion"
            "emacs-ox-hugo"
            "emacs-ox-pandoc"
            "emacs-htmlize" ;; ement: -> ox-export html: org src blocks

            ;;"emacs-artbollocks"

            "emacs-repology"

            "emacs-slime"
            "emacs-slime-repl-ansi-color"
            "emacs-slime-volleyball"

            "emacs-calfw"
            "emacs-edit-server"

            "emacs-json-snatcher"
            "emacs-logview" ;; https://github.com/doublep/logview
            ;;"emacs-vlf" ;; TODO guix: package emacs-vlf

            "emacs-explain-pause-mode"
            ;; TODO feature-emacs-lsp
            "emacs-jq-mode"
            "emacs-eglot"
            "emacs-lsp-ui"
            "emacs-lsp-mode"
            "emacs-python-black"
            "emacs-py-isort"
            "emacs-gnuplot"
            "emacs-protobuf-mode"
            "emacs-go-mode"
            "emacs-eros"
            "emacs-terraform-mode"
            "emacs-string-inflection"
            "emacs-htmlize" ;; ement: -> ox-export html: org src blocks

            ;; emacs-impostman
            ;; "emacs-org-autotangle"
            )))

    (feature-emacs-appearance
     #:light? #f
     #:deuteranopia? #f)
    (feature-emacs-faces)
    (feature-emacs-completion
     #:mini-frame? #f)
    (feature-emacs-vertico)
    (feature-emacs-project)
    (feature-emacs-perspective)
    (feature-emacs-input-methods)
    (feature-emacs-which-key)
    (feature-emacs-keycast)
    (feature-emacs-perfect-margin
     #:visible-width 150)

    (feature-emacs-dired)
    ;;(feature-emacs-vterm) ;; TODO merge with feature-vterm
    (feature-emacs-monocle)
    (feature-emacs-message)
    (feature-emacs-erc
     #:erc-kill-buffers-on-quit #t
     #:erc-nick "qzdl"
     #:align-nicknames? #f
     #:erc-autojoin-channels-alist
     '((Libera.Chat "#guix" "#emacs" "#tropin" "#rde" "#sway")
       (OFTC        "#pipewire" "#wayland"))
     #:log? #t
     ;; #:erc-server "chat.sr.ht"
     #:extra-config
     `((setq rde-bouncer-network-alist
             `((irc.libera.chat . "qzdl")
               (irc.oftc.net . "qzdl")))
       (setq rde-bouncer-nick "qzdl")

       ;; Rename server buffers to reflect the current network name instead
       ;; of SERVER:PORT (e.g., "freenode" instead of "irc.freenode.net:6667").
       ;; This is useful when using a bouncer like ZNC where you have multiple
       ;; connections to the same server.
       (setq erc-rename-buffers t)

       (defun rde-erc-connect-bouncer-oftc ()
         (interactive)
         (setq erc-email-userid "qzdl/irc.oftc.net")
         (erc-tls :server "chat.sr.ht" :nick rde-bouncer-nick))
       (defun rde-erc-connect-bouncer-libera ()
         (interactive)
         (setq erc-email-userid "qzdl/irc.libera.chat")
         (erc-tls :server "chat.sr.ht" :nick rde-bouncer-nick))))
    (feature-emacs-elpher)
    (feature-emacs-ement)
    (feature-emacs-telega)
    (feature-emacs-pdf-tools)
    (feature-emacs-nov-el)
    ;; TODO: Revisit <https://en.wikipedia.org/wiki/Git-annex>
    (feature-emacs-git)
    ;; TODO: <https://www.labri.fr/perso/nrougier/GTD/index.html#table-of-contents>
    (feature-emacs-org-roam
     ;; TODO: Rewrite to states
     #:org-roam-directory my-notes-directory
     #:org-roam-dailies-directory (string-append my-notes-directory "/daily"))
    ;; FIXME guix: org-roam-ui: httpd communication problem (endemic to guix)
    ;; (feature-emacs-org-roam-ui)
    ;; (feature-emacs-ref
    ;;  ;; why error with nil for reftex-default-bibliography
    ;;  ;; TODO: Rewrite to states
    ;;  #:bibliography-paths
    ;;  (list (string-append my-org-directory "/tex.bib"))
    ;;  #:bibliography-notes
    ;;  (list(string-append my-org-directory "/bib.org")
    ;;  #:bibliography-directory my-notes-directory)

    (feature-emacs-org-agenda
     #:org-agenda-files '("~/life/roam/inbox.org"))
    (feature-emacs-org
     #:org-directory my-org-directory
     #:org-agenda-directory my-notes-directory)

    (feature-emacs-es-mode
     #:package emacs-es-mode-latest)
    (feature-emacs-restclient
     #:package-ob emacs-ob-restclient-latest)

    ;; ------------------------------------------------

    (feature-mpv)
    (feature-isync #:isync-verbose #t)
    (feature-l2md)
    (feature-msmtp
     #:msmtp msmtp-latest)
    (feature-notmuch
     #:extra-tag-updates-post
     '("notmuch tag +guix-home -- 'thread:\"\
{((subject:guix and subject:home) or subject:/home:/) and tag:new}\"'"
       "notmuch tag +guix -- \"{to:guix or subject:guix}\"")
     #:notmuch-saved-searches
     (cons*
      '(:name "Personal Inbox"  :key "P" :query "tag:personal and tag:inbox")
      '(:name "Work Inbox"      :key "W" :query "tag:work and tag:inbox")
      '(:name "Guix Home Inbox" :key "H" :query "tag:guix-home and tag:unread")
      '(:name "RDE Inbox"       :key "R" :query "((to:/rde/ or cc:/rde/) or subject:/rde/) and tag:unread")
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
       "figlet" ;; TODO: Move to emacs-artist-mode
       "calibre"
       "icecat" "nyxt"
       "ungoogled-chromium-wayland" "ublock-origin-chromium"

       "utox" "qtox" "jami"

       "alsa-utils" "mpv" "youtube-dl" "imv" "vim"
       "cozy" "pavucontrol"
       "wev"
       "obs" "obs-wlrobs"
       "recutils" "binutils"
       "fheroes2"
       ;; TODO: Enable pipewire support to chromium by default
       ;; chrome://flags/#enable-webrtc-pipewire-capturer
       "ungoogled-chromium-wayland" "ublock-origin-chromium"
       "nyxt"
       ;;
       "hicolor-icon-theme" "adwaita-icon-theme" "gnome-themes-standard"
       "papirus-icon-theme" "arc-theme"
       "thunar"
       ;; "glib:bin"

       ;; TODO: Fix telega package!
       "ffmpeg"

       "ripgrep" "curl" "make"
       )))
    )))

;;(map pretty-print %main-features)
(pretty-print "post-%main-features")



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
;;        ((subvol . mount-point)
;;         (file-system
;;           (type "btrfs")
;;           (device "/dev/mapper/enc")
;;           (mount-point mount-point)
;;           (options (format #f "subvol=~a" subvol))
;;           (dependencies ixy-mapped-devices))))
;;      '((root . "/")
;;        (boot . "/boot")
;;        (gnu  . "/gnu")
;;        (home . "/home")
;;        (data . "/data")
;;        (log  . "/var/log")))
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
   ;; os
   (feature-kernel
    ;;#:kernel nongnu:linux-lts ;; nvidia
    #:kernel nongnu:linux
    #:kernel-arguments
    '("quiet" "ipv6.disable=1" "net.ifnames=0"
      "nouveau.modeset=1"
      ;; https://forums.developer.nvidia.com/t/nvidia-495-on-sway-tutorial-questions-arch-based-distros/192212
       ;;"nvidia-drm.modeset=1" "nouveau.blacklist=1" "modprobe.blacklist=nouveau"
      )
    ;; removed "modprobe.blacklist=snd_hda_intel,snd_soc_skl"
    #:firmware (list nongnu:linux-firmware
                     nongnu:sof-firmware
                     ;;nvidia-driver
                     )
    #:initrd nongnu-sys:microcode-initrd
    #:kernel-loadable-modules (list v4l2loopback-linux-module
                                    ;;nvidia-driver
                                    ))
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
(define-public ixy-os
  (rde-config-operating-system ixy-config))

(define ixy-he
  (rde-config-home-environment ixy-config))



(use-modules (gnu system file-systems))
(define live-file-systems
  (list (file-system
           (mount-point "/")
           (device (file-system-label "Guix_image"))
           (type "ext4"))

         ;; Make /tmp a tmpfs instead of keeping the overlayfs.  This
         ;; originally was used for unionfs because FUSE creates
         ;; '.fuse_hiddenXYZ' files for each open file, and this confuses
         ;; Guix's test suite, for instance (see
         ;; <http://bugs.gnu.org/23056>).  We keep this for overlayfs to be
         ;; on the safe side.
         (file-system
           (mount-point "/tmp")
           (device "none")
           (type "tmpfs")
           (check? #f))

         ;; XXX: This should be %BASE-FILE-SYSTEMS but we don't need
         ;; elogind's cgroup file systems.
         ;; (list %pseudo-terminal-file-system
         ;;       %shared-memory-file-system
         ;;       %efivars-file-system
         ;;       %immutable-store)
         ))

(use-modules (gnu services))
(define-public live-config
  (rde-config
   (features
    (append
     %abcdw-features
     %main-features
     (list
      (feature-host-info
       #:host-name "gnu"
       #:timezone  "Europe/Moscow")

      (feature-file-systems
       #:file-systems live-file-systems)
      (feature-hidpi)
      (feature-custom-services
       #:feature-name-prefix 'live
       #:system-services
       (list
        (simple-service
         'channels-and-sources
         etc-service-type
         `(("channels.scm" ,(local-file "live-channels"))
           ("guix-sources" ,(local-file "/home/bob/work/gnu/guix"
                                        #:recursive? #t))
           ("rde-sources" ,(local-file "/home/bob/work/abcdw/rde"
                                       #:recursive? #t))))
        ;; (service
        ;;  guix-home-service-type
        ;;  `(("bob" . ,ixy-he)))
        (service
         gc-root-service-type
         (list ixy-he))
        )))))))

(define-public live-os
  (rde-config-operating-system live-config))

(define (dispatcher)
  (let ((rde-target (getenv "RDE_TARGET")))
    (match rde-target
      ("ixy-home" ixy-he)
      ("ixy-system" ixy-os)
      ("live-system" live-os)
      (_ ixy-he))))

;; (pretty-print-rde-config ixy-config)
;; (use-modules (gnu services)
;;           (gnu services base))
;; (display
;;  (filter (lambda (x)
;;         (eq? (service-kind x) console-font-service-type))
;;       (rde-config-system-services ixy-config)))

;; (use-modules (rde features))
;; ((@ (ice-9 pretty-print) pretty-print)
;;  (map feature-name (rde-config-features ixy-config)))

(pretty-print "pre-dispatch")
(dispatcher)
