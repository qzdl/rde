;;; rde --- Reproducible development environment.
;;;
;;; Copyright © 2021, 2022 Andrew Tropin <andrew@trop.in>
;;;
;;; This file is part of rde.
;;;
;;; rde is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; rde is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rde.  If not, see <http://www.gnu.org/licenses/>.

(define-module (rde packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages base)

  #:use-module (gnu packages man)
  #:use-module (srfi srfi-1)

  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define (search-patch file-name)
  "Search the patch FILE-NAME.  Raise an error if not found."
  (or (search-path (%rde-patch-path) file-name)
      (raise (formatted-message (G_ "~a: patch not found")
                                file-name))))

(define-syntax-rule (search-patches file-name ...)
  "Return the list of absolute file names corresponding to each
FILE-NAME found in %PATCH-PATH."
  (list (search-patch file-name) ...))

(define %channel-root
  (find (lambda (path)
          (file-exists? (string-append path "/rde/packages.scm")))
        %load-path))

(define %rde-patch-path
  (make-parameter
   (append
    (list (string-append %channel-root "rde/packages/patches"))
    (%patch-path))))

(define-public rofi-wayland
  (package
   (inherit rofi)
   (name "rofi-wayland")
   (version "1.7.2+wayland1")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/lbonn/rofi"
                                "/releases/download/"
                                version "/rofi-" version ".tar.xz"))
            (sha256
             (base32
              "1smrxjq693z48c7n5pcfrvb0m0vsn6pxn7qpn8bm68j942n8rg3x"))))
   (build-system meson-build-system)
   (arguments
    (substitute-keyword-arguments (package-arguments rofi)
      ((#:configure-flags flags '())
       #~(list "-Dxcb=disabled"))))
    (inputs
     (list cairo
           glib
           libjpeg-turbo
           librsvg
           libxkbcommon
           wayland
           wayland-protocols
           pango
           startup-notification))
    (description "Rofi is a minimalist application launcher.  It memorizes which
applications you regularly use and also allows you to search for an application
by name.

This is a fork with added support for Wayland via layer shell protocol.")))

(define-public wtype
  (package
   (name "wtype")
   (version "0.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://github.com/atx/wtype"
                                "/archive/refs/tags/v" version ".tar.gz"))
            (sha256
             (base32
              "1ya6hxmgmsmxsy3yzssq4q2xm7lkfc253g44p0lnwslbh9npi4fs"))))
   (build-system meson-build-system)
   (inputs (list libxkbcommon wayland pkg-config))
   (home-page "https://github.com/atx/wtype")
   (synopsis "xdotool type for wayland")
   (description "wtype is a Wayland tool that allows you to simulate
keyboard input like xdotool type for X11.")
   (license license:gpl3+)))

(use-modules (guix build-system go)
             (gnu packages golang)
             (gnu packages syncthing))

(define-public go-gopkg-in-alecthomas-kingpin-v2
  (package
    (name "go-gopkg-in-alecthomas-kingpin-v2")
    (version "2.2.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/alecthomas/kingpin.v2")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0mndnv3hdngr3bxp7yxfd47cas4prv98sqw534mx7vp38gd88n5r"))))
    (build-system go-build-system)
    (native-inputs
     (list go-github-com-alecthomas-template go-github-com-alecthomas-units
           go-github-com-stretchr-testify))
    (arguments
      '(#:import-path
        "gopkg.in/alecthomas/kingpin.v2"
        #:unpack-path
        "gopkg.in/alecthomas/kingpin.v2"
        #:phases %standard-phases))
    (home-page "https://gopkg.in/alecthomas/kingpin.v2")
    (synopsis "Kingpin - A Go (golang) command line and flag parser")
    (description "Package kingpin provides command line interfaces like this:")
    (license license:expat)))

(define-public clipman
  (package
    (name "clipman")
    (version "1.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yory8/clipman")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0b9kvj0dif4221dy6c1npknhhjxvbc4kygzhwxjirpwjws0yv6v9"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/yory8/clipman"))
    (propagated-inputs
     (list
      go-gopkg-in-alecthomas-kingpin-v2
      go-github-com-kballard-go-shellquote
      go-github-com-alecthomas-units
      go-github-com-alecthomas-template))
    (home-page "https://github.com/yory8/clipman")
    (synopsis "Clipman")
    (description "GPL v3.0 2019- (C) yory8 <yory8@users.noreply.github.com>")
    (license license:gpl3)))

(define-public rde
  (package
    (name "rde")
    (version "0.1.0")
    (home-page "https://trop.in/rde")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference (url "https://git.sr.ht/~abcdw/rde")
                          (commit "0d72e180b7eb92ea5a23d8dd481bde93b6ec252c")))
      (sha256
       (base32
        "0vcsgbziv6cm4b4sccllsg67anpxg0q9mm3d80nms60ng6ld3i6b"))
      (file-name (string-append "rde-" version "-checkout"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gnu-make texinfo))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'check)
          (add-after 'install 'install-info
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (info (string-append out "/share/info")))
                (install-file "doc/rde.info" info)))))))
    (synopsis "Developers and power user friendly GNU/Linux distribution")
    (description "The GNU/Linux distribution, a set of tools for managing
development environments, home environments, and operating systems, a set of
predefined configurations, practices and workflows.")
    (license license:gpl3+)))

;; (define-public rde-latest
;;   (package
;;     (inherit rde)
;;     (source
;;      (local-file (dirname (dirname (current-filename))) #:recursive? #t))))

(use-modules
  (guix packages)
  (guix download)
  (guix git-download)
  (guix build-system emacs)
  (gnu packages emacs-xyz)
  ((guix licenses) #:prefix license:))

(define-public emacs-consult-recoll
  (package
   (name "emacs-consult-recoll")
   (version "0.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://codeberg.org/jao/consult-recoll")
           (commit "42dea1d40fedf7894e2515b4566a783b7b85486a")))
     (sha256
      (base32 "0nzch4x58vgvmcjr6p622lkzms2gvjfdgpvi6bbj5qdzkln5q23a"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("emacs-consult" ,emacs-consult)))
   (home-page "https://codeberg.org/jao/consult-recoll")
   (synopsis "A consulting-read interface for recoll")
   (description
    "A consulting-read interface for recoll")
   (license license:gpl3+)))

(use-modules
 (guix packages)
 (guix download)
 (guix git-download)
 (guix build-system emacs)
 (gnu packages emacs-xyz)
 ((guix licenses) #:prefix license:))

(define-public emacs-consult-eglot
  (package
   (name "emacs-consult-eglot")
   (version "20210905.1830")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/mohkale/consult-eglot.git")
           (commit "f93c571dc392a8b11d35541bffde30bd9f411d30")))
     (sha256
      (base32 "1jqg6sg6iaqxpfn7symiy221mg9sn4y1rn0l1rw9rj9xmcnng7s0"))))
   (build-system emacs-build-system)
   (propagated-inputs
    `(("emacs-eglot" ,emacs-eglot) ("emacs-consult" ,emacs-consult)))
   (home-page "https://github.com/mohkale/consult-eglot")
   (synopsis "A consulting-read interface for eglot")
   (description
    "Query workspace symbol from eglot using consult.

This package provides a single command `consult-eglot-symbols' that uses the
lsp workspace/symbol procedure to get a list of symbols exposed in the current
workspace. This differs from the default document/symbols call, that eglot
exposes through imenu, in that it can present symbols from multiple open files
or even files not indirectly loaded by an open file but still used by your
project.

This code was partially adapted from the excellent consult-lsp package.")
   (license license:expat)))

(define-public emacs-ob-restclient-latest
  (let ((commit "f81f2f4f3fe6882947b8547ccd570f540106ed4d"))
    (package
      (name "emacs-ob-restclient")
      (version (git-version "0.02" "2" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alf/ob-restclient.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16byn85gsl99k818686jp3r4ipjcwdyq3nilmb32g4hgg0dlgaij"))))
      (propagated-inputs
       (list emacs-restclient))
      (build-system emacs-build-system)
      (home-page "https://github.com/alf/ob-restclient.el")
      (synopsis "Org-babel functionality for @code{restclient-mode}")
      (description
       "This package integrates @code{restclient-mode} with Org.")
      (license license:gpl3+))))

(define-public emacs-modus-themes-latest
  (package
   (inherit emacs-modus-themes)
   (name "emacs-modus-themes")
   (version "2.0.0-patch")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://gitlab.com/protesilaos/modus-themes")
           (commit "4f177f036b30dd6c1f2e6c5342f2b87e034dc97e")))
     (file-name (git-file-name name version))
     (sha256
      (base32 "15zxab2wg97ldy85wv2sx7j31z0cg0h28vm9yjnp3b7vl7sbzf33"))
     (patches (search-patches
               "0001-DRAFT-Add-tentative-support-for-ement.el.patch"))))))

(define-public emacs-justify-kp
 (let ((commit "385e6b8b909ae0f570f30101cec3677e21c9e0a0"))
  (package
   (name "emacs-justify-kp")
   (version "20171119")
   (home-page "https://github.com/qzdl/justify-kp")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url home-page)
           (commit commit)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "13fylx4mvw7cgzd2mq060x43b1x7g5vdf16jm49c31f6b3jj1qi0"))))
   (build-system emacs-build-system)
   (inputs (list emacs-dash emacs-s))
   (synopsis "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (description
    "Paragraph justification for emacs using Knuth/Plass algorithm ")
   (license license:gpl3+))))

(define-public tessen
  (package
    (name "tessen")
    (version "1.3.1")
    (home-page "https://github.com/ayushnix/tessen")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/archives/refs/tags/v" version ".tar.gz"))
       (sha256
        (base32 "07ddb5himj4c9ijdibjy25dshil3k5nqd7869z9rkldzh97zh29f"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (delete 'check))))
    (inputs (list password-store
                  wl-clipboard
                  libnotify
                  pass-otp
                  xdg-utils
                  scdoc))
    (synopsis
     "An interactive menu to autotype and copy password-store data ")
    (description
     "tessen is a bash script that can use any wayland native
dmenu-like backend as an interface for auto-typing and copying
password-store data, known to work with bemenu, fuzzel, rofi, wofi")
    (license license:gpl3+)))

(define-public xdg-desktop-portal-wlr-latest
  (let ((commit "c34d09877cb55eb353311b5e85bf50443be9439d"))
  (package
    (inherit xdg-desktop-portal-wlr)
    (name "xdg-desktop-portal-wlr")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/emersion/xdg-desktop-portal-wlr")
                     (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xw5hmx816w4hsy1pwz3qikxc7m6pcx1ly03hhbb6vp94gfcwpr3"))
              (patches (search-patches "xdg-desktop-portal-wlr-harcoded-length.patch"))))
        (license license:expat))))

(use-modules (gnu packages wm)
             (gnu packages gl)
             (gnu packages xorg))
(define-public wlroots-latest
  (package
    (inherit wlroots)
    (name "wlroots-latest")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00s73nhi3sc48l426jdlqwpclg41kx1hv0yk4yxhbzw19gqpfm1h"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "xwayland/server.c"
               (("Xwayland") (string-append (assoc-ref inputs
                                                       "xorg-server-xwayland")
                                            "/bin/Xwayland")))
             #t)))))
    (propagated-inputs
     (list ;; As required by wlroots.pc.
           eudev
           libinput
           libxkbcommon
           mesa
           pixman
           seatd
           wayland-latest
           wayland-protocols
           xcb-util-errors
           xcb-util-wm
           xorg-server-xwayland))
    (native-inputs
     (list pkg-config wayland))
    (home-page "https://github.com/swaywm/wlroots")
    (synopsis "Pluggable, composable, unopinionated modules for building a
Wayland compositor")
    (description "wlroots is a set of pluggable, composable, unopinionated
modules for building a Wayland compositor.")
    (license license:expat)))

(use-modules (gnu packages libffi)
             (gnu packages xml)
             (gnu packages docbook))
(define-public wayland-latest
  (package
    (inherit wayland)
    (name "wayland-latest")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wayland.freedesktop.org/releases/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "09c7rpbwavjg4y16mrfa57gk5ix6rnzpvlnv1wp7fnbh9hak985q"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:parallel-tests? #f
        #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-docbook-xml
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (with-directory-excursion "doc"
               (substitute* (find-files "." "\\.xml$")
                 (("http://www.oasis-open.org/docbook/xml/4\\.5/")
                  (string-append (assoc-ref (or native-inputs inputs)
                                            "docbook-xml")
                                 "/xml/dtd/docbook/"))
                 (("http://www.oasis-open.org/docbook/xml/4\\.2/")
                  (string-append (assoc-ref (or native-inputs inputs)
                                            "docbook-xml-4.2")
                                 "/xml/dtd/docbook/"))))))
         (add-after 'install 'move-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (assoc-ref outputs "doc")))
               (mkdir-p (string-append doc "/share"))
               (rename-file
                (string-append out "/share/doc")
                (string-append doc "/share/doc"))))))))
    (native-inputs
     `(("docbook-xml-4.2" ,docbook-xml-4.2)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("xmlto" ,xmlto)
       ("xsltproc" ,libxslt)
       ,@(if (%current-target-system)
             `(("pkg-config-for-build" ,pkg-config-for-build)
               ("wayland" ,this-package)) ; for wayland-scanner
             '())))
    (inputs
     (list expat libxml2))           ; for XML_CATALOG_FILES
    (propagated-inputs
     (list libffi))
    (home-page "https://wayland.freedesktop.org/")
    (synopsis "Core Wayland window system code and protocol")
    (description "Wayland is a project to define a protocol for a compositor to
talk to its clients as well as a library implementation of the protocol.  The
compositor can be a standalone display server running on Linux kernel
modesetting and evdev input devices, an X application, or a wayland client
itself.  The clients can be traditional applications, X servers (rootless or
fullscreen) or other display servers.")
    (license license:expat)))
