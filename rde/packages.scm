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
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages python)
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

(define-public emacs-org-roam-ui
  (package
  (name "emacs-org-roam-ui")
  (version "20220225.2151")
  (source
    (origin
      (method git-fetch)
      (uri (git-reference
             (url "https://github.com/org-roam/org-roam-ui.git")
             (commit "df1f9522c5a9cdb248208427fa9df4f2a7666e2a")))
      (sha256
        (base32 "03kyg95f012ql0gpzy58kzxgdfksig5zlbr1p9m9ycgqmmxyq4jp"))))
  (build-system emacs-build-system)
  (propagated-inputs
   (list emacs-org-roam
         emacs-simple-httpd
         emacs-websocket))
  (arguments
    '(#:include
      '("^[^/]+.el$"
        "^[^/]+.el.in$"
        "^dir$"
        "^[^/]+.info$"
        "^[^/]+.texi$"
        "^[^/]+.texinfo$"
        "^doc/dir$"
        "^doc/[^/]+.info$"
        "^doc/[^/]+.texi$"
        "^doc/[^/]+.texinfo$"
        "^out$")
      #:exclude
      '("^.dir-locals.el$"
        "^test.el$"
        "^tests.el$"
        "^[^/]+-test.el$"
        "^[^/]+-tests.el$")))
  (home-page "https://github.com/org-roam/org-roam-ui")
  (synopsis "User Interface for Org-roam")
  (description
    "Org-roam-ui provides a web interface for navigating around notes
created within Org-roam.")
  (license license:gpl3+)))

(use-modules (gnu packages python))
(define-public python-3.6.4
  (package (inherit python-2)
    (version "3.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "python-fix-tests.patch"
                        "python-3-fix-tests.patch"
                        "python-3-deterministic-build-info.patch"
                        "python-3-search-paths.patch"))
              (patch-flags '("-p0"))
              (sha256
               (base32
                "1fna7g8jxzl4kd2pqmmqhva5724c5m920x3fsrpsgskaylmr76qm"))
              (snippet
               '(begin
                  (for-each delete-file
                            '("Lib/ctypes/test/test_structures.py" ; fails on aarch64
                              "Lib/ctypes/test/test_win32.py" ; fails on aarch64
                              "Lib/test/test_fcntl.py")) ; fails on aarch64
                  #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments python-2)
       ((#:tests? _) #t)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'patch-timestamp-for-pyc-files
             (lambda _
               ;; We set DETERMINISTIC_BUILD to only override the mtime when
               ;; building with Guix, lest we break auto-compilation in
               ;; environments.
               (setenv "DETERMINISTIC_BUILD" "1")
               (substitute* "Lib/py_compile.py"
                 (("source_stats\\['mtime'\\]")
                  "(1 if 'DETERMINISTIC_BUILD' in os.environ else source_stats['mtime'])"))

               ;; Use deterministic hashes for strings, bytes, and datetime
               ;; objects.
               (setenv "PYTHONHASHSEED" "0")

               ;; Reset mtime when validating bytecode header.
               (substitute* "Lib/importlib/_bootstrap_external.py"
                 (("source_mtime = int\\(source_stats\\['mtime'\\]\\)")
                  "source_mtime = 1"))
               #t))
           ;; These tests fail because of our change to the bytecode
           ;; validation.  They fail because expected exceptions do not get
           ;; thrown.  This seems to be no problem.
           (add-after 'unpack 'disable-broken-bytecode-tests
             (lambda _
               (substitute* "Lib/test/test_importlib/source/test_file_loader.py"
                 (("test_bad_marshal")
                  "disable_test_bad_marshal")
                 (("test_no_marshal")
                  "disable_test_no_marshal")
                 (("test_non_code_marshal")
                  "disable_test_non_code_marshal"))
               #t))
           ;; Unset DETERMINISTIC_BUILD to allow for tests that check that
           ;; stale pyc files are rebuilt.
           (add-before 'check 'allow-non-deterministic-compilation
             (lambda _ (unsetenv "DETERMINISTIC_BUILD") #t))
           ;; We need to rebuild all pyc files for three different
           ;; optimization levels to replace all files that were not built
           ;; deterministically.

           ;; FIXME: Without this phase we have close to 2000 files that
           ;; differ across different builds of this package.  With this phase
           ;; there are about 500 files left that differ.
           (add-after 'install 'rebuild-bytecode
             (lambda* (#:key outputs #:allow-other-keys)
               (setenv "DETERMINISTIC_BUILD" "1")
               (let ((out (assoc-ref outputs "out")))
                 (for-each
                  (lambda (opt)
                    (format #t "Compiling with optimization level: ~a\n"
                            (if (null? opt) "none" (car opt)))
                    (for-each (lambda (file)
                                (apply invoke
                                       `(,(string-append out "/bin/python3")
                                         ,@opt
                                         "-m" "compileall"
                                         "-f" ; force rebuild
                                         ;; Don't build lib2to3, because it's Python 2 code.
                                         ;; Also don't build obviously broken test code.
                                         "-x" "(lib2to3|test/bad.*)"
                                         ,file)))
                              (find-files out "\\.py$")))
                  (list '() '("-O") '("-OO"))))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor version)
                                        "/site-packages"))))))))
(define-public emacs-sql-indent
  (package
   (name "emacs-sql-indent")
   (version "1.6")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://elpa.gnu.org/packages/sql-indent-"
           version
           ".tar"))
     (sha256
      (base32 "000pimlg0k4mrv2wpqq8w8l51wpr1lzlaq6ai8iaximm2a92ap5b"))))
   (build-system emacs-build-system)
   (home-page "https://github.com/alex-hhh/emacs-sql-indent")
   (synopsis "Support for indenting code in SQL files.")
   (description
    "`sqlind-minor-mode' is a minor mode that enables syntax-based indentation for
`sql-mode' buffers: the TAB key indents the current line based on the SQL code
on previous lines.  To setup syntax-based indentation for every SQL buffer, add
`sqlind-minor-mode' to `sql-mode-hook'.  Indentation rules are flexible and can
be customized to match your personal coding style.  For more information, see
the \"sql-indent.org\" file.

The package also defines align rules so that the `align' function works for SQL
statements, see `sqlind-align-rules'.")
   (license license:gpl3+)))

(define-public emacs-uuidgen
  (package
   (name "emacs-uuidgen")
   (version "20200816.1308")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/kanru/uuidgen-el.git")
           (commit "b50e6fef2de4199a8f207b46588c2cb3890ddd85")))
     (sha256
      (base32 "08m74kj7h70kna3pifk3sgsy7mck11p32vi48h9wzqnafyq3n55d"))))
   (build-system emacs-build-system)
   (home-page "unspecified")
   (synopsis "Provides various UUID generating functions")
   (description
    " This is a naive implementation of RFC4122 Universally Unique IDentifier
generation in elisp.  Currently implemented are UUID v1 v3, v4 and v5
generation.  The resolution of the time based UUID is microseconds, which is 10
times of the suggested 100-nanosecond resolution, but should be enough for
general usage.

Get development version from git:

    git clone git://github.com/kanru/uuidgen-el.git")
   (license #f)))

(define-public emacs-code-review
  (package
   (name "emacs-code-review")
   (version "20220328.108")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/wandersoncferreira/code-review.git")
           (commit "85ab2080e489b4ca01c787f5a316ade02a4ee877")))
     (sha256
      (base32 "0j0ijnzfd7b3a2jqi94zlky8iqv9g7vj9fx5fd4g2k53ilgapmdl"))))
   (build-system emacs-build-system)
   (propagated-inputs
    (list emacs-closql
          emacs-magit
          emacs-a
          emacs-ghub
          emacs-uuidgen
          emacs-deferred
          emacs-markdown-mode
          emacs-forge
          emacs-emojify))
   (home-page "https://github.com/wandersoncferreira/code-review")
   (synopsis "Perform code review from Github, Gitlab, and Bitbucket Cloud")
   (description
    "Review Pull Request in Emacs using a modern interface based on Magit
Section and Transient.  Currently supports Github, Gitlab, and
Bitbucket Cloud.")
   (license license:gpl3+)))
