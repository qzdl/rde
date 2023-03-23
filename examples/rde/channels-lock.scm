(use-modules (guix channels))

(list (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        (branch "master")
        (commit
          "110f914f81d0d2c67a45f9b2f87a5dc7ec54dfd0"))
      (channel
        (name 'rde)
        (url "https://git.sr.ht/~abcdw/rde")
        (branch "master")
        (commit
          "30d6073b3ac26381be892d4cb26a99575ae38ad3")
        (introduction
          (make-channel-introduction
            "257cebd587b66e4d865b3537a9a88cccd7107c95"
            (openpgp-fingerprint
              "2841 9AC6 5038 7440 C7E9  2FFA 2208 D209 58C1 DEB0"))))
      (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (commit
          "e9c1ea84fcf4a7f3da9aea9fbfe6136c75643c07")
        (introduction
          (make-channel-introduction
            "9edb3f66fd807b096b48283debdcddccfea34bad"
            (openpgp-fingerprint
              "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
