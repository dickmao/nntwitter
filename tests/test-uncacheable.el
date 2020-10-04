(require 'nntwitter-test)

(ert-deftest nntwitter-should-not-cache ()
  (should (string-match gnus-uncacheable-groups "nntwitter:melpa_emacs")))

