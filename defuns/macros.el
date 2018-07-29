(fset 'rac-react-func-to-class
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217828 99 108 97 115 115 19 61 return backspace 101 120 116 101 110 100 115 32 67 111 109 112 111 110 101 110 116 32 123 4 4 4 134217834 114 101 110 100 101 114 32 40 6 123 4 134217834 99 111 110 115 116 19 125 return 4 32 61 116 104 105 115 46 112 114 111 112 115 4 4 4 134217834 114 101 116 114 110 backspace backspace 117 114 110 134217734 125 125 24 19] 0 "%d")) arg)))

(fset 'rac-react-class-to-func
   [?\M-d ?  ?c ?o ?n ?s ?t ?\C-s ?e ?x return ?\M-b ?\M-d ?\M-d ?= ?\C-f ?\( ?\C-d ?\C-f ?\M-d ?\M-d ?\C-d ?\C-d ?\C-s ?\} return ?\) ?\C-f ?\C-f ?> ?\M-d ?\M-d ?\M-d ?\C-f ?\C-\M-f ?\C-n backspace ?\C-n backspace])

(fset 'rac-string-to-template
   [?\C-r ?\' return ?\C-  ?\C-s ?\' ?\C-s return ?` ?\C-b backspace ?\C-r ?\' return ?\C-d])

