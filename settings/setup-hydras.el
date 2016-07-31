(eval-after-load 'css-mode '(defhydra hydra-color (css-mode-map "C-c C-c")
  "color"
  ("u" kurecolor-increase-hue-by-step "increase hue")
  ("j" kurecolor-decrease-hue-by-step "decreasee hue")
  ("i" kurecolor-increase-saturation-by-step "increase saturation")
  ("k" kurecolor-decrease-saturation-by-step "decrease saturation")
  ("o" kurecolor-increase-brightness-by-step "increase brightness")
  ("l" kurecolor-decrease-brightness-by-step "decrease brightness")))


(provide 'setup-hydras)
