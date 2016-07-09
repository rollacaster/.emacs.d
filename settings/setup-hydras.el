(defhydra hydra-hue (css-mode-map "C-c C-h")
  "color-hue"
  ("i" kurecolor-increase-hue-by-step "increase hue")
  ("d" kurecolor-decrease-hue-by-step "decreasee hue"))

(defhydra hydra-brightness (css-mode-map "C-c C-b")
  "color-brightness"
  ("i" kurecolor-increase-brightness-by-step "increase brightness")
  ("d" kurecolor-decrease-brightness-by-step "decrease brightness"))

(defhydra hydra-saturation (css-mode-map "C-c C-s")
  "color-saturation"
  ("i" kurecolor-increase-saturation-by-step "increase saturation")
  ("d" kurecolor-decrease-saturation-by-step "decrease saturation"))

(provide 'setup-hydras)
