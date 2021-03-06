(defun rac-insert-init ()
  (interactive)
  (erase-buffer)
  (insert "const _ = require('ramda')
const {
  // algebraic operations
  map,
  traverse,
  chain,
  // helper functions
  pipe,
  prop,
  slice
} = _
const { Future } = require('ramda-fantasy')

const {
  // loadPage :: URL -> Future Error Response
  loadPage,
  // scrapePage :: Selector -> HTML -> [DOMElement]
  scrapePage
} = require('./scrape-fns')

module.exports = loadPage(
  'http://127.0.0.1/open-call-for-reactiveconf-lightning-talks-2017-a4f5394e5f96.html'
)
")
  (save-buffer))

(defun rac-start-talk ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (npm-mode--exec-process "npm-run" (format "npm run %s" "start")))

(defun rac-insert-pretty ()
  (interactive)
  (insert  "const { makeThingsPretty } = require('./pretty-fns')")
  (kill-new "findStars")
  (kill-new "makeThingsPretty"))

(defun rac-insert-traverse ()
  (interactive)
  (insert "map(
      map(
        pipe(
          scrapePage('.social-count'),
          chain(pipe(prop('children'), map(prop('data')))),
          _.init,
          _.last,
          parseInt
        )
      )
    )"))

(defun rac-class-to-class-selector (beg end)
  (interactive "r")
  (let ((class (buffer-substring beg end)))
    (kill-region beg end)
    (insert (replace-regexp-in-string " " "." (concat "." class)))))

(defun wrap-in-pipe (beg end)
  (interactive "r")
  (let ((functions (buffer-substring beg end)))
    (kill-region beg end)
    (insert (concat "pipe(" (concat functions ")"))))
    (backward-char)
    (insert ", "))

(setq explain-dir (file-name-directory (or load-file-name buffer-file-name)))

(defun explain-emacs ()
  "Explains emacs to your coworkers."
  (interactive)
  (let ((newFrame (make-frame '((width . 34) (height . 15)))))
    (switch-to-buffer-other-frame (buffer-name))
    (setq inhibit-message t)
    (find-file "/Users/thomas/.emacs.d/gifs/magic.gif")
    (image-goto-frame 17)
    (image-toggle-animation)
    (run-at-time "1.5 sec" nil (lambda () (delete-frame) (kill-buffer "magic.gif")))
    (setq inhibit-message nil)))

(defun rac-blow-mind ()
  "Explains emacs to your coworkers."
  (interactive)
  (let ((newFrame (make-frame '((width . 34) (height . 12)))))
    (switch-to-buffer-other-frame (buffer-name))
    (setq inhibit-message t)
    (find-file "/Users/thomas/.emacs.d/gifs/mind-blown.gif")
    (image-goto-frame 15)
    (image-toggle-animation)
    (run-at-time "1.5 sec" nil (lambda () (delete-frame) (kill-buffer "mind-blown.gif")))
    (setq inhibit-message nil)))

(defun rac-wow ()
  "Explains emacs to your coworkers."
  (interactive)
  (let ((newFrame (make-frame '((width . 34) (height . 12)))))
    (switch-to-buffer-other-frame (buffer-name))
    (setq inhibit-message t)
    (find-file "/Users/thomas/.emacs.d/gifs/wow.gif")
    (image-goto-frame 15)
    (image-toggle-animation)
    (run-at-time "1.5 sec" nil (lambda () (delete-frame) (kill-buffer "wow.gif")))
    (setq inhibit-message nil)))

(defun rac-insert-talks ()
    (interactive)
    (insert "const findTalks = map(
  pipe(
    scrapePage('.markup--strong.markup--p-strong'),
    chain(pipe(prop('children'), map(prop('data')))),
    slice(6, Infinity)
  )
)")
    (kill-new "findTalks"))

(defun rac-setup-talk ()
  (interactive)
  (find-file "/Users/thomas/Projects/reactive-conf/lightning-talk.js")
  (delete-other-windows)
  (rac-kill-term-buffers)
  (rac-insert-init))

(defun rac-insert-final ()
  (interactive)
  (insert "const _ = require('ramda')
const {
  // algebraic operations
  map,
  traverse,
  chain,
  // helper functions
  pipe,
  prop,
  slice
} = _
const { Future } = require('ramda-fantasy')

const {
  // loadPage :: URL -> Future Error Response
  loadPage,
  // scrapePage :: Selector -> HTML -> [DOMElement]
  scrapePage
} = require('./scrape-fns')
const crawlPage = pipe(
  loadPage,
  map(pipe(prop('data')))
)
const findStars = chain(
  pipe(
    scrapePage(
      '.markup--anchor.markup--p-anchor'
    ),
    map(pipe(prop('attribs'), prop('href'))),
    slice(2, Infinity),
    traverse(Future.of, crawlPage),
    map(
      map(
        pipe(
          scrapePage('.social-count'),
          chain(
            pipe(
              prop('children'),
              map(prop('data'))
            )
          ),
          _.init,
          _.last,
          parseInt
        )
      )
    )
  )
)
const findTalks = map(
  pipe(
    scrapePage(
      '.markup--strong.markup--p-strong'
    ),
    chain(
      pipe(prop('children'), map(prop('data')))
    ),
    slice(6, Infinity)
  )
)
const {
  makeThingsPretty
} = require('./pretty-fns')
module.exports = pipe(
  crawlPage,
  makeThingsPretty(findTalks, findStars)
)(
  'http://127.0.0.1/open-call-for-reactiveconf-lightning-talks-2017-a4f5394e5f96.html'
)
"))


(global-set-key (kbd "C-c b f") 'rac-insert-final)
(global-set-key (kbd "C-c b n") 'rac-insert-pretty)
(global-set-key (kbd "C-c b i") 'rac-setup-talk)
(global-set-key (kbd "C-c b s") 'rac-start-talk)
(global-set-key (kbd "C-c b m") 'explain-emacs)
(global-set-key (kbd "C-c b r") 'rac-class-to-class-selector)
(global-set-key (kbd "C-c b p") 'wrap-in-pipe)
(global-set-key (kbd "C-c b t") 'rac-insert-traverse)
(global-set-key (kbd "C-c b m") 'rac-insert-talks)
(global-set-key (kbd "C-c b b") 'rac-blow-mind)
(global-set-key (kbd "C-c b w") 'rac-wow)


(defun rac-bugs-rectangles ()
  (interactive)
  (insert "const leftEar = rect(30, 70).map(c(move(110, 0), fill('#848e9b')))
const leftInnerEar = rect(10, 50).map(c(move(120, 10), fill('white')))
const leftHead = map(c(move(110, 70), fill('#848e9b')), rect(40, 40))
const leftBeard = map(c(move(80, 110), fill('white')))(rect(70, 40))
const leftEye = map(c(move(120, 80), fill('black')))(rect(20, 30))
const leftEyePupil = map(c(move(130, 90), fill('white')))(rect(10, 15))
const leftNose = map(c(move(140, 110), fill('red')))(rect(10, 10))
const leftmouth = map(c(move(120, 130), fill('black')))(rect(30, 10))

const leftSide = [
  leftEar,
  leftInnerEar,
  leftHead,
  leftBeard,
  leftEye,
  leftEyePupil,
  leftNose,
  leftmouth
]
"))
(global-set-key (kbd "C-c b b") 'rac-bugs-rectangles)

(defun rac-reactive-green ()
    (interactive)
  (insert "#4eb363"))

(global-set-key (kbd "C-c b g") 'rac-reactive-green)

(defun rac-setup-compose ()
  (interactive)
  (find-file "/Users/thomas/Projects/compose-all-the-things/src/index.js")
  (erase-buffer)
  (insert "import {
  render,
  ellipse,
  fill,
  stroke,
  rotate
} from './slowest-graphics-library-ever-created'
import {
  compose as c,
  range,
  map,
  applyTo,
  multiply,
  ap
} from 'ramda'

// Build shapes

// Rotate vertical ellipse

// Rotate all ellipses

// Render

")
  (setq text-scale-mode-amount 1)
  (text-scale-mode (if (zerop text-scale-mode-amount) -1 1))
  (visual-line-mode)
  (delete-other-windows)
  (rac-kill-term-buffers)
  (npm-mode--exec-process (format "npm run %s" "start"))
  (delete-other-windows &optional WINDOW))

(global-set-key (kbd "C-c b s") 'rac-setup-compose)

(provide 'talk-helpers.el)
;;; talk-helpers.el ends here
