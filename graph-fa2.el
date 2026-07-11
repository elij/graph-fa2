;;; graph-fa2.el --- ForceAtlas2 pure-elisp background-cached engine -*- lexical-binding: t -*-

;; Author: Elijah Charles
;; Version: 0.1.0

(eval-when-compile
  (when (boundp 'comp-speed)
    (setq comp-speed 3)))

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'seq)

(defgroup graph-fa2 nil
  "ForceAtlas2 graph layout engine."
  :group 'multimedia)

(defcustom graph-fa2-repulsion-x-y-threshold 80954
  "Threshold for coordinate differences when calculating node repulsion."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-repulsion-threshold 655360
  "Threshold distance used for calculating node repulsion."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-repulsion-max-dist-sq 6553600000
  "Maximum squared distance for repulsion force calculation."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-attraction-threshold 12800
  "Threshold used in calculating node attraction."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-speed-limit-threshold 12800
  "Maximum speed limit parameter for node layout integration."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-horizon-threshold 61440
  "Boundary threshold where layout node coordinates are clamped."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-horizon-start-threshold 49152
  "Threshold where friction starts damping node velocities near the horizon."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-surface-radius 38400
  "The maximum spherical radius nodes can occupy."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-surface-constraint 'anneal
  "How nodes are constrained to the simulation sphere radius.
'strict  - Nodes are permanently pinned exactly to the surface.
'none    - Nodes move freely in 3D space, limited only by the horizon.
'floor   - Nodes cannot enter the sphere, but can fly outward.
'ceiling - Nodes can enter the sphere, but cannot fly outward past the surface.
'anneal  - Nodes start free in 3D space and gradually collapse to the surface."
  :type '(choice (const :tag "Strictly pinned" strict)
                 (const :tag "Freely floating" none)
                 (const :tag "Floor minimum" floor)
                 (const :tag "Ceiling maximum" ceiling)
                 (const :tag "Anneal to surface" anneal))
  :group 'graph-fa2)

(defcustom graph-fa2-drag-action 'rotate
  "The action performed when dragging the graph background.
'rotate - Rotates the 3D physics coordinate sphere.
'pan    - Pans the 2D visual viewport."
  :type '(choice (const :tag "Rotate 3D Sphere" rotate)
                 (const :tag "Pan 2D Viewport" pan))
  :group 'graph-fa2)

(defcustom graph-fa2-engine '3d
  "The dimensionality engine of the ForceAtlas2 layout.
'3d - Three-dimensional physics engine and trackball rotation.
'2d - Two-dimensional physics engine and traditional panning."
  :type '(choice (const :tag "3D Engine" 3d)
                 (const :tag "2D Engine" 2d))
  :group 'graph-fa2)

(defcustom graph-fa2-simulation-frames 840
  "Total number of frames to calculate for the layout simulation."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-framerate 60.0
  "Target framerate for the animated graph playback."
  :type 'float
  :group 'graph-fa2)

(defcustom graph-fa2-zoom-friction 0.85
  "Friction applied to zoom velocity per frame."
  :type 'float
  :group 'graph-fa2)

(defcustom graph-fa2-zoom-acceleration 0.06
  "Amount of velocity added per scroll wheel tick."
  :type 'float
  :group 'graph-fa2)

(defcustom graph-fa2-edge-colour "#585b70"
  "Stroke colour, as a hex string, for graph edges."
  :type 'string
  :group 'graph-fa2)

(defcustom graph-fa2-edge-width 2
  "Stroke width, in SVG units, for graph edges."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-label-colour "#cdd6f4"
  "Fill colour, as a hex string, for node label text."
  :type 'string
  :group 'graph-fa2)

(defcustom graph-fa2-label-font-size 10
  "Font size, in SVG units, for node label text."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-label-offset-y 15
  "Vertical offset applied to the first line of a node label."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-label-font-family nil
  "Font family for node label text, or nil to use the renderer default."
  :type '(choice (const :tag "Renderer default" nil) string)
  :group 'graph-fa2)

(defcustom graph-fa2-label-font-weight nil
  "Font weight for node label text, or nil to use the renderer default."
  :type '(choice (const :tag "Renderer default" nil) string)
  :group 'graph-fa2)

(defcustom graph-fa2-label-wrap-chars 10
  "Maximum number of characters per line before a node label wraps."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-substeps 10
  "The number of physics substeps per frame."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-canvas-size 500.0
  "The size of the square rendering canvas."
  :type 'float
  :group 'graph-fa2)

(defvar-local graph-fa2-node-clicked-functions nil
  "List of functions to be called when a graph node is clicked.
Each function must accept one argument: the node identifier.")

(defvar-local graph-fa2-node-hovered-functions nil
  "List of functions to be called when a mouse hovers over a graph node.
Each function must accept one argument: the node identifier, or nil if cleared.")

(defvar-local graph-fa2--scale 1.0
  "Current zoom scale for the background engine.")

(defvar-local graph-fa2--pan-x 0.0
  "Horizontal pan offset of the graph viewport.")

(defvar-local graph-fa2--pan-y 0.0
  "Vertical pan offset of the graph viewport.")

(defvar-local graph-fa2--zoom-velocity 0.0
  "Current inertial velocity of the zoom operation.")

(defvar-local graph-fa2--zoom-timer nil
  "Timer object for the inertial zoom animation.")

(defvar-local graph-fa2--render-state nil
  "Tracks the parameters of the last render to prevent double rendering.")

(defvar-local graph-fa2--current-frame 0)
(defvar-local graph-fa2--frame-offsets nil)
(defvar-local graph-fa2-playback-buffer nil)
(defvar-local graph-fa2-current-svg nil)
(defvar-local graph-fa2--player-timer nil)
(defvar-local graph-fa2--hitbox-svg-string nil
  "Tracks the SVG string used to generate the current hitboxes.")

(defvar-local graph-fa2--active-hitboxes nil
  "A fast-access vector of identifiers and coordinates for the currently displayed frame.")

(defvar-local graph-fa2-hovered-node nil
  "Tracks the currently hovered node within the fa2 engine.")

(defvar-local graph-fa2--drag-context nil
  "The current drag context of the graph viewport.")

(defvar graph-fa2-after-render-functions nil
  "Hook run after a graph frame is rendered.")

(cl-defstruct graph-fa2-ctx
  "State structure for ForceAtlas2 physics simulation.
Contains node and edge definitions, pre-allocated vectors to minimise
garbage collection pressure, and running animation state."
  nodes
  edges
  mass-matrix
  pos-x
  pos-y
  pos-z
  vel-x
  vel-y
  vel-z
  rep-x
  rep-y
  rep-z
  bg-buffer
  bg-frame
  bg-timer
  frames-rendered
  heavy-frames
  heavy-time
  playback-started
  start-time)

(defvar-local graph-fa2-ctx nil
  "Buffer-local ForceAtlas2 simulation context.")

(defsubst fa2-id (n)
  "Return the identifier of node N."
  (aref n 0))

(defsubst fa2-label (n)
  "Return the label of node N."
  (aref n 1))

(defsubst fa2-x (n)
  "Return the x coordinate of node N."
  (aref n 2))

(defsubst fa2-y (n)
  "Return the y coordinate of node N."
  (aref n 3))

(defsubst fa2-z (n)
  "Return the z coordinate of node N."
  (aref n 4))

(defsubst fa2-dx (n)
  "Return the x velocity of node N."
  (aref n 5))

(defsubst fa2-dy (n)
  "Return the y velocity of node N."
  (aref n 6))

(defsubst fa2-dz (n)
  "Return the z velocity of node N."
  (aref n 7))

(defsubst fa2-mass (n)
  "Return the mass of node N."
  (aref n 8))

(defsubst fa2-colour (n)
  "Return the colour string of node N."
  (aref n 9))

(defsubst fa2-radius (n)
  "Return the radius of node N."
  (aref n 10))

(defsubst fa2-set-x (n v)
  "Set the x coordinate of node N to V."
  (aset n 2 v))

(defsubst fa2-set-y (n v)
  "Set the y coordinate of node N to V."
  (aset n 3 v))

(defsubst fa2-set-z (n v)
  "Set the z coordinate of node N to V."
  (aset n 4 v))

(defsubst fa2-set-dx (n v)
  "Set the x velocity of node N to V."
  (aset n 5 v))

(defsubst fa2-set-dy (n v)
  "Set the y velocity of node N to V."
  (aset n 6 v))

(defsubst fa2-set-dz (n v)
  "Set the z velocity of node N to V."
  (aset n 7 v))

(defsubst graph-fa2--dist-3d (dx dy dz)
  "Calculate approximate 3D distance using bitwise shifts.
Avoids floating-point allocations by approximating the Euclidean distance
using integer scaling."
  (let* ((ax (abs dx))
         (ay (abs dy))
         (az (abs dz))
         (max-d (max ax ay az))
         (min-d (min ax ay az))
         (mid-d (- (+ ax ay az) max-d min-d)))
    (if (= max-d 0)
        1
      (+ max-d (ash mid-d -2) (ash min-d -2)))))

(defconst graph-fa2-max-velocity 8192
  "Maximum velocity per tick to prevent coordinate overflow.")

(defun graph-fa2-clamp (value limit)
  "Restrict an integer value to the bounds of negative and positive limit.

This ensures that integer values do not exceed the specified threshold,
preventing coordinate overflows in the simulation loop."
  (max (- limit) (min limit value)))

(defun graph-fa2-normalise-coords (x y z)
  "Convert internal simulation coordinates to external viewport coordinates.

This divides the fixed-point integer coordinates by 256.0 and adds half
of the canvas size to shift the origin to the centre of the viewport.
It logs the transition values to aid in tracking rounding and scaling
errors during debugging."
  (when-let* ((half-canvas (/ graph-fa2-canvas-size 2.0))
              (norm-x (+ (/ (float x) 256.0) half-canvas))
              (norm-y (+ (/ (float y) 256.0) half-canvas))
              (norm-z (+ (/ (float z) 256.0) half-canvas)))
    (list norm-x norm-y norm-z)))

(defun graph-fa2-denormalise-coords (nx ny nz)
  "Convert external viewport coordinates back to internal simulation integers.

This subtracts half of the canvas size from the viewport coordinate,
multiplies by 256.0, and truncates the result to return a standard
integer representation. It logs the transition values to aid in debugging."
  (when-let* ((half-canvas (/ graph-fa2-canvas-size 2.0))
              (x (truncate (* 256.0 (- nx half-canvas))))
              (y (truncate (* 256.0 (- ny half-canvas))))
              (z (truncate (* 256.0 (- nz half-canvas)))))
    (list x y z)))

(defun graph-fa2--cancel-drag (&rest _)
  "Clear the drag context, typically used when window focus changes."
  (when graph-fa2--drag-context
    (setq graph-fa2--drag-context nil)))

(defun graph-fa2-rotate-3d-integer (pos-x pos-y pos-z len angle-x angle-y)
  "Rotate the integer 3D coordinate arrays by ANGLE-X and ANGLE-Y.
Converts coordinates to floats temporarily to apply trigonometric rotation,
then truncates back to fixed-point integers to preserve memory stability
in subsequent physics iterations."
  (let ((cos-x (cos angle-x))
        (sin-x (sin angle-x))
        (cos-y (cos angle-y))
        (sin-y (sin angle-y)))
    (dotimes (i len)
      (let* ((x (float (aref pos-x i)))
             (y (float (aref pos-y i)))
             (z (float (aref pos-z i)))
             (y1 (- (* y cos-x) (* z sin-x)))
             (z1 (+ (* y sin-x) (* z cos-x)))
             (x2 (+ (* x cos-y) (* z1 sin-y)))
             (z2 (- (* z1 cos-y) (* x sin-y))))
        (aset pos-x i (truncate x2))
        (aset pos-y i (truncate y1))
        (aset pos-z i (truncate z2))))))

(defun graph-fa2--2d-mouse-down-pan (mouse-x mouse-y img-w img-h)
  "Initialise the drag context for traditional 2D panning.

Parameters:
MOUSE-X: Horizontal click coordinate.
MOUSE-Y: Vertical click coordinate.
IMG-W: Rendered image width.
IMG-H: Rendered image height."
  (setq graph-fa2--drag-context
        (list (cons 'type 'pan)
              (cons 'start-mouse-x mouse-x)
              (cons 'start-mouse-y mouse-y)
              (cons 'img-width img-w)
              (cons 'img-height img-h)
              (cons 'start-pan-x graph-fa2--pan-x)
              (cons 'start-pan-y graph-fa2--pan-y))))

(defun graph-fa2--2d-pan (drag-ctx pixel-dx pixel-dy viewbox-scale)
  "Perform traditional 2D viewport panning.

This updates the horizontal and vertical pan offsets based on the starting
offsets and the scaled mouse displacement delta.

Parameters:
DRAG-CTX: The active drag context.
PIXEL-DX: Absolute horizontal mouse displacement.
PIXEL-DY: Absolute vertical mouse displacement.
VIEWBOX-SCALE: The conversion factor between pixels and canvas units."
  (let ((start-pan-x (cdr (assoc 'start-pan-x drag-ctx)))
        (start-pan-y (cdr (assoc 'start-pan-y drag-ctx))))
    (setq graph-fa2--pan-x (+ start-pan-x (* pixel-dx viewbox-scale)))
    (setq graph-fa2--pan-y (+ start-pan-y (* pixel-dy viewbox-scale)))))

(defun graph-fa2-track-mouse (event)
  "Track mouse movement, handling hovering, panning, and node dragging.

When a drag operation is active, calculate the difference between the current
and starting mouse coordinates. Scale this delta by the current zoom level to
keep the movement speed matching the visual scale. For viewport panning,
either rotate the underlying physics coordinates or pan the 2D visual layout based
on the user settings. For node movement, update the physics arrays directly
and trigger a display refresh. When no drag is active, update the hovered
node state and change the cursor."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (window (posn-window posn))
              ((window-live-p window)))
    (with-current-buffer (window-buffer window)
      (if-let* ((drag-ctx graph-fa2--drag-context)
                (coords (posn-object-x-y posn))
                (type (cdr (assoc 'type drag-ctx)))
                (start-x (cdr (assoc 'start-mouse-x drag-ctx)))
                (start-y (cdr (assoc 'start-mouse-y drag-ctx)))
                (img-w (cdr (assoc 'img-width drag-ctx)))
                (img-h (cdr (assoc 'img-height drag-ctx))))
          (let* ((curr-x (float (car coords)))
                 (curr-y (float (cdr coords)))
                 (pixel-dx (- curr-x start-x))
                 (pixel-dy (- curr-y start-y))
                 (min-dim (min img-w img-h))
                 (viewbox-scale (/ graph-fa2-canvas-size (* graph-fa2--scale min-dim))))
            (cond
             ((eq type 'pan)
              (if (eq graph-fa2-engine '2d)
                  (let* ((pixel-dx (- curr-x start-x))
                         (pixel-dy (- curr-y start-y)))
                    (graph-fa2--2d-pan drag-ctx pixel-dx pixel-dy viewbox-scale)
                    (setq graph-fa2--render-state nil)
                    (graph-fa2--update-display))
                (when-let* ((last-x (cdr (assoc 'last-mouse-x drag-ctx)))
                            (last-y (cdr (assoc 'last-mouse-y drag-ctx)))
                            (pixel-dx-since-last (- curr-x last-x))
                            (pixel-dy-since-last (- curr-y last-y)))
                  (if (eq graph-fa2-drag-action 'rotate)
                      (when-let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
                                  (ctx (graph-fa2--discover-context base-buf))
                                  (sensitivity 0.005)
                                  (angle-y (* pixel-dx-since-last sensitivity))
                                  (angle-x (* pixel-dy-since-last sensitivity)))
                        (graph-fa2-rotate-3d-integer 
                         (graph-fa2-ctx-pos-x ctx) 
                         (graph-fa2-ctx-pos-y ctx) 
                         (graph-fa2-ctx-pos-z ctx) 
                         (length (graph-fa2-ctx-nodes ctx)) 
                         angle-x angle-y)
                        (setq graph-fa2-current-svg (graph-fa2--render-current-to-svg ctx)))
                    (let ((canvas-dx (* pixel-dx-since-last viewbox-scale))
                          (canvas-dy (* pixel-dy-since-last viewbox-scale)))
                      (cl-incf graph-fa2--pan-x canvas-dx)
                      (cl-incf graph-fa2--pan-y canvas-dy)))
                  (setcdr (assoc 'last-mouse-x drag-ctx) curr-x)
                  (setcdr (assoc 'last-mouse-y drag-ctx) curr-y)
                  (setq graph-fa2--render-state nil)
                  (graph-fa2--update-display))))
             ((eq type 'node-move)
              (when-let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
                          (ctx (graph-fa2--discover-context base-buf))
                          (node-id (cdr (assoc 'node-id drag-ctx)))
                          (orig-x (cdr (assoc 'orig-x drag-ctx)))
                          (orig-y (cdr (assoc 'orig-y drag-ctx)))
                          (orig-z (or (cdr (assoc 'orig-z drag-ctx)) 0.0))
                          (canvas-dx (* pixel-dx viewbox-scale))
                          (canvas-dy (* pixel-dy viewbox-scale))
                          (new-x (+ orig-x canvas-dx))
                          (new-y (+ orig-y canvas-dy))
                          (nodes (graph-fa2-ctx-nodes ctx))
                          (len (length nodes))
                          (idx (seq-position nodes node-id (lambda (n id) (equal (fa2-id n) id))))
                          (new-coords (graph-fa2-denormalise-coords new-x new-y orig-z))
                          (internal-x (car new-coords))
                          (internal-y (cadr new-coords))
                          (internal-z (caddr new-coords)))
                (aset (graph-fa2-ctx-pos-x ctx) idx internal-x)
                (aset (graph-fa2-ctx-pos-y ctx) idx internal-y)
                (aset (graph-fa2-ctx-pos-z ctx) idx internal-z)
                (aset (graph-fa2-ctx-vel-x ctx) idx 0)
                (aset (graph-fa2-ctx-vel-y ctx) idx 0)
                (aset (graph-fa2-ctx-vel-z ctx) idx 0)
                (setq graph-fa2-current-svg (graph-fa2--render-current-to-svg ctx))
                (when-let* ((hitbox (seq-find (lambda (hb) (equal (aref hb 0) node-id)) graph-fa2--active-hitboxes)))
                  (aset hitbox 1 new-x)
                  (aset hitbox 2 new-y)
                  (when (> (length hitbox) 4)
                    (aset hitbox 4 orig-z)))
                (setcdr (assoc 'last-mouse-x drag-ctx) curr-x)
                (setcdr (assoc 'last-mouse-y drag-ctx) curr-y)
                (setq graph-fa2--render-state nil)
                (graph-fa2--update-display)))))
        (let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
               (is-playing (or graph-fa2--player-timer
                               (buffer-local-value 'graph-fa2--player-timer base-buf))))
          (unless is-playing
            (let* ((coords (posn-object-x-y posn))
                   (size (posn-object-width-height posn))
                   (node (when (and coords size)
                           (graph-fa2-node-at-scaled-pos
                            (float (car coords))
                            (float (cdr coords))
                            (max 1.0 (float (car size)))
                            (max 1.0 (float (cdr size)))))))
              (unless (equal node graph-fa2-hovered-node)
                (setq graph-fa2-hovered-node node)
                (let* ((inhibit-read-only t)
                       (overlays (overlays-in (point-min) (point-max)))
                       (ov (seq-find (lambda (o) (eq (overlay-get o 'window) window)) overlays)))
                  (if ov
                      (overlay-put ov 'pointer (if node 'hand nil))
                    (if node
                        (put-text-property (point-min) (point-max) 'pointer 'hand)
                      (put-text-property (point-min) (point-max) 'pointer nil))))
                (run-hook-with-args 'graph-fa2-node-hovered-functions node)))))))))

(defun graph-fa2-mouse-down (event)
  "Handle mouse button press to start panning the viewport or moving a node.

This function extracts the clicked coordinates and checks if a node is
located under the cursor. If a node is found, initialise a node-move drag
context storing its ID and starting mouse coordinates. If no node is found,
initialise a panning drag context storing the starting mouse coordinates."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (window (posn-window posn)))
    (if (not (eq window (selected-window)))
        (select-window window)
      (when-let* (((window-live-p window)))
        (with-current-buffer (window-buffer window)
          (let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
                 (is-playing (or graph-fa2--player-timer
                                 (buffer-local-value 'graph-fa2--player-timer base-buf))))
            (when-let* ((coords (posn-object-x-y posn))
                        (size (posn-object-width-height posn))
                        (mouse-x (float (car coords)))
                        (mouse-y (float (cdr coords)))
                        (img-w (max 1.0 (float (car size))))
                        (img-h (max 1.0 (float (cdr size)))))
              (if is-playing
                  (when-let* ((node (graph-fa2-node-at-scaled-pos mouse-x mouse-y img-w img-h)))
                    (setq graph-fa2--drag-context
                          (list (cons 'type 'click-only)
                                (cons 'node-id node))))
                (let ((ctx (graph-fa2--discover-context base-buf)))
                  (when ctx
                    (graph-fa2--sync-physics ctx graph-fa2--active-hitboxes))
                  (if-let* ((node (graph-fa2-node-at-scaled-pos mouse-x mouse-y img-w img-h))
                            (hitbox (seq-find (lambda (hb) (equal (aref hb 0) node)) graph-fa2--active-hitboxes)))
                      (setq graph-fa2--drag-context
                            (list (cons 'type 'node-move)
                                  (cons 'start-mouse-x mouse-x)
                                  (cons 'start-mouse-y mouse-y)
                                  (cons 'last-mouse-x mouse-x)
                                  (cons 'last-mouse-y mouse-y)
                                  (cons 'img-width img-w)
                                  (cons 'img-height img-h)
                                  (cons 'node-id node)
                                  (cons 'orig-x (aref hitbox 1))
                                  (cons 'orig-y (aref hitbox 2))
                                  (cons 'orig-z (if (> (length hitbox) 4) (aref hitbox 4) 0.0))))
                    (if (eq graph-fa2-engine '2d)
                        (graph-fa2--2d-mouse-down-pan mouse-x mouse-y img-w img-h)
                      (setq graph-fa2--drag-context
                            (list (cons 'type 'pan)
                                  (cons 'start-mouse-x mouse-x)
                                  (cons 'start-mouse-y mouse-y)
                                  (cons 'last-mouse-x mouse-x)
                                  (cons 'last-mouse-y mouse-y)
                                  (cons 'img-width img-w)
                                  (cons 'img-height img-h))))))))))))))

(defun graph-fa2--render-current-to-svg (ctx)
  "Render the current state of nodes and edges in CTX to an SVG string."
  (let ((len (length (graph-fa2-ctx-nodes ctx)))
        (original-buffer (graph-fa2-ctx-bg-buffer ctx)))
    (with-temp-buffer
      (unwind-protect
          (progn
            (setf (graph-fa2-ctx-bg-buffer ctx) (current-buffer))
            (graph-fa2--render-svg ctx len)
            (goto-char (point-min))
            (if-let* ((end (search-forward "<FRAME_SPLIT>\n" nil t)))
                (buffer-substring-no-properties (point-min) (match-beginning 0))
              (buffer-substring-no-properties (point-min) (point-max))))
        (setf (graph-fa2-ctx-bg-buffer ctx) original-buffer)))))

(defun graph-fa2--discover-context (base-buf)
  "Locate and return the active physics simulation context.
Iterates through local variables of BASE-BUF and its associated
playback buffer to find the physics context struct."
  (let* ((pb (buffer-local-value 'graph-fa2-playback-buffer base-buf))
         (ctx nil))
    (dolist (var (buffer-local-variables base-buf))
      (when (and (consp var) (graph-fa2-ctx-p (cdr var)))
        (setq ctx (cdr var))))
    (when (and (not ctx) (buffer-live-p pb))
      (dolist (var (buffer-local-variables pb))
        (when (and (consp var) (graph-fa2-ctx-p (cdr var)))
          (setq ctx (cdr var)))))
    ctx))

(defun graph-fa2--sync-physics (ctx hitboxes)
  "Synchronise physics arrays with visual hitboxes.

Halts all ongoing momentum and maps the current visual coordinates
from the SVG canvas back to the physics simulation state."
  (let* ((nodes (graph-fa2-ctx-nodes ctx))
         (len (length nodes))
         (pos-x (graph-fa2-ctx-pos-x ctx))
         (pos-y (graph-fa2-ctx-pos-y ctx))
         (pos-z (graph-fa2-ctx-pos-z ctx))
         (vel-x (graph-fa2-ctx-vel-x ctx))
         (vel-y (graph-fa2-ctx-vel-y ctx))
         (vel-z (graph-fa2-ctx-vel-z ctx))
         (hitbox-map (make-hash-table :test 'equal)))
    (fillarray vel-x 0)
    (fillarray vel-y 0)
    (fillarray vel-z 0)
    (seq-doseq (hb hitboxes)
      (puthash (aref hb 0) hb hitbox-map))
    (dotimes (i len)
      (let* ((n (aref nodes i))
             (n-id (fa2-id n)))
        (when-let* ((hitbox (gethash n-id hitbox-map))
                    (depth (if (> (length hitbox) 4) (aref hitbox 4) 0.0))
                    (hb-coords (graph-fa2-denormalise-coords (aref hitbox 1) (aref hitbox 2) depth))
                    (internal-x (car hb-coords))
                    (internal-y (cadr hb-coords))
                    (internal-z (caddr hb-coords)))
          (aset pos-x i internal-x)
          (aset pos-y i internal-y)
          (aset pos-z i internal-z))))))

(defun graph-fa2--init-background-worker (ctx pb base-buf)
  "Initialise a purely in-memory background worker for physics calculation.
Erases relevant buffers, resets tracking variables, ticks the physics
synchronously for one immediate frame, triggers a hot reload, and
schedules the continuous cooperative rendering chunk timer."
  (unless (buffer-live-p (graph-fa2-ctx-bg-buffer ctx))
    (setf (graph-fa2-ctx-bg-buffer ctx) (generate-new-buffer " *graph-fa2-bg*")))
  (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
    (erase-buffer))
  (when (buffer-live-p pb)
    (with-current-buffer pb
      (erase-buffer)))
  (with-current-buffer base-buf
    (setq graph-fa2--current-frame 0)
    (setq graph-fa2--frame-offsets nil))
  (setf (graph-fa2-ctx-playback-started ctx) t)
  (when (graph-fa2-ctx-bg-timer ctx)
    (cancel-timer (graph-fa2-ctx-bg-timer ctx)))
  
  (setf (graph-fa2-ctx-bg-frame ctx) 100)
  (graph-fa2--physics-tick ctx 100)
  (setf (graph-fa2-ctx-frames-rendered ctx) 101)
  
  (graph-fa2--hot-reload-player base-buf (graph-fa2-ctx-bg-buffer ctx))
  (setf (graph-fa2-ctx-bg-timer ctx)
        (run-at-time 0 nil #'graph-fa2--render-chunk 
                     ctx nil nil nil 
                     base-buf 250 graph-fa2-framerate)))

(defun graph-fa2-mouse-up (event)
  "Handle mouse release to end dragging or trigger a node click function.
If the distance calculation confirms a click, execute registered hook functions.
If it confirms a drag, discover the active physics context, synchronise
the canvas coordinates with the simulation, and respawn the worker."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (window (posn-window posn))
              ((eq window (selected-window)))
              (drag-ctx graph-fa2--drag-context))
    (setq graph-fa2--drag-context nil)
    (graph-fa2--update-display)
    (when-let* ((type (cdr (assoc 'type drag-ctx))))
      (cond
       ((eq type 'click-only)
        (run-hook-with-args 'graph-fa2-node-clicked-functions (cdr (assoc 'node-id drag-ctx))))
       
       ((eq type 'node-move)
        (when-let* ((node-id (cdr (assoc 'node-id drag-ctx)))
                    (start-x (cdr (assoc 'start-mouse-x drag-ctx)))
                    (start-y (cdr (assoc 'start-mouse-y drag-ctx)))
                    (last-x (cdr (assoc 'last-mouse-x drag-ctx)))
                    (last-y (cdr (assoc 'last-mouse-y drag-ctx)))
                    (dx (- last-x start-x))
                    (dy (- last-y start-y)))
          (if (< (+ (* dx dx) (* dy dy)) 4.0)
              (run-hook-with-args 'graph-fa2-node-clicked-functions node-id)
            (when-let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
                        (pb (buffer-local-value 'graph-fa2-playback-buffer base-buf))
                        (ctx (graph-fa2--discover-context base-buf))
                        ((buffer-live-p pb)))
              (graph-fa2--sync-physics ctx graph-fa2--active-hitboxes)
              (graph-fa2--init-background-worker ctx pb base-buf))))))))
  (when graph-fa2--drag-context
    (setq graph-fa2--drag-context nil)))

(defun graph-fa2-node-at-scaled-pos (active-x active-y img-w img-h)
  "Extract the closest node identifier at the given coordinates.

This function translates the screen pixel coordinates to SVG coordinates,
accounting for zoom and panning, and searches the active hitboxes vector
for the node closest to the cursor."
  (when (and graph-fa2-current-svg
             (not (equal graph-fa2-current-svg graph-fa2--hitbox-svg-string)))
    (let ((hitboxes nil)
          (start 0))
      (while (string-match "<circle cx=\"\\([0-9.-]+\\)\" cy=\"\\([0-9.-]+\\)\" r=\"\\([0-9.-]+\\)\"[^>]*data-name=\"\\([^\"]+\\)\"\\(?:[^>]*data-depth=\"\\([0-9.-]+\\)\"\\)?" graph-fa2-current-svg start)
        (when-let* ((cx (string-to-number (match-string 1 graph-fa2-current-svg)))
                    (cy (string-to-number (match-string 2 graph-fa2-current-svg)))
                    (r (string-to-number (match-string 3 graph-fa2-current-svg)))
                    (id (graph-fa2--unescape-xml (match-string 4 graph-fa2-current-svg)))
                    (depth-str (match-string 5 graph-fa2-current-svg))
                    (depth (if depth-str (string-to-number depth-str) 0.0)))
          (push (vector id cx cy r depth) hitboxes))
        (setq start (match-end 0)))
      (setq graph-fa2--active-hitboxes (vconcat (nreverse hitboxes)))
      (setq graph-fa2--hitbox-svg-string graph-fa2-current-svg)))
  (let* ((min-dim (min img-w img-h))
         (pad-x (/ (- img-w min-dim) 2.0))
         (pad-y (/ (- img-h min-dim) 2.0))
         (adj-x (- active-x pad-x))
         (adj-y (- active-y pad-y)))
    (when (and (>= adj-x 0) (<= adj-x min-dim)
               (>= adj-y 0) (<= adj-y min-dim))
      (let* ((viewbox-dim (/ graph-fa2-canvas-size graph-fa2--scale))
             (viewbox-x (- (- (/ graph-fa2-canvas-size 2.0) graph-fa2--pan-x) (/ viewbox-dim 2.0)))
             (viewbox-y (- (- (/ graph-fa2-canvas-size 2.0) graph-fa2--pan-y) (/ viewbox-dim 2.0)))
             (viewbox-scale (/ viewbox-dim min-dim))
             (mouse-x (+ viewbox-x (* adj-x viewbox-scale)))
             (mouse-y (+ viewbox-y (* adj-y viewbox-scale)))
             (nodes graph-fa2--active-hitboxes)
             (len (if nodes (length nodes) 0))
             (closest-node nil)
             (min-dist-sq 900.0))
        (dotimes (i len)
          (let* ((n (aref nodes i))
                 (nx (aref n 1))
                 (ny (aref n 2))
                 (dx (- mouse-x nx))
                 (dy (- mouse-y ny))
                 (dist-sq (+ (* dx dx) (* dy dy))))
            (when (< dist-sq min-dist-sq)
              (setq min-dist-sq dist-sq)
              (setq closest-node (aref n 0)))))
        closest-node))))

(defun graph-fa2--escape-xml (str)
  "Escape XML characters in STR."
  (let ((s (replace-regexp-in-string "&" "&amp;" str t t)))
    (setq s (replace-regexp-in-string "<" "&lt;" s t t))
    (setq s (replace-regexp-in-string ">" "&gt;" s t t))
    (setq s (replace-regexp-in-string "\"" "&quot;" s t t))
    s))

(defun graph-fa2--unescape-xml (str)
  "Restore standard characters from XML-escaped node names.
This is the inverse of the XML escape function."
  (let ((s (replace-regexp-in-string "&quot;" "\"" str t t)))
    (setq s (replace-regexp-in-string "&gt;" ">" s t t))
    (setq s (replace-regexp-in-string "&lt;" "<" s t t))
    (setq s (replace-regexp-in-string "&amp;" "&" s t t))
    s))

(defun graph-fa2--hash-pos (str offset)
  "Return a pseudo-random number between -500 and 500 based on STR and OFFSET."
  (if (and (boundp 'graph-fa2-deterministic-positions) graph-fa2-deterministic-positions)
      (- (mod (string-to-number (substring (secure-hash 'md5 (concat str offset)) 0 8) 16) 1000) 500.0)
    (- (random 1000) 500.0)))

(defun graph-fa2--create-ctx (nodes edges)
  "Create and initialise a graph-fa2-ctx struct from generic NODES and EDGES.
This pre-allocates the nine physics vectors for 3D simulation to completely
eliminate garbage collection pressure during background rendering."
  (let ((degree-map (make-hash-table :test #'equal)))
    (seq-doseq (edge edges)
      (let ((src (car edge))
            (tgt (cdr edge)))
        (puthash src (1+ (gethash src degree-map 0)) degree-map)
        (puthash tgt (1+ (gethash tgt degree-map 0)) degree-map)))
    (let* ((id-to-idx (make-hash-table :test #'equal))
           (len (length nodes))
           (internal-nodes (make-vector len nil))
           (idx 0))
      (seq-doseq (n nodes)
        (let* ((id (plist-get n :id))
               (label (plist-get n :label))
               (colour (or (plist-get n :colour) (plist-get n :color) "#89b4fa"))
               (radius (or (plist-get n :radius) 10.0))
               (mass (+ 1 (gethash id degree-map 0)))
               (x (truncate (* (graph-fa2--hash-pos id "x") 256.0)))
               (y (truncate (* (graph-fa2--hash-pos id "y") 256.0)))
               (z (if (eq graph-fa2-engine '2d) 0 (truncate (* (graph-fa2--hash-pos id "z") 256.0)))))
          (puthash id idx id-to-idx)
          (aset internal-nodes idx (vector id label x y z 0 0 0 mass colour radius))
          (cl-incf idx)))
      (let (internal-edges)
        (seq-doseq (edge edges)
          (let* ((src (car edge))
                 (tgt (cdr edge))
                 (s-idx (gethash src id-to-idx))
                 (t-idx (gethash tgt id-to-idx)))
            (when (and s-idx t-idx)
              (push (cons s-idx t-idx) internal-edges))))
        (let* ((matrix (make-vector (* len len) 0)))
          (dotimes (i len)
            (let ((ni (aref internal-nodes i)))
              (dotimes (j len)
                (when (> j i)
                  (let ((nj (aref internal-nodes j)))
                    (aset matrix (+ (* i len) j)
                          (truncate (* 50.0 (fa2-mass ni) (fa2-mass nj)))))))))
          (let* ((pos-x (make-vector len 0))
                 (pos-y (make-vector len 0))
                 (pos-z (make-vector len 0))
                 (vel-x (make-vector len 0))
                 (vel-y (make-vector len 0))
                 (vel-z (make-vector len 0))
                 (rep-x (make-vector len 0))
                 (rep-y (make-vector len 0))
                 (rep-z (make-vector len 0)))
            (dotimes (i len)
              (let ((n (aref internal-nodes i)))
                (aset pos-x i (fa2-x n))
                (aset pos-y i (fa2-y n))
                (aset pos-z i (fa2-z n))))
            (make-graph-fa2-ctx
             :nodes internal-nodes
             :edges (nreverse internal-edges)
             :mass-matrix matrix
             :pos-x pos-x
             :pos-y pos-y
             :pos-z pos-z
             :vel-x vel-x
             :vel-y vel-y
             :vel-z vel-z
             :rep-x rep-x
             :rep-y rep-y
             :rep-z rep-z
             :bg-frame 0
             :frames-rendered 0
             :heavy-frames 0
             :heavy-time 0.0
             :playback-started nil
             :start-time (current-time))))))))

(defun graph-fa2--wrap-text (text max-chars)
  "Wrap TEXT to lines of at most MAX-CHARS."
  (let ((words (split-string text " "))
        (lines nil)
        (current-line ""))
    (dolist (word words)
      (if (string= current-line "")
          (setq current-line word)
        (if (<= (+ (length current-line) 1 (length word)) max-chars)
            (setq current-line (concat current-line " " word))
          (push current-line lines)
          (setq current-line word))))
    (when (not (string= current-line ""))
      (push current-line lines))
    (nreverse lines)))

(defun graph-fa2--render-empty (ctx)
  "Render zero-node SVG contents."
  (let ((gc-cons-threshold most-positive-fixnum))
    (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
      (insert "<FRAME_SPLIT>\n"))))

(defun graph-fa2--compute-repulsion-3d (ctx len a)
  "Compute 3D repulsion between all active node pairs.

Uses fixed-point integer approximations and enforces minimum distance
floors to prevent division by zero or coordinate overflows."
  (let* ((pos-x (graph-fa2-ctx-pos-x ctx))
         (pos-y (graph-fa2-ctx-pos-y ctx))
         (pos-z (graph-fa2-ctx-pos-z ctx))
         (rep-x (graph-fa2-ctx-rep-x ctx))
         (rep-y (graph-fa2-ctx-rep-y ctx))
         (rep-z (graph-fa2-ctx-rep-z ctx))
         (mass-matrix (graph-fa2-ctx-mass-matrix ctx))
         (total-nodes (length (graph-fa2-ctx-nodes ctx))))
    (fillarray rep-x 0)
    (fillarray rep-y 0)
    (fillarray rep-z 0)
    (let ((gc-cons-threshold most-positive-fixnum))
      (dotimes (i len)
        (let ((nix (aref pos-x i))
              (niy (aref pos-y i))
              (niz (aref pos-z i))
              (i-offset (* i total-nodes)))
          (cl-loop for j from (1+ i) below len do
                   (when-let* ((dx (- nix (aref pos-x j)))
                               (dy (- niy (aref pos-y j)))
                               (dz (- niz (aref pos-z j)))
                               (dist (max 16 (graph-fa2--dist-3d dx dy dz)))
                               (raw-dist-sq (+ (* dx dx) (* dy dy) (* dz dz)))
                               (dist-sq (max 256 (if (< raw-dist-sq graph-fa2-repulsion-threshold) graph-fa2-repulsion-threshold raw-dist-sq)))
                               ((< dist-sq graph-fa2-repulsion-max-dist-sq))
                               (mass-mult (truncate (aref mass-matrix (+ i-offset j))))
                               (num (ash (truncate (* a mass-mult)) 16))
                               (den (max 256 (* dist dist-sq)))
                               (fdx (/ (* dx num) den))
                               (fdy (/ (* dy num) den))
                               (fdz (/ (* dz num) den)))
                     (aset rep-x i (+ (aref rep-x i) fdx))
                     (aset rep-y i (+ (aref rep-y i) fdy))
                     (aset rep-z i (+ (aref rep-z i) fdz))
                     (aset rep-x j (- (aref rep-x j) fdx))
                     (aset rep-y j (- (aref rep-y j) fdy))
                     (aset rep-z j (- (aref rep-z j) fdz)))))))))

(defun graph-fa2--apply-repulsion-3d (ctx len)
  "Add the accumulated 3D repulsion forces."
  (let ((vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx))
        (rep-x (graph-fa2-ctx-rep-x ctx))
        (rep-y (graph-fa2-ctx-rep-y ctx))
        (rep-z (graph-fa2-ctx-rep-z ctx)))
    (dotimes (i len)
      (aset vel-x i (+ (aref vel-x i) (aref rep-x i)))
      (aset vel-y i (+ (aref vel-y i) (aref rep-y i)))
      (aset vel-z i (+ (aref vel-z i) (aref rep-z i))))))

(defun graph-fa2--apply-attraction-3d (ctx len a)
  "Calculate and apply edge-based 3D attraction forces."
  (let ((pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (pos-z (graph-fa2-ctx-pos-z ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx))
        (edges (graph-fa2-ctx-edges ctx)))
    (dolist (edge edges)
      (when (and (< (car edge) len) (< (cdr edge) len))
        (let* ((u (car edge))
               (v (cdr edge))
               (dx (- (aref pos-x u) (aref pos-x v)))
               (dy (- (aref pos-y u) (aref pos-y v)))
               (dz (- (aref pos-z u) (aref pos-z v)))
               (dist (graph-fa2--dist-3d dx dy dz))
               (dist-diff (- dist graph-fa2-attraction-threshold))
               (num (* a dist-diff))
               (den (ash dist 16))
               (fdx (/ (* dx num) den))
               (fdy (/ (* dy num) den))
               (fdz (/ (* dz num) den)))
          (aset vel-x u (- (aref vel-x u) fdx))
          (aset vel-y u (- (aref vel-y u) fdy))
          (aset vel-z u (- (aref vel-z u) fdz))
          (aset vel-x v (+ (aref vel-x v) fdx))
          (aset vel-y v (+ (aref vel-y v) fdy))
          (aset vel-z v (+ (aref vel-z v) fdz)))))))

(defun graph-fa2--integrate-and-cull-3d (ctx len a progress-fp)
  "Process gravity, enforce speed limits, integrate positions, and manage spherical boundaries.

This function uses speed limits and a hard terminal velocity clamp to ensure
simulation stability and avoid coordinate integer overflows."
  (let ((pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (pos-z (graph-fa2-ctx-pos-z ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx))
        (nodes (graph-fa2-ctx-nodes ctx)))
    (dotimes (i len)
      (let* ((nx (aref pos-x i))
             (ny (aref pos-y i))
             (nz (aref pos-z i))
             (dist (max 1 (graph-fa2--dist-3d nx ny nz)))
             (mass (truncate (fa2-mass (aref nodes i))))
             (num (* a mass))
             (den (max 1 (ash dist 8)))
             (fdx (/ (* nx num) den))
             (fdy (/ (* ny num) den))
             (fdz (/ (* nz num) den)))
        (aset vel-x i (- (aref vel-x i) fdx))
        (aset vel-y i (- (aref vel-y i) fdy))
        (aset vel-z i (- (aref vel-z i) fdz))
        (let* ((vx (graph-fa2-clamp (aref vel-x i) 131072))
               (vy (graph-fa2-clamp (aref vel-y i) 131072))
               (vz (graph-fa2-clamp (aref vel-z i) 131072))
               (speed (graph-fa2--dist-3d vx vy vz))
               (speed-limit (truncate graph-fa2-speed-limit-threshold)))
          (if (> speed speed-limit)
              (progn
                (aset vel-x i (/ (* vx speed-limit) (+ speed speed-limit)))
                (aset vel-y i (/ (* vy speed-limit) (+ speed speed-limit)))
                (aset vel-z i (/ (* vz speed-limit) (+ speed speed-limit))))
            (aset vel-x i vx)
            (aset vel-y i vy)
            (aset vel-z i vz)))
        (let* ((vx-disp (graph-fa2-clamp (ash (aref vel-x i) -4) graph-fa2-max-velocity))
               (vy-disp (graph-fa2-clamp (ash (aref vel-y i) -4) graph-fa2-max-velocity))
               (vz-disp (graph-fa2-clamp (ash (aref vel-z i) -4) graph-fa2-max-velocity)))
          (aset pos-x i (+ nx vx-disp))
          (aset pos-y i (+ ny vy-disp))
          (aset pos-z i (+ nz vz-disp)))
        (let* ((horizon (truncate graph-fa2-horizon-threshold))
               (horizon-start (truncate graph-fa2-horizon-start-threshold))
               (new-nx (aref pos-x i))
               (new-ny (aref pos-y i))
               (new-nz (aref pos-z i))
               (new-dist (graph-fa2--dist-3d new-nx new-ny new-nz))
               (scaled-radius (ash (truncate graph-fa2-surface-radius) 8))
               (target-dist new-dist))
          (cond
           ((eq graph-fa2-surface-constraint 'strict)
            (setq target-dist scaled-radius))
           ((eq graph-fa2-surface-constraint 'floor)
            (when (< new-dist scaled-radius)
              (setq target-dist scaled-radius)))
           ((eq graph-fa2-surface-constraint 'ceiling)
            (when (> new-dist scaled-radius)
              (setq target-dist scaled-radius)))
           ((eq graph-fa2-surface-constraint 'anneal)
            (let ((inv-progress (- 256 progress-fp)))
              (setq target-dist (ash (+ (* new-dist inv-progress) (* scaled-radius progress-fp)) -8)))))
          (when (and (> new-dist 0) (not (= target-dist new-dist)))
            (let ((clamp-scale (/ (ash target-dist 16) new-dist)))
              (aset pos-x i (ash (* new-nx clamp-scale) -16))
              (aset pos-y i (ash (* new-ny clamp-scale) -16))
              (aset pos-z i (ash (* new-nz clamp-scale) -16))
              (setq new-dist target-dist)))
          (when (= new-dist 0)
            (aset pos-x i target-dist)
            (setq new-dist target-dist))
          (when (> new-dist horizon)
            (let ((clamp-scale (/ (ash horizon 16) new-dist)))
              (aset pos-x i (ash (* (aref pos-x i) clamp-scale) -16))
              (aset pos-y i (ash (* (aref pos-y i) clamp-scale) -16))
              (aset pos-z i (ash (* (aref pos-z i) clamp-scale) -16))
              (setq new-dist horizon)))
          (cond
           ((>= new-dist horizon)
            (aset vel-x i 0)
            (aset vel-y i 0)
            (aset vel-z i 0))
           ((> new-dist horizon-start)
            (aset vel-x i (- (aref vel-x i) (ash (aref vel-x i) -2)))
            (aset vel-y i (- (aref vel-y i) (ash (aref vel-y i) -2)))
            (aset vel-z i (- (aref vel-z i) (ash (aref vel-z i) -2))))
           (t
            (aset vel-x i (- (aref vel-x i) (ash (aref vel-x i) -6)))
            (aset vel-y i (- (aref vel-y i) (ash (aref vel-y i) -6)))
            (aset vel-z i (- (aref vel-z i) (ash (aref vel-z i) -6))))))))))

(defun graph-fa2--sync-nodes (ctx total-nodes)
  "Sync tracking arrays with internal node structs."
  (let ((nodes (graph-fa2-ctx-nodes ctx))
        (pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (pos-z (graph-fa2-ctx-pos-z ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx)))
    (dotimes (i total-nodes)
      (let ((n (aref nodes i)))
        (fa2-set-x n (aref pos-x i))
        (fa2-set-y n (aref pos-y i))
        (fa2-set-z n (aref pos-z i))
        (fa2-set-dx n (aref vel-x i))
        (fa2-set-dy n (aref vel-y i))
        (fa2-set-dz n (aref vel-z i))))))

(defun graph-fa2--render-svg (ctx len)
  "Render the current layout integer arrays to an SVG string using a 2D projection.

This function uses coordinate normalisation to translate high-precision
internal coordinates to external canvas coordinates for drawing, and formats
all floats to standard decimal format to prevent scientific notation crash."
  (when-let* ((nodes (graph-fa2-ctx-nodes ctx))
              (edges (graph-fa2-ctx-edges ctx))
              (pos-x (graph-fa2-ctx-pos-x ctx))
              (pos-y (graph-fa2-ctx-pos-y ctx))
              (pos-z (graph-fa2-ctx-pos-z ctx)))
    (let ((gc-cons-threshold most-positive-fixnum))
      (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
        (dolist (edge edges)
          (when-let* (((< (car edge) len))
                      ((< (cdr edge) len))
                      (u-idx (car edge))
                      (v-idx (cdr edge))
                      (u-coords (graph-fa2-normalise-coords (aref pos-x u-idx) (aref pos-y u-idx) (aref pos-z u-idx)))
                      (v-coords (graph-fa2-normalise-coords (aref pos-x v-idx) (aref pos-y v-idx) (aref pos-z v-idx)))
                      (ux (format "%.3f" (car u-coords)))
                      (uy (format "%.3f" (cadr u-coords)))
                      (vx (format "%.3f" (car v-coords)))
                      (vy (format "%.3f" (cadr v-coords))))
            (insert "  <line x1=\"" ux "\" y1=\"" uy "\" x2=\"" vx "\" y2=\"" vy "\" stroke=\"" graph-fa2-edge-colour "\" stroke-width=\"" (number-to-string graph-fa2-edge-width) "\" />\n")))
        (dotimes (i len)
          (when-let* ((n (aref nodes i))
                      (coords (graph-fa2-normalise-coords (aref pos-x i) (aref pos-y i) (aref pos-z i)))
                      (nx-float (car coords))
                      (ny-float (cadr coords))
                      (nz-float (caddr coords))
                      (nx (format "%.3f" nx-float))
                      (ny (format "%.3f" ny-float))
                      (nz (format "%.3f" nz-float))
                      (id (fa2-id n))
                      (label (fa2-label n))
                      (radius (fa2-radius n))
                      (colour (fa2-colour n))
                      (name-escaped (graph-fa2--escape-xml label))
                      (lines (graph-fa2--wrap-text name-escaped graph-fa2-label-wrap-chars))
                      (line-height (max 1 (round (* graph-fa2-label-font-size 1.2))))
                      (start-y (- ny-float graph-fa2-label-offset-y (* (1- (length lines)) (/ line-height 2)))))
            (insert "  <circle cx=\"" nx "\" cy=\"" ny "\" r=\"" (format "%.3f" radius) "\" fill=\"" colour "\" data-name=\"" (graph-fa2--escape-xml id) "\" data-depth=\"" nz "\" />\n")
            (insert "  <text fill=\"" graph-fa2-label-colour
                    "\" font-size=\"" (number-to-string graph-fa2-label-font-size) "\""
                    (if graph-fa2-label-font-family
                        (concat " font-family=\"" graph-fa2-label-font-family "\"") "")
                    (if graph-fa2-label-font-weight
                        (concat " font-weight=\"" graph-fa2-label-font-weight "\"") "")
                    " text-anchor=\"middle\">\n")
            (let ((curr-y start-y))
              (dolist (line lines)
                (insert "    <tspan x=\"" nx "\" y=\"" (format "%.3f" curr-y) "\">" line "</tspan>\n")
                (cl-incf curr-y line-height)))
            (insert "  </text>\n")))
        (insert "<FRAME_SPLIT>\n")))))

(defun graph-fa2--2d-compute-repulsion (ctx len a)
  "Compute 2D repulsion between all active node pairs.

Parameters:
CTX: The simulation context.
LEN: The number of active nodes.
A: The repulsion scaling parameter."
  (let* ((pos-x (graph-fa2-ctx-pos-x ctx))
         (pos-y (graph-fa2-ctx-pos-y ctx))
         (rep-x (graph-fa2-ctx-rep-x ctx))
         (rep-y (graph-fa2-ctx-rep-y ctx))
         (mass-matrix (graph-fa2-ctx-mass-matrix ctx))
         (total-nodes (length (graph-fa2-ctx-nodes ctx))))
    (fillarray rep-x 0)
    (fillarray rep-y 0)
    (let ((gc-cons-threshold most-positive-fixnum))
      (dotimes (i len)
        (let ((nix (aref pos-x i))
              (niy (aref pos-y i))
              (i-offset (* i total-nodes)))
          (cl-loop for j from (1+ i) below len do
                   (let* ((dx (- nix (aref pos-x j)))
                          (abs-dx (if (< dx 0) (- dx) dx)))
                     (when (< abs-dx graph-fa2-repulsion-x-y-threshold)
                       (let* ((dy (- niy (aref pos-y j)))
                              (abs-dy (if (< dy 0) (- dy) dy)))
                         (when (< abs-dy graph-fa2-repulsion-x-y-threshold)
                           (let* ((max-d (if (> abs-dx abs-dy) abs-dx abs-dy))
                                  (min-d (if (> abs-dx abs-dy) abs-dy abs-dx))
                                  (dist (if (= max-d 0) 1 (+ max-d (ash (truncate min-d) -1))))
                                  (dist-sq (+ (* dx dx) (* dy dy)))
                                  (dist-sq (if (< dist-sq graph-fa2-repulsion-threshold) graph-fa2-repulsion-threshold dist-sq)))
                             (when (< dist-sq graph-fa2-repulsion-max-dist-sq)
                               (let* ((mass-mult (truncate (aref mass-matrix (+ i-offset j))))
                                      (num (ash (truncate (* a mass-mult)) 16))
                                      (den (* dist dist-sq))
                                      (fdx (/ (* dx num) den))
                                      (fdy (/ (* dy num) den)))
                                 (aset rep-x i (+ (aref rep-x i) fdx))
                                 (aset rep-y i (+ (aref rep-y i) fdy))
                                 (aset rep-x j (- (aref rep-x j) fdx))
                                 (aset rep-y j (- (aref rep-y j) fdy)))))))))))))))

(defun graph-fa2--2d-apply-repulsion (ctx len)
  "Add the accumulated 2D repulsion forces.

Parameters:
CTX: The simulation context.
LEN: The number of active nodes."
  (let ((vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (rep-x (graph-fa2-ctx-rep-x ctx))
        (rep-y (graph-fa2-ctx-rep-y ctx)))
    (dotimes (i len)
      (aset vel-x i (+ (aref vel-x i) (aref rep-x i)))
      (aset vel-y i (+ (aref vel-y i) (aref rep-y i))))))

(defun graph-fa2--2d-apply-attraction (ctx len a)
  "Calculate and apply edge-based 2D attraction forces.

Parameters:
CTX: The simulation context.
LEN: The number of active nodes.
A: The attraction scaling parameter."
  (let ((pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (edges (graph-fa2-ctx-edges ctx)))
    (dolist (edge edges)
      (when (and (< (car edge) len) (< (cdr edge) len))
        (let* ((u (car edge))
               (v (cdr edge))
               (dx (- (aref pos-x u) (aref pos-x v)))
               (dy (- (aref pos-y u) (aref pos-y v)))
               (abs-dx (if (< dx 0) (- dx) dx))
               (abs-dy (if (< dy 0) (- dy) dy))
               (max-d (if (> abs-dx abs-dy) abs-dx abs-dy))
               (min-d (if (> abs-dx abs-dy) abs-dy abs-dx))
               (dist (if (= max-d 0) 1 (+ max-d (ash (truncate min-d) -1))))
               (dist-diff (- dist graph-fa2-attraction-threshold))
               (num (* a dist-diff))
               (den (ash (truncate dist) 16))
               (fdx (/ (* dx num) den))
               (fdy (/ (* dy num) den)))
          (aset vel-x u (- (aref vel-x u) fdx))
          (aset vel-y u (- (aref vel-y u) fdy))
          (aset vel-x v (+ (aref vel-x v) fdx))
          (aset vel-y v (+ (aref vel-y v) fdy)))))))

(defun graph-fa2--2d-integrate-and-cull (ctx len a)
  "Process gravity, enforce speed limits, integrate positions, and cull nodes in 2D.

Parameters:
CTX: The simulation context.
LEN: The number of active nodes.
A: The integration scaling parameter."
  (let ((pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (nodes (graph-fa2-ctx-nodes ctx)))
    (dotimes (i len)
      (let* ((nx (aref pos-x i))
             (ny (aref pos-y i))
             (abs-nx (if (< nx 0) (- nx) nx))
             (abs-ny (if (< ny 0) (- ny) ny))
             (max-n (if (> abs-nx abs-ny) abs-nx abs-ny))
             (min-n (if (> abs-nx abs-ny) abs-ny abs-nx))
             (dist (if (= max-n 0) 1 (+ max-n (ash (truncate min-n) -1))))
             (mass (truncate (fa2-mass (aref nodes i))))
             (num (* a mass))
             (den (ash (truncate dist) 8))
             (fdx (/ (* nx num) den))
             (fdy (/ (* ny num) den)))
        (aset vel-x i (- (aref vel-x i) fdx))
        (aset vel-y i (- (aref vel-y i) fdy))
        (let* ((vx (aref vel-x i))
               (vy (aref vel-y i))
               (abs-vx (if (< vx 0) (- vx) vx))
               (abs-vy (if (< vy 0) (- vy) vy))
               (max-v (if (> abs-vx abs-vy) abs-vx abs-vy))
               (min-v (if (> abs-vx abs-vy) abs-vy abs-vx))
               (speed (if (= max-v 0) 1 (+ max-v (ash (truncate min-v) -1)))))
          (when (> speed 25)
            (let ((v-max graph-fa2-speed-limit-threshold))
              (aset vel-x i (/ (* (truncate vx) v-max) (+ speed v-max)))
              (aset vel-y i (/ (* (truncate vy) v-max) (+ speed v-max))))))
        (aset pos-x i (+ nx (ash (truncate (aref vel-x i)) -4)))
        (aset pos-y i (+ ny (ash (truncate (aref vel-y i)) -4)))
        (let* ((horizon graph-fa2-horizon-threshold)
               (horizon-start graph-fa2-horizon-start-threshold)
               (new-nx (aref pos-x i))
               (new-ny (aref pos-y i))
               (abs-new-nx (if (< new-nx 0) (- new-nx) new-nx))
               (abs-new-ny (if (< new-ny 0) (- new-ny) new-ny))
               (max-new (if (> abs-new-nx abs-new-ny) abs-new-nx abs-new-ny))
               (min-new (if (> abs-new-nx abs-new-ny) abs-new-ny abs-new-nx))
               (new-dist (if (= max-new 0) 1 (+ max-new (ash (truncate min-new) -1)))))
          (when (> new-dist horizon)
            (let ((clamp-scale (/ (ash horizon 16) new-dist)))
              (aset pos-x i (ash (truncate (* new-nx clamp-scale)) -16))
              (aset pos-y i (ash (truncate (* new-ny clamp-scale)) -16))
              (setq new-dist horizon)))
          (cond
           ((>= new-dist horizon)
            (aset vel-x i 0)
            (aset vel-y i 0))
           ((> new-dist horizon-start)
            (aset vel-x i (- (aref vel-x i) (ash (truncate (aref vel-x i)) -2)))
            (aset vel-y i (- (aref vel-y i) (ash (truncate (aref vel-y i)) -2))))
           (t
            (aset vel-x i (- (aref vel-x i) (ash (truncate (aref vel-x i)) -6)))
            (aset vel-y i (- (aref vel-y i) (ash (truncate (aref vel-y i)) -6))))))))))

(defun graph-fa2--2d-physics-tick (ctx max-frames)
  "Calculate ForceAtlas2 2D physics tick using pre-allocated arrays in CTX.

Evaluate node count and render empty context if devoid of data.
Determine active rendering slice and scale variables based on animation frames.
Delegate to the compute repulsion function to populate spacing arrays.
Run core physics iterations across attraction, integration, and bounds constraints.
Synchronise buffers to state and trigger background rendering.

Parameters:
CTX: The simulation context.
MAX-FRAMES: The simulation frame limit."
  (let* ((nodes (graph-fa2-ctx-nodes ctx))
         (total-nodes (length nodes)))
    (if (= total-nodes 0)
        (graph-fa2--render-empty ctx)
      (let* ((bg-frame (graph-fa2-ctx-bg-frame ctx))
             (len (if (< bg-frame 100)
                      (max 1 (truncate (* total-nodes (/ (float (1+ bg-frame)) 100.0))))
                    total-nodes))
             (a (max 2 (truncate (* 256.0 (- 1.0 (/ (float bg-frame) max-frames)))))))
        (graph-fa2--2d-compute-repulsion ctx len a)
        (let ((gc-cons-threshold most-positive-fixnum))
          (dotimes (_ graph-fa2-substeps)
            (graph-fa2--2d-apply-repulsion ctx len)
            (graph-fa2--2d-apply-attraction ctx len a)
            (graph-fa2--2d-integrate-and-cull ctx len a)))
        (graph-fa2--sync-nodes ctx total-nodes)
        (graph-fa2--render-svg ctx len)))))

(defun graph-fa2--physics-tick (ctx max-frames)
  "Calculate ForceAtlas2 physics tick using pre-allocated arrays in CTX.
Evaluates node count and renders empty context if devoid of data.
Determines active rendering slice and scale variables based on animation frames.
Delegates to the compute repulsion function to populate spacing arrays.
Runs core physics iterations across attraction, integration, and bounds constraints.
Synchronises buffers to state and triggers background rendering."
  (if (eq graph-fa2-engine '2d)
      (graph-fa2--2d-physics-tick ctx max-frames)
    (let* ((nodes (graph-fa2-ctx-nodes ctx))
           (total-nodes (length nodes)))
      (if (= total-nodes 0)
          (graph-fa2--render-empty ctx)
        (let* ((bg-frame (graph-fa2-ctx-bg-frame ctx))
               (len (if (< bg-frame 100)
                        (max 1 (truncate (* total-nodes (/ (float (1+ bg-frame)) 100.0))))
                      total-nodes))
               (a (max 2 (truncate (* 256.0 (- 1.0 (/ (float bg-frame) max-frames))))))
               (progress-fp (min 256 (max 0 (truncate (* 256.0 (/ (float bg-frame) max-frames)))))))
          (graph-fa2--compute-repulsion-3d ctx len a)
          (let ((gc-cons-threshold most-positive-fixnum))
            (dotimes (_ graph-fa2-substeps)
              (graph-fa2--apply-repulsion-3d ctx len)
              (graph-fa2--apply-attraction-3d ctx len a)
              (graph-fa2--integrate-and-cull-3d ctx len a progress-fp)))
          (graph-fa2--sync-nodes ctx total-nodes)
          (graph-fa2--render-svg ctx len))))))

(defun graph-fa2--hot-reload-player (buf bg-buffer)
  "Feed newly rendered frames into the live player without restarting."
  (when (buffer-live-p buf)
    (when-let* ((playback-buf (buffer-local-value 'graph-fa2-playback-buffer buf))
                ((buffer-live-p playback-buf)))
      (let ((bg-size (with-current-buffer bg-buffer (buffer-size)))
            (pb-size (with-current-buffer playback-buf (buffer-size))))
        (when (> bg-size pb-size)
          (with-current-buffer playback-buf
            (let ((inhibit-read-only t)
                  (new-offsets nil)
                  (start-pos (1+ pb-size)))
              (goto-char (point-max))
              (insert-buffer-substring bg-buffer start-pos)
              (goto-char start-pos)
              (let ((start (point)))
                (while (search-forward "<FRAME_SPLIT>\n" nil t)
                  (push (cons start (match-beginning 0)) new-offsets)
                  (when (looking-at "\n") (forward-char 1))
                  (setq start (point))))
              (with-current-buffer buf
                (let ((old-offsets (append graph-fa2--frame-offsets nil)))
                  (setq-local graph-fa2--frame-offsets (vconcat old-offsets (nreverse new-offsets))))
                (unless (and graph-fa2--drag-context
                             (eq (cdr (assoc 'type graph-fa2--drag-context)) 'node-move))
                  (graph-fa2-player-start))))))))))

(defun graph-fa2--load-playback-from-cache (ctx cache-file target-buf)
  "Load CACHE-FILE into a playback buffer, parse frame offsets, and start playback.

This function initialises the playback buffer, reads frame data, determines
the offsets for individual frames, and starts the player in TARGET-BUF.

Parameters:
CTX: The simulation context.
CACHE-FILE: Path to the cache file.
TARGET-BUF: The destination buffer.

Returns:
The total number of loaded frames."
  (let* ((playback-buf (generate-new-buffer " *graph-fa2-playback*"))
         (offsets nil))
    (with-current-buffer playback-buf
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents-literally cache-file))
      (goto-char (point-min))
      (let ((start (point)))
        (while (search-forward "<FRAME_SPLIT>\n" nil t)
          (push (cons start (match-beginning 0)) offsets)
          (when-let* ((is-newline (looking-at "\n")))
            (forward-char 1))
          (setq start (point)))
        (when-let* ((has-more (< start (point-max))))
          (push (cons start (point-max)) offsets))))
    (let* ((offsets-vec (vconcat (nreverse offsets)))
           (num-frames (length offsets-vec)))
      (when-let* ((is-live (buffer-live-p target-buf)))
        (with-current-buffer target-buf
          (setq-local graph-fa2-playback-buffer playback-buf)
          (setq-local graph-fa2--frame-offsets offsets-vec)
          (setq-local graph-fa2--current-frame 0)
          (when-let* ((has-frames (> num-frames 0))
                      (first-bounds (aref offsets-vec 0)))
            (setq-local graph-fa2-current-svg
                        (with-current-buffer playback-buf
                          (buffer-substring-no-properties (car first-bounds) (cdr first-bounds)))))
          (graph-fa2-mode 1)
          (graph-fa2--update-display)
          (message "Graph playback started.")
          (graph-fa2-player-start)))
      num-frames)))

(defun graph-fa2--render-chunk (ctx cache-file hash-file target-hash target-buf max-frames playback-fps)
  "Cooperatively render frames of the simulation and schedule the next chunk.

If CACHE-FILE is nil, disk caching is skipped to keep interactive simulations
purely in memory.

Parameters:
CTX: The simulation context.
CACHE-FILE: Path to the cache output file.
HASH-FILE: Path to the hash state file.
TARGET-HASH: The hash string representing current graph contents.
TARGET-BUF: The destination buffer.
MAX-FRAMES: The simulation frame limit.
PLAYBACK-FPS: Target frames per second.

Returns:
Nil."
  (let* ((cached-hash (when-let* ((has-hash (and hash-file (file-exists-p hash-file))))
                        (with-temp-buffer
                          (let ((coding-system-for-read 'utf-8))
                            (insert-file-contents hash-file))
                          (string-trim (buffer-string)))))
         (has-valid-cache (and cached-hash
                               target-hash
                               (string= target-hash cached-hash)
                               cache-file
                               (file-exists-p cache-file))))
    (if-let* ((valid has-valid-cache))
        (when-let* ((num-frames (graph-fa2--load-playback-from-cache ctx cache-file target-buf)))
          (setq max-frames num-frames)
          (setf (graph-fa2-ctx-frames-rendered ctx) max-frames)
          (setf (graph-fa2-ctx-playback-started ctx) t))
      (graph-fa2--render-chunk-cooperative ctx cache-file hash-file target-hash target-buf max-frames playback-fps))))

(defun graph-fa2--render-chunk-cooperative (ctx cache-file hash-file target-hash target-buf max-frames playback-fps)
  "Render a simulation chunk cooperatively and schedule the subsequent execution.

Parameters:
CTX: The simulation context.
CACHE-FILE: Path to the cache output file.
HASH-FILE: Path to the hash state file.
TARGET-HASH: The hash string representing current graph contents.
TARGET-BUF: The destination buffer.
MAX-FRAMES: The simulation frame limit.
PLAYBACK-FPS: Target frames per second.

Returns:
Nil."
  (cl-symbol-macrolet ((frames-rendered (graph-fa2-ctx-frames-rendered ctx))
                       (playback-started (graph-fa2-ctx-playback-started ctx))
                       (bg-buffer (graph-fa2-ctx-bg-buffer ctx))
                       (heavy-frames (graph-fa2-ctx-heavy-frames ctx))
                       (heavy-time (graph-fa2-ctx-heavy-time ctx))
                       (bg-frame (graph-fa2-ctx-bg-frame ctx))
                       (bg-timer (graph-fa2-ctx-bg-timer ctx)))
    (let* ((chunk-end-time (time-add nil 0.05))
           (slice-start-time (float-time))
           (slice-start-frames frames-rendered)
           (frames-in-slice 0)
           (playback-ms (/ 1.0 playback-fps))
           (gc-cons-threshold most-positive-fixnum))
      (while (and (< frames-rendered max-frames)
                  (time-less-p nil chunk-end-time)
                  (not (input-pending-p)))
        (setf bg-frame frames-rendered)
        (graph-fa2--physics-tick ctx max-frames)
        (setf frames-rendered (1+ frames-rendered))
        (cl-incf frames-in-slice))
      (let* ((slice-duration (* (- (float-time) slice-start-time) 1000.0))
             (valid-frames (max 0 (- frames-rendered (max 100 slice-start-frames)))))
        (when-let* ((has-valid-frames (> valid-frames 0)))
          (setf heavy-frames (+ heavy-frames valid-frames))
          (setf heavy-time (+ heavy-time (* slice-duration (/ (float valid-frames) frames-in-slice)))))
        (let ((cumulative-avg (if (> heavy-frames 0)
                                  (/ heavy-time heavy-frames)
                                0.0)))
          (unless (or playback-started
                      (< frames-rendered 100)
                      (= heavy-frames 0))
            (let* ((tg (/ cumulative-avg 1000.0))
                   (predicted-tg (+ tg 0.020))
                   (safe-buffer
                    (if (<= predicted-tg playback-ms)
                        1
                      (ceiling (* max-frames (/ (- predicted-tg playback-ms) predicted-tg))))))
              (when-let* ((buffer-ready (>= frames-rendered (+ 100 safe-buffer))))
                (setf playback-started t)
                (when-let* ((has-cache cache-file))
                  (with-current-buffer bg-buffer
                    (let ((coding-system-for-write 'utf-8))
                      (write-region (point-min) (point-max) cache-file nil 'silent))))
                (when-let* ((is-live (buffer-live-p target-buf)))
                  (if-let* ((has-cache cache-file))
                      (graph-fa2--load-playback-from-cache ctx cache-file target-buf)
                    (graph-fa2-player-start))))))
          (when-let* ((can-reload (and playback-started (< frames-rendered max-frames))))
            (graph-fa2--hot-reload-player target-buf bg-buffer)))
        (if-let* ((continue-sim (< frames-rendered max-frames)))
            (setf bg-timer
                  (run-at-time 0 nil #'graph-fa2--render-chunk ctx cache-file hash-file target-hash target-buf max-frames playback-fps))
          (when-let* ((has-cache cache-file))
            (with-current-buffer bg-buffer
              (let ((coding-system-for-write 'utf-8))
                (write-region (point-min) (point-max) cache-file nil 'silent))))
          (when-let* ((has-hash hash-file))
            (with-temp-file hash-file (insert target-hash)))
          (when-let* ((need-render (and (buffer-live-p target-buf) (not playback-started))))
            (when-let* ((has-cache cache-file))
              (graph-fa2--render-chunk ctx cache-file hash-file target-hash target-buf max-frames playback-fps))))))))

(defun graph-fa2--zoom-tick (buffer)
  "Apply velocity to scale and redraw. Stop when velocity is near zero."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (< (abs graph-fa2--zoom-velocity) 0.001)
          (progn
            (setq graph-fa2--zoom-velocity 0.0)
            (when graph-fa2--zoom-timer
              (cancel-timer graph-fa2--zoom-timer)
              (setq graph-fa2--zoom-timer nil)))
        
        (setq graph-fa2--scale 
              (max 0.05 (* graph-fa2--scale (+ 1.0 graph-fa2--zoom-velocity))))
        
        (setq graph-fa2--zoom-velocity (* graph-fa2--zoom-velocity graph-fa2-zoom-friction))

        (graph-fa2--update-display)))))

(defun graph-fa2--start-zoom-inertia ()
  "Ensure the zoom timer is running for the current buffer."
  (unless graph-fa2--zoom-timer
    (let ((buf (current-buffer)))
      (setq graph-fa2--zoom-timer
            (run-with-timer 0 0.016 #'graph-fa2--zoom-tick buf)))))

(defun graph-fa2-zoom-in (&optional event)
  "Increase the scale of the rendered graph with momentum."
  (interactive (list last-input-event))
  (let* ((posn (and (listp event) (event-start event)))
         (window (and posn (posn-window posn))))
    (if (and window (window-live-p window) (not (eq window (selected-window))))
        (select-window window)
      (when (eq (current-buffer) (window-buffer (selected-window)))
        (setq graph-fa2--zoom-velocity (+ graph-fa2--zoom-velocity graph-fa2-zoom-acceleration))
        (graph-fa2--start-zoom-inertia)))))

(defun graph-fa2-zoom-out (&optional event)
  "Decrease the scale of the rendered graph with momentum."
  (interactive (list last-input-event))
  (let* ((posn (and (listp event) (event-start event)))
         (window (and posn (posn-window posn))))
    (if (and window (window-live-p window) (not (eq window (selected-window))))
        (select-window window)
      (when (eq (current-buffer) (window-buffer (selected-window)))
        (setq graph-fa2--zoom-velocity (- graph-fa2--zoom-velocity graph-fa2-zoom-acceleration))
        (graph-fa2--start-zoom-inertia)))))

(defun graph-fa2-zoom-reset ()
  "Reset the graph scale and pan offsets to default and kill active momentum."
  (interactive)
  (when (eq (current-buffer) (window-buffer (selected-window)))
    (setq graph-fa2--zoom-velocity 0.0)
    (when graph-fa2--zoom-timer
      (cancel-timer graph-fa2--zoom-timer)
      (setq graph-fa2--zoom-timer nil))
    (setq graph-fa2--scale 1.0)
    (setq graph-fa2--pan-x 0.0)
    (setq graph-fa2--pan-y 0.0)
    (graph-fa2--update-display)))

(defun graph-fa2--grab-inner-elements (svg-string)
  "Extract the inner elements from SVG-STRING.
This removes any outer SVG tags to allow the viewBox attributes to
be added directly during rendering."
  (cond
   ((string-match "<svg[^>]*>" svg-string)
    (let ((start (match-end 0))
          (end (string-match "</svg>" svg-string)))
      (if end
          (substring svg-string start end)
        (substring svg-string start))))
   (t svg-string)))

(defun graph-fa2--update-display (&rest _)
  "Render the current SVG frame into the buffer natively using window-specific overlays.
This function checks for an existing overlay associated with the current window.
If one does not exist, it creates the overlay and restricts its visibility
to that window. This prevents frame lockups when multiple frames view the same buffer."
  (when-let* ((current-svg graph-fa2-current-svg)
              (win (get-buffer-window (current-buffer) t)))
    (let* ((width (max 100 (window-pixel-width win)))
           (height (max 100 (window-pixel-height win)))
           (state (list current-svg graph-fa2--scale width height graph-fa2-hovered-node graph-fa2--pan-x graph-fa2--pan-y)))
      (unless (equal state graph-fa2--render-state)
        (setq graph-fa2--render-state state)
        (let* ((inhibit-read-only t)
               (inner-elements (graph-fa2--grab-inner-elements current-svg))
               (viewbox-dim (/ graph-fa2-canvas-size graph-fa2--scale))
               (viewbox-x (- (- (/ graph-fa2-canvas-size 2.0) graph-fa2--pan-x) (/ viewbox-dim 2.0)))
               (viewbox-y (- (- (/ graph-fa2-canvas-size 2.0) graph-fa2--pan-y) (/ viewbox-dim 2.0)))
               (full-svg (format "<svg width=\"%d\" height=\"%d\" viewBox=\"%.2f %.2f %.2f %.2f\" xmlns=\"http://www.w3.org/2000/svg\" preserveAspectRatio=\"xMidYMid meet\">\n%s\n</svg>"
                                 width height viewbox-x viewbox-y viewbox-dim viewbox-dim inner-elements))
               (encoded-svg (if (multibyte-string-p full-svg)
                                (encode-coding-string full-svg 'utf-8)
                              full-svg)))
          (when (= (buffer-size) 0) (insert " "))
          (let ((overlays (overlays-in (point-min) (point-max))))
            (dolist (o overlays)
              (when (eq (overlay-get o 'window) win)
                (delete-overlay o))))
          (let ((ov (make-overlay (point-min) (point-max))))
            (overlay-put ov 'window win)
            (overlay-put ov 'display (create-image encoded-svg 'svg t))
            (overlay-put ov 'pointer (if graph-fa2-hovered-node 'hand nil)))
          (run-hooks 'graph-fa2-after-render-functions))))))

(defun graph-fa2--player-tick ()
  "Advance the animation frame natively from memory buffers."
  (let ((gc-cons-threshold most-positive-fixnum))
    (when (buffer-live-p (current-buffer))
      (unless (and graph-fa2--drag-context
                   (eq (cdr (assoc 'type graph-fa2--drag-context)) 'node-move))
        (let ((total-frames (or (and graph-fa2--frame-offsets (length graph-fa2--frame-offsets)) 0)))
          (when (> total-frames 0)
            (if (< graph-fa2--current-frame total-frames)
                (progn
                  (when-let* ((bounds (when graph-fa2--frame-offsets 
                                        (aref graph-fa2--frame-offsets graph-fa2--current-frame))))
                    (setq graph-fa2-current-svg
                          (with-current-buffer graph-fa2-playback-buffer
                            (buffer-substring-no-properties (car bounds) (cdr bounds)))))
                  (graph-fa2--update-display)
                  (cl-incf graph-fa2--current-frame))
              (graph-fa2-player-stop))))))))

(defun graph-fa2-player-start ()
  "Starts the animation playback loop if frames are populated."
  (when graph-fa2--frame-offsets
    (unless graph-fa2--player-timer
      (let ((buf (current-buffer)))
        (setq graph-fa2--player-timer 
              (run-with-timer 0 0.016 
                              (lambda ()
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (graph-fa2--player-tick))))))))))

(defun graph-fa2-player-stop ()
  "Halts the animation loop."
  (when graph-fa2--player-timer
    (cancel-timer graph-fa2--player-timer)
    (setq graph-fa2--player-timer nil)))

(defvar graph-fa2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-movement] #'graph-fa2-track-mouse)
    (define-key map (kbd "<mouse-movement>") #'graph-fa2-track-mouse)
    (define-key map [down-mouse-1] #'graph-fa2-mouse-down)
    (define-key map (kbd "<down-mouse-1>") #'graph-fa2-mouse-down)
    (define-key map [drag-mouse-1] #'graph-fa2-mouse-up)
    (define-key map (kbd "<drag-mouse-1>") #'graph-fa2-mouse-up)
    (define-key map [mouse-1] #'graph-fa2-mouse-up)
    (define-key map (kbd "<mouse-1>") #'graph-fa2-mouse-up)
    (define-key map (kbd "+") #'graph-fa2-zoom-in)
    (define-key map (kbd "-") #'graph-fa2-zoom-out)
    (define-key map (kbd "0") #'graph-fa2-zoom-reset)
    (define-key map (kbd "w") #'graph-fa2-open-in-new-window)
    (define-key map (kbd "f") #'graph-fa2-open-in-new-frame)
    (define-key map (kbd "<wheel-up>") #'graph-fa2-zoom-in)
    (define-key map (kbd "<wheel-down>") #'graph-fa2-zoom-out)
    map)
  "Keymap for graph-fa2 minor mode.")

(define-minor-mode graph-fa2-mode
  "Minor mode for viewing and interacting with ForceAtlas2 graphs."
  :lighter " FA2"
  :keymap graph-fa2-mode-map
  (if graph-fa2-mode
      (progn
        (setq-local track-mouse t)
        (add-hook 'window-size-change-functions #'graph-fa2--update-display nil t)
        (add-hook 'window-selection-change-functions #'graph-fa2--cancel-drag nil t)
        (add-hook 'focus-out-hook #'graph-fa2--cancel-drag nil t))
    (progn
      (setq-local track-mouse nil)
      (remove-hook 'window-size-change-functions #'graph-fa2--update-display t)
      (remove-hook 'window-selection-change-functions #'graph-fa2--cancel-drag t)
      (remove-hook 'focus-out-hook #'graph-fa2--cancel-drag t)
      (let ((overlays (overlays-in (point-min) (point-max))))
        (dolist (o overlays)
          (when (overlay-get o 'window)
            (delete-overlay o)))))))

(defun graph-fa2-view-indirect (&optional frame)
  "Spawn an indirect buffer for the current graph and display it.
This ensures each window or frame has its own independent view with its own
zoom scale, pan offsets, and active hitboxes, resolving frame lockups."
  (interactive "P")
  (let* ((base-buf (current-buffer))
         (indirect-name (generate-new-buffer-name (concat (buffer-name base-buf) "-view")))
         (indirect-buf (make-indirect-buffer base-buf indirect-name t)))
    (with-current-buffer indirect-buf
      (graph-fa2-mode 1)
      (setq-local graph-fa2--scale (buffer-local-value 'graph-fa2--scale base-buf))
      (setq-local graph-fa2--pan-x (buffer-local-value 'graph-fa2--pan-x base-buf))
      (setq-local graph-fa2--pan-y (buffer-local-value 'graph-fa2--pan-y base-buf))
      (setq-local graph-fa2--active-hitboxes (buffer-local-value 'graph-fa2--active-hitboxes base-buf))
      (setq-local graph-fa2-current-svg (buffer-local-value 'graph-fa2-current-svg base-buf))
      (setq-local graph-fa2-ctx (buffer-local-value 'graph-fa2-ctx base-buf))
      (setq-local graph-fa2-playback-buffer (buffer-local-value 'graph-fa2-playback-buffer base-buf)))
    (if frame
        (let ((win (frame-selected-window (make-frame))))
          (set-window-buffer win indirect-buf)
          indirect-buf)
      (pop-to-buffer indirect-buf))
    indirect-buf))

(defun graph-fa2-open-in-new-window ()
  "Open the current graph in a new window using an indirect buffer.
This avoids sharing display properties across windows and frames,
eliminating lockups."
  (interactive)
  (graph-fa2-view-indirect nil))

(defun graph-fa2-open-in-new-frame ()
  "Open the current graph in a new frame using an indirect buffer.
This avoids sharing display properties across windows and frames,
eliminating lockups."
  (interactive)
  (graph-fa2-view-indirect t))

(defun graph-fa2--plist-to-alist (item)
  "Convert a property list ITEM to an association list if it is a property list.
This guarantees deterministic JSON encoding across different Emacs versions."
  (if (and (listp item) (keywordp (car item)))
      (let (alist)
        (while item
          (let* ((key (car item))
                 (val (cadr item))
                 (key-str (replace-regexp-in-string "^:" "" (symbol-name key))))
            (push (cons key-str val) alist))
          (setq item (cddr item)))
        (nreverse alist))
    item))

;;;###autoload
(cl-defun graph-fa2-start (buf nodes edges &key cache-dir)
  "Initialise the cooperative physics background worker or load from cache.

Creates the context structure from the provided properties, configures
the pre-allocated arrays, and starts the asynchronous rendering thread.

Parameters:
BUF: The buffer displaying the graph.
NODES: List of graph nodes.
EDGES: List of graph edges.
CACHE-DIR: Directory for storing cache files (optional).

Returns:
Nil."
  (let* ((resolved-cache-dir (or cache-dir (expand-file-name "graph-fa2-cache" temporary-file-directory)))
         (hash-file (expand-file-name "fa2-graph.hash" resolved-cache-dir))
         (data-file (expand-file-name "fa2-graph.dat" resolved-cache-dir))
         (normalised-nodes (mapcar #'graph-fa2--plist-to-alist nodes))
         (normalised-edges (mapcar (lambda (e) (list (car e) (cdr e))) edges))
         (payload (list normalised-nodes normalised-edges (symbol-name graph-fa2-engine)))
         (json-payload (json-encode payload))
         (current-hash (secure-hash 'md5 json-payload)))
    (unless (file-exists-p resolved-cache-dir)
      (make-directory resolved-cache-dir t))
    
    (let ((old-ctx (with-current-buffer buf (and (boundp 'graph-fa2-ctx) graph-fa2-ctx))))
      (when old-ctx
        (when (graph-fa2-ctx-bg-timer old-ctx)
          (cancel-timer (graph-fa2-ctx-bg-timer old-ctx)))
        (when (buffer-live-p (graph-fa2-ctx-bg-buffer old-ctx))
          (kill-buffer (graph-fa2-ctx-bg-buffer old-ctx)))))

    (let ((ctx (graph-fa2--create-ctx nodes edges)))
      (with-current-buffer buf
        (setq-local graph-fa2-ctx ctx))
      
      (setf (graph-fa2-ctx-bg-buffer ctx) (generate-new-buffer " *graph-fa2-bg*"))
      (setf (graph-fa2-ctx-bg-timer ctx)
            (run-at-time 0 nil #'graph-fa2--render-chunk 
                         ctx data-file hash-file current-hash buf 
                         graph-fa2-simulation-frames graph-fa2-framerate)))))

;;;###autoload
(defun graph-fa2-clear-cache (&optional cache-dir)
  "Clears the background render cache to force a fresh physics simulation."
  (interactive)
  (let* ((resolved-cache-dir (or cache-dir (expand-file-name "graph-fa2-cache" temporary-file-directory)))
         (hash-file (expand-file-name "fa2-graph.hash" resolved-cache-dir))
         (data-file (expand-file-name "fa2-graph.dat" resolved-cache-dir)))
    (when (file-exists-p hash-file) (delete-file hash-file))
    (when (file-exists-p data-file) (delete-file data-file))
    (message "ForceAtlas2 cache cleared.")))

(provide 'graph-fa2)
;;; graph-fa2.el ends here
