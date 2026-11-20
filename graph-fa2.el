;;; graph-fa2.el --- ForceAtlas2 pure-elisp background-cached engine -*- lexical-binding: t -*-

;; Author: Elijah Charles <https://github.com/elij>
;; URL: https://github.com/elij/graph-fa2
;; Keywords: multimedia, graphs, visualization
;; Version: 0.3.2
;; Package-Requires: ((emacs "27.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ForceAtlas2 layout engine implemented in pure Emacs Lisp.
;;
;; This package provides background calculation and caching for interactive
;; two-dimensional and three-dimensional graph visualisation in Emacs.
;; It features physics calculation pipelines, trackball rotation, viewport
;; panning, zoom inertia, customisable display properties, and interactive
;; node dragging.  Calculations run cooperatively in the background to
;; ensure smooth rendering and responsive UI interaction.

;;; Code:

(eval-when-compile
  (when (boundp 'comp-speed)
    (setq comp-speed 3))
  (when (boundp 'native-comp-speed)
    (setq native-comp-speed 3))
  (unless (assq 'speed defun-declarations-alist)
    (push (list 'speed (if (fboundp 'byte-run--set-speed) #'byte-run--set-speed #'ignore))
          defun-declarations-alist))
  (unless (assq 'safety defun-declarations-alist)
    (push (list 'safety (if (fboundp 'byte-run--set-safety) #'byte-run--set-safety #'ignore))
          defun-declarations-alist)))

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'seq)

(declare-function image-flush "image.c" (spec &optional frame))

(cl-declaim (optimize (speed 3) (safety 0)))

(defconst graph-fa2--default-font-family "Helvetica, Arial, sans-serif"
  "Fallback SVG font-family stack avoiding generic Pango alias scans.")

(defmacro graph-fa2--fixnum (x)
  "Emit a native compiler fixnum hint for X when supported."
  (if (fboundp 'comp-hint-fixnum)
      `(comp-hint-fixnum ,x)
    x))

(defmacro graph-fa2--cons (x)
  "Emit a native compiler cons hint for X when supported."
  (if (fboundp 'comp-hint-cons)
      `(comp-hint-cons ,x)
    x))

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
  "Threshold where friction starts damping node velocities near horizon."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-surface-radius 38400
  "The maximum spherical radius nodes can occupy."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-surface-constraint 'anneal
  "How nodes are constrained to the simulation sphere radius.
\\='strict  - Nodes are permanently pinned exactly to the surface.
\\='none    - Nodes move freely in 3D space, limited only by the horizon.
\\='floor   - Nodes cannot enter the sphere, but can fly outward.
\\='ceiling - Nodes can enter the sphere, but cannot fly outward past the surface.
\\='anneal  - Nodes start free in 3D space and collapse to surface."
  :type '(choice (const :tag "Strictly pinned" strict)
                 (const :tag "Freely floating" none)
                 (const :tag "Floor minimum" floor)
                 (const :tag "Ceiling maximum" ceiling)
                 (const :tag "Anneal to surface" anneal))
  :group 'graph-fa2)

(defcustom graph-fa2-drag-action 'rotate
  "The action performed when dragging the graph background.
\\='rotate - Rotates the 3D physics coordinate sphere.
\\='pan    - Pans the 2D visual viewport."
  :type '(choice (const :tag "Rotate 3D Sphere" rotate)
                 (const :tag "Pan 2D Viewport" pan))
  :group 'graph-fa2)

(defcustom graph-fa2-drag-deadband 4.0
  "Deadband threshold in pixels before a mouse movement triggers active dragging."
  :type 'float
  :group 'graph-fa2)

(defcustom graph-fa2-engine '3d
  "The dimensionality engine of the ForceAtlas2 layout.
\\='3d - Three-dimensional physics engine and trackball rotation.
\\='2d - Two-dimensional physics engine and traditional panning."
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

(defcustom graph-fa2-show-edge-labels nil
  "Non-nil enables rendering of inline textual labels along graph edges."
  :type 'boolean
  :group 'graph-fa2)

(defcustom graph-fa2-edge-label-font-size 8
  "Font size in points for inline edge textual annotations."
  :type 'integer
  :group 'graph-fa2)

(defcustom graph-fa2-edge-label-colour "#a6adc8"
  "Hexadecimal colour specification for inline edge labels."
  :type 'string
  :group 'graph-fa2)

(defcustom graph-fa2-edge-label-font-family nil
  "Font family string for edge labels, or nil to use default sans-serif."
  :type '(choice (const :tag "Default" nil) string)
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
  "Font family for node label text, or \\='nil\\=' for default."
  :type '(choice (const :tag "Renderer default" nil) string)
  :group 'graph-fa2)

(defcustom graph-fa2-label-font-weight nil
  "Font weight for node label text, or \\='nil\\=' for default."
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
Each function must accept one argument: the node identifier,
or \\='nil\\=' if cleared.")

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

(defvar-local graph-fa2--current-frame 0
  "Current frame index in animation playback.")

(defvar-local graph-fa2--frame-offsets nil
  "Vector of start and end buffer offsets for rendered playback frames.")

(defvar-local graph-fa2-playback-buffer nil
  "Buffer storing raw SVG strings for frame playback.")

(defvar-local graph-fa2-cache-dir nil
  "Buffer-local path to active cache directory for current graph.")

(defvar-local graph-fa2-current-svg nil
  "Active SVG string for rendering in current buffer.")

(defvar-local graph-fa2--player-timer nil
  "Timer object driving animation frame playback.")

(defvar-local graph-fa2--playback-start-time nil
  "Wall-clock timestamp when active playback segment began.")

(defvar-local graph-fa2--playback-base-frame 0
  "Starting frame index corresponding to `graph-fa2--playback-start-time'.")

(defvar-local graph-fa2--hitbox-svg-string nil
  "Tracks SVG string used to generate current hitboxes.")

(defvar-local graph-fa2--active-hitboxes nil
  "Vector of identifiers and coordinates for current frame.")

(defvar-local graph-fa2-hovered-node nil
  "Tracks currently hovered node within the \\='graph-fa2\\=' engine.")

(defvar-local graph-fa2--drag-context nil
  "Context data structure tracking active or pending mouse drag state.")

(defconst graph-fa2-nav-states
  '(idle hovering pending-drag dragging-node dragging-viewport zooming)
  "List of valid navigation states in the graph-fa2 state machine.")

(defvar-local graph-fa2--nav-state 'idle
  "Current navigation state in the graph-fa2 state machine.
Valid states are members of `graph-fa2-nav-states':
- `idle': No interactive mouse action or movement is active.
- `hovering': Pointer is hovering over an interactive node.
- `pending-drag': Mouse button pressed without exceeding deadband.
- `dragging-node': Active dragging of a node beyond deadband threshold.
- `dragging-viewport': Active panning or rotating of the viewport.
- `zooming': Zoom momentum animation is active.")

(defvar graph-fa2-after-render-functions nil
  "Hook run after a graph frame is rendered.")

(cl-defstruct graph-fa2-ctx
  "State structure \\='graph-fa2-ctx\\=' for ForceAtlas2 physics simulation.
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
  last-pos-x
  last-pos-y
  last-pos-z
  norm-x
  norm-y
  norm-z
  norm-x-str
  norm-y-str
  norm-z-str
  circle-strs
  label-strs
  label-cache-key
  escaped-ids
  radius-strs
  wrapped-lines
  text-open-tag
  render-edges
  cached-render-edges-ref
  last-rendered-svg
  bg-buffer
  scratch-buffer
  bg-frame
  bg-timer
  frames-rendered
  heavy-frames
  heavy-time
  playback-started
  coords-synced
  start-time
  tick-len
  tick-a
  tick-progress
  tick-rep-computed
  px-list
  py-list
  pz-list
  vx-list
  vy-list
  vz-list
  rx-list
  ry-list
  rz-list
  mass-list
  mass-rows
  phys-edges-3d
  phys-edges-2d
  phys-edge-bounds
  cached-edges-ref)

(defvar-local graph-fa2-ctx nil
  "Buffer-local \\='graph-fa2-ctx\\=' simulation context.")

(defmacro graph-fa2-vec-add (arr-x arr-y arr-z idx dx dy dz)
  "In-place addition of DX, DY, DZ into ARR-X, ARR-Y, ARR-Z at IDX."
  `(progn
     (aset ,arr-x ,idx (+ (aref ,arr-x ,idx) ,dx))
     (aset ,arr-y ,idx (+ (aref ,arr-y ,idx) ,dy))
     (aset ,arr-z ,idx (+ (aref ,arr-z ,idx) ,dz))))

(defmacro graph-fa2-vec-sub (arr-x arr-y arr-z idx dx dy dz)
  "In-place subtraction of DX, DY, DZ from ARR-X, ARR-Y, ARR-Z at IDX."
  `(progn
     (aset ,arr-x ,idx (- (aref ,arr-x ,idx) ,dx))
     (aset ,arr-y ,idx (- (aref ,arr-y ,idx) ,dy))
     (aset ,arr-z ,idx (- (aref ,arr-z ,idx) ,dz))))

(defmacro graph-fa2-vec-set (arr-x arr-y arr-z idx vx vy vz)
  "Set ARR-X, ARR-Y, ARR-Z arrays at IDX to VX, VY, VZ."
  `(progn
     (aset ,arr-x ,idx ,vx)
     (aset ,arr-y ,idx ,vy)
     (aset ,arr-z ,idx ,vz)))

(defmacro graph-fa2-set-drag-coords (alist x y)
  "Update last-mouse-x and last-mouse-y with X and Y in ALIST."
  (let ((a (make-symbol "alist")))
    `(let ((,a ,alist))
       (when-let* ((cell-x (assoc 'last-mouse-x ,a)))
         (setcdr cell-x ,x))
       (when-let* ((cell-y (assoc 'last-mouse-y ,a)))
         (setcdr cell-y ,y)))))

(defmacro graph-fa2-incf-pan (dx dy)
  "Increment global pan offsets by DX and DY."
  `(progn
     (cl-incf graph-fa2--pan-x ,dx)
     (cl-incf graph-fa2--pan-y ,dy)))

(defsubst graph-fa2-id (n)
  "Return identifier of node N."
  (aref n 0))

(defsubst graph-fa2-label (n)
  "Return label of node N."
  (aref n 1))

(defsubst graph-fa2-x (n)
  "Return X coordinate of node N."
  (aref n 2))

(defsubst graph-fa2-y (n)
  "Return Y coordinate of node N."
  (aref n 3))

(defsubst graph-fa2-z (n)
  "Return Z coordinate of node N."
  (aref n 4))

(defsubst graph-fa2-dx (n)
  "Return X velocity of node N."
  (aref n 5))

(defsubst graph-fa2-dy (n)
  "Return Y velocity of node N."
  (aref n 6))

(defsubst graph-fa2-dz (n)
  "Return Z velocity of node N."
  (aref n 7))

(defsubst graph-fa2-mass (n)
  "Return mass of node N."
  (aref n 8))

(defsubst graph-fa2-colour (n)
  "Return colour string of node N."
  (aref n 9))

(defsubst graph-fa2-radius (n)
  "Return radius of node N."
  (aref n 10))

(defsubst graph-fa2-set-x (n v)
  "Set X coordinate of node N to V."
  (aset n 2 v))

(defsubst graph-fa2-set-y (n v)
  "Set Y coordinate of node N to V."
  (aset n 3 v))

(defsubst graph-fa2-set-z (n v)
  "Set Z coordinate of node N to V."
  (aset n 4 v))

(defsubst graph-fa2-set-dx (n v)
  "Set X velocity of node N to V."
  (aset n 5 v))

(defsubst graph-fa2-set-dy (n v)
  "Set Y velocity of node N to V."
  (aset n 6 v))

(defsubst graph-fa2-set-dz (n v)
  "Set Z velocity of node N to V."
  (aset n 7 v))

(defsubst graph-fa2--dist-2d (dx dy)
  "Calculate approximate 2D distance for DX and DY using bitwise shifts."
  (let ((ax (abs dx))
        (ay (abs dy)))
    (if (> ax ay)
        (+ ax (ash ay -1))
      (let ((d (+ ay (ash ax -1))))
        (if (eq d 0) 1 d)))))

(defsubst graph-fa2--dist-3d (dx dy dz)
  "Calculate approximate 3D distance for DX, DY, DZ using bitwise shifts.
Avoids floating-point allocations by approximating the Euclidean distance
using integer scaling."
  (let* ((ax (abs dx))
         (ay (abs dy))
         (az (abs dz))
         (mx (max ax ay az)))
    (if (eq mx 0)
        1
      (+ mx (ash (- (+ ax ay az) mx) -2)))))

(defconst graph-fa2-max-velocity 8192
  "Maximum velocity per tick to prevent coordinate overflow.")

(defsubst graph-fa2-clamp (value limit)
  "Restrict integer VALUE to bounds of negative and positive LIMIT.

This ensures that integer values do not exceed the specified threshold,
preventing coordinate overflows in the simulation loop."
  (let ((neg-limit (- (graph-fa2--fixnum limit))))
    (if (< neg-limit value limit)
        value
      (if (< value 0) neg-limit limit))))

(defun graph-fa2-normalise-coords (x y z)
  "Convert internal simulation coordinates X, Y, Z to external viewport.

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
  "Convert external viewport coordinates NX, NY, NZ to internal integers.

This subtracts half of the canvas size from the viewport coordinate,
multiplies by 256.0, and truncates the result to return a standard
integer representation.  It logs the transition values to aid in debugging."
  (when-let* ((half-canvas (/ graph-fa2-canvas-size 2.0))
              (x (truncate (* 256.0 (- nx half-canvas))))
              (y (truncate (* 256.0 (- ny half-canvas))))
              (z (truncate (* 256.0 (- nz half-canvas)))))
    (list x y z)))

(defun graph-fa2--set-hover-pointer (window node)
  "Set cursor pointer to hand when hovering over NODE in WINDOW."
  (let* ((inhibit-read-only t)
         (overlays (overlays-in (point-min) (point-max)))
         (ov (seq-find (lambda (o) (eq (overlay-get o 'window) window)) overlays)))
    (if ov
        (overlay-put ov 'pointer (if node 'hand nil))
      (if node
          (put-text-property (point-min) (point-max) 'pointer 'hand)
        (put-text-property (point-min) (point-max) 'pointer nil)))))

(defun graph-fa2--clear-hover-pointer ()
  "Clear hover pointer overlay and text property in current buffer."
  (let ((inhibit-read-only t)
        (overlays (overlays-in (point-min) (point-max))))
    (dolist (ov overlays)
      (overlay-put ov 'pointer nil))
    (put-text-property (point-min) (point-max) 'pointer nil)))

(defun graph-fa2-nav-state ()
  "Return current navigation state for the active buffer."
  graph-fa2--nav-state)

(defun graph-fa2--set-alist-entry (alist key val)
  "Set KEY to VAL in non-empty ALIST by mutating existing cell or tail."
  (if-let* ((cell (assoc key alist)))
      (setcdr cell val)
    (when (consp alist)
      (setcdr (last alist) (list (cons key val)))))
  alist)

(defun graph-fa2--nav-transition (to-state &optional payload)
  "Transition the navigation state machine to TO-STATE with optional PAYLOAD.
Valid states are members of `graph-fa2-nav-states': `idle', `hovering',
`pending-drag', `dragging-node', `dragging-viewport', and `zooming'.
Returns TO-STATE."
  (unless (memq to-state graph-fa2-nav-states)
    (error "Invalid navigation state: %s" to-state))
  (let ((from-state graph-fa2--nav-state)
        (prev-node graph-fa2-hovered-node))
    (setq graph-fa2--nav-state to-state)
    (pcase to-state
      ('idle
       (setq graph-fa2--drag-context nil)
       (setq graph-fa2-hovered-node nil)
       (when (eq from-state 'hovering)
         (graph-fa2--clear-hover-pointer))
       (run-hook-with-args 'graph-fa2-node-hovered-functions nil))
      ('hovering
       (setq graph-fa2--drag-context nil)
       (let ((node (if (consp payload) (plist-get payload :node) payload))
             (window (and (consp payload) (plist-get payload :window))))
         (setq graph-fa2-hovered-node node)
         (graph-fa2--set-hover-pointer window node)
         (when (and node (not (equal node (and (eq from-state 'hovering) prev-node))))
           (run-hook-with-args 'graph-fa2-node-hovered-functions node))))
      ('pending-drag
       (when payload
         (setq graph-fa2--drag-context payload)))
      ((or 'dragging-node 'dragging-viewport)
       (let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
              (is-playing (or graph-fa2--player-timer
                              (buffer-local-value 'graph-fa2--player-timer base-buf)))
              (ctx (graph-fa2--discover-context base-buf)))
         (when (or (eq to-state 'dragging-node)
                   (and (eq to-state 'dragging-viewport)
                        (eq graph-fa2-engine '3d)))
           (when is-playing
             (with-current-buffer base-buf
               (graph-fa2-player-stop))
             (setq is-playing nil))
           (when (and ctx (graph-fa2-ctx-bg-timer ctx))
             (when (timerp (graph-fa2-ctx-bg-timer ctx))
               (cancel-timer (graph-fa2-ctx-bg-timer ctx)))
             (setf (graph-fa2-ctx-bg-timer ctx) nil)))
         (when (and ctx (not is-playing) (not (graph-fa2-ctx-coords-synced ctx)))
           (graph-fa2--parse-hitboxes-from-svg)
           (when graph-fa2--active-hitboxes
             (graph-fa2--sync-physics ctx graph-fa2--active-hitboxes)))
         (when (and (eq to-state 'dragging-viewport)
                    (eq graph-fa2-engine '3d)
                    ctx)
           (let ((dc (if (and (consp payload) (consp (car payload)))
                         payload
                       graph-fa2--drag-context)))
             (when (consp dc)
               (graph-fa2--set-alist-entry
                dc 'snapshot-pos-x (copy-sequence (graph-fa2-ctx-pos-x ctx)))
               (graph-fa2--set-alist-entry
                dc 'snapshot-pos-y (copy-sequence (graph-fa2-ctx-pos-y ctx)))
               (graph-fa2--set-alist-entry
                dc 'snapshot-pos-z (copy-sequence (graph-fa2-ctx-pos-z ctx)))
               (setq graph-fa2--drag-context dc))))))
      ('zooming
       (setq graph-fa2--drag-context nil)))
    to-state))

(defun graph-fa2--cancel-drag (&rest _)
  "Cancel active drag context and reset navigation state.
This ensures navigation does not get stuck when crossing window boundaries,
switching buffers, or losing window focus."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (let ((is-graph-mode (buffer-local-value 'graph-fa2-mode buf))
            (drag-ctx (buffer-local-value 'graph-fa2--drag-context buf))
            (nav-st (buffer-local-value 'graph-fa2--nav-state buf)))
        (when (or is-graph-mode drag-ctx (memq nav-st '(pending-drag dragging-node dragging-viewport)))
          (with-current-buffer buf
            (when (or graph-fa2--drag-context
                      (memq graph-fa2--nav-state '(pending-drag dragging-node dragging-viewport)))
              (when (or (eq graph-fa2--nav-state 'dragging-node)
                        (and (eq graph-fa2--nav-state 'dragging-viewport)
                             (eq graph-fa2-engine '3d)))
                (when-let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
                            (ctx (graph-fa2--discover-context base-buf)))
                  (setq graph-fa2-current-svg (graph-fa2--render-current-to-svg ctx nil))
                  (setq graph-fa2--render-state nil)))
              (setq graph-fa2--drag-context nil)
              (setq graph-fa2--nav-state 'idle)
              (graph-fa2--clear-hover-pointer)
              (graph-fa2--update-display))))))))

(defun graph-fa2-rotate-3d-integer
    (pos-x pos-y pos-z len angle-x angle-y &optional src-x src-y src-z)
  "Rotate integer 3D coordinate arrays POS-X, POS-Y, POS-Z by ANGLE-X, ANGLE-Y.
LEN specifies the number of elements to process.  Optional SRC-X, SRC-Y,
and SRC-Z supply source coordinate vectors for drift-free trackball rotation.
Uses 16-bit fixed-point matrix arithmetic with half-ULP rounding."
  (declare (speed 3) (safety 0))
  (let ((sx (or src-x pos-x))
        (sy (or src-y pos-y))
        (sz (or src-z pos-z)))
    (if (and (= angle-x 0.0) (= angle-y 0.0))
        (unless (eq sx pos-x)
          (let ((i 0))
            (while (< i len)
              (aset pos-x i (aref sx i))
              (aset pos-y i (aref sy i))
              (aset pos-z i (aref sz i))
              (setq i (1+ (graph-fa2--fixnum i))))))
      (let* ((cos-x (cos angle-x))
             (sin-x (sin angle-x))
             (cos-y (cos angle-y))
             (sin-y (sin angle-y))
             (m00 (round (* cos-y 65536.0)))
             (m01 (round (* (* sin-x sin-y) 65536.0)))
             (m02 (round (* (* cos-x sin-y) 65536.0)))
             (m11 (round (* cos-x 65536.0)))
             (m12 (round (* (- sin-x) 65536.0)))
             (m20 (round (* (- sin-y) 65536.0)))
             (m21 (round (* (* sin-x cos-y) 65536.0)))
             (m22 (round (* (* cos-x cos-y) 65536.0)))
             (i 0))
        (while (< i len)
          (let ((x (aref sx i))
                (y (aref sy i))
                (z (aref sz i)))
            (aset pos-x i (ash (+ (* x m00) (* y m01) (* z m02) 32768) -16))
            (aset pos-y i (ash (+ (* y m11) (* z m12) 32768) -16))
            (aset pos-z i (ash (+ (* x m20) (* y m21) (* z m22) 32768) -16)))
          (setq i (1+ (graph-fa2--fixnum i))))))))

(defun graph-fa2--2d-mouse-down-pan (mouse-x mouse-y img-w img-h &optional is-alt-btn)
  "Initialise the drag context for traditional 2D panning.

Parameters:
MOUSE-X: Horizontal click coordinate.
MOUSE-Y: Vertical click coordinate.
IMG-W: Rendered image width.
IMG-H: Rendered image height.
IS-ALT-BTN: Non-nil if alternative mouse button was pressed."
  (let ((drag-ctx
         (list (cons 'type 'pan)
               (cons 'is-alt-button is-alt-btn)
               (cons 'start-mouse-x mouse-x)
               (cons 'start-mouse-y mouse-y)
               (cons 'last-mouse-x mouse-x)
               (cons 'last-mouse-y mouse-y)
               (cons 'img-width img-w)
               (cons 'img-height img-h)
               (cons 'start-pan-x graph-fa2--pan-x)
               (cons 'start-pan-y graph-fa2--pan-y))))
    (graph-fa2--nav-transition 'pending-drag drag-ctx)
    drag-ctx))

(defun graph-fa2--2d-pan (drag-ctx pixel-dx pixel-dy viewbox-scale)
  "Perform traditional 2D viewport panning using DRAG-CTX.

This updates horizontal and vertical pan offsets based on starting
offsets and scaled mouse displacement deltas PIXEL-DX and PIXEL-DY
with VIEWBOX-SCALE."
  (let ((start-pan-x (cdr (assoc 'start-pan-x drag-ctx)))
        (start-pan-y (cdr (assoc 'start-pan-y drag-ctx))))
    (setq graph-fa2--pan-x (+ start-pan-x (* pixel-dx viewbox-scale)))
    (setq graph-fa2--pan-y (+ start-pan-y (* pixel-dy viewbox-scale)))))

(defun graph-fa2--handle-pan-rotate (drag-ctx curr-x curr-y viewbox-scale)
  "Handle viewport panning and trackball rotation using DRAG-CTX.
CURR-X and CURR-Y are current coordinates, and VIEWBOX-SCALE is scale."
  (let* ((start-x (cdr (assoc 'start-mouse-x drag-ctx)))
         (start-y (cdr (assoc 'start-mouse-y drag-ctx)))
         (last-x (or (cdr (assoc 'last-mouse-x drag-ctx)) start-x curr-x))
         (last-y (or (cdr (assoc 'last-mouse-y drag-ctx)) start-y curr-y))
         (pixel-dx-since-last (- curr-x last-x))
         (pixel-dy-since-last (- curr-y last-y)))
    (if (eq graph-fa2-engine '2d)
        (let ((pixel-dx (- curr-x start-x))
              (pixel-dy (- curr-y start-y)))
          (graph-fa2--2d-pan drag-ctx pixel-dx pixel-dy viewbox-scale))
      (let* ((is-alt-button (cdr (assoc 'is-alt-button drag-ctx)))
             (effective-action (if is-alt-button
                                   (if (eq graph-fa2-drag-action 'rotate) 'pan 'rotate)
                                 graph-fa2-drag-action)))
        (if (eq effective-action 'rotate)
            (when-let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
                        (ctx (graph-fa2--discover-context base-buf))
                        (sensitivity 0.005))
              (let ((snap-x (cdr (assoc 'snapshot-pos-x drag-ctx)))
                    (snap-y (cdr (assoc 'snapshot-pos-y drag-ctx)))
                    (snap-z (cdr (assoc 'snapshot-pos-z drag-ctx))))
                (if (and snap-x snap-y snap-z start-x start-y)
                    (graph-fa2-rotate-3d-integer
                     (graph-fa2-ctx-pos-x ctx)
                     (graph-fa2-ctx-pos-y ctx)
                     (graph-fa2-ctx-pos-z ctx)
                     (length (graph-fa2-ctx-nodes ctx))
                     (* (- curr-y start-y) sensitivity)
                     (* (- curr-x start-x) sensitivity)
                     snap-x snap-y snap-z)
                  (graph-fa2-rotate-3d-integer
                   (graph-fa2-ctx-pos-x ctx)
                   (graph-fa2-ctx-pos-y ctx)
                   (graph-fa2-ctx-pos-z ctx)
                   (length (graph-fa2-ctx-nodes ctx))
                   (* pixel-dy-since-last sensitivity)
                   (* pixel-dx-since-last sensitivity))))
              (setf (graph-fa2-ctx-coords-synced ctx) t)
              (unless (input-pending-p)
                (setq graph-fa2-current-svg (graph-fa2--render-current-to-svg ctx t))))
          (let ((canvas-dx (* pixel-dx-since-last viewbox-scale))
                (canvas-dy (* pixel-dy-since-last viewbox-scale)))
            (graph-fa2-incf-pan canvas-dx canvas-dy)))))
    (graph-fa2-set-drag-coords drag-ctx curr-x curr-y)
    (unless (input-pending-p)
      (setq graph-fa2--render-state nil)
      (graph-fa2--update-display))))

(defun graph-fa2--handle-node-move (drag-ctx curr-x curr-y viewbox-scale)
  "Handle interactive node dragging with DRAG-CTX.
CURR-X and CURR-Y are current coordinates, and VIEWBOX-SCALE is scale."
  (when-let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
              (ctx (graph-fa2--discover-context base-buf))
              (node-id (cdr (assoc 'node-id drag-ctx)))
              (start-x (cdr (assoc 'start-mouse-x drag-ctx)))
              (start-y (cdr (assoc 'start-mouse-y drag-ctx)))
              (orig-x (cdr (assoc 'orig-x drag-ctx)))
              (orig-y (cdr (assoc 'orig-y drag-ctx)))
              (orig-z (or (cdr (assoc 'orig-z drag-ctx)) 0.0))
              (canvas-dx (* (- curr-x start-x) viewbox-scale))
              (canvas-dy (* (- curr-y start-y) viewbox-scale))
              (new-x (+ orig-x canvas-dx))
              (new-y (+ orig-y canvas-dy))
              (nodes (graph-fa2-ctx-nodes ctx))
              (idx (seq-position nodes node-id (lambda (n id) (equal (graph-fa2-id n) id))))
              (new-coords (graph-fa2-denormalise-coords new-x new-y orig-z))
              (internal-x (car new-coords))
              (internal-y (cadr new-coords))
              (internal-z (caddr new-coords)))
    (graph-fa2-vec-set
     (graph-fa2-ctx-pos-x ctx) (graph-fa2-ctx-pos-y ctx) (graph-fa2-ctx-pos-z ctx)
     idx internal-x internal-y internal-z)
    (graph-fa2-vec-set
     (graph-fa2-ctx-vel-x ctx) (graph-fa2-ctx-vel-y ctx) (graph-fa2-ctx-vel-z ctx)
     idx 0 0 0)
    (setf (graph-fa2-ctx-coords-synced ctx) t)
    (when-let*
        ((hitbox
          (seq-find (lambda (hb) (equal (aref hb 0) node-id)) graph-fa2--active-hitboxes)))
      (aset hitbox 1 new-x)
      (aset hitbox 2 new-y)
      (when (> (length hitbox) 4)
        (aset hitbox 4 orig-z)))
    (graph-fa2-set-drag-coords drag-ctx curr-x curr-y)
    (unless (input-pending-p)
      (setq graph-fa2-current-svg (graph-fa2--render-current-to-svg ctx 'focus idx))
      (setq graph-fa2--hitbox-svg-string graph-fa2-current-svg)
      (setq graph-fa2--render-state nil)
      (graph-fa2--update-display))))

(defun graph-fa2--handle-hover (window posn)
  "Handle cursor icon changes when hovering over interactive nodes in WINDOW.
POSN is the mouse position structure."
  (let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
         (is-playing (or graph-fa2--player-timer
                         (buffer-local-value 'graph-fa2--player-timer base-buf))))
    (unless is-playing
      (let* ((coords (or (posn-object-x-y posn) (posn-x-y posn)))
             (size (or (posn-object-width-height posn)
                       (and window (let ((edges (window-inside-pixel-edges window)))
                                     (cons (- (nth 2 edges) (nth 0 edges))
                                           (- (nth 3 edges) (nth 1 edges)))))
                       '(500 . 500)))
             (node (when (and coords size)
                     (graph-fa2-node-at-scaled-pos
                      (float (car coords))
                      (float (cdr coords))
                      (max 1.0 (float (car size)))
                      (max 1.0 (float (cdr size)))))))
        (if node
            (graph-fa2--nav-transition 'hovering (list :node node :window window))
          (when (eq graph-fa2--nav-state 'hovering)
            (graph-fa2--nav-transition 'idle)))))))

(defun graph-fa2-track-mouse (event)
  "Track mouse movement EVENT, delegating to hover, pan, or node-drag handlers."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (window (posn-window posn))
              ((window-live-p window)))
    (let ((target-buf (window-buffer window)))
      (let ((drag-buf (seq-find (lambda (b)
                                  (and (buffer-live-p b)
                                       (buffer-local-value 'graph-fa2--drag-context b)))
                                (buffer-list))))
        (with-current-buffer (or drag-buf target-buf)
          (if-let* ((drag-ctx graph-fa2--drag-context)
                    (coords (or (posn-object-x-y posn) (posn-x-y posn)))
                    (type (cdr (assoc 'type drag-ctx)))
                    (start-x (cdr (assoc 'start-mouse-x drag-ctx)))
                    (start-y (cdr (assoc 'start-mouse-y drag-ctx)))
                    (img-w (or (cdr (assoc 'img-width drag-ctx)) 500.0))
                    (img-h (or (cdr (assoc 'img-height drag-ctx)) 500.0)))
              (let* ((curr-x (float (car coords)))
                     (curr-y (float (cdr coords)))
                     (dx (- curr-x (or start-x curr-x)))
                     (dy (- curr-y (or start-y curr-y)))
                     (dist-sq (+ (* dx dx) (* dy dy)))
                     (deadband-sq (* graph-fa2-drag-deadband graph-fa2-drag-deadband))
                     (min-dim (min img-w img-h))
                     (viewbox-scale (/ graph-fa2-canvas-size (* graph-fa2--scale min-dim))))
                (cond
                 ((and (eq graph-fa2--nav-state 'pending-drag)
                       (< dist-sq deadband-sq))
                  nil)
                 ((or (eq type 'pan) (eq graph-fa2--nav-state 'dragging-viewport))
                  (when (eq graph-fa2--nav-state 'pending-drag)
                    (graph-fa2--nav-transition 'dragging-viewport drag-ctx))
                  (graph-fa2--handle-pan-rotate drag-ctx curr-x curr-y viewbox-scale))
                 ((or (eq type 'node-move) (eq graph-fa2--nav-state 'dragging-node))
                  (unless (eq type 'click-only)
                    (when (eq graph-fa2--nav-state 'pending-drag)
                      (graph-fa2--nav-transition 'dragging-node drag-ctx))
                    (graph-fa2--handle-node-move drag-ctx curr-x curr-y viewbox-scale)))))
            (graph-fa2--handle-hover window posn)))))))

(defun graph-fa2--start-node-drag (node mouse-x mouse-y img-w img-h is-playing)
  "Set up drag context for NODE at MOUSE-X, MOUSE-Y with IMG-W and IMG-H.
If IS-PLAYING is non-nil, set up a click-only context."
  (let ((drag-ctx
         (if is-playing
             (list (cons 'type 'click-only)
                   (cons 'node-id node)
                   (cons 'start-mouse-x mouse-x)
                   (cons 'start-mouse-y mouse-y)
                   (cons 'last-mouse-x mouse-x)
                   (cons 'last-mouse-y mouse-y)
                   (cons 'img-width img-w)
                   (cons 'img-height img-h))
           (let ((hitbox (seq-find (lambda (hb) (equal (aref hb 0) node)) graph-fa2--active-hitboxes)))
             (list (cons 'type 'node-move)
                   (cons 'start-mouse-x mouse-x)
                   (cons 'start-mouse-y mouse-y)
                   (cons 'last-mouse-x mouse-x)
                   (cons 'last-mouse-y mouse-y)
                   (cons 'img-width img-w)
                   (cons 'img-height img-h)
                   (cons 'node-id node)
                   (cons 'orig-x (if hitbox (aref hitbox 1) 0.0))
                   (cons 'orig-y (if hitbox (aref hitbox 2) 0.0))
                   (cons 'orig-z
                         (if (and hitbox (> (length hitbox) 4)) (aref hitbox 4) 0.0)))))))
    (graph-fa2--nav-transition 'pending-drag drag-ctx)
    drag-ctx))

(defun graph-fa2--start-bg-drag (mouse-x mouse-y img-w img-h is-alt-btn)
  "Set up drag context at MOUSE-X, MOUSE-Y with IMG-W, IMG-H and IS-ALT-BTN."
  (if (eq graph-fa2-engine '2d)
      (graph-fa2--2d-mouse-down-pan mouse-x mouse-y img-w img-h is-alt-btn)
    (let ((drag-ctx
           (list (cons 'type 'pan)
                 (cons 'is-alt-button is-alt-btn)
                 (cons 'start-mouse-x mouse-x)
                 (cons 'start-mouse-y mouse-y)
                 (cons 'last-mouse-x mouse-x)
                 (cons 'last-mouse-y mouse-y)
                 (cons 'img-width img-w)
                 (cons 'img-height img-h)
                 (cons 'start-pan-x graph-fa2--pan-x)
                 (cons 'start-pan-y graph-fa2--pan-y))))
      (graph-fa2--nav-transition 'pending-drag drag-ctx)
      drag-ctx)))

(defun graph-fa2-mouse-down (event)
  "Dispatch mouse down EVENT to initiate panning or node dragging."
  (interactive "e")
  (when-let* ((posn (event-start event))
              (window (posn-window posn))
              ((window-live-p window)))
    (when (not (eq window (selected-window)))
      (select-window window))
    (with-current-buffer (window-buffer window)
      (when (eq graph-fa2--nav-state 'zooming)
        (when graph-fa2--zoom-timer
          (cancel-timer graph-fa2--zoom-timer)
          (setq graph-fa2--zoom-timer nil))
        (setq graph-fa2--zoom-velocity 0.0)
        (graph-fa2--nav-transition 'idle))
      (let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
             (is-playing (or graph-fa2--player-timer
                             (buffer-local-value 'graph-fa2--player-timer base-buf)))
             (event-type (car event))
             (is-alt-btn (string-match-p "mouse-2" (symbol-name event-type))))
        (when-let* ((coords (or (posn-object-x-y posn) (posn-x-y posn)))
                    (size (or (posn-object-width-height posn)
                              (and window (let ((edges (window-inside-pixel-edges window)))
                                            (cons (- (nth 2 edges) (nth 0 edges))
                                                  (- (nth 3 edges) (nth 1 edges)))))
                              '(500 . 500)))
                    (mouse-x (float (car coords)))
                    (mouse-y (float (cdr coords)))
                    (img-w (max 1.0 (float (car size))))
                    (img-h (max 1.0 (float (cdr size)))))
          (if-let* ((node (graph-fa2-node-at-scaled-pos mouse-x mouse-y img-w img-h)))
              (graph-fa2--start-node-drag node mouse-x mouse-y img-w img-h is-playing)
            (graph-fa2--start-bg-drag mouse-x mouse-y img-w img-h is-alt-btn)))))))

(defun graph-fa2--render-current-to-svg (ctx &optional label-mode focus-idx)
  "Render the current state of nodes and edges in CTX to an SVG string.
Reuses `graph-fa2-ctx-bg-buffer' when no background timer is active, or
a persistent unibyte scratch buffer in CTX, omitting the frame delimiter.
LABEL-MODE may be nil (all labels), t (omit labels), or \\='focus\\=' (render
only the label for node index FOCUS-IDX)."
  (let* ((len (length (graph-fa2-ctx-nodes ctx)))
         (original-buffer (graph-fa2-ctx-bg-buffer ctx))
         (can-reuse-bg (and (not (graph-fa2-ctx-bg-timer ctx))
                            (buffer-live-p original-buffer)))
         (render-buf (if can-reuse-bg
                         original-buffer
                       (let ((sb (graph-fa2-ctx-scratch-buffer ctx)))
                         (unless (buffer-live-p sb)
                           (setq sb (generate-new-buffer " *graph-fa2-scratch*"))
                           (with-current-buffer sb
                             (set-buffer-multibyte nil))
                           (setf (graph-fa2-ctx-scratch-buffer ctx) sb))
                         sb))))
    (with-current-buffer render-buf
      (set-buffer-multibyte nil)
      (erase-buffer)
      (unwind-protect
          (progn
            (setf (graph-fa2-ctx-bg-buffer ctx) render-buf)
            (graph-fa2--render-svg ctx len t label-mode focus-idx)
            (let ((svg (buffer-substring-no-properties (point-min) (point-max))))
              (setf (graph-fa2-ctx-last-rendered-svg ctx) svg)
              svg))
        (setf (graph-fa2-ctx-bg-buffer ctx) original-buffer)))))

(defun graph-fa2--discover-context (base-buf)
  "Locate and return active physics simulation context for BASE-BUF.
Iterates through local variables of BASE-BUF and its associated
playback buffer to find the physics context struct."
  (let* ((ctx (buffer-local-value 'graph-fa2-ctx base-buf))
         (pb (buffer-local-value 'graph-fa2-playback-buffer base-buf)))
    (unless (and ctx (graph-fa2-ctx-p ctx))
      (setq ctx nil)
      (dolist (var (buffer-local-variables base-buf))
        (when (and (consp var) (graph-fa2-ctx-p (cdr var)))
          (setq ctx (cdr var))))
      (when (and (not ctx) (buffer-live-p pb))
        (dolist (var (buffer-local-variables pb))
          (when (and (consp var) (graph-fa2-ctx-p (cdr var)))
            (setq ctx (cdr var))))))
    ctx))

(defun graph-fa2--sync-physics (ctx hitboxes)
  "Synchronise physics arrays in CTX with visual HITBOXES."
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
             (n-id (graph-fa2-id n)))
        (when-let*
            ((hitbox (gethash n-id hitbox-map))
             (depth (if (> (length hitbox) 4) (aref hitbox 4) 0.0))
             (hb-coords (graph-fa2-denormalise-coords (aref hitbox 1) (aref hitbox 2) depth))
             (internal-x (car hb-coords))
             (internal-y (cadr hb-coords))
             (internal-z (caddr hb-coords)))
          (graph-fa2-vec-set pos-x pos-y pos-z i internal-x internal-y internal-z))))
    (setf (graph-fa2-ctx-coords-synced ctx) t)))

(defun graph-fa2--init-background-worker (ctx pb base-buf)
  "Initialise in-memory background worker CTX for PB and BASE-BUF.
Erases relevant buffers, resets tracking variables, pre-renders an initial
headroom burst with consistent cooling, triggers a hot reload, and schedules
cooperative chunk slices."
  (unless (buffer-live-p (graph-fa2-ctx-bg-buffer ctx))
    (let ((bg (generate-new-buffer " *graph-fa2-bg*")))
      (with-current-buffer bg
        (set-buffer-multibyte nil))
      (setf (graph-fa2-ctx-bg-buffer ctx) bg)))
  (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
    (set-buffer-multibyte nil)
    (erase-buffer))
  (when (buffer-live-p pb)
    (with-current-buffer pb
      (set-buffer-multibyte nil)
      (erase-buffer)))
  (with-current-buffer base-buf
    (graph-fa2-player-stop)
    (setq graph-fa2--current-frame 0)
    (setq graph-fa2--playback-base-frame 0)
    (setq graph-fa2--playback-start-time nil)
    (setq graph-fa2--frame-offsets nil))
  (when (graph-fa2-ctx-bg-timer ctx)
    (cancel-timer (graph-fa2-ctx-bg-timer ctx))
    (setf (graph-fa2-ctx-bg-timer ctx) nil))
  (setf (graph-fa2-ctx-heavy-frames ctx) 0)
  (setf (graph-fa2-ctx-heavy-time ctx) 0.0)
  (setf (graph-fa2-ctx-playback-started ctx) t)

  ;; Pre-render 4 frames against the target 250-frame schedule to avoid
  ;; both the frame-100 cooling discontinuity and initial buffer underrun.
  (let ((f 100))
    (while (< f 104)
      (setf (graph-fa2-ctx-bg-frame ctx) f)
      (graph-fa2--physics-tick ctx 250)
      (setq f (1+ f))))
  (setf (graph-fa2-ctx-frames-rendered ctx) 104)

  (graph-fa2--hot-reload-player base-buf (graph-fa2-ctx-bg-buffer ctx))
  (let ((delay (if (and (buffer-live-p base-buf)
                        (get-buffer-window base-buf t))
                   0.008
                 0)))
    (setf (graph-fa2-ctx-bg-timer ctx)
          (run-at-time delay nil #'graph-fa2--render-chunk
                       ctx nil nil nil
                       base-buf 250 graph-fa2-framerate))))

(defun graph-fa2--handle-node-drop (drag-ctx)
  "Handle dropping a node using DRAG-CTX.
Differentiates between a click and a physical drag."
  (let* ((node-id (cdr (assoc 'node-id drag-ctx)))
         (start-x (cdr (assoc 'start-mouse-x drag-ctx)))
         (start-y (cdr (assoc 'start-mouse-y drag-ctx)))
         (last-x (cdr (assoc 'last-mouse-x drag-ctx)))
         (last-y (cdr (assoc 'last-mouse-y drag-ctx)))
         (dx (- (or last-x start-x 0.0) (or start-x 0.0)))
         (dy (- (or last-y start-y 0.0) (or start-y 0.0)))
         (deadband-sq (* graph-fa2-drag-deadband graph-fa2-drag-deadband)))
    (if (< (+ (* dx dx) (* dy dy)) deadband-sq)
        (run-hook-with-args 'graph-fa2-node-clicked-functions node-id)
      (when-let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
                  (pb (buffer-local-value 'graph-fa2-playback-buffer base-buf))
                  (ctx (graph-fa2--discover-context base-buf))
                  ((buffer-live-p pb)))
        (graph-fa2--sync-physics ctx graph-fa2--active-hitboxes)
        (graph-fa2--init-background-worker ctx pb base-buf)))))

(defun graph-fa2-mouse-up (event)
  "Dispatch mouse up EVENT to conclude drags or trigger clicks.
Cleans up drag context and resets navigation state
regardless of which window or buffer receives the event."
  (interactive "e")
  (let* ((posn (event-start event))
         (window (and posn (posn-window posn)))
         (event-buf (and window (window-live-p window) (window-buffer window)))
         (active-bufs (seq-filter (lambda (b)
                                    (and (buffer-live-p b)
                                         (or (buffer-local-value 'graph-fa2--drag-context b)
                                             (memq (buffer-local-value 'graph-fa2--nav-state b)
                                                   '(pending-drag dragging-node dragging-viewport)))))
                                  (buffer-list))))
    (if active-bufs
        (dolist (buf active-bufs)
          (with-current-buffer buf
            (let ((drag-ctx graph-fa2--drag-context)
                  (state graph-fa2--nav-state))
              (setq graph-fa2--drag-context nil)
              (cond
               ((eq state 'pending-drag)
                (let ((node-id (cdr (assoc 'node-id drag-ctx))))
                  (when node-id
                    (run-hook-with-args 'graph-fa2-node-clicked-functions node-id)))
                (graph-fa2--nav-transition 'idle)
                (graph-fa2--update-display))
               ((eq state 'dragging-node)
                (graph-fa2--handle-node-drop drag-ctx)
                (unless graph-fa2--player-timer
                  (when-let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
                              (ctx (graph-fa2--discover-context base-buf)))
                    (setq graph-fa2-current-svg (graph-fa2--render-current-to-svg ctx nil))
                    (setq graph-fa2--hitbox-svg-string graph-fa2-current-svg)
                    (setq graph-fa2--render-state nil)))
                (graph-fa2--nav-transition 'idle)
                (graph-fa2--update-display))
               ((eq state 'dragging-viewport)
                (when (eq graph-fa2-engine '3d)
                  (when-let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
                              (ctx (graph-fa2--discover-context base-buf)))
                    (setq graph-fa2-current-svg (graph-fa2--render-current-to-svg ctx nil))
                    (setq graph-fa2--render-state nil)))
                (graph-fa2--nav-transition 'idle)
                (graph-fa2--update-display))
               (t
                (when-let* ((type (cdr (assoc 'type drag-ctx))))
                  (when (eq type 'click-only)
                    (run-hook-with-args 'graph-fa2-node-clicked-functions
                                        (cdr (assoc 'node-id drag-ctx)))))
                (graph-fa2--nav-transition 'idle)
                (graph-fa2--update-display))))))
      (when (and event-buf (buffer-live-p event-buf))
        (with-current-buffer event-buf
          (when graph-fa2--drag-context
            (setq graph-fa2--drag-context nil)
            (graph-fa2--nav-transition 'idle)
            (graph-fa2--update-display)))))))

(defun graph-fa2--parse-hitboxes-from-svg ()
  "Populate and cache active hitboxes vector from synced context or SVG."
  (when (and graph-fa2-current-svg
             (not (equal graph-fa2-current-svg graph-fa2--hitbox-svg-string)))
    (let* ((base-buf (or (buffer-base-buffer) (current-buffer)))
           (ctx (graph-fa2--discover-context base-buf)))
      (if (and ctx
               (eq graph-fa2-current-svg (graph-fa2-ctx-last-rendered-svg ctx))
               (graph-fa2-ctx-norm-x ctx))
          (let* ((nodes (graph-fa2-ctx-nodes ctx))
                 (len (length nodes))
                 (nx (graph-fa2-ctx-norm-x ctx))
                 (ny (graph-fa2-ctx-norm-y ctx))
                 (nz (graph-fa2-ctx-norm-z ctx))
                 (hbs (make-vector len nil))
                 (i 0))
            (while (< i len)
              (let ((n (aref nodes i)))
                (aset hbs i (vector (graph-fa2-id n)
                                    (aref nx i)
                                    (aref ny i)
                                    (graph-fa2-radius n)
                                    (aref nz i))))
              (setq i (1+ i)))
            (setq graph-fa2--active-hitboxes hbs)
            (setq graph-fa2--hitbox-svg-string graph-fa2-current-svg))
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
          (setq graph-fa2--hitbox-svg-string graph-fa2-current-svg))))))

(defun graph-fa2-node-at-scaled-pos (active-x active-y img-w img-h)
  "Extract closest node ID at ACTIVE-X, ACTIVE-Y with IMG-W and IMG-H."
  (graph-fa2--parse-hitboxes-from-svg)
  (let* ((min-dim (min img-w img-h))
         (pad-x (/ (- img-w min-dim) 2.0))
         (pad-y (/ (- img-h min-dim) 2.0))
         (adj-x (- active-x pad-x))
         (adj-y (- active-y pad-y)))
    (when (and (>= adj-x 0) (<= adj-x min-dim)
               (>= adj-y 0) (<= adj-y min-dim))
      (let*
          ((viewbox-dim (/ graph-fa2-canvas-size graph-fa2--scale))
           (viewbox-x
            (- (- (/ graph-fa2-canvas-size 2.0) graph-fa2--pan-x) (/ viewbox-dim 2.0)))
           (viewbox-y
            (- (- (/ graph-fa2-canvas-size 2.0) graph-fa2--pan-y) (/ viewbox-dim 2.0)))
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
  "Escape XML characters and non-ASCII codepoints in STR as unibyte ASCII."
  (if (and (not (multibyte-string-p str))
           (not (string-match-p "[&<>\"']" str)))
      str
    (let ((s (replace-regexp-in-string "&" "&amp;" str t t)))
      (setq s (replace-regexp-in-string "<" "&lt;" s t t))
      (setq s (replace-regexp-in-string ">" "&gt;" s t t))
      (setq s (replace-regexp-in-string "\"" "&quot;" s t t))
      (setq s (replace-regexp-in-string "'" "&apos;" s t t))
      (if (multibyte-string-p s)
          (string-as-unibyte
           (mapconcat (lambda (ch)
                        (if (< ch 128)
                            (char-to-string ch)
                          (format "&#%d;" ch)))
                      s ""))
        s))))

(defun graph-fa2--unescape-xml (str)
  "Restore standard characters from XML-escaped node names in STR.
This is the inverse of the XML escape function."
  (if (not (string-match-p "&" str))
      str
    (let ((s (replace-regexp-in-string
              "&#\\([0-9]+\\);"
              (lambda (m) (char-to-string (string-to-number (match-string 1 m))))
              str t t)))
      (setq s (replace-regexp-in-string "&apos;" "'" s t t))
      (setq s (replace-regexp-in-string "&quot;" "\"" s t t))
      (setq s (replace-regexp-in-string "&gt;" ">" s t t))
      (setq s (replace-regexp-in-string "&lt;" "<" s t t))
      (setq s (replace-regexp-in-string "&amp;" "&" s t t))
      s)))

(defun graph-fa2--parse-edge (edge)
  "Parse heterogeneous EDGE into (list src tgt label colour width directed)."
  (cond
   ((and (consp edge) (keywordp (car edge)))
    (list (or (plist-get edge :from) (plist-get edge :source))
          (or (plist-get edge :to) (plist-get edge :target))
          (plist-get edge :label)
          (or (plist-get edge :colour) (plist-get edge :color))
          (plist-get edge :width)
          (plist-get edge :directed)))
   ((and (consp edge) (listp (cdr edge)))
    (list (nth 0 edge)
          (nth 1 edge)
          (nth 2 edge)
          (nth 3 edge)
          (nth 4 edge)
          (nth 5 edge)))
   ((consp edge)
    (list (car edge)
          (cdr edge)
          nil nil nil nil))
   ((vectorp edge)
    (list (aref edge 0)
          (aref edge 1)
          (when (> (length edge) 2) (aref edge 2))
          (when (> (length edge) 3) (aref edge 3))
          (when (> (length edge) 4) (aref edge 4))
          (when (> (length edge) 5) (aref edge 5))))
   (t (list nil nil nil nil nil nil))))

(defvar graph-fa2-deterministic-positions nil
  "Non-nil enables deterministic MD5 initial node coordinates.")

(defun graph-fa2--hash-pos (str offset)
  "Return pseudo-random number between -500 and 500 based on STR and OFFSET."
  (if graph-fa2-deterministic-positions
      (- (mod (string-to-number
               (substring (secure-hash 'md5 (concat str offset)) 0 8) 16) 1000) 500.0)
    (- (random 1000) 500.0)))

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

(defun graph-fa2--create-ctx (nodes edges)
  "Create and initialise a \\='graph-fa2-ctx\\=' struct from NODES and EDGES.
Pre-allocates physics vectors, cons spines, coordinate normalisation caches,
and static SVG fragments to eliminate allocation pressure during rendering."
  (let* ((len (length nodes))
         (node-map (make-hash-table :test #'equal))
         (internal-nodes (make-vector len nil))
         (degree-map (make-hash-table :test #'equal))
         (idx 0))
    (seq-doseq (n nodes)
      (let ((id (plist-get n :id)))
        (puthash id idx node-map)
        (cl-incf idx)))
    (seq-doseq (edge edges)
      (let* ((parsed (graph-fa2--parse-edge edge))
             (src (nth 0 parsed))
             (tgt (nth 1 parsed))
             (s-idx (or (gethash src node-map)
                        (and (integerp src) (>= src 0) (< src len) src)))
             (t-idx (or (gethash tgt node-map)
                        (and (integerp tgt) (>= tgt 0) (< tgt len) tgt))))
        (when s-idx
          (puthash s-idx (1+ (gethash s-idx degree-map 0)) degree-map))
        (when (and t-idx (not (equal s-idx t-idx)))
          (puthash t-idx (1+ (gethash t-idx degree-map 0)) degree-map))))
    (setq idx 0)
    (let ((escaped-ids (make-vector len ""))
          (radius-strs (make-vector len ""))
          (wrapped-lines (make-vector len nil)))
      (seq-doseq (n nodes)
        (let* ((id (plist-get n :id))
               (label (plist-get n :label))
               (colour (or (plist-get n :colour) (plist-get n :color) "#89b4fa"))
               (radius (or (plist-get n :radius) 10.0))
               (mass (+ 1 (gethash idx degree-map 0)))
               (id-str (if (stringp id) id (format "%s" id)))
               (label-str (if (stringp label) label (format "%s" (or label id-str))))
               (name-escaped (graph-fa2--escape-xml label-str))
               (x (truncate (* (graph-fa2--hash-pos id-str "x") 256.0)))
               (y (truncate (* (graph-fa2--hash-pos id-str "y") 256.0)))
               (z
                (if (eq graph-fa2-engine '2d) 0
                  (truncate (* (graph-fa2--hash-pos id-str "z") 256.0)))))
          (aset internal-nodes idx (vector id label x y z 0 0 0 mass colour radius))
          (aset escaped-ids idx (graph-fa2--escape-xml id-str))
          (aset radius-strs idx (format "%.3f" radius))
          (aset wrapped-lines idx
                (graph-fa2--wrap-text name-escaped graph-fa2-label-wrap-chars))
          (cl-incf idx)))
      (let (internal-edges)
        (seq-doseq (edge edges)
          (let* ((parsed (graph-fa2--parse-edge edge))
                 (src (nth 0 parsed))
                 (tgt (nth 1 parsed))
                 (label (nth 2 parsed))
                 (colour (nth 3 parsed))
                 (width (nth 4 parsed))
                 (directed (nth 5 parsed))
                 (s-idx (or (gethash src node-map)
                            (and (integerp src) (>= src 0) (< src len) src)))
                 (t-idx (or (gethash tgt node-map)
                            (and (integerp tgt) (>= tgt 0) (< tgt len) tgt))))
            (when (and s-idx t-idx)
              (push (vector s-idx t-idx label colour width directed) internal-edges))))
        (let* ((matrix (make-vector (* len len) 0)))
          (dotimes (i len)
            (let ((ni (aref internal-nodes i)))
              (dotimes (j len)
                (when (> j i)
                  (let ((nj (aref internal-nodes j)))
                    (aset matrix (+ (* i len) j)
                          (truncate (* 50.0 (graph-fa2-mass ni) (graph-fa2-mass nj)))))))))
          (let* ((pos-x (make-vector len 0))
                 (pos-y (make-vector len 0))
                 (pos-z (make-vector len 0))
                 (vel-x (make-vector len 0))
                 (vel-y (make-vector len 0))
                 (vel-z (make-vector len 0))
                 (rep-x (make-vector len 0))
                 (rep-y (make-vector len 0))
                 (rep-z (make-vector len 0))
                 (last-pos-x (make-vector len nil))
                 (last-pos-y (make-vector len nil))
                 (last-pos-z (make-vector len nil))
                 (norm-x (make-vector len 0.0))
                 (norm-y (make-vector len 0.0))
                 (norm-z (make-vector len 0.0))
                 (norm-x-str (make-vector len ""))
                 (norm-y-str (make-vector len ""))
                 (norm-z-str (make-vector len ""))
                 (circle-strs (make-vector len nil))
                 (label-strs (make-vector len nil))
                 (font-fam (or graph-fa2-label-font-family
                               graph-fa2--default-font-family))
                 (text-open-tag
                  (concat "  <text fill=\"" graph-fa2-label-colour
                          "\" font-size=\"" (number-to-string graph-fa2-label-font-size)
                          "\" font-family=\"" font-fam "\""
                          (if graph-fa2-label-font-weight
                              (concat " font-weight=\"" graph-fa2-label-font-weight "\"") "")
                          " text-anchor=\"middle\">\n")))
            (dotimes (i len)
              (let ((n (aref internal-nodes i)))
                (aset pos-x i (graph-fa2-x n))
                (aset pos-y i (graph-fa2-y n))
                (aset pos-z i (graph-fa2-z n))))
            (let ((ctx (make-graph-fa2-ctx
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
                        :last-pos-x last-pos-x
                        :last-pos-y last-pos-y
                        :last-pos-z last-pos-z
                        :norm-x norm-x
                        :norm-y norm-y
                        :norm-z norm-z
                        :norm-x-str norm-x-str
                        :norm-y-str norm-y-str
                        :norm-z-str norm-z-str
                        :circle-strs circle-strs
                        :label-strs label-strs
                        :escaped-ids escaped-ids
                        :radius-strs radius-strs
                        :wrapped-lines wrapped-lines
                        :text-open-tag text-open-tag
                        :bg-frame 0
                        :frames-rendered 0
                        :heavy-frames 0
                        :heavy-time 0.0
                        :playback-started nil
                        :coords-synced nil
                        :start-time (current-time))))
              (graph-fa2--build-spines ctx)
              ctx)))))))

(defun graph-fa2--build-edge-spines (ctx px-cells py-cells pz-cells vx-cells vy-cells vz-cells len)
  "Populate pre-linked edge pointer lists in CTX.
PX-CELLS, PY-CELLS, PZ-CELLS, VX-CELLS, VY-CELLS, and VZ-CELLS map
indices below LEN to interior cons cells of the coordinate spines."
  (let ((edges (graph-fa2-ctx-edges ctx))
        (valid-pairs nil))
    (dolist (edge edges)
      (let ((u (if (vectorp edge) (aref edge 0) (car edge)))
            (v (if (vectorp edge) (aref edge 1) (cdr edge))))
        (when (and (integerp u) (integerp v)
                   (>= u 0) (< u len)
                   (>= v 0) (< v len)
                   (not (eq u v)))
          (push (cons (max u v) (cons u v)) valid-pairs))))
    (setq valid-pairs
          (sort (nreverse valid-pairs)
                (lambda (a b) (< (car a) (car b)))))
    (let ((edges-3d nil)
          (edges-2d nil)
          (bounds nil))
      (dolist (entry valid-pairs)
        (let* ((bound (car entry))
               (uv (cdr entry))
               (u (car uv))
               (v (cdr uv))
               (upx (aref px-cells u))
               (vpx (aref px-cells v))
               (upy (aref py-cells u))
               (vpy (aref py-cells v))
               (upz (aref pz-cells u))
               (vpz (aref pz-cells v))
               (uvx (aref vx-cells u))
               (vvx (aref vx-cells v))
               (uvy (aref vy-cells u))
               (vvy (aref vy-cells v))
               (uvz (aref vz-cells u))
               (vvz (aref vz-cells v)))
          (push bound bounds)
          (push (cons (cons upx vpx)
                      (cons (cons upy vpy)
                            (cons (cons uvx vvx)
                                  (cons uvy vvy))))
                edges-2d)
          (push (cons (cons upx vpx)
                      (cons (cons upy vpy)
                            (cons (cons upz vpz)
                                  (cons (cons uvx vvx)
                                        (cons (cons uvy vvy)
                                              (cons uvz vvz))))))
                edges-3d)))
      (setf (graph-fa2-ctx-phys-edges-3d ctx) (nreverse edges-3d))
      (setf (graph-fa2-ctx-phys-edges-2d ctx) (nreverse edges-2d))
      (setf (graph-fa2-ctx-phys-edge-bounds ctx) (nreverse bounds))
      (setf (graph-fa2-ctx-cached-edges-ref ctx) edges))))

(defun graph-fa2--build-spines (ctx)
  "Initialise pre-allocated structure-of-arrays cons spines on CTX."
  (let* ((nodes (graph-fa2-ctx-nodes ctx))
         (len (length nodes)))
    (when (> len 0)
      (let ((px-l (make-list len 0))
            (py-l (make-list len 0))
            (pz-l (make-list len 0))
            (vx-l (make-list len 0))
            (vy-l (make-list len 0))
            (vz-l (make-list len 0))
            (rx-l (make-list len 0))
            (ry-l (make-list len 0))
            (rz-l (make-list len 0))
            (mass-l (make-list len 1))
            (mrows (make-list len nil))
            (px-cells (make-vector len nil))
            (py-cells (make-vector len nil))
            (pz-cells (make-vector len nil))
            (vx-cells (make-vector len nil))
            (vy-cells (make-vector len nil))
            (vz-cells (make-vector len nil))
            (mmat (graph-fa2-ctx-mass-matrix ctx)))
        (unless mmat
          (setq mmat (make-vector (* len len) 0))
          (dotimes (i len)
            (let ((mi (graph-fa2-mass (aref nodes i))))
              (cl-loop for j from (1+ i) below len do
                       (let ((mj (graph-fa2-mass (aref nodes j))))
                         (aset mmat (+ (* i len) j)
                               (truncate (* 50.0 mi mj)))))))
          (setf (graph-fa2-ctx-mass-matrix ctx) mmat))
        (unless (graph-fa2-ctx-rep-x ctx)
          (setf (graph-fa2-ctx-rep-x ctx) (make-vector len 0)))
        (unless (graph-fa2-ctx-rep-y ctx)
          (setf (graph-fa2-ctx-rep-y ctx) (make-vector len 0)))
        (unless (graph-fa2-ctx-rep-z ctx)
          (setf (graph-fa2-ctx-rep-z ctx) (make-vector len 0)))
        (let ((cpx px-l) (cpy py-l) (cpz pz-l)
              (cvx vx-l) (cvy vy-l) (cvz vz-l)
              (cm mass-l) (cmr mrows)
              (i 0))
          (while (< i len)
            (aset px-cells i cpx)
            (aset py-cells i cpy)
            (aset pz-cells i cpz)
            (aset vx-cells i cvx)
            (aset vy-cells i cvy)
            (aset vz-cells i cvz)
            (setcar cm (truncate (graph-fa2-mass (aref nodes i))))
            (let ((row nil)
                  (j (1- len))
                  (row-off (* i len)))
              (while (> j i)
                (push (truncate (aref mmat (+ row-off j))) row)
                (setq j (1- j)))
              (setcar cmr row))
            (setq cpx (cdr cpx) cpy (cdr cpy) cpz (cdr cpz)
                  cvx (cdr cvx) cvy (cdr cvy) cvz (cdr cvz)
                  cm (cdr cm) cmr (cdr cmr)
                  i (1+ i))))
        (setf (graph-fa2-ctx-px-list ctx) px-l)
        (setf (graph-fa2-ctx-py-list ctx) py-l)
        (setf (graph-fa2-ctx-pz-list ctx) pz-l)
        (setf (graph-fa2-ctx-vx-list ctx) vx-l)
        (setf (graph-fa2-ctx-vy-list ctx) vy-l)
        (setf (graph-fa2-ctx-vz-list ctx) vz-l)
        (setf (graph-fa2-ctx-rx-list ctx) rx-l)
        (setf (graph-fa2-ctx-ry-list ctx) ry-l)
        (setf (graph-fa2-ctx-rz-list ctx) rz-l)
        (setf (graph-fa2-ctx-mass-list ctx) mass-l)
        (setf (graph-fa2-ctx-mass-rows ctx) mrows)
        (graph-fa2--build-edge-spines
         ctx px-cells py-cells pz-cells vx-cells vy-cells vz-cells len)))))

(defun graph-fa2--ensure-spines (ctx)
  "Ensure cons spines and pre-linked edge lists on CTX are up to date."
  (if (null (graph-fa2-ctx-px-list ctx))
      (graph-fa2--build-spines ctx)
    (unless (eq (graph-fa2-ctx-cached-edges-ref ctx)
                (graph-fa2-ctx-edges ctx))
      (let* ((len (length (graph-fa2-ctx-nodes ctx)))
             (px-cells (make-vector len nil))
             (py-cells (make-vector len nil))
             (pz-cells (make-vector len nil))
             (vx-cells (make-vector len nil))
             (vy-cells (make-vector len nil))
             (vz-cells (make-vector len nil))
             (cpx (graph-fa2-ctx-px-list ctx))
             (cpy (graph-fa2-ctx-py-list ctx))
             (cpz (graph-fa2-ctx-pz-list ctx))
             (cvx (graph-fa2-ctx-vx-list ctx))
             (cvy (graph-fa2-ctx-vy-list ctx))
             (cvz (graph-fa2-ctx-vz-list ctx))
             (i 0))
        (while (< i len)
          (aset px-cells i cpx)
          (aset py-cells i cpy)
          (aset pz-cells i cpz)
          (aset vx-cells i cvx)
          (aset vy-cells i cvy)
          (aset vz-cells i cvz)
          (setq cpx (cdr cpx) cpy (cdr cpy) cpz (cdr cpz)
                cvx (cdr cvx) cvy (cdr cvy) cvz (cdr cvz)
                i (1+ i)))
        (graph-fa2--build-edge-spines
         ctx px-cells py-cells pz-cells vx-cells vy-cells vz-cells len)))))

(defun graph-fa2--nth-cons (lst idx)
  "Return cons cell at zero-based index IDX in LST."
  (declare (speed 3) (safety 0))
  (let ((cur lst)
        (i idx))
    (while (> i 0)
      (setq cur (cdr (graph-fa2--cons cur))
            i (1- (graph-fa2--fixnum i))))
    cur))

(defun graph-fa2--active-edge-tail (edges bounds len)
  "Return the last cons cell in EDGES whose entry in BOUNDS is below LEN."
  (declare (speed 3) (safety 0))
  (if (or (null bounds) (>= (car (graph-fa2--cons bounds)) len))
      nil
    (let ((ec edges)
          (bc (cdr (graph-fa2--cons bounds))))
      (while (and bc (< (car (graph-fa2--cons bc)) len))
        (setq ec (cdr (graph-fa2--cons ec))
              bc (cdr (graph-fa2--cons bc))))
      ec)))

(defun graph-fa2--load-spines-3d (ctx len)
  "Copy 3D coordinate and velocity vectors in CTX into spines for LEN nodes."
  (declare (speed 3) (safety 0))
  (let ((pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (pos-z (graph-fa2-ctx-pos-z ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx))
        (cpx (graph-fa2-ctx-px-list ctx))
        (cpy (graph-fa2-ctx-py-list ctx))
        (cpz (graph-fa2-ctx-pz-list ctx))
        (cvx (graph-fa2-ctx-vx-list ctx))
        (cvy (graph-fa2-ctx-vy-list ctx))
        (cvz (graph-fa2-ctx-vz-list ctx))
        (i 0))
    (while (< i len)
      (setcar (graph-fa2--cons cpx) (aref pos-x i))
      (setcar (graph-fa2--cons cpy) (aref pos-y i))
      (setcar (graph-fa2--cons cpz) (aref pos-z i))
      (setcar (graph-fa2--cons cvx) (aref vel-x i))
      (setcar (graph-fa2--cons cvy) (aref vel-y i))
      (setcar (graph-fa2--cons cvz) (aref vel-z i))
      (setq cpx (cdr (graph-fa2--cons cpx))
            cpy (cdr (graph-fa2--cons cpy))
            cpz (cdr (graph-fa2--cons cpz))
            cvx (cdr (graph-fa2--cons cvx))
            cvy (cdr (graph-fa2--cons cvy))
            cvz (cdr (graph-fa2--cons cvz))
            i (1+ (graph-fa2--fixnum i))))))

(defun graph-fa2--store-spines-3d (ctx len total-nodes)
  "Write 3D spines in CTX back to LEN vector slots and TOTAL-NODES structs."
  (declare (speed 3) (safety 0))
  (let ((nodes (graph-fa2-ctx-nodes ctx))
        (pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (pos-z (graph-fa2-ctx-pos-z ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx))
        (rep-x (graph-fa2-ctx-rep-x ctx))
        (rep-y (graph-fa2-ctx-rep-y ctx))
        (rep-z (graph-fa2-ctx-rep-z ctx))
        (cpx (graph-fa2-ctx-px-list ctx))
        (cpy (graph-fa2-ctx-py-list ctx))
        (cpz (graph-fa2-ctx-pz-list ctx))
        (cvx (graph-fa2-ctx-vx-list ctx))
        (cvy (graph-fa2-ctx-vy-list ctx))
        (cvz (graph-fa2-ctx-vz-list ctx))
        (crx (graph-fa2-ctx-rx-list ctx))
        (cry (graph-fa2-ctx-ry-list ctx))
        (crz (graph-fa2-ctx-rz-list ctx))
        (i 0))
    (while (< i len)
      (let ((px (car (graph-fa2--cons cpx)))
            (py (car (graph-fa2--cons cpy)))
            (pz (car (graph-fa2--cons cpz)))
            (vx (car (graph-fa2--cons cvx)))
            (vy (car (graph-fa2--cons cvy)))
            (vz (car (graph-fa2--cons cvz)))
            (n (aref nodes i)))
        (aset pos-x i px)
        (aset pos-y i py)
        (aset pos-z i pz)
        (aset vel-x i vx)
        (aset vel-y i vy)
        (aset vel-z i vz)
        (aset rep-x i (car (graph-fa2--cons crx)))
        (aset rep-y i (car (graph-fa2--cons cry)))
        (aset rep-z i (car (graph-fa2--cons crz)))
        (graph-fa2-set-x n px)
        (graph-fa2-set-y n py)
        (graph-fa2-set-z n pz)
        (graph-fa2-set-dx n vx)
        (graph-fa2-set-dy n vy)
        (graph-fa2-set-dz n vz))
      (setq cpx (cdr (graph-fa2--cons cpx))
            cpy (cdr (graph-fa2--cons cpy))
            cpz (cdr (graph-fa2--cons cpz))
            cvx (cdr (graph-fa2--cons cvx))
            cvy (cdr (graph-fa2--cons cvy))
            cvz (cdr (graph-fa2--cons cvz))
            crx (cdr (graph-fa2--cons crx))
            cry (cdr (graph-fa2--cons cry))
            crz (cdr (graph-fa2--cons crz))
            i (1+ (graph-fa2--fixnum i))))
    (while (< i total-nodes)
      (let ((n (aref nodes i)))
        (graph-fa2-set-x n (aref pos-x i))
        (graph-fa2-set-y n (aref pos-y i))
        (graph-fa2-set-z n (aref pos-z i))
        (graph-fa2-set-dx n (aref vel-x i))
        (graph-fa2-set-dy n (aref vel-y i))
        (graph-fa2-set-dz n (aref vel-z i)))
      (setq i (1+ (graph-fa2--fixnum i))))))

(defun graph-fa2--load-spines-2d (ctx len)
  "Copy 2D coordinate and velocity vectors in CTX into spines for LEN nodes."
  (declare (speed 3) (safety 0))
  (let ((pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (cpx (graph-fa2-ctx-px-list ctx))
        (cpy (graph-fa2-ctx-py-list ctx))
        (cvx (graph-fa2-ctx-vx-list ctx))
        (cvy (graph-fa2-ctx-vy-list ctx))
        (i 0))
    (while (< i len)
      (setcar (graph-fa2--cons cpx) (aref pos-x i))
      (setcar (graph-fa2--cons cpy) (aref pos-y i))
      (setcar (graph-fa2--cons cvx) (aref vel-x i))
      (setcar (graph-fa2--cons cvy) (aref vel-y i))
      (setq cpx (cdr (graph-fa2--cons cpx))
            cpy (cdr (graph-fa2--cons cpy))
            cvx (cdr (graph-fa2--cons cvx))
            cvy (cdr (graph-fa2--cons cvy))
            i (1+ (graph-fa2--fixnum i))))))

(defun graph-fa2--store-spines-2d (ctx len total-nodes)
  "Write 2D spines in CTX back to LEN vector slots and TOTAL-NODES structs."
  (declare (speed 3) (safety 0))
  (let ((nodes (graph-fa2-ctx-nodes ctx))
        (pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (pos-z (graph-fa2-ctx-pos-z ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx))
        (rep-x (graph-fa2-ctx-rep-x ctx))
        (rep-y (graph-fa2-ctx-rep-y ctx))
        (cpx (graph-fa2-ctx-px-list ctx))
        (cpy (graph-fa2-ctx-py-list ctx))
        (cvx (graph-fa2-ctx-vx-list ctx))
        (cvy (graph-fa2-ctx-vy-list ctx))
        (crx (graph-fa2-ctx-rx-list ctx))
        (cry (graph-fa2-ctx-ry-list ctx))
        (i 0))
    (while (< i len)
      (let ((px (car (graph-fa2--cons cpx)))
            (py (car (graph-fa2--cons cpy)))
            (vx (car (graph-fa2--cons cvx)))
            (vy (car (graph-fa2--cons cvy)))
            (n (aref nodes i)))
        (aset pos-x i px)
        (aset pos-y i py)
        (aset vel-x i vx)
        (aset vel-y i vy)
        (aset rep-x i (car (graph-fa2--cons crx)))
        (aset rep-y i (car (graph-fa2--cons cry)))
        (graph-fa2-set-x n px)
        (graph-fa2-set-y n py)
        (graph-fa2-set-z n (aref pos-z i))
        (graph-fa2-set-dx n vx)
        (graph-fa2-set-dy n vy)
        (graph-fa2-set-dz n (aref vel-z i)))
      (setq cpx (cdr (graph-fa2--cons cpx))
            cpy (cdr (graph-fa2--cons cpy))
            cvx (cdr (graph-fa2--cons cvx))
            cvy (cdr (graph-fa2--cons cvy))
            crx (cdr (graph-fa2--cons crx))
            cry (cdr (graph-fa2--cons cry))
            i (1+ (graph-fa2--fixnum i))))
    (while (< i total-nodes)
      (let ((n (aref nodes i)))
        (graph-fa2-set-x n (aref pos-x i))
        (graph-fa2-set-y n (aref pos-y i))
        (graph-fa2-set-z n (aref pos-z i))
        (graph-fa2-set-dx n (aref vel-x i))
        (graph-fa2-set-dy n (aref vel-y i))
        (graph-fa2-set-dz n (aref vel-z i)))
      (setq i (1+ (graph-fa2--fixnum i))))))

(defun graph-fa2--render-empty (ctx)
  "Render zero-node SVG contents into CTX."
  (let ((gc-cons-threshold most-positive-fixnum))
    (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
      (insert "<FRAME_SPLIT>\n"))))

(defun graph-fa2-compose (&rest funcs)
  "Return a function that threads an argument through FUNCS left-to-right.
Allows treating a chain of mutating functions as a single pipeline."
  (lambda (ctx)
    (seq-reduce (lambda (current-ctx fn) (funcall fn current-ctx))
                funcs
                ctx)))

(defun graph-fa2--2d-repulsion-cons (a px-l py-l rx-l ry-l mrows-l)
  "Compute 2D repulsion with scaling A across node coordinate spines.
Uses position spines PX-L and PY-L, force spines RX-L and RY-L,
and upper-triangular mass rows MROWS-L."
  (declare (speed 3) (safety 0))
  (let* ((xy-th graph-fa2-repulsion-x-y-threshold)
         (neg-xy-th (- (graph-fa2--fixnum xy-th)))
         (rep-th graph-fa2-repulsion-threshold)
         (rep-max-sq graph-fa2-repulsion-max-dist-sq))
    (let ((rx rx-l)
          (ry ry-l))
      (while rx
        (setcar (graph-fa2--cons rx) 0)
        (setcar (graph-fa2--cons ry) 0)
        (setq rx (cdr (graph-fa2--cons rx))
              ry (cdr (graph-fa2--cons ry)))))
    (let ((pxi px-l)
          (pyi py-l)
          (rxi-c rx-l)
          (ryi-c ry-l)
          (mrow mrows-l))
      (while pxi
        (let ((nix (car (graph-fa2--cons pxi)))
              (niy (car (graph-fa2--cons pyi)))
              (rxi (car (graph-fa2--cons rxi-c)))
              (ryi (car (graph-fa2--cons ryi-c)))
              (pxj (cdr (graph-fa2--cons pxi)))
              (pyj (cdr (graph-fa2--cons pyi)))
              (rxj (cdr (graph-fa2--cons rxi-c)))
              (ryj (cdr (graph-fa2--cons ryi-c)))
              (mj (car (graph-fa2--cons mrow))))
          (while pxj
            (let ((dx (- nix (car (graph-fa2--cons pxj)))))
              (when (< neg-xy-th dx xy-th)
                (let ((dy (- niy (car (graph-fa2--cons pyj)))))
                  (when (< neg-xy-th dy xy-th)
                    (let* ((raw-sq (+ (* dx dx) (* dy dy)))
                           (dist-sq (if (< raw-sq rep-th) rep-th raw-sq)))
                      (when (< dist-sq rep-max-sq)
                        (let* ((dist (graph-fa2--dist-2d dx dy))
                               (num (ash (* a (car (graph-fa2--cons mj))) 16))
                               (den (* dist dist-sq))
                               (fdx (/ (* dx num) den))
                               (fdy (/ (* dy num) den)))
                          (setq rxi (+ rxi fdx)
                                ryi (+ ryi fdy))
                          (setcar (graph-fa2--cons rxj)
                                  (- (car (graph-fa2--cons rxj)) fdx))
                          (setcar (graph-fa2--cons ryj)
                                  (- (car (graph-fa2--cons ryj)) fdy)))))))))
            (setq pxj (cdr (graph-fa2--cons pxj))
                  pyj (cdr (graph-fa2--cons pyj))
                  rxj (cdr (graph-fa2--cons rxj))
                  ryj (cdr (graph-fa2--cons ryj))
                  mj (cdr (graph-fa2--cons mj))))
          (setcar (graph-fa2--cons rxi-c) rxi)
          (setcar (graph-fa2--cons ryi-c) ryi)
          (setq pxi (cdr (graph-fa2--cons pxi))
                pyi (cdr (graph-fa2--cons pyi))
                rxi-c (cdr (graph-fa2--cons rxi-c))
                ryi-c (cdr (graph-fa2--cons ryi-c))
                mrow (cdr (graph-fa2--cons mrow))))))))

(defun graph-fa2--2d-attract-cons (a linked-edges)
  "Apply 2D edge attraction with scaling A across LINKED-EDGES."
  (declare (speed 3) (safety 0))
  (let ((attr-th graph-fa2-attraction-threshold)
        (el linked-edges))
    (while el
      (let* ((ed (car (graph-fa2--cons el)))
             (p1 (car (graph-fa2--cons ed)))
             (d1 (cdr (graph-fa2--cons ed)))
             (p2 (car (graph-fa2--cons d1)))
             (d2 (cdr (graph-fa2--cons d1)))
             (v1 (car (graph-fa2--cons d2)))
             (v2 (cdr (graph-fa2--cons d2)))
             (upx (car (graph-fa2--cons p1)))
             (vpx (cdr (graph-fa2--cons p1)))
             (upy (car (graph-fa2--cons p2)))
             (vpy (cdr (graph-fa2--cons p2)))
             (uvx (car (graph-fa2--cons v1)))
             (vvx (cdr (graph-fa2--cons v1)))
             (uvy (car (graph-fa2--cons v2)))
             (vvy (cdr (graph-fa2--cons v2)))
             (dx (- (car (graph-fa2--cons upx)) (car (graph-fa2--cons vpx))))
             (dy (- (car (graph-fa2--cons upy)) (car (graph-fa2--cons vpy))))
             (dist (graph-fa2--dist-2d dx dy))
             (num (* a (- dist attr-th)))
             (den (ash dist 16))
             (fdx (/ (* dx num) den))
             (fdy (/ (* dy num) den)))
        (setcar (graph-fa2--cons uvx) (- (car (graph-fa2--cons uvx)) fdx))
        (setcar (graph-fa2--cons vvx) (+ (car (graph-fa2--cons vvx)) fdx))
        (setcar (graph-fa2--cons uvy) (- (car (graph-fa2--cons uvy)) fdy))
        (setcar (graph-fa2--cons vvy) (+ (car (graph-fa2--cons vvy)) fdy))
        (setq el (cdr (graph-fa2--cons el)))))))

(defun graph-fa2--2d-integrate-cons (a px-l py-l vx-l vy-l rx-l ry-l mass-l)
  "Fused 2D repulsion application and integration with scaling A.
Operates on spines PX-L, PY-L, VX-L, VY-L, RX-L, RY-L, and MASS-L."
  (declare (speed 3) (safety 0))
  (let ((v-max graph-fa2-speed-limit-threshold)
        (horizon graph-fa2-horizon-threshold)
        (horizon-start graph-fa2-horizon-start-threshold)
        (px px-l)
        (py py-l)
        (vx-c vx-l)
        (vy-c vy-l)
        (rx-c rx-l)
        (ry-c ry-l)
        (mc mass-l))
    (while px
      (let* ((nx (car (graph-fa2--cons px)))
             (ny (car (graph-fa2--cons py)))
             (dist (graph-fa2--dist-2d nx ny))
             (num (* a (car (graph-fa2--cons mc))))
             (den (ash dist 8))
             (vx (- (+ (car (graph-fa2--cons vx-c)) (car (graph-fa2--cons rx-c)))
                    (/ (* nx num) den)))
             (vy (- (+ (car (graph-fa2--cons vy-c)) (car (graph-fa2--cons ry-c)))
                    (/ (* ny num) den)))
             (speed (graph-fa2--dist-2d vx vy)))
        (when (> speed 25)
          (let ((sden (+ speed v-max)))
            (setq vx (/ (* vx v-max) sden)
                  vy (/ (* vy v-max) sden))))
        (let* ((nnx (+ nx (ash vx -4)))
               (nny (+ ny (ash vy -4)))
               (ndist (graph-fa2--dist-2d nnx nny)))
          (when (> ndist horizon)
            (let ((cscale (/ (ash horizon 16) ndist)))
              (setq nnx (ash (* nnx cscale) -16)
                    nny (ash (* nny cscale) -16)
                    ndist horizon)))
          (setcar (graph-fa2--cons px) nnx)
          (setcar (graph-fa2--cons py) nny)
          (cond
           ((>= ndist horizon)
            (setcar (graph-fa2--cons vx-c) 0)
            (setcar (graph-fa2--cons vy-c) 0))
           ((> ndist horizon-start)
            (setcar (graph-fa2--cons vx-c) (- vx (ash vx -2)))
            (setcar (graph-fa2--cons vy-c) (- vy (ash vy -2))))
           (t
            (setcar (graph-fa2--cons vx-c) (- vx (ash vx -6)))
            (setcar (graph-fa2--cons vy-c) (- vy (ash vy -6)))))))
      (setq px (cdr (graph-fa2--cons px))
            py (cdr (graph-fa2--cons py))
            vx-c (cdr (graph-fa2--cons vx-c))
            vy-c (cdr (graph-fa2--cons vy-c))
            rx-c (cdr (graph-fa2--cons rx-c))
            ry-c (cdr (graph-fa2--cons ry-c))
            mc (cdr (graph-fa2--cons mc))))))

(defun graph-fa2--2d-pipe-compute-repulsion (ctx)
  "Compute 2D repulsion forces between active node pairs in CTX."
  (declare (speed 3) (safety 0))
  (if (graph-fa2-ctx-tick-rep-computed ctx)
      ctx
    (let* ((len (graph-fa2-ctx-tick-len ctx))
           (a (graph-fa2-ctx-tick-a ctx))
           (pos-x (graph-fa2-ctx-pos-x ctx))
           (pos-y (graph-fa2-ctx-pos-y ctx))
           (rep-x (graph-fa2-ctx-rep-x ctx))
           (rep-y (graph-fa2-ctx-rep-y ctx))
           (mass-matrix (graph-fa2-ctx-mass-matrix ctx))
           (total-nodes (length (graph-fa2-ctx-nodes ctx)))
           (xy-th graph-fa2-repulsion-x-y-threshold)
           (neg-xy-th (- (graph-fa2--fixnum xy-th)))
           (rep-th graph-fa2-repulsion-threshold)
           (rep-max-sq graph-fa2-repulsion-max-dist-sq)
           (i 0))
      (fillarray rep-x 0)
      (fillarray rep-y 0)
      (let ((gc-cons-threshold most-positive-fixnum))
        (while (< i len)
          (let ((nix (aref pos-x i))
                (niy (aref pos-y i))
                (rxi (aref rep-x i))
                (ryi (aref rep-y i))
                (i-offset (* i total-nodes))
                (j (1+ (graph-fa2--fixnum i))))
            (while (< j len)
              (let ((dx (- nix (aref pos-x j))))
                (when (< neg-xy-th dx xy-th)
                  (let ((dy (- niy (aref pos-y j))))
                    (when (< neg-xy-th dy xy-th)
                      (let* ((raw-sq (+ (* dx dx) (* dy dy)))
                             (dist-sq (if (< raw-sq rep-th) rep-th raw-sq)))
                        (when (< dist-sq rep-max-sq)
                          (let* ((dist (graph-fa2--dist-2d dx dy))
                                 (num (ash (* a (aref mass-matrix (+ i-offset j))) 16))
                                 (den (* dist dist-sq))
                                 (fdx (/ (* dx num) den))
                                 (fdy (/ (* dy num) den)))
                            (setq rxi (+ rxi fdx)
                                  ryi (+ ryi fdy))
                            (aset rep-x j (- (aref rep-x j) fdx))
                            (aset rep-y j (- (aref rep-y j) fdy)))))))))
              (setq j (1+ (graph-fa2--fixnum j))))
            (aset rep-x i rxi)
            (aset rep-y i ryi)
            (setq i (1+ (graph-fa2--fixnum i)))))))
    (setf (graph-fa2-ctx-tick-rep-computed ctx) t)
    ctx))

(defun graph-fa2--2d-pipe-apply-repulsion (ctx)
  "Apply accumulated 2D repulsion forces in CTX."
  (declare (speed 3) (safety 0))
  (let ((len (graph-fa2-ctx-tick-len ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (rep-x (graph-fa2-ctx-rep-x ctx))
        (rep-y (graph-fa2-ctx-rep-y ctx))
        (i 0))
    (while (< i len)
      (aset vel-x i (+ (aref vel-x i) (aref rep-x i)))
      (aset vel-y i (+ (aref vel-y i) (aref rep-y i)))
      (setq i (1+ (graph-fa2--fixnum i))))
    ctx))

(defun graph-fa2--2d-pipe-apply-attraction (ctx)
  "Calculate and apply edge-based 2D attraction forces in CTX."
  (declare (speed 3) (safety 0))
  (let ((len (graph-fa2-ctx-tick-len ctx))
        (a (graph-fa2-ctx-tick-a ctx))
        (attr-th graph-fa2-attraction-threshold)
        (pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (edges (graph-fa2-ctx-edges ctx)))
    (dolist (edge edges)
      (let ((u (if (vectorp edge) (aref edge 0) (car edge)))
            (v (if (vectorp edge) (aref edge 1) (cdr edge))))
        (when (and (< u len) (< v len) (not (eq u v)))
          (let* ((dx (- (aref pos-x u) (aref pos-x v)))
                 (dy (- (aref pos-y u) (aref pos-y v)))
                 (dist (graph-fa2--dist-2d dx dy))
                 (num (* a (- dist attr-th)))
                 (den (ash dist 16))
                 (fdx (/ (* dx num) den))
                 (fdy (/ (* dy num) den)))
            (aset vel-x u (- (aref vel-x u) fdx))
            (aset vel-y u (- (aref vel-y u) fdy))
            (aset vel-x v (+ (aref vel-x v) fdx))
            (aset vel-y v (+ (aref vel-y v) fdy))))))
    ctx))

(defun graph-fa2--2d-pipe-integrate (ctx)
  "Process gravity, speed limits, integrate positions, and cull nodes in CTX."
  (declare (speed 3) (safety 0))
  (let ((len (graph-fa2-ctx-tick-len ctx))
        (a (graph-fa2-ctx-tick-a ctx))
        (v-max graph-fa2-speed-limit-threshold)
        (horizon graph-fa2-horizon-threshold)
        (horizon-start graph-fa2-horizon-start-threshold)
        (pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (nodes (graph-fa2-ctx-nodes ctx))
        (i 0))
    (while (< i len)
      (let* ((nx (aref pos-x i))
             (ny (aref pos-y i))
             (dist (graph-fa2--dist-2d nx ny))
             (mass (graph-fa2-mass (aref nodes i)))
             (num (* a mass))
             (den (ash dist 8))
             (vx (- (aref vel-x i) (/ (* nx num) den)))
             (vy (- (aref vel-y i) (/ (* ny num) den)))
             (speed (graph-fa2--dist-2d vx vy)))
        (when (> speed 25)
          (let ((sden (+ speed v-max)))
            (setq vx (/ (* vx v-max) sden)
                  vy (/ (* vy v-max) sden))))
        (let* ((new-nx (+ nx (ash vx -4)))
               (new-ny (+ ny (ash vy -4)))
               (new-dist (graph-fa2--dist-2d new-nx new-ny)))
          (when (> new-dist horizon)
            (let ((clamp-scale (/ (ash horizon 16) new-dist)))
              (setq new-nx (ash (* new-nx clamp-scale) -16)
                    new-ny (ash (* new-ny clamp-scale) -16)
                    new-dist horizon)))
          (aset pos-x i new-nx)
          (aset pos-y i new-ny)
          (cond
           ((>= new-dist horizon)
            (aset vel-x i 0)
            (aset vel-y i 0))
           ((> new-dist horizon-start)
            (aset vel-x i (- vx (ash vx -2)))
            (aset vel-y i (- vy (ash vy -2))))
           (t
            (aset vel-x i (- vx (ash vx -6)))
            (aset vel-y i (- vy (ash vy -6)))))))
      (setq i (1+ (graph-fa2--fixnum i))))
    ctx))

(defvar graph-fa2-2d-repulsion-block
  (graph-fa2-compose #'graph-fa2--2d-pipe-compute-repulsion
                     #'graph-fa2--2d-pipe-apply-repulsion)
  "Pipeline block for 2D repulsion calculation and application.")

(defconst graph-fa2--default-2d-pipeline
  (graph-fa2-compose graph-fa2-2d-repulsion-block
                     #'graph-fa2--2d-pipe-apply-attraction
                     #'graph-fa2--2d-pipe-integrate)
  "Default 2D pipeline closure used to detect custom pipeline overrides.")

(defvar graph-fa2-2d-pipeline graph-fa2--default-2d-pipeline
  "The complete ForceAtlas2 2D simulation pipeline.")

(defun graph-fa2--3d-repulsion-cons (a px-l py-l pz-l rx-l ry-l rz-l mrows-l)
  "Compute 3D repulsion with scaling A across node coordinate spines.
Uses position spines PX-L, PY-L, and PZ-L, force spines RX-L, RY-L,
and RZ-L, and upper-triangular mass rows MROWS-L."
  (declare (speed 3) (safety 0))
  (let* ((xy-th graph-fa2-repulsion-x-y-threshold)
         (neg-xy-th (- (graph-fa2--fixnum xy-th)))
         (rep-th graph-fa2-repulsion-threshold)
         (rep-max-sq graph-fa2-repulsion-max-dist-sq))
    (let ((rx rx-l)
          (ry ry-l)
          (rz rz-l))
      (while rx
        (setcar (graph-fa2--cons rx) 0)
        (setcar (graph-fa2--cons ry) 0)
        (setcar (graph-fa2--cons rz) 0)
        (setq rx (cdr (graph-fa2--cons rx))
              ry (cdr (graph-fa2--cons ry))
              rz (cdr (graph-fa2--cons rz)))))
    (let ((pxi px-l)
          (pyi py-l)
          (pzi pz-l)
          (rxi-c rx-l)
          (ryi-c ry-l)
          (rzi-c rz-l)
          (mrow mrows-l))
      (while pxi
        (let ((nix (car (graph-fa2--cons pxi)))
              (niy (car (graph-fa2--cons pyi)))
              (niz (car (graph-fa2--cons pzi)))
              (rxi (car (graph-fa2--cons rxi-c)))
              (ryi (car (graph-fa2--cons ryi-c)))
              (rzi (car (graph-fa2--cons rzi-c)))
              (pxj (cdr (graph-fa2--cons pxi)))
              (pyj (cdr (graph-fa2--cons pyi)))
              (pzj (cdr (graph-fa2--cons pzi)))
              (rxj (cdr (graph-fa2--cons rxi-c)))
              (ryj (cdr (graph-fa2--cons ryi-c)))
              (rzj (cdr (graph-fa2--cons rzi-c)))
              (mj (car (graph-fa2--cons mrow))))
          (while pxj
            (let ((dx (- nix (car (graph-fa2--cons pxj)))))
              (when (< neg-xy-th dx xy-th)
                (let ((dy (- niy (car (graph-fa2--cons pyj)))))
                  (when (< neg-xy-th dy xy-th)
                    (let ((dz (- niz (car (graph-fa2--cons pzj)))))
                      (when (< neg-xy-th dz xy-th)
                        (let* ((raw-sq (+ (* dx dx) (* dy dy) (* dz dz)))
                               (dist-sq (max 256 (if (< raw-sq rep-th) rep-th raw-sq))))
                          (when (< dist-sq rep-max-sq)
                            (let* ((ax (abs dx))
                                   (ay (abs dy))
                                   (az (abs dz))
                                   (mx (max 16 ax ay az))
                                   (dist (+ mx (ash (- (+ ax ay az) mx) -2)))
                                   (num (ash (* a (car (graph-fa2--cons mj))) 16))
                                   (den (* dist dist-sq))
                                   (fdx (/ (* dx num) den))
                                   (fdy (/ (* dy num) den))
                                   (fdz (/ (* dz num) den)))
                              (setq rxi (+ rxi fdx)
                                    ryi (+ ryi fdy)
                                    rzi (+ rzi fdz))
                              (setcar (graph-fa2--cons rxj)
                                      (- (car (graph-fa2--cons rxj)) fdx))
                              (setcar (graph-fa2--cons ryj)
                                      (- (car (graph-fa2--cons ryj)) fdy))
                              (setcar (graph-fa2--cons rzj)
                                      (- (car (graph-fa2--cons rzj)) fdz)))))))))))
            (setq pxj (cdr (graph-fa2--cons pxj))
                  pyj (cdr (graph-fa2--cons pyj))
                  pzj (cdr (graph-fa2--cons pzj))
                  rxj (cdr (graph-fa2--cons rxj))
                  ryj (cdr (graph-fa2--cons ryj))
                  rzj (cdr (graph-fa2--cons rzj))
                  mj (cdr (graph-fa2--cons mj))))
          (setcar (graph-fa2--cons rxi-c) rxi)
          (setcar (graph-fa2--cons ryi-c) ryi)
          (setcar (graph-fa2--cons rzi-c) rzi)
          (setq pxi (cdr (graph-fa2--cons pxi))
                pyi (cdr (graph-fa2--cons pyi))
                pzi (cdr (graph-fa2--cons pzi))
                rxi-c (cdr (graph-fa2--cons rxi-c))
                ryi-c (cdr (graph-fa2--cons ryi-c))
                rzi-c (cdr (graph-fa2--cons rzi-c))
                mrow (cdr (graph-fa2--cons mrow))))))))

(defun graph-fa2--3d-attract-cons (a linked-edges)
  "Apply 3D edge attraction with scaling A across LINKED-EDGES."
  (declare (speed 3) (safety 0))
  (let ((attr-th graph-fa2-attraction-threshold)
        (el linked-edges))
    (while el
      (let* ((ed (car (graph-fa2--cons el)))
             (p1 (car (graph-fa2--cons ed)))
             (d1 (cdr (graph-fa2--cons ed)))
             (p2 (car (graph-fa2--cons d1)))
             (d2 (cdr (graph-fa2--cons d1)))
             (p3 (car (graph-fa2--cons d2)))
             (d3 (cdr (graph-fa2--cons d2)))
             (v1 (car (graph-fa2--cons d3)))
             (d4 (cdr (graph-fa2--cons d3)))
             (v2 (car (graph-fa2--cons d4)))
             (v3 (cdr (graph-fa2--cons d4)))
             (upx (car (graph-fa2--cons p1)))
             (vpx (cdr (graph-fa2--cons p1)))
             (upy (car (graph-fa2--cons p2)))
             (vpy (cdr (graph-fa2--cons p2)))
             (upz (car (graph-fa2--cons p3)))
             (vpz (cdr (graph-fa2--cons p3)))
             (uvx (car (graph-fa2--cons v1)))
             (vvx (cdr (graph-fa2--cons v1)))
             (uvy (car (graph-fa2--cons v2)))
             (vvy (cdr (graph-fa2--cons v2)))
             (uvz (car (graph-fa2--cons v3)))
             (vvz (cdr (graph-fa2--cons v3)))
             (dx (- (car (graph-fa2--cons upx)) (car (graph-fa2--cons vpx))))
             (dy (- (car (graph-fa2--cons upy)) (car (graph-fa2--cons vpy))))
             (dz (- (car (graph-fa2--cons upz)) (car (graph-fa2--cons vpz))))
             (dist (graph-fa2--dist-3d dx dy dz))
             (num (* a (- dist attr-th)))
             (den (ash dist 16))
             (fdx (/ (* dx num) den))
             (fdy (/ (* dy num) den))
             (fdz (/ (* dz num) den)))
        (setcar (graph-fa2--cons uvx) (- (car (graph-fa2--cons uvx)) fdx))
        (setcar (graph-fa2--cons vvx) (+ (car (graph-fa2--cons vvx)) fdx))
        (setcar (graph-fa2--cons uvy) (- (car (graph-fa2--cons uvy)) fdy))
        (setcar (graph-fa2--cons vvy) (+ (car (graph-fa2--cons vvy)) fdy))
        (setcar (graph-fa2--cons uvz) (- (car (graph-fa2--cons uvz)) fdz))
        (setcar (graph-fa2--cons vvz) (+ (car (graph-fa2--cons vvz)) fdz))
        (setq el (cdr (graph-fa2--cons el)))))))

(defun graph-fa2--3d-integrate-cons
    (a progress-fp px-l py-l pz-l vx-l vy-l vz-l rx-l ry-l rz-l mass-l)
  "Fused 3D repulsion application and integration with A and PROGRESS-FP.
Operates on spines PX-L, PY-L, PZ-L, VX-L, VY-L, VZ-L, RX-L, RY-L,
RZ-L, and MASS-L."
  (declare (speed 3) (safety 0))
  (let* ((spd-lim graph-fa2-speed-limit-threshold)
         (max-vel graph-fa2-max-velocity)
         (neg-max-vel (- (graph-fa2--fixnum max-vel)))
         (horizon graph-fa2-horizon-threshold)
         (horizon-start graph-fa2-horizon-start-threshold)
         (constraint graph-fa2-surface-constraint)
         (scaled-rad (ash graph-fa2-surface-radius 8))
         (px px-l)
         (py py-l)
         (pz pz-l)
         (vx-c vx-l)
         (vy-c vy-l)
         (vz-c vz-l)
         (rx-c rx-l)
         (ry-c ry-l)
         (rz-c rz-l)
         (mc mass-l))
    (while px
      (let* ((nx (car (graph-fa2--cons px)))
             (ny (car (graph-fa2--cons py)))
             (nz (car (graph-fa2--cons pz)))
             (dist (graph-fa2--dist-3d nx ny nz))
             (num (* a (car (graph-fa2--cons mc))))
             (den (ash dist 8))
             (vx (- (+ (car (graph-fa2--cons vx-c)) (car (graph-fa2--cons rx-c)))
                    (/ (* nx num) den)))
             (vy (- (+ (car (graph-fa2--cons vy-c)) (car (graph-fa2--cons ry-c)))
                    (/ (* ny num) den)))
             (vz (- (+ (car (graph-fa2--cons vz-c)) (car (graph-fa2--cons rz-c)))
                    (/ (* nz num) den)))
             (vx (if (< -131072 vx 131072) vx (if (< vx 0) -131072 131072)))
             (vy (if (< -131072 vy 131072) vy (if (< vy 0) -131072 131072)))
             (vz (if (< -131072 vz 131072) vz (if (< vz 0) -131072 131072)))
             (speed (graph-fa2--dist-3d vx vy vz)))
        (when (> speed spd-lim)
          (let ((sden (+ speed spd-lim)))
            (setq vx (/ (* vx spd-lim) sden)
                  vy (/ (* vy spd-lim) sden)
                  vz (/ (* vz spd-lim) sden))))
        (let* ((vxd (ash vx -4))
               (vyd (ash vy -4))
               (vzd (ash vz -4))
               (vxd (if (< neg-max-vel vxd max-vel) vxd (if (< vxd 0) neg-max-vel max-vel)))
               (vyd (if (< neg-max-vel vyd max-vel) vyd (if (< vyd 0) neg-max-vel max-vel)))
               (vzd (if (< neg-max-vel vzd max-vel) vzd (if (< vzd 0) neg-max-vel max-vel)))
               (nnx (+ nx vxd))
               (nny (+ ny vyd))
               (nnz (+ nz vzd))
               (ndist (graph-fa2--dist-3d nnx nny nnz))
               (tdist ndist))
          (unless (eq constraint 'none)
            (cond
             ((eq constraint 'strict)
              (setq tdist scaled-rad))
             ((eq constraint 'floor)
              (when (< ndist scaled-rad)
                (setq tdist scaled-rad)))
             ((eq constraint 'ceiling)
              (when (> ndist scaled-rad)
                (setq tdist scaled-rad)))
             ((eq constraint 'anneal)
              (setq tdist
                    (ash (+ (* ndist (- 256 progress-fp))
                            (* scaled-rad progress-fp))
                         -8))))
            (when (not (eq tdist ndist))
              (let ((cscale (/ (ash tdist 16) ndist)))
                (setq nnx (ash (* nnx cscale) -16)
                      nny (ash (* nny cscale) -16)
                      nnz (ash (* nnz cscale) -16)
                      ndist tdist))))
          (when (> ndist horizon)
            (let ((cscale (/ (ash horizon 16) ndist)))
              (setq nnx (ash (* nnx cscale) -16)
                    nny (ash (* nny cscale) -16)
                    nnz (ash (* nnz cscale) -16)
                    ndist horizon)))
          (setcar (graph-fa2--cons px) nnx)
          (setcar (graph-fa2--cons py) nny)
          (setcar (graph-fa2--cons pz) nnz)
          (cond
           ((>= ndist horizon)
            (setcar (graph-fa2--cons vx-c) 0)
            (setcar (graph-fa2--cons vy-c) 0)
            (setcar (graph-fa2--cons vz-c) 0))
           ((> ndist horizon-start)
            (setcar (graph-fa2--cons vx-c) (- vx (ash vx -2)))
            (setcar (graph-fa2--cons vy-c) (- vy (ash vy -2)))
            (setcar (graph-fa2--cons vz-c) (- vz (ash vz -2))))
           (t
            (setcar (graph-fa2--cons vx-c) (- vx (ash vx -6)))
            (setcar (graph-fa2--cons vy-c) (- vy (ash vy -6)))
            (setcar (graph-fa2--cons vz-c) (- vz (ash vz -6)))))))
      (setq px (cdr (graph-fa2--cons px))
            py (cdr (graph-fa2--cons py))
            pz (cdr (graph-fa2--cons pz))
            vx-c (cdr (graph-fa2--cons vx-c))
            vy-c (cdr (graph-fa2--cons vy-c))
            vz-c (cdr (graph-fa2--cons vz-c))
            rx-c (cdr (graph-fa2--cons rx-c))
            ry-c (cdr (graph-fa2--cons ry-c))
            rz-c (cdr (graph-fa2--cons rz-c))
            mc (cdr (graph-fa2--cons mc))))))

(defun graph-fa2--3d-pipe-compute-repulsion (ctx)
  "Compute 3D repulsion between active node pairs in CTX."
  (declare (speed 3) (safety 0))
  (if (graph-fa2-ctx-tick-rep-computed ctx)
      ctx
    (let* ((len (graph-fa2-ctx-tick-len ctx))
           (a (graph-fa2-ctx-tick-a ctx))
           (pos-x (graph-fa2-ctx-pos-x ctx))
           (pos-y (graph-fa2-ctx-pos-y ctx))
           (pos-z (graph-fa2-ctx-pos-z ctx))
           (rep-x (graph-fa2-ctx-rep-x ctx))
           (rep-y (graph-fa2-ctx-rep-y ctx))
           (rep-z (graph-fa2-ctx-rep-z ctx))
           (mass-matrix (graph-fa2-ctx-mass-matrix ctx))
           (total-nodes (length (graph-fa2-ctx-nodes ctx)))
           (xy-th graph-fa2-repulsion-x-y-threshold)
           (neg-xy-th (- (graph-fa2--fixnum xy-th)))
           (rep-th graph-fa2-repulsion-threshold)
           (rep-max-sq graph-fa2-repulsion-max-dist-sq)
           (i 0))
      (fillarray rep-x 0)
      (fillarray rep-y 0)
      (fillarray rep-z 0)
      (let ((gc-cons-threshold most-positive-fixnum))
        (while (< i len)
          (let ((nix (aref pos-x i))
                (niy (aref pos-y i))
                (niz (aref pos-z i))
                (rxi (aref rep-x i))
                (ryi (aref rep-y i))
                (rzi (aref rep-z i))
                (i-offset (* i total-nodes))
                (j (1+ (graph-fa2--fixnum i))))
            (while (< j len)
              (let ((dx (- nix (aref pos-x j))))
                (when (< neg-xy-th dx xy-th)
                  (let ((dy (- niy (aref pos-y j))))
                    (when (< neg-xy-th dy xy-th)
                      (let ((dz (- niz (aref pos-z j))))
                        (when (< neg-xy-th dz xy-th)
                          (let* ((raw-sq (+ (* dx dx) (* dy dy) (* dz dz)))
                                 (dist-sq (max 256 (if (< raw-sq rep-th) rep-th raw-sq))))
                            (when (< dist-sq rep-max-sq)
                              (let* ((ax (abs dx))
                                     (ay (abs dy))
                                     (az (abs dz))
                                     (mx (max 16 ax ay az))
                                     (dist (+ mx (ash (- (+ ax ay az) mx) -2)))
                                     (num (ash (* a (aref mass-matrix (+ i-offset j))) 16))
                                     (den (* dist dist-sq))
                                     (fdx (/ (* dx num) den))
                                     (fdy (/ (* dy num) den))
                                     (fdz (/ (* dz num) den)))
                                (setq rxi (+ rxi fdx)
                                      ryi (+ ryi fdy)
                                      rzi (+ rzi fdz))
                                (aset rep-x j (- (aref rep-x j) fdx))
                                (aset rep-y j (- (aref rep-y j) fdy))
                                (aset rep-z j (- (aref rep-z j) fdz)))))))))))
              (setq j (1+ (graph-fa2--fixnum j))))
            (aset rep-x i rxi)
            (aset rep-y i ryi)
            (aset rep-z i rzi)
            (setq i (1+ (graph-fa2--fixnum i)))))))
    (setf (graph-fa2-ctx-tick-rep-computed ctx) t)
    ctx))

(defun graph-fa2--3d-pipe-apply-repulsion (ctx)
  "Apply accumulated 3D repulsion forces in CTX."
  (declare (speed 3) (safety 0))
  (let ((len (graph-fa2-ctx-tick-len ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx))
        (rep-x (graph-fa2-ctx-rep-x ctx))
        (rep-y (graph-fa2-ctx-rep-y ctx))
        (rep-z (graph-fa2-ctx-rep-z ctx))
        (i 0))
    (while (< i len)
      (graph-fa2-vec-add vel-x vel-y vel-z i
                         (aref rep-x i) (aref rep-y i) (aref rep-z i))
      (setq i (1+ (graph-fa2--fixnum i))))
    ctx))

(defun graph-fa2--3d-pipe-apply-attraction (ctx)
  "Calculate and apply edge-based 3D attraction forces in CTX."
  (declare (speed 3) (safety 0))
  (let ((len (graph-fa2-ctx-tick-len ctx))
        (a (graph-fa2-ctx-tick-a ctx))
        (attr-th graph-fa2-attraction-threshold)
        (pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (pos-z (graph-fa2-ctx-pos-z ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx))
        (edges (graph-fa2-ctx-edges ctx)))
    (dolist (edge edges)
      (let ((u (if (vectorp edge) (aref edge 0) (car edge)))
            (v (if (vectorp edge) (aref edge 1) (cdr edge))))
        (when (and (< u len) (< v len) (not (eq u v)))
          (let* ((dx (- (aref pos-x u) (aref pos-x v)))
                 (dy (- (aref pos-y u) (aref pos-y v)))
                 (dz (- (aref pos-z u) (aref pos-z v)))
                 (dist (graph-fa2--dist-3d dx dy dz))
                 (num (* a (- dist attr-th)))
                 (den (ash dist 16))
                 (fdx (/ (* dx num) den))
                 (fdy (/ (* dy num) den))
                 (fdz (/ (* dz num) den)))
            (graph-fa2-vec-sub vel-x vel-y vel-z u fdx fdy fdz)
            (graph-fa2-vec-add vel-x vel-y vel-z v fdx fdy fdz))))))
  ctx)

(defun graph-fa2--3d-pipe-integrate (ctx)
  "Process gravity, speed limits, integrate, and manage boundaries in CTX."
  (declare (speed 3) (safety 0))
  (let* ((len (graph-fa2-ctx-tick-len ctx))
         (a (graph-fa2-ctx-tick-a ctx))
         (progress-fp (graph-fa2-ctx-tick-progress ctx))
         (speed-limit graph-fa2-speed-limit-threshold)
         (max-vel graph-fa2-max-velocity)
         (horizon graph-fa2-horizon-threshold)
         (horizon-start graph-fa2-horizon-start-threshold)
         (constraint graph-fa2-surface-constraint)
         (scaled-radius (ash graph-fa2-surface-radius 8))
         (pos-x (graph-fa2-ctx-pos-x ctx))
         (pos-y (graph-fa2-ctx-pos-y ctx))
         (pos-z (graph-fa2-ctx-pos-z ctx))
         (vel-x (graph-fa2-ctx-vel-x ctx))
         (vel-y (graph-fa2-ctx-vel-y ctx))
         (vel-z (graph-fa2-ctx-vel-z ctx))
         (nodes (graph-fa2-ctx-nodes ctx))
         (i 0))
    (while (< i len)
      (let* ((nx (aref pos-x i))
             (ny (aref pos-y i))
             (nz (aref pos-z i))
             (dist (graph-fa2--dist-3d nx ny nz))
             (mass (graph-fa2-mass (aref nodes i)))
             (num (* a mass))
             (den (ash dist 8))
             (vx (graph-fa2-clamp (- (aref vel-x i) (/ (* nx num) den)) 131072))
             (vy (graph-fa2-clamp (- (aref vel-y i) (/ (* ny num) den)) 131072))
             (vz (graph-fa2-clamp (- (aref vel-z i) (/ (* nz num) den)) 131072))
             (speed (graph-fa2--dist-3d vx vy vz)))
        (when (> speed speed-limit)
          (let ((sden (+ speed speed-limit)))
            (setq vx (/ (* vx speed-limit) sden)
                  vy (/ (* vy speed-limit) sden)
                  vz (/ (* vz speed-limit) sden))))
        (let* ((vx-disp (graph-fa2-clamp (ash vx -4) max-vel))
               (vy-disp (graph-fa2-clamp (ash vy -4) max-vel))
               (vz-disp (graph-fa2-clamp (ash vz -4) max-vel))
               (new-nx (+ nx vx-disp))
               (new-ny (+ ny vy-disp))
               (new-nz (+ nz vz-disp))
               (new-dist (graph-fa2--dist-3d new-nx new-ny new-nz))
               (target-dist new-dist))
          (unless (eq constraint 'none)
            (cond
             ((eq constraint 'strict)
              (setq target-dist scaled-radius))
             ((eq constraint 'floor)
              (when (< new-dist scaled-radius)
                (setq target-dist scaled-radius)))
             ((eq constraint 'ceiling)
              (when (> new-dist scaled-radius)
                (setq target-dist scaled-radius)))
             ((eq constraint 'anneal)
              (setq target-dist
                    (ash (+ (* new-dist (- 256 progress-fp))
                            (* scaled-radius progress-fp))
                         -8))))
            (when (not (eq target-dist new-dist))
              (let ((clamp-scale (/ (ash target-dist 16) new-dist)))
                (setq new-nx (ash (* new-nx clamp-scale) -16)
                      new-ny (ash (* new-ny clamp-scale) -16)
                      new-nz (ash (* new-nz clamp-scale) -16)
                      new-dist target-dist))))
          (when (> new-dist horizon)
            (let ((clamp-scale (/ (ash horizon 16) new-dist)))
              (setq new-nx (ash (* new-nx clamp-scale) -16)
                    new-ny (ash (* new-ny clamp-scale) -16)
                    new-nz (ash (* new-nz clamp-scale) -16)
                    new-dist horizon)))
          (aset pos-x i new-nx)
          (aset pos-y i new-ny)
          (aset pos-z i new-nz)
          (cond
           ((>= new-dist horizon)
            (aset vel-x i 0)
            (aset vel-y i 0)
            (aset vel-z i 0))
           ((> new-dist horizon-start)
            (aset vel-x i (- vx (ash vx -2)))
            (aset vel-y i (- vy (ash vy -2)))
            (aset vel-z i (- vz (ash vz -2))))
           (t
            (aset vel-x i (- vx (ash vx -6)))
            (aset vel-y i (- vy (ash vy -6)))
            (aset vel-z i (- vz (ash vz -6)))))))
      (setq i (1+ (graph-fa2--fixnum i))))
    ctx))

(defvar graph-fa2-3d-repulsion-block
  (graph-fa2-compose #'graph-fa2--3d-pipe-compute-repulsion
                     #'graph-fa2--3d-pipe-apply-repulsion)
  "Pipeline block for 3D repulsion calculation and application.")

(defconst graph-fa2--default-3d-pipeline
  (graph-fa2-compose graph-fa2-3d-repulsion-block
                     #'graph-fa2--3d-pipe-apply-attraction
                     #'graph-fa2--3d-pipe-integrate)
  "Default 3D pipeline closure used to detect custom pipeline overrides.")

(defvar graph-fa2-3d-pipeline graph-fa2--default-3d-pipeline
  "The complete ForceAtlas2 3D simulation pipeline.")

(defun graph-fa2--sync-nodes (ctx total-nodes)
  "Sync tracking arrays in CTX for TOTAL-NODES with internal node structs."
  (declare (speed 3) (safety 0))
  (let ((nodes (graph-fa2-ctx-nodes ctx))
        (pos-x (graph-fa2-ctx-pos-x ctx))
        (pos-y (graph-fa2-ctx-pos-y ctx))
        (pos-z (graph-fa2-ctx-pos-z ctx))
        (vel-x (graph-fa2-ctx-vel-x ctx))
        (vel-y (graph-fa2-ctx-vel-y ctx))
        (vel-z (graph-fa2-ctx-vel-z ctx))
        (i 0))
    (while (< i total-nodes)
      (let ((n (aref nodes i)))
        (graph-fa2-set-x n (aref pos-x i))
        (graph-fa2-set-y n (aref pos-y i))
        (graph-fa2-set-z n (aref pos-z i))
        (graph-fa2-set-dx n (aref vel-x i))
        (graph-fa2-set-dy n (aref vel-y i))
        (graph-fa2-set-dz n (aref vel-z i)))
      (setq i (1+ (graph-fa2--fixnum i))))))

(defun graph-fa2--ensure-render-cache (ctx total-nodes)
  "Ensure coordinate normalisation vectors and static SVG caches exist in CTX.
Allocates vectors of length TOTAL-NODES when absent or resized."
  (unless (and (graph-fa2-ctx-norm-x ctx)
               (= (length (graph-fa2-ctx-norm-x ctx)) total-nodes))
    (setf (graph-fa2-ctx-last-pos-x ctx) (make-vector total-nodes nil))
    (setf (graph-fa2-ctx-last-pos-y ctx) (make-vector total-nodes nil))
    (setf (graph-fa2-ctx-last-pos-z ctx) (make-vector total-nodes nil))
    (setf (graph-fa2-ctx-norm-x ctx) (make-vector total-nodes 0.0))
    (setf (graph-fa2-ctx-norm-y ctx) (make-vector total-nodes 0.0))
    (setf (graph-fa2-ctx-norm-z ctx) (make-vector total-nodes 0.0))
    (setf (graph-fa2-ctx-norm-x-str ctx) (make-vector total-nodes ""))
    (setf (graph-fa2-ctx-norm-y-str ctx) (make-vector total-nodes ""))
    (setf (graph-fa2-ctx-norm-z-str ctx) (make-vector total-nodes ""))
    (setf (graph-fa2-ctx-circle-strs ctx) (make-vector total-nodes nil))
    (setf (graph-fa2-ctx-label-strs ctx) (make-vector total-nodes nil)))
  (unless (and (graph-fa2-ctx-escaped-ids ctx)
               (= (length (graph-fa2-ctx-escaped-ids ctx)) total-nodes))
    (let ((nodes (graph-fa2-ctx-nodes ctx))
          (escaped-ids (make-vector total-nodes ""))
          (radius-strs (make-vector total-nodes ""))
          (wrapped-lines (make-vector total-nodes nil)))
      (dotimes (i total-nodes)
        (when-let* ((n (aref nodes i)))
          (let* ((id (graph-fa2-id n))
                 (label (graph-fa2-label n))
                 (radius (graph-fa2-radius n))
                 (id-str (if (stringp id) id (format "%s" id)))
                 (label-str (if (stringp label) label (format "%s" (or label id-str))))
                 (name-escaped (graph-fa2--escape-xml label-str)))
            (aset escaped-ids i (graph-fa2--escape-xml id-str))
            (aset radius-strs i (format "%.3f" radius))
            (aset wrapped-lines i
                  (graph-fa2--wrap-text name-escaped graph-fa2-label-wrap-chars)))))
      (setf (graph-fa2-ctx-escaped-ids ctx) escaped-ids)
      (setf (graph-fa2-ctx-radius-strs ctx) radius-strs)
      (setf (graph-fa2-ctx-wrapped-lines ctx) wrapped-lines))))

(defun graph-fa2--ensure-render-edges (ctx)
  "Build deduplicated render-only edge descriptors on CTX when edges change.
Collapses duplicate entries and bidirectional unlabelled undirected pairs
without mutating `graph-fa2-ctx-edges'."
  (let ((edges (graph-fa2-ctx-edges ctx)))
    (if (and (eq edges (graph-fa2-ctx-cached-render-edges-ref ctx))
             (or (null edges) (graph-fa2-ctx-render-edges ctx)))
        (graph-fa2-ctx-render-edges ctx)
      (let ((seen (make-hash-table :test #'equal))
            (dedup nil))
        (dolist (edge edges)
          (let* ((vecp (vectorp edge))
                 (vlen (if vecp (length edge) 0))
                 (u (if vecp (aref edge 0) (car edge)))
                 (v (if vecp (aref edge 1) (cdr edge)))
                 (raw-label (and vecp (> vlen 2) (aref edge 2)))
                 (colour (and vecp (> vlen 3) (aref edge 3)))
                 (width (and vecp (> vlen 4) (aref edge 4)))
                 (directed (and vecp (> vlen 5) (aref edge 5)))
                 (key (if (and (null directed)
                               (null raw-label)
                               (integerp u)
                               (integerp v)
                               (not (eq u v)))
                          (list (min u v) (max u v) nil colour width nil)
                        (list u v raw-label colour width directed))))
            (unless (gethash key seen)
              (puthash key t seen)
              (push (vector u v
                            (and raw-label
                                 (graph-fa2--escape-xml (format "%s" raw-label)))
                            colour
                            (and width (number-to-string width)))
                    dedup))))
        (let ((res (nreverse dedup)))
          (setf (graph-fa2-ctx-render-edges ctx) res)
          (setf (graph-fa2-ctx-cached-render-edges-ref ctx) edges)
          res)))))

(defun graph-fa2--render-svg (ctx len &optional omit-delimiter label-mode focus-idx)
  "Render layout integer arrays in CTX for LEN nodes into background buffer.
When OMIT-DELIMITER is non-nil, omits the trailing frame delimiter.
LABEL-MODE may be nil (all labels), t (no labels), or \\='focus\\=' (render only
the label at node index FOCUS-IDX)."
  (declare (speed 3) (safety 0))
  (when-let* ((nodes (graph-fa2-ctx-nodes ctx))
              (pos-x (graph-fa2-ctx-pos-x ctx))
              (pos-y (graph-fa2-ctx-pos-y ctx))
              (pos-z (graph-fa2-ctx-pos-z ctx)))
    (let* ((gc-cons-threshold most-positive-fixnum)
           (total-nodes (length nodes))
           (half-canvas (/ graph-fa2-canvas-size 2.0))
           (omit-all-labels (eq label-mode t))
           (focus-only (eq label-mode 'focus))
           (show-edge-labels (and (null label-mode) graph-fa2-show-edge-labels))
           (default-edge-colour graph-fa2-edge-colour)
           (default-edge-width-str (number-to-string graph-fa2-edge-width))
           (edge-label-font-size-str
            (when show-edge-labels
              (number-to-string graph-fa2-edge-label-font-size)))
           (edge-label-font-family-str
            (when show-edge-labels
              (or graph-fa2-edge-label-font-family
                  graph-fa2--default-font-family)))
           (label-font-fam (or graph-fa2-label-font-family
                               graph-fa2--default-font-family))
           (text-open-tag
            (unless omit-all-labels
              (if (and (graph-fa2-ctx-text-open-tag ctx)
                       (null graph-fa2-label-font-family)
                       (null graph-fa2-label-font-weight)
                       (equal graph-fa2-label-colour "#cdd6f4")
                       (eq graph-fa2-label-font-size 10))
                  (graph-fa2-ctx-text-open-tag ctx)
                (concat "  <text fill=\"" graph-fa2-label-colour
                        "\" font-size=\"" (number-to-string graph-fa2-label-font-size)
                        "\" font-family=\"" label-font-fam "\""
                        (if graph-fa2-label-font-weight
                            (concat " font-weight=\"" graph-fa2-label-font-weight "\"") "")
                        " text-anchor=\"middle\">\n"))))
           (line-height (max 1 (round (* graph-fa2-label-font-size 1.2))))
           (half-line-height (/ line-height 2))
           (offset-y graph-fa2-label-offset-y)
           (label-cfg (cons offset-y line-height)))
      (graph-fa2--ensure-render-cache ctx total-nodes)
      (let ((render-edges (graph-fa2--ensure-render-edges ctx))
            (last-pos-x (graph-fa2-ctx-last-pos-x ctx))
            (last-pos-y (graph-fa2-ctx-last-pos-y ctx))
            (last-pos-z (graph-fa2-ctx-last-pos-z ctx))
            (norm-x (graph-fa2-ctx-norm-x ctx))
            (norm-y (graph-fa2-ctx-norm-y ctx))
            (norm-z (graph-fa2-ctx-norm-z ctx))
            (norm-x-str (graph-fa2-ctx-norm-x-str ctx))
            (norm-y-str (graph-fa2-ctx-norm-y-str ctx))
            (norm-z-str (graph-fa2-ctx-norm-z-str ctx))
            (circle-strs (graph-fa2-ctx-circle-strs ctx))
            (label-strs (graph-fa2-ctx-label-strs ctx))
            (escaped-ids (graph-fa2-ctx-escaped-ids ctx))
            (radius-strs (graph-fa2-ctx-radius-strs ctx))
            (wrapped-lines (graph-fa2-ctx-wrapped-lines ctx)))
        (unless (equal label-cfg (graph-fa2-ctx-label-cache-key ctx))
          (fillarray label-strs nil)
          (setf (graph-fa2-ctx-label-cache-key ctx) label-cfg))
        (let ((i 0))
          (while (< i len)
            (let ((px (aref pos-x i))
                  (py (aref pos-y i))
                  (pz (aref pos-z i))
                  (pos-changed nil)
                  (xy-changed nil))
              (unless (eql px (aref last-pos-x i))
                (let ((fx (+ (* (float px) 0.00390625) half-canvas)))
                  (aset last-pos-x i px)
                  (aset norm-x i fx)
                  (aset norm-x-str i (format "%.3f" fx))
                  (setq pos-changed t
                        xy-changed t)))
              (unless (eql py (aref last-pos-y i))
                (let ((fy (+ (* (float py) 0.00390625) half-canvas)))
                  (aset last-pos-y i py)
                  (aset norm-y i fy)
                  (aset norm-y-str i (format "%.3f" fy))
                  (setq pos-changed t
                        xy-changed t)))
              (unless (eql pz (aref last-pos-z i))
                (let ((fz (+ (* (float pz) 0.00390625) half-canvas)))
                  (aset last-pos-z i pz)
                  (aset norm-z i fz)
                  (aset norm-z-str i (format "%.3f" fz))
                  (setq pos-changed t)))
              (when xy-changed
                (aset label-strs i nil))
              (when (or pos-changed (null (aref circle-strs i)))
                (when-let* ((n (aref nodes i)))
                  (aset circle-strs i
                        (concat "  <circle cx=\"" (aref norm-x-str i)
                                "\" cy=\"" (aref norm-y-str i)
                                "\" r=\"" (aref radius-strs i)
                                "\" fill=\"" (graph-fa2-colour n)
                                "\" data-name=\"" (aref escaped-ids i)
                                "\" data-depth=\"" (aref norm-z-str i) "\" />\n"))))
              (when (and (not omit-all-labels)
                         (or (not focus-only) (eq i focus-idx))
                         (null (aref label-strs i)))
                (if-let* ((n (aref nodes i))
                          (lines (aref wrapped-lines i)))
                    (let* ((nx (aref norm-x-str i))
                           (curr-y (- (aref norm-y i) offset-y
                                      (* (1- (length lines)) half-line-height)))
                           (acc nil))
                      (dolist (line lines)
                        (push (concat "    <tspan x=\"" nx "\" y=\""
                                      (format "%.3f" curr-y) "\">" line "</tspan>\n")
                              acc)
                        (setq curr-y (+ curr-y line-height)))
                      (aset label-strs i (apply #'concat (nreverse acc))))
                  (aset label-strs i ""))))
            (setq i (1+ (graph-fa2--fixnum i)))))
        (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
          (dolist (edge render-edges)
            (let ((u-idx (aref edge 0))
                  (v-idx (aref edge 1)))
              (when (and (< u-idx len) (< v-idx len))
                (let ((escaped-label (aref edge 2))
                      (edge-colour (or (aref edge 3) default-edge-colour))
                      (edge-width-str (or (aref edge 4) default-edge-width-str))
                      (ux-float (aref norm-x u-idx))
                      (uy-float (aref norm-y u-idx)))
                  (if (eq u-idx v-idx)
                      (let* ((u-node (aref nodes u-idx))
                             (u-radius (if u-node (graph-fa2-radius u-node) 10.0))
                             (p0x (- ux-float (* 0.7 u-radius)))
                             (p0y (- uy-float (* 0.7 u-radius)))
                             (c1x (- ux-float (* 1.8 u-radius)))
                             (c1y (- uy-float (* 2.5 u-radius)))
                             (c2x (+ ux-float (* 1.8 u-radius)))
                             (c2y (- uy-float (* 2.5 u-radius)))
                             (p3x (+ ux-float (* 0.7 u-radius)))
                             (p3y (- uy-float (* 0.7 u-radius))))
                        (insert (format "  <path d=\"M %.3f %.3f C %.3f %.3f, %.3f %.3f, %.3f %.3f\" stroke=\"%s\" stroke-width=\"%s\" fill=\"none\" />\n"
                                        p0x p0y c1x c1y c2x c2y p3x p3y
                                        edge-colour edge-width-str))
                        (when (and show-edge-labels escaped-label)
                          (let ((lbl-x ux-float)
                                (lbl-y (- uy-float (* 2.8 u-radius))))
                            (insert "  <text x=\"" (format "%.3f" lbl-x)
                                    "\" y=\"" (format "%.3f" lbl-y)
                                    "\" fill=\"" graph-fa2-edge-label-colour
                                    "\" font-size=\"" edge-label-font-size-str
                                    "\" font-family=\"" edge-label-font-family-str
                                    "\" text-anchor=\"middle\" dominant-baseline=\"central\""
                                    " paint-order=\"stroke\" stroke=\"#1e1e2e\" stroke-width=\"2px\">"
                                    escaped-label "</text>\n"))))
                    (let ((ux (aref norm-x-str u-idx))
                          (uy (aref norm-y-str u-idx))
                          (vx (aref norm-x-str v-idx))
                          (vy (aref norm-y-str v-idx)))
                      (insert "  <line x1=\"" ux "\" y1=\"" uy
                              "\" x2=\"" vx "\" y2=\"" vy
                              "\" stroke=\"" edge-colour
                              "\" stroke-width=\"" edge-width-str "\" />\n")
                      (when (and show-edge-labels escaped-label)
                        (let ((lbl-x (* (+ ux-float (aref norm-x v-idx)) 0.5))
                              (lbl-y (* (+ uy-float (aref norm-y v-idx)) 0.5)))
                          (insert "  <text x=\"" (format "%.3f" lbl-x)
                                  "\" y=\"" (format "%.3f" lbl-y)
                                  "\" fill=\"" graph-fa2-edge-label-colour
                                  "\" font-size=\"" edge-label-font-size-str
                                  "\" font-family=\"" edge-label-font-family-str
                                  "\" text-anchor=\"middle\" dominant-baseline=\"central\""
                                  " paint-order=\"stroke\" stroke=\"#1e1e2e\" stroke-width=\"2px\">"
                                  escaped-label "</text>\n")))))))))
          (let ((i 0))
            (while (< i len)
              (when-let* ((cs (aref circle-strs i)))
                (insert cs))
              (setq i (1+ (graph-fa2--fixnum i)))))
          (unless omit-all-labels
            (if focus-only
                (when (and focus-idx (>= focus-idx 0) (< focus-idx len))
                  (when-let* ((ls (aref label-strs focus-idx)))
                    (unless (eq (length ls) 0)
                      (insert text-open-tag ls "  </text>\n"))))
              (when (> len 0)
                (insert text-open-tag)
                (let ((i 0))
                  (while (< i len)
                    (when-let* ((ls (aref label-strs i)))
                      (unless (eq (length ls) 0)
                        (insert ls)))
                    (setq i (1+ (graph-fa2--fixnum i)))))
                (insert "  </text>\n"))))
          (unless omit-delimiter
            (insert "<FRAME_SPLIT>\n")))))))

(defun graph-fa2--2d-tick-fast (ctx len a total-nodes)
  "Run fused 2D physics step on CTX for LEN nodes with A and TOTAL-NODES."
  (declare (speed 3) (safety 0))
  (graph-fa2--ensure-spines ctx)
  (graph-fa2--load-spines-2d ctx len)
  (let* ((px-l (graph-fa2-ctx-px-list ctx))
         (py-l (graph-fa2-ctx-py-list ctx))
         (vx-l (graph-fa2-ctx-vx-list ctx))
         (vy-l (graph-fa2-ctx-vy-list ctx))
         (rx-l (graph-fa2-ctx-rx-list ctx))
         (ry-l (graph-fa2-ctx-ry-list ctx))
         (mass-l (graph-fa2-ctx-mass-list ctx))
         (mrows (graph-fa2-ctx-mass-rows ctx))
         (all-edges (graph-fa2-ctx-phys-edges-2d ctx))
         (partial (< len total-nodes))
         (px-tail (when partial (graph-fa2--nth-cons px-l (1- len))))
         (rx-tail (when partial (graph-fa2--nth-cons rx-l (1- len))))
         (edge-tail
          (when partial
            (graph-fa2--active-edge-tail
             all-edges (graph-fa2-ctx-phys-edge-bounds ctx) len)))
         (active-edges (if partial (and edge-tail all-edges) all-edges))
         (saved-px-cdr (and px-tail (cdr px-tail)))
         (saved-rx-cdr (and rx-tail (cdr rx-tail)))
         (saved-edge-cdr (and edge-tail (cdr edge-tail))))
    (unwind-protect
        (progn
          (when px-tail (setcdr px-tail nil))
          (when rx-tail (setcdr rx-tail nil))
          (when edge-tail (setcdr edge-tail nil))
          (graph-fa2--2d-repulsion-cons a px-l py-l rx-l ry-l mrows)
          (let ((s graph-fa2-substeps))
            (while (> s 0)
              (when active-edges
                (graph-fa2--2d-attract-cons a active-edges))
              (graph-fa2--2d-integrate-cons a px-l py-l vx-l vy-l rx-l ry-l mass-l)
              (setq s (1- (graph-fa2--fixnum s)))))
          (setf (graph-fa2-ctx-tick-rep-computed ctx) t))
      (when px-tail (setcdr px-tail saved-px-cdr))
      (when rx-tail (setcdr rx-tail saved-rx-cdr))
      (when edge-tail (setcdr edge-tail saved-edge-cdr))))
  (graph-fa2--store-spines-2d ctx len total-nodes))

(defun graph-fa2--3d-tick-fast (ctx len a progress-fp total-nodes)
  "Run fused 3D step on CTX for LEN nodes with A, PROGRESS-FP, TOTAL-NODES."
  (declare (speed 3) (safety 0))
  (graph-fa2--ensure-spines ctx)
  (graph-fa2--load-spines-3d ctx len)
  (let* ((px-l (graph-fa2-ctx-px-list ctx))
         (py-l (graph-fa2-ctx-py-list ctx))
         (pz-l (graph-fa2-ctx-pz-list ctx))
         (vx-l (graph-fa2-ctx-vx-list ctx))
         (vy-l (graph-fa2-ctx-vy-list ctx))
         (vz-l (graph-fa2-ctx-vz-list ctx))
         (rx-l (graph-fa2-ctx-rx-list ctx))
         (ry-l (graph-fa2-ctx-ry-list ctx))
         (rz-l (graph-fa2-ctx-rz-list ctx))
         (mass-l (graph-fa2-ctx-mass-list ctx))
         (mrows (graph-fa2-ctx-mass-rows ctx))
         (all-edges (graph-fa2-ctx-phys-edges-3d ctx))
         (partial (< len total-nodes))
         (px-tail (when partial (graph-fa2--nth-cons px-l (1- len))))
         (rx-tail (when partial (graph-fa2--nth-cons rx-l (1- len))))
         (edge-tail
          (when partial
            (graph-fa2--active-edge-tail
             all-edges (graph-fa2-ctx-phys-edge-bounds ctx) len)))
         (active-edges (if partial (and edge-tail all-edges) all-edges))
         (saved-px-cdr (and px-tail (cdr px-tail)))
         (saved-rx-cdr (and rx-tail (cdr rx-tail)))
         (saved-edge-cdr (and edge-tail (cdr edge-tail))))
    (unwind-protect
        (progn
          (when px-tail (setcdr px-tail nil))
          (when rx-tail (setcdr rx-tail nil))
          (when edge-tail (setcdr edge-tail nil))
          (graph-fa2--3d-repulsion-cons a px-l py-l pz-l rx-l ry-l rz-l mrows)
          (let ((s graph-fa2-substeps))
            (while (> s 0)
              (when active-edges
                (graph-fa2--3d-attract-cons a active-edges))
              (graph-fa2--3d-integrate-cons
               a progress-fp px-l py-l pz-l vx-l vy-l vz-l rx-l ry-l rz-l mass-l)
              (setq s (1- (graph-fa2--fixnum s)))))
          (setf (graph-fa2-ctx-tick-rep-computed ctx) t))
      (when px-tail (setcdr px-tail saved-px-cdr))
      (when rx-tail (setcdr rx-tail saved-rx-cdr))
      (when edge-tail (setcdr edge-tail saved-edge-cdr))))
  (graph-fa2--store-spines-3d ctx len total-nodes))

(defun graph-fa2--2d-physics-tick (ctx max-frames)
  "Calculate ForceAtlas2 2D physics tick in CTX up to MAX-FRAMES."
  (let* ((nodes (graph-fa2-ctx-nodes ctx))
         (total-nodes (length nodes)))
    (if (eq total-nodes 0)
        (graph-fa2--render-empty ctx)
      (let* ((bg-frame (graph-fa2-ctx-bg-frame ctx))
             (len (if (< bg-frame 100)
                      (max 1 (truncate (* total-nodes (/ (float (1+ bg-frame)) 100.0))))
                    total-nodes))
             (a (max 2 (truncate (* 256.0 (- 1.0 (/ (float bg-frame) max-frames)))))))
        (setf (graph-fa2-ctx-tick-len ctx) len)
        (setf (graph-fa2-ctx-tick-a ctx) a)
        (setf (graph-fa2-ctx-tick-rep-computed ctx) nil)
        (let ((gc-cons-threshold most-positive-fixnum))
          (if (eq graph-fa2-2d-pipeline graph-fa2--default-2d-pipeline)
              (graph-fa2--2d-tick-fast ctx len a total-nodes)
            (dotimes (_ graph-fa2-substeps)
              (funcall graph-fa2-2d-pipeline ctx))
            (graph-fa2--sync-nodes ctx total-nodes)))
        (setf (graph-fa2-ctx-coords-synced ctx) nil)
        (graph-fa2--render-svg ctx len)))))

(defun graph-fa2--physics-tick (ctx max-frames)
  "Calculate ForceAtlas2 3D physics tick in CTX up to MAX-FRAMES."
  (if (eq graph-fa2-engine '2d)
      (graph-fa2--2d-physics-tick ctx max-frames)
    (let* ((nodes (graph-fa2-ctx-nodes ctx))
           (total-nodes (length nodes)))
      (if (eq total-nodes 0)
          (graph-fa2--render-empty ctx)
        (let*
            ((bg-frame (graph-fa2-ctx-bg-frame ctx))
             (len (if (< bg-frame 100)
                      (max 1 (truncate (* total-nodes (/ (float (1+ bg-frame)) 100.0))))
                    total-nodes))
             (a (max 2 (truncate (* 256.0 (- 1.0 (/ (float bg-frame) max-frames))))))
             (progress-fp
              (min 256 (max 0 (truncate (* 256.0 (/ (float bg-frame) max-frames)))))))
          (setf (graph-fa2-ctx-tick-len ctx) len)
          (setf (graph-fa2-ctx-tick-a ctx) a)
          (setf (graph-fa2-ctx-tick-progress ctx) progress-fp)
          (setf (graph-fa2-ctx-tick-rep-computed ctx) nil)
          (let ((gc-cons-threshold most-positive-fixnum))
            (if (eq graph-fa2-3d-pipeline graph-fa2--default-3d-pipeline)
                (graph-fa2--3d-tick-fast ctx len a progress-fp total-nodes)
              (dotimes (_ graph-fa2-substeps)
                (funcall graph-fa2-3d-pipeline ctx))
              (graph-fa2--sync-nodes ctx total-nodes)))
          (setf (graph-fa2-ctx-coords-synced ctx) nil)
          (graph-fa2--render-svg ctx len))))))

(defun graph-fa2--ensure-playback-buffer (buf)
  "Return a live unibyte playback buffer for BUF, creating one when absent."
  (with-current-buffer buf
    (unless (buffer-live-p graph-fa2-playback-buffer)
      (let ((pb (generate-new-buffer " *graph-fa2-playback*")))
        (with-current-buffer pb
          (set-buffer-multibyte nil))
        (setq-local graph-fa2-playback-buffer pb)
        (setq-local graph-fa2--frame-offsets nil)
        (setq-local graph-fa2--current-frame 0)
        (setq-local graph-fa2--playback-base-frame 0)
        (setq-local graph-fa2--playback-start-time nil)))
    graph-fa2-playback-buffer))

(defun graph-fa2--hot-reload-player (buf bg-buffer)
  "Feed newly rendered frames from BG-BUFFER into live player in BUF."
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
              (let ((added-vec (vconcat (nreverse new-offsets))))
                (with-current-buffer buf
                  (setq-local graph-fa2--frame-offsets
                              (if graph-fa2--frame-offsets
                                  (vconcat graph-fa2--frame-offsets added-vec)
                                added-vec))
                  (when (and (null graph-fa2-current-svg)
                             (> (length graph-fa2--frame-offsets) 0))
                    (let ((first-bounds (aref graph-fa2--frame-offsets 0)))
                      (when graph-fa2-ctx
                        (setf (graph-fa2-ctx-coords-synced graph-fa2-ctx) nil))
                      (setq-local graph-fa2-current-svg
                                  (with-current-buffer playback-buf
                                    (buffer-substring-no-properties
                                     (car first-bounds) (cdr first-bounds))))
                      (graph-fa2--update-display)))
                  (unless (and graph-fa2--drag-context
                               (let ((drag-type (cdr (assoc 'type graph-fa2--drag-context))))
                                 (or (eq drag-type 'node-move)
                                     (and (eq drag-type 'pan) (eq graph-fa2-engine '3d)))))
                    (graph-fa2-player-start)))))))))))

(defun graph-fa2--load-playback-from-cache (ctx cache-file target-buf)
  "Load CACHE-FILE into a playback buffer, parse offsets, and start playback.

This function initialises the playback buffer, reads frame data, determines
the offsets for individual frames, and starts the player in TARGET-BUF.

Parameters:
CTX: The simulation context.
CACHE-FILE: Path to the cache file.
TARGET-BUF: The destination buffer.

Returns:
The total number of loaded frames."
  (when ctx
    (setf (graph-fa2-ctx-coords-synced ctx) nil))
  (let* ((playback-buf (generate-new-buffer " *graph-fa2-playback*"))
         (offsets nil))
    (with-current-buffer playback-buf
      (set-buffer-multibyte nil)
      (insert-file-contents-literally cache-file)
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
          (when (and (buffer-live-p graph-fa2-playback-buffer)
                     (not (eq graph-fa2-playback-buffer playback-buf)))
            (kill-buffer graph-fa2-playback-buffer))
          (graph-fa2-player-stop)
          (setq-local graph-fa2-playback-buffer playback-buf)
          (setq-local graph-fa2--frame-offsets offsets-vec)
          (setq-local graph-fa2--current-frame 0)
          (setq-local graph-fa2--playback-base-frame 0)
          (setq-local graph-fa2--playback-start-time nil)
          (when-let* ((has-frames (> num-frames 0))
                      (first-bounds (aref offsets-vec 0)))
            (setq-local
             graph-fa2-current-svg
             (with-current-buffer playback-buf
               (buffer-substring-no-properties (car first-bounds) (cdr first-bounds)))))
          (graph-fa2-mode 1)
          (graph-fa2--update-display)
          (message "Graph playback started.")
          (graph-fa2-player-start)))
      num-frames)))

(defun graph-fa2--render-chunk
    (ctx cache-file hash-file target-hash target-buf max-frames playback-fps)
  "Cooperatively render frames of simulation CTX and schedule next chunk.

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
        (progn
          (setf (graph-fa2-ctx-bg-timer ctx) nil)
          (when-let*
              ((num-frames (graph-fa2--load-playback-from-cache ctx cache-file target-buf)))
            (setq max-frames num-frames)
            (setf (graph-fa2-ctx-frames-rendered ctx) max-frames)
            (setf (graph-fa2-ctx-playback-started ctx) t)))
      (graph-fa2--render-chunk-cooperative
       ctx cache-file hash-file target-hash target-buf max-frames playback-fps))))

(defun graph-fa2--render-chunk-cooperative
    (ctx cache-file hash-file target-hash target-buf max-frames playback-fps)
  "Render a simulation chunk for CTX cooperatively and schedule execution.

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
    (let* ((visible-target (and (buffer-live-p target-buf)
                                (get-buffer-window target-buf t)))
           (slice-budget (if (and playback-started visible-target) 0.010 0.025))
           (chunk-end-time (time-add nil slice-budget))
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
        (when-let*
            ((has-valid-frames (> valid-frames 0)))
          (setf heavy-frames (+ heavy-frames valid-frames))
          (setf
           heavy-time
           (+ heavy-time (* slice-duration (/ (float valid-frames) frames-in-slice)))))
        (let ((cumulative-avg (if (> heavy-frames 0)
                                  (/ heavy-time heavy-frames)
                                0.0)))
          (unless (or playback-started
                      (< frames-rendered 100)
                      (= heavy-frames 0))
            (let*
                ((tg (/ cumulative-avg 1000.0))
                 (predicted-tg (* tg 1.25))
                 (remaining (max 1 (- max-frames 100)))
                 (safe-buffer
                  (if (<= predicted-tg playback-ms)
                      1
                    (ceiling (* remaining (/ (- predicted-tg playback-ms) predicted-tg))))))
              (when-let* ((buffer-ready (>= frames-rendered (+ 100 safe-buffer))))
                (setf playback-started t)
                (when (buffer-live-p target-buf)
                  (graph-fa2--ensure-playback-buffer target-buf)
                  (with-current-buffer target-buf
                    (unless (bound-and-true-p graph-fa2-mode)
                      (graph-fa2-mode 1)))))))
          (when-let* ((can-reload (and playback-started (<= frames-rendered max-frames))))
            (graph-fa2--hot-reload-player target-buf bg-buffer)))
        (if-let*
            ((continue-sim (< frames-rendered max-frames)))
            (let ((next-delay (if (and playback-started visible-target) 0.008 0)))
              (setf bg-timer
                    (run-at-time
                     next-delay nil #'graph-fa2--render-chunk
                     ctx cache-file hash-file target-hash target-buf max-frames playback-fps)))
          (setf bg-timer nil)
          (when-let* ((has-cache cache-file))
            (with-current-buffer bg-buffer
              (let ((coding-system-for-write 'binary))
                (write-region (point-min) (point-max) cache-file nil 'silent))))
          (when-let* ((has-hash hash-file))
            (with-temp-file hash-file (insert target-hash)))
          (when (and (buffer-live-p target-buf) (not playback-started))
            (setf playback-started t)
            (graph-fa2--ensure-playback-buffer target-buf)
            (with-current-buffer target-buf
              (unless (bound-and-true-p graph-fa2-mode)
                (graph-fa2-mode 1)))
            (graph-fa2--hot-reload-player target-buf bg-buffer)))))))

(defun graph-fa2--zoom-tick (buffer)
  "Apply velocity to scale and redraw BUFFER.  Stop when velocity is near zero."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (if (< (abs graph-fa2--zoom-velocity) 0.001)
          (progn
            (setq graph-fa2--zoom-velocity 0.0)
            (when graph-fa2--zoom-timer
              (cancel-timer graph-fa2--zoom-timer)
              (setq graph-fa2--zoom-timer nil))
            (when (eq graph-fa2--nav-state 'zooming)
              (graph-fa2--nav-transition 'idle))
            (setq graph-fa2--render-state nil)
            (graph-fa2--update-display))

        (setq graph-fa2--scale
              (max 0.05 (* graph-fa2--scale (+ 1.0 graph-fa2--zoom-velocity))))

        (setq graph-fa2--zoom-velocity (* graph-fa2--zoom-velocity graph-fa2-zoom-friction))

        (graph-fa2--update-display)))))

(defun graph-fa2--start-zoom-inertia ()
  "Ensure the zoom timer is running for the current buffer."
  (graph-fa2--nav-transition 'zooming)
  (unless graph-fa2--zoom-timer
    (let ((buf (current-buffer)))
      (setq graph-fa2--zoom-timer
            (run-with-timer 0 0.016 #'graph-fa2--zoom-tick buf)))))

(defun graph-fa2-zoom-in (&optional event)
  "Increase scale of rendered graph with momentum from EVENT."
  (interactive (list last-input-event))
  (let* ((posn (and (listp event) (event-start event)))
         (window (and posn (posn-window posn))))
    (if (and window (window-live-p window) (not (eq window (selected-window))))
        (select-window window)
      (when (eq (current-buffer) (window-buffer (selected-window)))
        (setq
         graph-fa2--zoom-velocity (+ graph-fa2--zoom-velocity graph-fa2-zoom-acceleration))
        (graph-fa2--start-zoom-inertia)))))

(defun graph-fa2-zoom-out (&optional event)
  "Decrease scale of rendered graph with momentum from EVENT."
  (interactive (list last-input-event))
  (let* ((posn (and (listp event) (event-start event)))
         (window (and posn (posn-window posn))))
    (if (and window (window-live-p window) (not (eq window (selected-window))))
        (select-window window)
      (when (eq (current-buffer) (window-buffer (selected-window)))
        (setq
         graph-fa2--zoom-velocity (- graph-fa2--zoom-velocity graph-fa2-zoom-acceleration))
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
    (graph-fa2--nav-transition 'idle)
    (graph-fa2--update-display)))

(defun graph-fa2--grab-inner-elements (svg-string)
  "Extract inner elements from SVG-STRING.
This removes any outer SVG tags to allow the viewBox attributes to
be added directly during rendering."
  (cond
   ((and (string-prefix-p "<svg" svg-string)
         (string-match "\\`<svg[^>]*>" svg-string))
    (let ((start (match-end 0))
          (end (string-match "</svg>" svg-string)))
      (if end
          (substring svg-string start end)
        (substring svg-string start))))
   (t svg-string)))

(defun graph-fa2--update-display (&rest _)
  "Render current SVG frame into buffer natively using overlays.
This function checks for an existing overlay associated with the current window.
If one does not exist, it creates the overlay and restricts its visibility
to that window.  This prevents frame lockups when multiple frames view
the same buffer."
  (when-let*
      ((current-svg graph-fa2-current-svg)
       (win (get-buffer-window (current-buffer) t)))
    (when (= (buffer-size) 0)
      (let ((inhibit-read-only t))
        (insert (propertize " "
                            'face '(:height 1 :line-height 1.0 :family "Monospace")
                            'line-height 1.0)))
      (setq graph-fa2--render-state nil))
    (let*
        ((width (max 100 (window-body-width win t)))
         (height (max 100 (window-body-height win t)))
         (fast-render (or graph-fa2--player-timer
                          (memq graph-fa2--nav-state
                                '(dragging-viewport dragging-node zooming))))
         (state
          (list graph-fa2--scale
                graph-fa2--pan-x
                graph-fa2--pan-y
                width
                height
                fast-render
                graph-fa2-hovered-node
                current-svg)))
      (unless (equal state graph-fa2--render-state)
        (setq graph-fa2--render-state state)
        (let*
            ((inhibit-read-only t)
             (inner-elements (graph-fa2--grab-inner-elements current-svg))
             (viewbox-dim (/ graph-fa2-canvas-size graph-fa2--scale))
             (viewbox-x
              (- (- (/ graph-fa2-canvas-size 2.0) graph-fa2--pan-x) (/ viewbox-dim 2.0)))
             (viewbox-y
              (- (- (/ graph-fa2-canvas-size 2.0) graph-fa2--pan-y) (/ viewbox-dim 2.0)))
             (render-hint
              (if fast-render
                  " shape-rendering=\"optimizeSpeed\" text-rendering=\"optimizeSpeed\""
                ""))
             (header (format "<svg width=\"%d\" height=\"%d\" viewBox=\"%.2f %.2f %.2f %.2f\"%s xmlns=\"http://www.w3.org/2000/svg\" preserveAspectRatio=\"xMidYMid meet\">\n"
                             width height viewbox-x viewbox-y viewbox-dim viewbox-dim render-hint))
             (full-svg (concat header inner-elements "\n</svg>"))
             (encoded-svg (if (multibyte-string-p full-svg)
                              (encode-coding-string full-svg 'utf-8)
                            full-svg))
             (overlays (overlays-in (point-min) (point-max)))
             (ov (if-let* ((existing (seq-find (lambda (o) (eq (overlay-get o 'window) win)) overlays)))
                     (progn
                       (move-overlay existing (point-min) (point-max))
                       existing)
                   (let ((new-ov (make-overlay (point-min) (point-max) nil nil t)))
                     (overlay-put new-ov 'window win)
                     new-ov)))
             (old-img (overlay-get ov 'display))
             (new-img (list 'image
                            :type 'svg
                            :data encoded-svg
                            :scale 1
                            :transform-smoothing nil
                            :ascent 100)))
          (when (and (fboundp 'image-flush)
                     (consp old-img)
                     (eq (car old-img) 'image))
            (ignore-errors (image-flush old-img (window-frame win))))
          (overlay-put ov 'display new-img)
          (overlay-put ov 'pointer (if graph-fa2-hovered-node 'hand nil))
          (run-hooks 'graph-fa2-after-render-functions))))))

(defun graph-fa2--schedule-player-tick (buf delay)
  "Schedule next animation tick in BUF after DELAY seconds without timer debt."
  (setq graph-fa2--player-timer
        (run-at-time delay nil
                     (lambda ()
                       (when (and (buffer-live-p buf)
                                  (buffer-local-value 'graph-fa2--player-timer buf))
                         (with-current-buffer buf
                           (graph-fa2--player-tick)))))))

(defun graph-fa2--player-tick ()
  "Advance the animation frame natively from memory buffers at wall-clock rate."
  (let ((gc-cons-threshold most-positive-fixnum)
        (tick-start (float-time)))
    (when (buffer-live-p (current-buffer))
      (if (and graph-fa2--drag-context
               (let ((drag-type (cdr (assoc 'type graph-fa2--drag-context))))
                 (or (eq drag-type 'node-move)
                     (and (eq drag-type 'pan) (eq graph-fa2-engine '3d)))))
          (progn
            (setq graph-fa2--playback-start-time tick-start)
            (setq graph-fa2--playback-base-frame graph-fa2--current-frame)
            (when (timerp graph-fa2--player-timer)
              (graph-fa2--schedule-player-tick
               (current-buffer) (/ 1.0 graph-fa2-framerate))))
        (let ((total-frames
               (or (and graph-fa2--frame-offsets (length graph-fa2--frame-offsets)) 0))
              (streaming (and graph-fa2-ctx (graph-fa2-ctx-bg-timer graph-fa2-ctx))))
          (when (> total-frames 0)
            (let* ((wall-frame
                    (if (numberp graph-fa2--playback-start-time)
                        (+ graph-fa2--playback-base-frame
                           (floor (* (max 0.0 (- tick-start graph-fa2--playback-start-time))
                                     graph-fa2-framerate)))
                      graph-fa2--current-frame))
                   (target-frame (max graph-fa2--current-frame wall-frame)))
              (cond
               ((< target-frame total-frames)
                (when-let* ((bounds (aref graph-fa2--frame-offsets target-frame)))
                  (when graph-fa2-ctx
                    (setf (graph-fa2-ctx-coords-synced graph-fa2-ctx) nil))
                  (setq graph-fa2-current-svg
                        (with-current-buffer graph-fa2-playback-buffer
                          (buffer-substring-no-properties (car bounds) (cdr bounds)))))
                (unless (input-pending-p)
                  (graph-fa2--update-display))
                (setq graph-fa2--current-frame (1+ target-frame))
                (when (timerp graph-fa2--player-timer)
                  (let* ((elapsed (- (float-time) tick-start))
                         (next-delay (max 0.004 (- (/ 1.0 graph-fa2-framerate) elapsed))))
                    (graph-fa2--schedule-player-tick (current-buffer) next-delay))))
               (streaming
                (when (< graph-fa2--current-frame total-frames)
                  (when-let* ((bounds (aref graph-fa2--frame-offsets (1- total-frames))))
                    (when graph-fa2-ctx
                      (setf (graph-fa2-ctx-coords-synced graph-fa2-ctx) nil))
                    (setq graph-fa2-current-svg
                          (with-current-buffer graph-fa2-playback-buffer
                            (buffer-substring-no-properties (car bounds) (cdr bounds)))))
                  (unless (input-pending-p)
                    (graph-fa2--update-display)))
                (setq graph-fa2--current-frame total-frames)
                (setq graph-fa2--playback-start-time (float-time))
                (setq graph-fa2--playback-base-frame total-frames)
                (when (timerp graph-fa2--player-timer)
                  (graph-fa2--schedule-player-tick (current-buffer) 0.012)))
               (t
                (when-let* ((bounds (aref graph-fa2--frame-offsets (1- total-frames))))
                  (when graph-fa2-ctx
                    (setf (graph-fa2-ctx-coords-synced graph-fa2-ctx) nil))
                  (setq graph-fa2-current-svg
                        (with-current-buffer graph-fa2-playback-buffer
                          (buffer-substring-no-properties (car bounds) (cdr bounds)))))
                (setq graph-fa2--current-frame total-frames)
                (graph-fa2-player-stop)
                (setq graph-fa2--render-state nil)
                (graph-fa2--update-display))))))))))

(defun graph-fa2-player-start ()
  "Start animation playback loop if frames are populated."
  (when graph-fa2--frame-offsets
    (unless graph-fa2--player-timer
      (setq graph-fa2--playback-start-time (float-time))
      (setq graph-fa2--playback-base-frame graph-fa2--current-frame)
      (graph-fa2--schedule-player-tick (current-buffer) 0))))

(defun graph-fa2-player-stop ()
  "Halt the animation loop."
  (when (timerp graph-fa2--player-timer)
    (cancel-timer graph-fa2--player-timer))
  (setq graph-fa2--player-timer nil)
  (setq graph-fa2--playback-start-time nil))

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

    (define-key map [down-mouse-2] #'graph-fa2-mouse-down)
    (define-key map (kbd "<down-mouse-2>") #'graph-fa2-mouse-down)
    (define-key map [drag-mouse-2] #'graph-fa2-mouse-up)
    (define-key map (kbd "<drag-mouse-2>") #'graph-fa2-mouse-up)
    (define-key map [mouse-2] #'graph-fa2-mouse-up)
    (define-key map (kbd "<mouse-2>") #'graph-fa2-mouse-up)

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
        (setq-local graph-fa2--nav-state 'idle)
        (setq-local graph-fa2--drag-context nil)
        (setq-local line-spacing nil)
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local truncate-lines t)
        (setq-local auto-window-vscroll nil)
        (setq-local left-margin-width 0)
        (setq-local right-margin-width 0)
        (set-window-vscroll (selected-window) 0)
        (add-hook 'window-size-change-functions #'graph-fa2--update-display nil t)
        (add-hook 'window-selection-change-functions #'graph-fa2--cancel-drag nil t)
        (add-hook 'mouse-leave-buffer-hook #'graph-fa2--cancel-drag nil t)
        (add-function :after (local 'after-focus-change-function) #'graph-fa2--cancel-drag))
    (progn
      (setq-local track-mouse nil)
      (graph-fa2--cancel-drag)
      (remove-hook 'window-size-change-functions #'graph-fa2--update-display t)
      (remove-hook 'window-selection-change-functions #'graph-fa2--cancel-drag t)
      (remove-hook 'mouse-leave-buffer-hook #'graph-fa2--cancel-drag t)
      (remove-function (local 'after-focus-change-function) #'graph-fa2--cancel-drag)
      (let ((overlays (overlays-in (point-min) (point-max))))
        (dolist (o overlays)
          (when (overlay-get o 'window)
            (delete-overlay o)))))))

(defun graph-fa2-view-indirect (&optional frame)
  "Spawn indirect buffer for current graph and display it in FRAME if non-nil.
This ensures each window or frame has its own independent view with its own
zoom scale, pan offsets, and active hitboxes, resolving frame lockups."
  (interactive "P")
  (let* ((base-buf (current-buffer))
         (indirect-name (generate-new-buffer-name (concat (buffer-name base-buf) "-view")))
         (indirect-buf (make-indirect-buffer base-buf indirect-name t)))
    (with-current-buffer indirect-buf
      (graph-fa2-mode 1)
      (setq-local graph-fa2--nav-state 'idle)
      (setq-local graph-fa2--drag-context nil)
      (setq-local graph-fa2--scale (buffer-local-value 'graph-fa2--scale base-buf))
      (setq-local graph-fa2--pan-x (buffer-local-value 'graph-fa2--pan-x base-buf))
      (setq-local graph-fa2--pan-y (buffer-local-value 'graph-fa2--pan-y base-buf))
      (setq-local
       graph-fa2--active-hitboxes (buffer-local-value 'graph-fa2--active-hitboxes base-buf))
      (setq-local graph-fa2-current-svg (buffer-local-value 'graph-fa2-current-svg base-buf))
      (setq-local graph-fa2-ctx (buffer-local-value 'graph-fa2-ctx base-buf))
      (setq-local
       graph-fa2-playback-buffer (buffer-local-value 'graph-fa2-playback-buffer base-buf))
      (setq-local graph-fa2-cache-dir (buffer-local-value 'graph-fa2-cache-dir base-buf)))
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
  "Convert a property list ITEM to an association list if it is a plist.
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
  "Initialise background worker or load from cache for BUF, NODES, and EDGES.
CACHE-DIR specifies optional directory for storing cache files.

Parameters:
BUF: The buffer displaying the graph.
NODES: List of graph nodes.
EDGES: List of graph edges.
CACHE-DIR: Directory for storing cache files (optional).

Returns:
Nil."
  (let*
      ((resolved-cache-dir
        (or cache-dir (expand-file-name "graph-fa2-cache" temporary-file-directory)))
       (hash-file (expand-file-name "fa2-graph.hash" resolved-cache-dir))
       (data-file (expand-file-name "fa2-graph.dat" resolved-cache-dir))
       (normalised-nodes (mapcar #'graph-fa2--plist-to-alist nodes))
       (normalised-edges (mapcar #'graph-fa2--parse-edge edges))
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
          (kill-buffer (graph-fa2-ctx-bg-buffer old-ctx)))
        (when (buffer-live-p (graph-fa2-ctx-scratch-buffer old-ctx))
          (kill-buffer (graph-fa2-ctx-scratch-buffer old-ctx)))))

    (let ((ctx (graph-fa2--create-ctx nodes edges)))
      (with-current-buffer buf
        (graph-fa2-player-stop)
        (when (buffer-live-p graph-fa2-playback-buffer)
          (kill-buffer graph-fa2-playback-buffer))
        (setq-local graph-fa2-playback-buffer nil)
        (setq-local graph-fa2--frame-offsets nil)
        (setq-local graph-fa2--current-frame 0)
        (setq-local graph-fa2--playback-base-frame 0)
        (setq-local graph-fa2--playback-start-time nil)
        (setq-local graph-fa2-current-svg nil)
        (setq-local graph-fa2--active-hitboxes nil)
        (setq-local graph-fa2--hitbox-svg-string nil)
        (setq-local graph-fa2--render-state nil)
        (setq-local graph-fa2-ctx ctx)
        (setq-local graph-fa2-cache-dir resolved-cache-dir))

      (let ((bg (generate-new-buffer " *graph-fa2-bg*")))
        (with-current-buffer bg
          (set-buffer-multibyte nil))
        (setf (graph-fa2-ctx-bg-buffer ctx) bg))
      (setf (graph-fa2-ctx-bg-timer ctx)
            (run-at-time 0 nil #'graph-fa2--render-chunk
                         ctx data-file hash-file current-hash buf
                         graph-fa2-simulation-frames graph-fa2-framerate)))))

;;;###autoload
(defun graph-fa2-clear-cache (&optional cache-dir)
  "Clear background render cache CACHE-DIR to force fresh simulation."
  (interactive)
  (let*
      ((local-cache (or cache-dir graph-fa2-cache-dir))
       (resolved-cache-dir
        (or local-cache (expand-file-name "graph-fa2-cache" temporary-file-directory)))
       (hash-file (expand-file-name "fa2-graph.hash" resolved-cache-dir))
       (data-file (expand-file-name "fa2-graph.dat" resolved-cache-dir)))
    (when (file-exists-p hash-file) (delete-file hash-file))
    (when (file-exists-p data-file) (delete-file data-file))
    (message "ForceAtlas2 cache cleared from %s." resolved-cache-dir)))

;;;###autoload
(defun graph-fa2-demo ()
  "Demonstrate the graph-fa2 engine with a four-node interactive graph in 2D."
  (interactive)
  (let ((demo-nodes
         (list
          (list :id "emacs" :label "Emacs" :colour "#89b4fa" :radius 14.0)
          (list :id "fa2" :label "ForceAtlas2" :colour "#a6e3a1" :radius 12.0)
          (list :id "svg" :label "SVG" :colour "#f9e2af" :radius 12.0)
          (list :id "physics" :label "Physics" :colour "#fab387" :radius 12.0)))
        (demo-edges
         (list
          (list :from "emacs" :to "fa2" :label "runs" :colour "#89b4fa" :width 2)
          (list "fa2" "svg" "renders")
          (cons "svg" "physics")
          (list :from "fa2" :to "fa2" :label "iterates" :colour "#a6e3a1" :width 2))))
    (with-current-buffer (get-buffer-create "*graph-fa2-demo*")
      (erase-buffer)
      (graph-fa2-clear-cache)
      (graph-fa2-mode 1)
      (setq-local graph-fa2-engine '2d)
      (setq-local graph-fa2-show-edge-labels t)
      (graph-fa2-start (current-buffer) demo-nodes demo-edges)
      (pop-to-buffer (current-buffer)))))

(provide 'graph-fa2)
;;; graph-fa2.el ends here
