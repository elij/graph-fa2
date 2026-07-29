<div align="center">

# graph-fa2

graph-fa2 is a pure Emacs Lisp ForceAtlas2 graph layout engine with SVG rendering. It enables network graph visualisations directly within Emacs buffers without requiring an external browser engine or external process dependencies.</div>

https://github.com/user-attachments/assets/6bdd8aac-201b-49d2-82eb-4d555d665437

The layout engine uses high-precision fixed-point integer arithmetic and pre-allocated vector arrays to minimise garbage collection pressure during continuous physics calculations.

## Features

- 2D and 3D simulation engines.
- Background asynchronous caching.
- Momentum zooming and viewport panning.
- Interactive node repositioning.
- Custom hook handlers for node click and hover events.
- Indirect view support for multi-window and multi-frame rendering.

### Related projects

- [vulpea-graph](https://github.com/neonmei/vulpea-graph)
- [grove-extra](https://github.com/elij/grove-extra)

## Physics engine architecture

The physics engine in `graph-fa2` performs simulation and rendering in Emacs Lisp without dependencies.

### Performance

To eliminate floating-point object (and the GC overhead that follows), `graph-fa2` uses bit shifted fixed-point arithmetic combined with a slightly naive AABB culling on repulsion.

### Spherical surface constraints

For 3D layouts, `graph-fa2` supports five spherical surface constraint modes configured via `graph-fa2-surface-constraint`:

`'anneal`
Gradually pulls nodes towards the target spherical surface radius as simulation frames progress, transitioning from fluid force propagation to a structured spherical shell.

`'strict`
Enforces hard boundary constraints on every simulation step, locking node coordinates strictly onto the spherical surface radius.

`'none`
Disables spherical surface constraints, allowing unconstrained spatial movement within boundary horizons.

`'floor` Acts as an inner distance floor, preventing nodes from falling inside the target spherical surface radius while permitting expansion outward.

`'ceiling`
Acts as an outer distance ceiling, preventing nodes from extending beyond the target spherical surface radius while permitting contraction inward.

## Installation

```elisp
(use-package graph-fa2
  :vc (:url "https://github.com/elij/graph-fa2")
  :custom
  (graph-fa2-engine '3d)
  (graph-fa2-framerate 60.0))
```

Alternatively, clone the repository into your Emacs load path:

```elisp
(add-to-list 'load-path "/path/to/graph-fa2")
(require 'graph-fa2)
```

## Quick Example

The following example demonstrates building a network visualisation of a Denote note archive and attaching a custom node click hook to open notes directly upon interaction:

```elisp
(defun denote-graph-fa2-open-note (id)
  "Open the Denote file corresponding to ID when clicked."
  (when-let* ((file (car (denote-directory-files id))))
    (find-file file)))

(defun denote-graph-fa2-network ()
  "Generate and display a ForceAtlas2 graph of the Denote network."
  (interactive)
  (let* ((files (denote-directory-files nil nil t))
         (nodes (mapcar (lambda (file)
                          (let ((id (denote-retrieve-filename-identifier file))
                                (type (denote-filetype-heuristics file)))
                            (list :id id
                                  :label (denote-retrieve-title-or-filename file type)
                                  :colour "#89b4fa"
                                  :radius 8.0)))
                        files))
         (edges nil)
         (buf (get-buffer-create "*denote-graph-fa2*")))

    (let ((links-xref (xref-matches-in-files (concat "denote:" denote-id-regexp) files)))
      (dolist (match links-xref)
        (let* ((loc (xref-match-item-location match))
               (source-file (xref-location-group loc))
               (source-id (denote-retrieve-filename-identifier source-file))
               (summary (xref-match-item-summary match)))
          (when (string-match denote-id-regexp summary)
            (let ((target-id (match-string 0 summary)))
              (push (cons source-id target-id) edges))))))

    (with-current-buffer buf
      (add-hook 'graph-fa2-node-clicked-functions #'denote-graph-fa2-open-note nil t))

    (pop-to-buffer buf)
    (graph-fa2-start buf nodes edges)))
```

## Customisation options

https://github.com/user-attachments/assets/1c79973a-973a-4ba0-9a48-6c8ad87ed54f

[@yuruyurau](https://bsky.app/profile/yuruyurau.bsky.social/post/3mq7wcge7bs24) inspired visualisation

You can customise the physics engine, display parameters, and rendering performance using defcustoms.

## Interactive controls

When `graph-fa2-mode` is active in a buffer, you can interact with the graph using mouse actions and keyboard commands.

### Mouse interactions

| Action | Input | Description |
| --- | --- | --- |
| Drag node | Left mouse drag on node | Interactively move individual nodes across the viewport. |
| Drag background | Left mouse drag on background | Rotate the 3D trackball sphere or pan the viewport depending on `graph-fa2-drag-action`. |
| Alternate drag | Middle mouse drag or `Alt` with left mouse drag | Toggle between rotating and panning actions in 3D mode. |
| Node click | Left click on node | Triggers functions registered in `graph-fa2-node-clicked-functions`. |
| Node hover | Mouse pointer hover over node | Highlights the active node under the cursor and invokes functions in `graph-fa2-node-hovered-functions`. |

