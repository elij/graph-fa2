# Graph-fa2

https://github.com/user-attachments/assets/6bdd8aac-201b-49d2-82eb-4d555d665437

*the engine defaults to 3d use `graph-fa2-engine` to change to 2d*

Optimised ForceAtlas2 simulation using librsvg to allow dynamic graphs directly within emacs buffers without a browser engine.

## Use cases

- Observability (graphs of sub agents in [`macher-agent`](https://github.com/elij/macher-agent))
- Knowledge Graph (used to render the `grove` graph in [`grove-extra`](https://github.com/elij/grove-extra))

## Features

- Momentum zoom
- 60fps performance target (older machines cache simulation before playback)
- Hooks for clicks and hover

## Quick Start

Create a click hook handler

```elisp
(defun denote-graph-fa2-open-note (id)
  "Open the Denote file corresponding to ID when clicked."
  (when-let ((file (car (denote-directory-files id))))
    (find-file file)))
```

Generate your network and start render playback

```elisp
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
