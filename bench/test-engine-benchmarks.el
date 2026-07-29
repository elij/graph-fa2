;;; test-engine-benchmarks.el --- Benchmark suite for graph-fa2 -*- lexical-binding: t; no-byte-compile: t;-*-

(require 'buttercup)
(require 'graph-fa2)
(require 'graph-fa2-baseline)
(require 'bench-helper)

(describe "Graph-fa2 Expensive Functions Benchmark"
  :var (ctx max-frames)
  
  (before-each
    (setq max-frames 100)
    (let* ((mock-data (graph-fa2-bench-generate-data 250 500))
           (nodes (car mock-data))
           (edges (cadr mock-data)))
      (setq ctx (graph-fa2--create-ctx nodes edges))
      (setf (graph-fa2-ctx-bg-buffer ctx) (generate-new-buffer " *graph-fa2-bg*"))))

  (after-each
    (when (and ctx (buffer-live-p (graph-fa2-ctx-bg-buffer ctx)))
      (kill-buffer (graph-fa2-ctx-bg-buffer ctx))))
  
  (it "optimises the 2D physics tick"
    (graph-fa2-expect-faster 
     (baseline-2d-physics-tick ctx max-frames)
     (graph-fa2--2d-physics-tick ctx max-frames)
     10))

  (it "optimises the 3D physics tick"
    (graph-fa2-expect-faster
     (baseline-3d-physics-tick ctx max-frames)
     (graph-fa2--physics-tick ctx max-frames)
     10))
  
  (it "optimises the SVG rendering pipeline"
    (let ((len (length (graph-fa2-ctx-nodes ctx))))
      (graph-fa2-expect-faster 
       (baseline-render-svg ctx len)
       (graph-fa2--render-svg ctx len)
       20)))

  (it "optimises the full render chunk lifecycle"
    (let ((target-buf (generate-new-buffer " *graph-fa2-target*")))
      (unwind-protect
          (graph-fa2-expect-faster
           (baseline-render-chunk ctx nil nil "hash" target-buf max-frames 60.0)
           (graph-fa2--render-chunk ctx nil nil "hash" target-buf max-frames 60.0)
           2)
        (when (buffer-live-p target-buf)
          (kill-buffer target-buf))))))
