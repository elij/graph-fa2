;;; bench/bench-helper.el -*- lexical-binding: t; no-byte-compile: t;-*-

(require 'buttercup)

(defmacro graph-fa2-expect-faster (baseline-form optimised-form iterations)
  "Run BASELINE-FORM and OPTIMISED-FORM for ITERATIONS.
Assert that OPTIMISED-FORM executes faster than BASELINE-FORM and log garbage collection data."
  `(let* ((_ (cancel-function-timers #'graph-fa2--render-chunk))
          (_ (cancel-function-timers #'baseline-render-chunk))
          (_ (when (and (boundp 'ctx) ctx (buffer-live-p (graph-fa2-ctx-bg-buffer ctx)))
               (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
                 (erase-buffer))))
          (_ (garbage-collect))
          (base-metrics (benchmark-run ,iterations ,baseline-form))
          (_ (cancel-function-timers #'graph-fa2--render-chunk))
          (_ (cancel-function-timers #'baseline-render-chunk))
          (_ (when (and (boundp 'ctx) ctx (buffer-live-p (graph-fa2-ctx-bg-buffer ctx)))
               (with-current-buffer (graph-fa2-ctx-bg-buffer ctx)
                 (erase-buffer))))
          (_ (garbage-collect))
          (opt-metrics (benchmark-run ,iterations ,optimised-form))
          (base-time (nth 0 base-metrics))
          (base-gcs (nth 1 base-metrics))
          (base-gc-time (nth 2 base-metrics))
          (opt-time (nth 0 opt-metrics))
          (opt-gcs (nth 1 opt-metrics))
          (opt-gc-time (nth 2 opt-metrics)))

     ;; Log the exact garbage collection and time differences
     (message "\n--- Benchmark Results (%d iterations) ---" ,iterations)
     (message "Baseline : %.4f sec (GCs: %d, GC Time: %.4f sec)" base-time base-gcs base-gc-time)
     (message "Optimised: %.4f sec (GCs: %d, GC Time: %.4f sec)" opt-time opt-gcs opt-gc-time)
     (message "Delta    : %s%.4f sec" (if (< opt-time base-time) "-" "+") (abs (- opt-time base-time)))
     (message "-----------------------------------------")

     ;; Assert that the new code is faster or comparable
     (expect opt-time :to-be-less-than (+ base-time (max 0.010 (* base-time 0.15))))))

(defun graph-fa2-bench-generate-data (num-nodes num-edges)
  "Generate a mock graph payload with NUM-NODES and NUM-EDGES."
  (let (nodes edges)
    (dotimes (i num-nodes)
      (push (list :id (number-to-string i)
                  :label (format "Node %d" i)
                  :radius 10.0
                  :colour "#89b4fa")
            nodes))
    (dotimes (_ num-edges)
      (let ((src (random num-nodes))
            (tgt (random num-nodes)))
        (push (cons (number-to-string src) (number-to-string tgt)) edges)))
    (list nodes edges)))

(provide 'bench-helper)
