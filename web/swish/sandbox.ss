(http:include "components.ss")

;; BUG very wrong still, but it's at least a scaffolding that we can fix.
(define (make-annotated-time-line div-id columns rows)
  `(script
    ,(let ([op (open-output-string)]
           [columns (vector-map string->symbol columns)]
           [fname (format "draw_~a" div-id)])
       (define ncols (vector-length columns))
       (define col1 (vector-ref columns 2)) ;; HACK
       (fprintf op "function ~a() {\n" fname)
       ;; TODO in theory, we'd do something to update the data in some way and the plot would magically adjust
       (fprintf op "var data = ~a;\n"
         (json:object->string
          (map (lambda (x)
                 (let ([obj (json:make-object [date (vector-ref x 0)])])
                   (do ([i 1 (+ i 1)]) ((fx= i ncols))
                     (json:set! obj (vector-ref columns i) (vector-ref x i)))
                   (printf "  HACK: currently plotting ~s, ~s\n" (json:ref obj 'date #f) (json:ref obj col1 #f))  
                   obj))
            rows)))
       (fprintf op "const plot = Plot.plot({x: {type: 'utc', grid: true}, y: {grid: true, label: '~a'},\n" col1)
       (fprintf op "              marks: [\n")
       (fprintf op "                new Plot.RuleY([0]),\n")
       (fprintf op "                new Plot.dot(data, {x: 'date', y: '~a'}),\n" col1)
       (fprintf op "                new Plot.line(data, {x: 'date', y: '~a', stroke: 'steelblue'})\n" col1)
       (fprintf op "              ]});\n")
       (fprintf op "const div = document.getElementById('~a');\n" div-id)
       (fprintf op "div.append(plot);\n")
       (fprintf op "}\n")
       ;; HACK?
       (fprintf op "document.addEventListener('DOMContentLoaded', (event) => { ~a(); });\n" fname)
       (get-output-string op))))

(define (chart-hack db chart-columns limit)
  ;; TODO what if we just queried the data as a json_object(...)
  (let* ([sql (format "SELECT CAST(timestamp AS INTEGER) as timestamp,reason,~a FROM statistics WHERE (timestamp/1000) > CAST(strftime('%s','now',?) AS INTEGER) ORDER BY timestamp DESC" (join chart-columns #\,))]
         [stmt (sqlite:prepare db sql)])
    (on-exit (sqlite:finalize stmt)
      (make-annotated-time-line "annotated_chart"
        (sqlite:columns stmt)
        (fill-gaps (sqlite:execute stmt (list limit)))))))

(define (fill-gaps rows)
  (printf "TODO do whatever this is supposed to do\n")
  rows)

(define standard-charts
  '(("memory" "bytes_allocated" "osi_bytes_used" "maximum_memory_bytes")
    ("time" "cpu" "gc_real")
    ("sqlite" "sqlite_memory" "sqlite_memory_highwater")
    ("bytes_allocated" "bytes_allocated")
    ("osi_bytes_used" "osi_bytes_used")
    ("cpu" "cpu")
    ("real" "real")
    ("bytes" "bytes")
    ("gc_count" "gc_count")
    ("gc_real" "gc_real")
    ("gc_bytes" "gc_bytes")))

(with-db [db (log-file) SQLITE_OPEN_READONLY]
  (let* ([valid-charts
          (append standard-charts
            (map
             (lambda (row)
               (match row
                 [#(,key)
                  `(,key ,(format "json_extract(foreign_handles, '$.~a')" key))]))
             (execute-sql db
               "select key from (select distinct key from json_each(foreign_handles), (select distinct foreign_handles from statistics)) order by key")))]
         [chart-type
          (cond
           [(find-param "chart-type") => (lambda (x) x)]
           [else (caar valid-charts)])]
         [cols (cond
                [(assoc chart-type valid-charts) => cdr]
                [else (throw `#(invalid-chart ,chart-type))])]
         [limit (or (find-param "limit") "-7 days")])
    (define (option category value text)
      (define (hidden-value name default)
        `(textarea (@ (name ,name) (class "hidden"))
           ,(if (equal? name category) value default)))
      (define selected?
        (equal? value
          (match category
            ["chart-type" chart-type]
            ["limit" limit])))
      `(form (@ (name "query") (method "get"))
         ,(hidden-value "chart-type" chart-type)
         ,(hidden-value "limit" limit)
         (button (@ (type "submit") ,@(if selected? '((disabled) (class "selected")) '())) ,text)))
    (hosted-page "Charts"
      (list
       (css-include "css/charts.css")
       (js-include "https://cdn.jsdelivr.net/npm/d3@7")
       (js-include "https://cdn.jsdelivr.net/npm/@observablehq/plot@0.6")
       (chart-hack db cols limit))
      `(div (@ (id "annotated_chart")))
      `(div
        (h3 "Chart Types")
        (div (@ (class "chart-options"))
          ,@(map
             (lambda (chart)
               (let ([type (car chart)])
                 (option "chart-type" type type)))
             valid-charts))
        (h3 "Chart Limits")
        (div (@ (class "chart-options"))
          ,@(map
             (lambda (lmt text)
               (option "limit" lmt text))
             '("-7 days" "-14 days" "-1 month" "-2 months" "-3 months")
             '("1 week" "2 weeks" "1 month" "2 months" "3 months")))))))
