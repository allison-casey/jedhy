"Expose jedhy's `API` for IDE and metaprogramming use-cases."

;; * Imports

(require [jedhy.macros [*]])
(import [jedhy.macros [*]])

(import [typing [Tuple Optional]]
        [jedhy.inspection [Inspect]]
        [jedhy.models [Candidate
                       Namespace
                       Prefix]])

(try
  (import jedi)
  (except [ImportError]
    (setv jedi None)))

(import astor
        re
        [hy.compiler [hy-compile HyASTCompiler]]
        [hy.lex [hy-parse mangle unmangle]]
        [hy.contrib.hy-repr [hy-repr]]
        [hy.cmdline [filtered-hy-exceptions]])


;; * Jedi Completions

(setv --COMPILER None
      -placeholder-sentinel "___COMPLETION_PLACEHOLDER___")


(defn format-output-elisp-tuple [completions obj]
  (defn annotate [completion]
    (setv klass completion.type
          annotation (cond [(in klass ["function" "builtin_function_or_method"])
                            "def"]

                           [(= klass "type")
                            "class"]

                           [(= klass "module")
                            "module"]

                           [True
                            "instance"]))
    f"<{annotation} {completion.name}>")

  (defn format-completion-tuple [completion]
    (setv
      attribute (unmangle completion.name)
      annotation (annotate completion)
      docstring (completion.docstring)
      completion-name f"{obj}.{attribute}")
    #[f[("{completion-name}" "{annotation}")]f])

  (->> completions
       (map format-completion-tuple)
       (.join " ")
       (.format "({})")))

(defn hy->python ^str
  [^str source]
  (global --COMPILER)

  (setv filename "<stdin>"
        module "__main__")

  (if-not --COMPILER
    (setv --COMPILER (HyASTCompiler module
                                    :filename filename
                                    :source source
                                    :replace-empty-attr-access -placeholder-sentinel))
    (setv (. --COMPILER source) source))

  (with [_ (filtered_hy_exceptions)]
    (setv hst (hy-parse source :filename filename)))

  (with [_ (filtered-hy-exceptions)]
    (setv -ast (hy-compile hst "__main__"
                           :compiler --COMPILER
                           :filename filename
                           :source source
                           :replace-empty-attr-access -placeholder-sentinel)))

  (astor.code-gen.to-source -ast))

(defn -get-placeholder-location ^(of Optional (of Tuple int int))
  [^str source]
  (for [(, linno line) (enumerate (source.splitlines) 1)]
    (when (in -placeholder-sentinel line)
      (setv start (line.find -placeholder-sentinel)
            end (+ start (len -placeholder-sentinel)))
      (return (, linno start)))))

(defn -get-attribute-location ^(of Optional (of Tuple int int))
  [^str source ^str obj ^str attribute]
  (setv s f"{obj}.{attribute}")
  (for [(, linno line) (enumerate (source.splitlines) 1)]
    (when (in s line)
      (setv start (line.find s))
      (return (, linno (+ start (len s)))))))

(defn complete-with-jedi [src lineno column &optional symbol]
  (unless jedi (raise (ImportError "Completions via this function require jedi to be installed")))
  (try
    (if-not symbol
            (do
              (setv
                completion-line (cut (get (src.splitlines) (dec lineno)) 0 column)
                match (re.search r"(.*)?\.(.+?)[\(\s\[]" (cut completion-line None None -1)))
              (unless match (return []))
              (setv (, attribute obj) (map (fn [x] (cut x None None -1)) (match.groups))))
            (setv (, obj attribute) (symbol.rsplit "." 1)))

    (setv compiled-python (hy->python src))
    (if-not attribute
            (setv
              (, lineno column) (-get-placeholder-location compiled-python)
              compiled-python (.replace compiled-python -placeholder-sentinel ""))
            (setv
              (, lineno column) (-get-attribute-location compiled-python (mangle obj) (mangle attribute))))

    (-> compiled-python
        jedi.Script
        (.complete lineno column)
        (format-output-elisp-tuple obj))
    (except [Exception]
      "()")))
;; * API

(defclass API [object]
  (defn --init-- [self &optional globals- locals- macros-]
    (self.set-namespace globals- locals- macros-)

    (setv self.-cached-prefix None))

  (defn set-namespace [self &optional globals- locals- macros-]
    "Rebuild namespace for possibly given `globals-`, `locals-`, and `macros-`.

Typically, the values passed are:
  globals- -> (globals)
  locals-  -> (locals)
  macros-  -> --macros--"
    (setv self.namespace (Namespace globals- locals- macros-)))

  (defn complete [self prefix-str]
    "Completions for a prefix string."
    (setv [cached-prefix prefix] [self.-cached-prefix
                                  (Prefix prefix-str :namespace self.namespace)])
    (setv self.-cached-prefix prefix)

    (.complete prefix :cached-prefix cached-prefix))

  (defn annotate [self candidate-str]
    "Annotate a candidate string."
    (-> candidate-str
      (Candidate :namespace self.namespace)
      (.annotate)))

  (defn -inspect [self candidate-str]
    "Inspect a candidate string."
    (-> candidate-str
       (Candidate :namespace self.namespace)
       (.get-obj)
       Inspect))

  (defn docs [self candidate-str]
    "Docstring for a candidate string."
    (-> candidate-str self.-inspect (.docs)))

  (defn full-docs [self candidate-str]
    "Full documentation for a candidate string."
    (-> candidate-str self.-inspect (.full-docs))))
