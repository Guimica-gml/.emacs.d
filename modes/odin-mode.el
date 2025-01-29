;;; odin-mode.el --- A major mode for odin

(require 'js)

(defconst odin-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b")
    (modify-syntax-entry ?* ". 23")
    (modify-syntax-entry ?\n "> b")
    (modify-syntax-entry ?\^m "> b")

    (modify-syntax-entry ?\" "\"")
    (modify-syntax-entry ?\\ "\\")

    ;; Need this for #directive regexes to work correctly
    (modify-syntax-entry ?# "_")

    ;; Additional symbols
    (modify-syntax-entry ?' "\"")
    (modify-syntax-entry ?` "\"")
    (modify-syntax-entry ?: ".")
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?- ".")
    (modify-syntax-entry ?% ".")
    (modify-syntax-entry ?& ".")
    (modify-syntax-entry ?| ".")
    (modify-syntax-entry ?^ ".")
    (modify-syntax-entry ?! ".")
    (modify-syntax-entry ?$ ".")
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?> ".")
    (modify-syntax-entry ?? ".")

    (syntax-table))
  "Syntax table for `odin-mode'.")

(eval-and-compile
  (defconst odin-builtins
    '("len" "cap"
      "typeid_of" "type_info_of"
      "swizzle" "complex" "real" "imag" "quaternion" "conj"
      "jmag" "kmag"
      "min" "max" "abs" "clamp"
      "expand_to_tuple"

      "init_global_temporary_allocator"
      "copy" "pop" "unordered_remove" "ordered_remove" "clear" "reserve"
      "resize" "new" "new_clone" "free" "free_all" "delete" "make"
      "clear_map" "reserve_map" "delete_key" "append_elem" "append_elems"
      "append" "append_string" "clear_dynamic_array" "reserve_dynamic_array"
      "resize_dynamic_array" "incl_elem" "incl_elems" "incl_bit_set"
      "excl_elem" "excl_elems" "excl_bit_set" "incl" "excl" "card"
      "assert" "panic" "unimplemented" "unreachable"))

  (defconst odin-keywords
    '("import" "foreign" "package"
      "where" "when" "if" "else" "for" "switch" "in" "notin" "do" "case"
      "break" "continue" "fallthrough" "defer" "return" "proc"
      "struct" "union" "enum" "bit_field" "bit_set" "map" "dynamic"
      "auto_cast" "cast" "transmute" "distinct" "opaque"
      "using" "inline" "no_inline"
      "size_of" "align_of" "offset_of" "type_of"
      "context"
      "macro" "const"))

  (defconst odin-constants
    '("nil" "true" "false" "---"
      "ODIN_OS" "ODIN_ARCH" "ODIN_ENDIAN" "ODIN_VENDOR"
      "ODIN_VERSION" "ODIN_ROOT" "ODIN_DEBUG"))

  (defconst odin-typenames
    '("bool" "b8" "b16" "b32" "b64"

      "int"  "i8" "i16" "i32" "i64"
      "i16le" "i32le" "i64le"
      "i16be" "i32be" "i64be"
      "i128" "u128"
      "i128le" "u128le"
      "i128be" "u128be"

      "uint" "u8" "u16" "u32" "u64"
      "u16le" "u32le" "u64le"
      "u16be" "u32be" "u64be"

      "f32" "f64"
      "complex64" "complex128"

      "quaternion128" "quaternion256"

      "rune"
      "string" "cstring"

      "uintptr" "rawptr"
      "typeid" "any"
      "byte"))

  (defconst odin-attributes
    '("builtin"
      "export"
      "static"
      "deferred_in" "deferred_none" "deferred_out"
      "require_results"
      "default_calling_convention" "link_name" "link_prefix"
      "deprecated" "private" "thread_local"))

  (defconst odin-proc-directives
    '("#force_inline"
      "#force_no_inline"
      "#type")
    "Directives that can appear before a proc declaration")

  (defconst odin-directives
    (append '("#align" "#packed"
              "#any_int"
              "#raw_union"
              "#no_nil"
              "#shared_nil"
              "#complete"
              "#no_alias"
              "#c_vararg"
              "#assert"
              "#file" "#line" "#location" "#procedure" "#caller_location"
              "#load"
              "#defined"
              "#bounds_check" "#no_bounds_check"
              "#partial")
            odin-proc-directives)))

(defconst odin-highlights
  `((,(regexp-opt odin-builtins 'symbols) . font-lock-builtin-face)
    (,(regexp-opt odin-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt odin-constants 'symbols) . font-lock-constant-face)
    (,(regexp-opt odin-typenames 'symbols) . font-lock-type-face)
    (,(regexp-opt odin-attributes 'symbols) . font-lock-preprocessor-face)
    (,(regexp-opt odin-directives 'symbols) . font-lock-preprocessor-face)
    (,(regexp-opt odin-proc-directives 'symbols) . font-lock-function-name-face)))

;;;###autoload
(define-derived-mode odin-mode prog-mode "odin"
  "Major Mode for editing Odin source code."
  :syntax-table odin-mode-syntax-table
  (setq font-lock-defaults '(odin-highlights))
  (setq-local require-final-newline mode-require-final-newline)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local indent-line-function 'js-indent-line)
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local electric-indent-chars
              (append "{}():;," electric-indent-chars)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.odin\\'" . odin-mode))

(provide 'odin-mode)

;;; odin-mode.el ends here
