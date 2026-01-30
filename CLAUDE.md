# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository and with Gerbil Scheme projects in general.

## Project Overview

Gerbil is a self-hosted dialect of Scheme built on the Gambit runtime and compiler. It features a syntax-case macro system, single-instantiation module system for AOT compilation, and an extensive standard library. Source files use the `.ss` extension.

## Build Commands

**Prerequisites:** `libssl-dev`, `zlib1g-dev`, `libsqlite3-dev`

```shell
./configure                    # generates build-env.sh; run once
make                           # full build (all stages)
make -j4                       # parallel build (passes -j to sub-builds)
sudo make install              # install to $GERBIL_PREFIX (default /opt/gerbil)
make check                     # run all tests
```

### Build Stages

The build is multi-stage and self-hosted. Individual stages can be run via `./build.sh <stage>`:

`prepare` → `gambit` → `boot-gxi` → `stage0` → `stage1` → `stdlib` → `libgerbil` → `lang` → `r7rs-large` → `srfi` → `tools`

When working on the standard library: `./build.sh stdlib` rebuilds just stdlib. Parallel builds use `GERBIL_BUILD_CORES` env var.

### Rebuilding After Changes

- **Standard library changes** (`src/std/`): `./build.sh stdlib`, then install, then test
- **Core changes** (`src/gerbil/` — prelude, runtime, compiler, expander): requires rebootstrapping via `./src/bootstrap.sh`. See `doc/reference/dev/bootstrap.md`
- **Tool changes** (`src/tools/`): `./build.sh tools`

## Testing

```shell
make check                          # all tests
./build.sh env gxtest ./...         # all tests (alternative)
./build.sh env gxtest src/std/...   # tests under src/std/
./build.sh env gxtest -v ./...      # verbose output
./build.sh env gxtest -q ./...      # quiet (errors only)
./build.sh env gxtest -r "pattern" ./...  # filter by name
```

CI runs: `gxtest -q src/gerbil/test/... src/std/... src/lang/... src/tools/...`

### Test Conventions

- Test files are named `*-test.ss` and live next to the code they test
- Test suites export symbols ending in `-test`
- Optional exports: `test-setup!`, `test-cleanup!`
- Framework: `:std/test`

```scheme
(import :std/test)
(export mylib-test)

(def mylib-test
  (test-suite "my lib"
    (test-case "addition"
      (check (+ 1 2) => 3))
    (test-case "exception"
      (check-exception (error "boom") error-exception?))
    (test-case "predicate"
      (check (string? "hi") ? values))))
```

## Source Structure

- `src/gerbil/` — Core language: runtime (`runtime/`), expander (`expander/`), compiler (`compiler/`), prelude (`prelude/`), boot (`boot/`)
- `src/std/` — Standard library. New modules must be added to `src/std/build-spec.ss`. Feature-gated by `build-features.ss` (zlib, sqlite, etc.)
- `src/gambit/` — Gambit Scheme (git submodule)
- `src/lang/` — Language implementations
- `src/r7rs-large/` — R7RS large standard support
- `src/srfi/` — SRFI shims
- `src/tools/` — CLI tools (gxtest, gxpkg, gxtags, gxprof, etc.)
- `bootstrap/` — Precompiled bootstrap sources (generated, not hand-edited)
- `doc/` — Documentation (VuePress), renders to https://cons.io

## Key Architecture Details

- **Self-hosted**: The compiler and expander are written in Gerbil. Bootstrap uses precompiled sources in `bootstrap/gerbil/` to build stage0, which then compiles the real stage1.
- **Single-instantiation modules**: Modules are instantiated once for performance. This is the key difference from Racket's module system.
- **Build tools**: All tools are symlinks to a single `gerbil` universal binary. `gxi` (interpreter/REPL), `gxc` (compiler), `gxtest` (test runner), `gxpkg` (package manager), `gxtags` (tag generator), `gxprof` (profiler).
- **Std lib module convention**: Module source in a directory, re-export file at the same level (e.g., `src/std/net/s3/` directory + `src/std/net/s3.ss` re-export file). Interfaces in `interface.ss`, public API in `api.ss`.

## Code Style

- 2-space indentation
- No formalized style guide; follow existing code patterns
- Stdlib contributions require accompanying documentation

## Branch Naming

- `feat-` for features, `bug-` for fixes, `doc-` for docs, `core-` for core compiler changes

---

# Gerbil Language Reference

## Definition Forms

```scheme
;; def — primary definition form, supports optional/keyword args
(def x 42)
(def (add x y) (+ x y))
(def (greet name (greeting "Hello"))    ; optional arg with default
  (string-append greeting ", " name))
(def (connect host port: (port 80))    ; keyword arg
  ...)

;; def* — multi-arity definition (case-lambda)
(def* my-fn
  (() 'no-args)
  ((x) (list 'one x))
  ((x y) (list 'two x y))
  (args (cons 'many args)))

;; defvalues — bind multiple return values
(defvalues (a b c) (values 1 2 3))

;; defconst — constant definition
(defconst PI 3.14159)

;; definline — inlined definition
(definline (add1 x) (+ x 1))
```

## Lambda and Binding

```scheme
;; lambda — extended lambda with optional/keyword args and destructuring
(lambda (x) (* x x))
(lambda (x (y 5) z: (z 10) . rest) body)

;; case-lambda — multi-arity
(case-lambda
  (() 'zero)
  ((x) (list 'one x))
  ((x y) (list 'two x y)))

;; let, let*, letrec, letrec*
(let ((x 1) (y 2)) (+ x y))
(let loop ((n 0)) (if (< n 10) (loop (+ n 1)) n))  ; named let
(let* ((x 1) (y (+ x 1))) y)
(letrec ((even? (lambda (n) (if (zero? n) #t (odd? (- n 1)))))
         (odd?  (lambda (n) (if (zero? n) #f (even? (- n 1))))))
  (even? 10))

;; alet / alet* / and-let* — anaphoric let, short-circuits on #f
(alet* ((x (find-something))
        (y (process x)))
  (use y))  ; returns #f if any binding is #f

;; let/cc, let/esc — continuation capture
(let/cc return (if condition (return 'early) 'normal))
(let/esc escape (if error? (escape 'bail) 'ok))

;; set! — mutation
(set! x 42)
(set! (@ obj slot) value)       ; slot mutation
(set! (vector-ref v i) value)   ; generalized set!
```

## Control Flow

```scheme
;; if, cond, case
(if test then else)
(cond ((> x 0) 'positive) ((< x 0) 'negative) (else 'zero))
(case val ((1 2) 'small) ((3 4) 'medium) (else 'other))

;; when, unless — one-armed conditionals (return void if not taken)
(when condition body ...)
(unless condition body ...)

;; and, or — short-circuit boolean
(and x y z)
(or x y z)

;; begin, begin0
(begin expr1 expr2 ... last)    ; returns last
(begin0 first expr2 ...)        ; returns first

;; do, while, until
(do ((i 0 (+ i 1)))
    ((>= i 10) 'done)
  (displayln i))
(while condition body ...)
(until condition body ...)

;; parameterize — dynamic binding
(parameterize ((current-port my-port))
  (displayln "goes to my-port"))
```

## Exception Handling

```scheme
;; try / catch / finally
(try
  (risky-operation)
  (catch (io-error? e) (handle-io e))
  (catch (e) (handle-generic e))         ; catch-all
  (finally (cleanup)))

;; with-catch — functional form
(with-catch handler thunk)

;; raise — throw exceptions
(raise (make-Error "message" where: 'my-proc irritants: [val]))

;; ignore-errors — returns #f on any exception
(ignore-errors (/ 1 0))

;; with-destroy — ensure cleanup via destroy method
(with-destroy resource
  (use resource))

;; unwind-protect
(unwind-protect body cleanup ...)
```

## Pattern Matching

```scheme
;; match — destructuring pattern match on a single expression
(match expr
  (0 'zero)                          ; literal
  ((? number?) 'a-number)            ; predicate
  ([a b c] (list c b a))             ; list destructure
  ([a _ . rest] rest)                ; wildcard + rest
  ((cons h t) h)                     ; pair
  (#(x y) (+ x y))                  ; vector
  ((and (? string?) (? (cut string-length <> 5))) 'len5-string)
  ((or 'foo 'bar) 'foo-or-bar)
  ((my-struct x y) (+ x y))         ; struct destructure
  ((my-class (slot val) ...) ...)    ; class destructure
  (_ 'default))                      ; catch-all

;; match* — match multiple values
(match* (x y)
  ((1 2) 'both)
  ((a b) (+ a b)))

;; with / with* — short destructuring bind
(with ([a b . c] some-list) (list a b c))
(with* (([h . t] lst)
        (x (process h)))
  (cons x t))

;; Predicate patterns
(? pred)                  ; test predicate
(? pred pat)              ; test then match
(? pred => proc)          ; test then apply proc
(? pred :: filter => K)   ; filter and transform

;; Boolean pattern combinators
(and pat ...)             ; all must match
(or pat ...)              ; any must match
(not pat)                 ; must not match

;; Equality patterns
(eq? val) (eqv? val) (equal? val) (quote datum)
```

## Module System

```scheme
;; import — bring bindings into scope
(import :std/iter                              ; absolute path
        :std/net/request
        ./local-module                         ; relative path
        (only-in :std/list map filter)         ; selective
        (except-in :std/list iota)             ; exclusion
        (rename-in :std/list (map my-map))     ; rename
        (prefix-in my- :std/list)              ; prefix all
        (for-syntax :std/stxutil))             ; compile-time phase

;; export — make bindings available
(export my-fn my-var)
(export #t)                                    ; export everything
(export (rename-out (internal-name public-name)))
(export (struct-out my-struct))                ; struct + accessors
(export (import: ./submodule))                 ; re-export

;; Module path conventions
;;   :std/...       — standard library
;;   :gerbil/...    — core gerbil
;;   :myapp/...     — application (relative to project root)
;;   ./relative     — relative to current file
;;   :srfi/N        — SRFI modules

;; cond-expand — conditional compilation
(cond-expand
  (gerbil-smp (import :std/misc/threads))
  (else (void)))
```

## Object-Oriented Programming

### Structs (Value Types)

```scheme
;; defstruct — simple record types
(defstruct point (x y)
  transparent: #t)           ; enables printing and equality

(defstruct (point3d point) (z)  ; inheritance
  transparent: #t)

;; Auto-generated:
;;   make-point, point?, point-x, point-y
;;   point-x-set!, point-y-set!
;;   make-point3d, point3d?, point3d-z

(def p (make-point 1 2))
(point-x p)                 ; => 1
(point? p)                  ; => #t
```

### Classes

```scheme
;; defclass — classes with keyword construction and methods
(defclass animal (name age)
  transparent: #t)

(defclass (dog animal) (breed)
  constructor: :init!)

;; Constructor method
(defmethod {:init! dog}
  (lambda (self name: name age: age breed: breed)
    (set! self.name name)
    (set! self.age age)
    (set! self.breed breed)))

;; Methods
(defmethod {speak dog}
  (lambda (self)
    (string-append self.name " says woof!")))

;; Usage
(def d (make-dog name: "Rex" age: 5 breed: "Lab"))
{d.speak}                    ; => "Rex says woof!"
(@ d name)                   ; => "Rex"   (dynamic slot access)
(set! (@ d age) 6)           ; slot mutation

;; Class options
;;   name:         display name
;;   id:           unique type id
;;   constructor:  constructor method name
;;   final:        prevent subclassing
;;   transparent:  enable equality/printing
;;   equal:        #t or list of slots for equality
;;   print:        #t or list of slots for display
```

### Method Invocation

```scheme
;; Dot syntax (reader macro)
{obj.method arg ...}         ; => (call-method obj 'method arg ...)
obj.field                    ; => (slot-ref obj 'field)

;; Dynamic slot access
(@ obj slot-name)            ; slot-ref
(set! (@ obj slot-name) val) ; slot-set!

;; using — typed access (optimized, no dynamic dispatch)
(using (obj :- MyClass)
  (+ obj.x obj.y))
```

### Interfaces

```scheme
(interface Printable
  (to-string))

(defclass widget (label)
  transparent: #t)

(defmethod {to-string widget}
  (lambda (self) self.label))

;; Type contracts
(def (print-it (obj : Printable))
  (displayln {obj.to-string}))
```

## Macro System

```scheme
;; defrules — pattern-based macros (like syntax-rules)
(defrules my-when ()
  ((_ test body ...)
   (if test (begin body ...) (void))))

;; defrule — single-clause variant
(defrule (swap! a b)
  (let ((tmp a)) (set! a b) (set! b tmp)))

;; defsyntax — procedural macros with syntax-case
(defsyntax (my-macro stx)
  (syntax-case stx ()
    ((_ x y)
     #'(list x y))
    ((_ x)
     (with-syntax ((default #'0))
       #'(list x default)))))

;; with-syntax — bind syntax objects in body
(with-syntax ((name #'my-name)
              ((arg ...) #'(a b c)))
  #'(def (name arg ...) (list arg ...)))

;; begin-syntax — compile-time evaluation
(begin-syntax
  (def (helper stx) ...))

;; syntax/loc — syntax with source location preservation
(syntax/loc stx (new-form args ...))
```

## Reader Syntax and Literals

```scheme
;; Lists
'(1 2 3)                     ; quoted list
[1 2 3]                      ; list sugar for (@list 1 2 3)
(cons 1 (cons 2 '()))        ; explicit cons

;; Vectors
#(1 2 3)                     ; vector literal

;; Byte vectors
#u8(0 1 2 255)               ; byte vector
(@bytes "hello")             ; utf-8 string -> u8vector

;; Hash tables (literal syntax)
(hash ("key" 1) ("other" 2))         ; hash-table with equal?
(hash-eq (a 1) (b 2))               ; hash-table with eq?
(hash-eqv (1 'one) (2 'two))        ; hash-table with eqv?

;; Booleans
#t #f

;; Characters
#\a #\space #\newline

;; Keywords
foo:                          ; keyword
name: value                   ; keyword argument

;; Method/slot reader macros
{obj.method args}             ; method invocation
obj.field                     ; field access

;; Quasiquote
`(a ,x ,@xs)                 ; quasiquote, unquote, unquote-splicing

;; Void
(void)                        ; void value
#!void                        ; void literal

;; Box
(box val)                     ; create box
#&pat                         ; box pattern/syntax
```

## Common Sugar and Idioms

### Pipeline with `chain`

```scheme
(import :std/sugar)
(chain [1 2 3]
  (map (cut + <> 1) <>)       ; <> is the slot for the piped value
  (filter even? <>)
  (apply + <>))
```

### Partial Application with `cut`

```scheme
;; SRFI-26 cut — create procedures with "slots"
(cut + <> 1)                  ; => (lambda (x) (+ x 1))
(cut map <> <>)               ; => (lambda (f xs) (map f xs))
(cut string-append "hi " <> <...>) ; <...> = rest args
```

### let-hash — Hash Table Destructuring

```scheme
(import :std/sugar)
(let-hash config
  .hostname                   ; (hash-ref config 'hostname)
  .?port                      ; (hash-get config 'port) — returns #f if missing
  .$name                      ; (hash-get config "name") — string key
  (connect .hostname (or .?port 80)))
```

### Conditional Binding

```scheme
(import :std/sugar)
(if-let (x (hash-get table 'key))
  (process x)
  (handle-missing))

(when-let ((x (find pred lst)))
  (use x))
```

### Predicate Construction with `is`

```scheme
(import :std/sugar)
(find (is car 'target) alist)     ; find by car
(filter (is cdr positive?) pairs) ; filter by cdr predicate
```

## Iteration

```scheme
(import :std/iter)

;; for — general iteration
(for (x '(1 2 3)) (displayln x))
(for (x (in-range 0 10)) (displayln x))
(for (x (in-range 0 10) when (even? x)) (displayln x))

;; for with multiple bindings (cartesian product)
(for ((x '(a b)) (y '(1 2)))
  (displayln x y))

;; for/collect — build a list
(for/collect (x '(1 2 3)) (* x x))   ; => (1 4 9)

;; for/fold — reduce
(for/fold (acc 0) (x '(1 2 3)) (+ acc x))  ; => 6

;; Hash table iteration
(for ((k v) (in-hash ht)) (displayln k " => " v))
(for (k (in-hash-keys ht)) (displayln k))
(for (v (in-hash-values ht)) (displayln v))

;; Iterating with index
(for ((x i) (in-indexed '(a b c)))
  (displayln i ": " x))

;; Generators (coroutine-based)
(import :std/coroutine)
(def my-gen
  (coroutine (lambda () (yield 1) (yield 2) (yield 3))))
(for (x my-gen) (displayln x))
```

## Hash Tables

```scheme
;; Creation
(def ht (make-hash-table))              ; default (eq? keys)
(def ht (make-hash-table test: equal?)) ; string/list keys
(def ht (hash ("a" 1) ("b" 2)))         ; literal syntax

;; Operations
(hash-put! ht key value)
(hash-ref ht key)               ; error if missing
(hash-get ht key)               ; #f if missing
(hash-get ht key default)       ; custom default
(hash-key? ht key)              ; membership test
(hash-remove! ht key)
(hash-update! ht key proc default)
(hash-clear! ht)

;; Iteration
(hash-for-each (lambda (k v) ...) ht)
(hash-fold (lambda (k v acc) ...) init ht)
(hash->list ht)                 ; => ((k . v) ...)
(list->hash-table alist)

;; Functional variants
(hash-ref-default ht key default)
(hash-merge ht1 ht2)
(hash-copy ht)
```

## Concurrency

### Threads

```scheme
;; Basic threads
(def t (spawn (lambda () (do-work))))
(def t (spawn/name 'worker (lambda () (do-work))))
(thread-join! t)

;; Mutexes
(def m (make-mutex 'my-mutex))
(mutex-lock! m)
(mutex-unlock! m)

;; Condition variables
(def cv (make-condition-variable 'signal))
(condition-variable-signal! cv)
(condition-variable-broadcast! cv)
(mutex-unlock! m cv timeout)     ; wait for signal
```

### Channels

```scheme
(import :std/misc/channel)

(def ch (make-channel))           ; unbuffered
(def ch (make-channel 10))        ; buffered

(channel-put ch value)            ; blocks if full
(channel-get ch)                  ; blocks if empty
(channel-try-put ch value)        ; non-blocking
(channel-try-get ch default)      ; non-blocking
(channel-close ch)

;; Iterate over channel until closed
(for (msg ch) (process msg))
```

### Coroutines

```scheme
(import :std/coroutine)

(def gen (coroutine (lambda () (yield 1) (yield 2) (yield 3))))
(continue gen)  ; => 1
(continue gen)  ; => 2
```

## I/O

```scheme
;; File I/O
(call-with-input-file "path" (lambda (port) (read-line port)))
(call-with-output-file "path" (lambda (port) (displayln "data" port)))

;; String I/O
(call-with-output-string (lambda (p) (display "hello" p)))

;; Display / Write
(displayln value)               ; human-readable + newline
(display value port)
(write value port)              ; machine-readable (re-readable)
(fprintf port "~a: ~d~n" name count)

;; Buffered I/O (std/io)
(import :std/io)
;; Uses reader/writer abstractions for buffered, typed I/O
```

## Networking

```scheme
;; HTTP client
(import :std/net/request)
(def resp (http-get "https://example.com"
            headers: '(("Authorization" . "Bearer token"))))
(request-status resp)            ; HTTP status code
(request-text resp)              ; response body as string
(request-json resp)              ; parse JSON response

(http-post url data: body headers: headers)
(http-put url data: body)
(http-delete url)

;; JSON
(import :std/text/json)
(def data (call-with-input-string json-str read-json))
(call-with-output-string (lambda (p) (write-json data p)))
```

## Text Processing

```scheme
;; String utilities
(import :std/misc/string)
(string-trim-prefix "Hello " "Hello World")  ; => "World"
(string-trim-eol "line\r\n")                 ; => "line"
(string-subst old new str)

;; Regular expressions
(import :std/pregexp)
(pregexp-match "([a-z]+)@([a-z.]+)" "test@example.com")
(pregexp-replace pattern text replacement)
(pregexp-split pattern text)

;; Format strings (printf-style)
(import :std/format)
(format "~a has ~d items~n" name count)
;;   ~a  display (human-readable)
;;   ~s  write (machine-readable)
;;   ~d  decimal integer
;;   ~n  newline
;;   ~w  write with shared structure
```

## Type Contracts

```scheme
;; Type annotations in function signatures
(def (add (x : integer) (y : integer)) (+ x y))

;; Optional types (value or #f)
(def (find-user (id :? string)) ...)

;; Using — typed access for optimization
(using (p :- point)
  (+ p.x p.y))

;; using with multiple bindings
(using ((p :- point) (q :- point))
  (+ p.x q.x))
```

## FFI (Foreign Function Interface)

FFI is through Gambit's C-interface. Use `:std/foreign` helpers:

```scheme
(import :std/foreign)
(export my-c-function)

(begin-ffi (my-c-function)
  (c-declare "#include <mylib.h>")
  (define-c-lambda my-c-function (int int) int "c_add"))

;; Common FFI forms inside begin-ffi:
;;   (define-c-lambda name (arg-types) ret-type "c_name")
;;   (define-const NAME)                     ; import C constant
;;   (define-with-errno name ffi-id args)    ; POSIX errno wrapper
;;   (define-c-struct name ((field type) ...)) ; struct interface
```

## Key Standard Library Modules

| Module | Purpose |
|--------|---------|
| `:std/iter` | Iteration (`for`, `for/collect`, `for/fold`, `in-range`) |
| `:std/test` | Testing framework |
| `:std/sugar` | Common sugar (`if-let`, `chain`, `let-hash`, `is`) |
| `:std/error` | Error types and helpers |
| `:std/format` | Printf-style formatting |
| `:std/sort` | Sorting |
| `:std/text/json` | JSON read/write |
| `:std/text/csv` | CSV parsing |
| `:std/text/utf8` | UTF-8 encoding |
| `:std/pregexp` | Regular expressions |
| `:std/net/request` | HTTP client |
| `:std/net/httpd` | HTTP server |
| `:std/net/websocket` | WebSocket support |
| `:std/net/ssl` | TLS/SSL |
| `:std/io` | Buffered I/O |
| `:std/os/socket` | Low-level sockets |
| `:std/misc/channel` | Thread channels |
| `:std/misc/threads` | Thread utilities |
| `:std/misc/string` | String utilities |
| `:std/misc/list` | List utilities (`unique`, `flatten`, `slice`) |
| `:std/misc/list-builder` | Efficient list accumulation |
| `:std/misc/queue` | FIFO queues |
| `:std/misc/deque` | Double-ended queues |
| `:std/misc/rbtree` | Red-black trees |
| `:std/misc/pqueue` | Priority queues |
| `:std/misc/sync` | Synchronized data structures |
| `:std/coroutine` | Coroutines and generators |
| `:std/generic` | Generic functions (multi-dispatch) |
| `:std/foreign` | FFI helpers |
| `:std/actor` | Actor model |
| `:std/crypto` | Cryptographic operations |
| `:std/db/postgresql` | PostgreSQL driver |
| `:std/db/sqlite` | SQLite driver |
| `:std/srfi/1` | SRFI-1 list library |
| `:std/srfi/13` | SRFI-13 string library |

## Environment Variables

```bash
GERBIL_PATH              # Workspace root (~/.gerbil default)
GERBIL_LOADPATH          # Colon-separated dirs appended to load path
GERBIL_BUILD_CORES       # Parallel build cores
GERBIL_BUILD_NOOPT       # Disable optimizations (set to t)
```

## Gotchas and Important Notes

1. **Always compile with `-O`** — Without optimizer, code runs 10-100x slower.
2. **`[x y z]` is list syntax**, not vector. Vectors use `#(x y z)`.
3. **`def` vs `define`** — `def` supports optional/keyword args; `define` uses plain `lambda%`. Prefer `def`.
4. **Keywords end with colon** — `name:`, `port:`, not `:name`.
5. **`#f` and `'()` are different** — `#f` is false; `'()` (empty list) is truthy.
6. **`set!` is generalized** — Works with `@`, `vector-ref`, etc. via macro dispatch.
7. **Module paths use colons** — `:std/iter` not `std/iter`.
8. **Gambit `##` functions** — Prefixed with `##` (e.g., `##vector-ref`). These are unsafe, unchecked Gambit primitives. Use only in performance-critical code with `(declare (not safe))`.
9. **Bootstrap API compatibility** — Changes to `src/gerbil/` core require multi-step migration to maintain bootstrap chain.
10. **New stdlib modules** must be registered in `src/std/build-spec.ss`.
11. **Test discovery** requires files named `*-test.ss` exporting `*-test` symbols.
12. **`(declare (not safe))`** — Disables bounds checks. Only use in inner loops where profiling shows overhead. Never ship with full-program `(not safe)`.
