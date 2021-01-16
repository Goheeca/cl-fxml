# Installation



```
(ql:quickload "cl-fxml")
```

# Usage

```
(named-readtables:in-readtable cl-fxml:syntax)
```

_TODO_: Explain `cl-fxml:*new-line-after-opening*`.

# Examples

```
((:html)
  ((:head)
    ((:title) "Html example"))
  ((:body)
    ((:a :href "#") "Lorem ipsum dolor sit amet")))
```

_TODO_: show *standard-output* capturing, Common Lisp interleaving template code.
