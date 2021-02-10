# Installation

```
(ql:quickload "cl-fxml")
```

# Usage

For template code interleaved with normal code including top-level forms use:

```
(named-readtables:in-readtable cl-fxml:syntax)
```
or wrap you normal code interleaving template code with `with-xml`:

```
(cl-fxml:with-xml
  ((:p)
    "Paragraph number one.))
```

The variable `cl-fxml:*new-line-after-opening*` controls whether to make a newline after `>` of the opening tags.

# Examples

```
((:html)
  ((:head)
    ((:title) "Html example"))
  ((:body)
    ((:a :href "#") "Lorem ipsum dolor sit amet")))
```

Output redirection to string:

```
(let ((str (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
  (with-output-to-string (*standard-output* str)
    ((:html)
      ((:head)
        ((:title) "Html example"))
      ((:body)
        ((:a :href "#") "Lorem ipsum dolor sit amet"))))
  str)
```

Interleaving code:

```
(let ((x 25))
  ((:root :x x 'attribute-without-value)
    (loop for i below 10
          do ((:element :index i)
               (format t "~:r" i)))))
```

A template in a variable (_defvar_ has to be top-level, that's why we switch syntax after top-level forms):

```
(defvar *x* '((:element :x x) "Test"))

(named-readtables:in-readtable cl-fxml:syntax)

((:root 'blah)
  (eval `(cl-fxml:with-xml
           (let ((x 123))
             ,*x*))))
```

Comment:

```
((:!-- "This is a comment."))
```

For lower case use the standard symbol character escaping (e.g. `:|RootElement|`):

```
((:|RootElement| '|Blah|)
  ((:|Test|)
    "Content"))
```

# TODO

- [x] comments `<!-- ... -->`
- [ ] escaping `<![CDATA[ ... ]]>`, ampersand escape sequences
- [ ] in-place computation of tags and attributes, seems hard to do
- [ ] hook for transforming values which aren't tags, we're now accepting strings (perhaps using multiple values for distinguishing nil caused by tags from other values)
