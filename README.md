# Installation

```
(ql:quickload "cl-fxml")
```

# Usage

For template code interleaved with normal code including top-level forms use:

```
(named-readtables:in-readtable cl-fxml:syntax)
```

or wrap your template code with `with-xml`:

```
(cl-fxml:with-xml
  ((:p)
    "Paragraph number one."))
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

A template in a variable:

```
(defvar *x* '((:element :x x) "Test"))

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

CDATA:

```
((:![CDATA[ "Raw data with symbols < >"))
```

[Billion laughs attack](https://en.wikipedia.org/wiki/Billion_laughs_attack):

```
((:?xml :version "1.0"))
((:!doctype 'lolz "["
  ((:!entity "lol0" (format nil "~s" "lol")))
  ((:!element 'lolz "(#PCDATA)"))
  (loop for i from 1 below 10
    do ((:!entity (format nil "lol~a" i)
                  (format nil "~s"
                          (apply #'concatenate 'string
                                 (loop repeat 10
                                   collect (format nil "&lol~a;" (1- i))))))))
  "]"))
((:lolz)
  "&lol9;")
```

## Namespaces

XSD file:

```
(setf (readtable-case *readtable*) :preserve)

(((:xsd :schema) (:xmlns :xsd) "http://www.w3.org/2001/XMLSchema"
                 (:xmlns :tns) "http://tempuri.org/PurchaseOrderSchema.xsd"
                 :targetNamespace "http://tempuri.org/PurchaseOrderSchema.xsd"
                 :elementFormDefault "qualified")
  (((:xsd :element) :name "PurchaseOrder" :type "tns:PurchaseOrderType"))
  (((:xsd :complexType) :name "PurchaseOrderType")
    (((:xsd :sequence))
      (((:xsd :element) :name "ShipTo" :type "tns:USAddress" :maxOccurs "2"))
      (((:xsd :element) :name "BillTo" :type "tns:USAddress")))
    (((:xsd :attribute) :name "OrderDate" :type "xsd:date")))

  (((:xsd :complexType) :name "USAddress")
    (((:xsd :sequence))
      (((:xsd :element) :name "name" :type "xsd:string"))
      (((:xsd :element) :name "street" :type "xsd:string"))
      (((:xsd :element) :name "city" :type "xsd:string"))
      (((:xsd :element) :name "state" :type "xsd:string"))
      (((:xsd :element) :name "zip" :type "xsd:integer")))
    (((:xsd :attribute) :name "country" :type "xsd:NMTOKEN" :fixed "US"))))

(SETF (READTABLE-CASE *READTABLE*) :UPCASE)
```

## Computable element and attribute names

### Names themselves

Names can be

* keywords: `:element-name`
* namespaced: `(:namespace :element-name)`
* computed: `(:|| (if (= 0 (random 2)) :even :odd))`
* computed and namespaced: `(:|| :namespace (if (= 0 (random 2)) :even :odd))`

In the composite names, only the first atom has to be a keyword, i.e. you can use: `(:|| (gensym) (gensym))`.

You can use it recursively:

```
(let ((r (random 2)))
  (((:|| (if (= 0 r) (:even :zero) (:odd :one)))
      :first-param "value-1"
      (:|| (if (= 0 r) (:zero r) (:one r))) "value-2"
      :last-param "value-3")
    "body"))
```

### Attributes

You can compute the sequence of attributes:

```
((:element :pre "pre" (loop for i below 3 append `(,(:ns (format nil "param-~a" i)) ,i)) :post "post")
  "body")
```

# TODO

- [x] comments `<!-- ... -->`
- [x] escaping `<![CDATA[ ... ]]>`
- [ ] ampersand escape sequences
- [x] namespaced names
- [x] in-place computation of element and attribute names
- [ ] hook for transforming body values which aren't elements, we're now accepting only strings (perhaps using multiple values for distinguishing nil caused by elements from other values)
