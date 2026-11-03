# Indentation

With inspiration from
[scmindent](https://github.com/ds26gte/scmindent), this code also
tries to provide a style similar to Emacs while making it
straightforward to add new forms.  This code does not currently
provide "options" as we'd like to apply the same rules to all
code. This may change.

This indenter reads tokens with Chez Scheme's `read-token`, and uses
regular expressions for what that skips, such as comments, or can't
read. This allows it to track both code and comments. Generally, it
follows this workflow:

* tokenize the input, reading strings, characters, and numbers whole
  and marking them with those properties

* mark block comments and datums commented out with `#;` as comments

* indent code

* indent comments

## Not Like Emacs

Default Emacs configurations use a `fill-column` of 70. Doom Emacs
defaults this to 80 without being explicitly set in the user's
configuration. This code uses 70.

The following examples attempt to express the differences between the
stock Emacs 27.2 configuration and this indentation code.

### Block comments `#| ... |#`

The indenter leaves block comments alone. This makes them more
convenient for embedding ASCII art and other insignificant trifles of
little importance.

### Datum comments `#;`

The indenter applies its normal code indentation strategy for datum
comments `#;`.

### Head subforms

In Emacs:
``` scheme
(f e1
   e2
   e3)

(foo e1
     e2
     e3)
```

The indenter opts for two spaces. For the `f` case, this seems less
pleasing, but has the advantage that the subsequent forms will remain
at the same level if `f` is renamed.
``` scheme
(f e1
  e2
  e3)

(foo e1
  e2
  e3)
```

In Emacs most mathematical operators and single-character identifiers
work as expected. To achieve similar results, the indenter overrides
its default behavior for `+`, `-`, `*`, `/`, `!`, `@`, `$`, `%`, `^`,
`&`, `:`, `?`, `~`, and `_`. Both indent like this:
``` scheme
(+ e1
   e2
   e3)

(@ e1
   e2
   e3)
```

### Certain bytevectors

In Emacs:
``` scheme
#vu8(#x01 #x02 #x03
          #x04 #x05 #x06)
```

The indenter:
``` scheme
#vu8(#x01 #x02 #x03
     #x04 #x05 #x06)
```

### Certain `cond` cases

In Emacs:
``` scheme
(cond
 [(and (> (- end start) 0)
       (as-token text start end #t)) =>
       (lambda (t) (proc acc t start end))]
 [else acc])
```

The indenter:
``` scheme
(cond
 [(and (> (- end start) 0)
       (as-token text start end #t)) =>
  (lambda (t) (proc acc t start end))]
 [else acc])
```

### Certain `match` cases

In Emacs:
``` scheme
(match x
       [,x
        12]
       [#vu8(1 2 3)
            13])
```

The indenter:
``` scheme
(match x
  [,x
   12]
  [#vu8(1 2 3)
   13])
```

### Lines that end in dot

In Emacs:
``` scheme
(match x
       [(,abc .
              ,def)
        12])
```

The indenter:
``` scheme
(match x
  [(,abc .
    ,def)
   12])
```

### Forms with a body

Emacs gives some forms, such as `lambda`, `let`, `case`, `library` and
`syntax-rules`, a special indentation. When the first subform starts on
the line after the head, Emacs indents it four columns and the body two.
The indenter indents these forms like any other.

In Emacs:
``` scheme
(lambda
    (args)
  body)
```

The indenter:
``` scheme
(lambda
 (args)
 body)
```

### Named Let

Emacs handles `let` differently than other forms. Emacs may win this
battle.

In Emacs:
``` scheme
(let lp
    ([x 12]
     [y 13])
  body
  ...)
```

The indenter:
``` scheme
(let lp
  ([x 12]
   [y 13])
  body
  ...)
```

### Unbalanced parenthesis

Emacs fails to indent this expression.

The indenter:
``` scheme
)))))(let ([x 12]
           [y 13])
       body
       ...)
```
