A major mode to edit Standard ML (SML) code.
Provides the following features, among others:
- Indentation.
- Syntax highlighting.
- Prettified display of ->, =>, fn, ...
- Imenu.
- which-function-mode.
- Skeletons/templates.
- Electric pipe key.
- outline-minor-mode (with some known problems).
- Interaction with a read-eval-print loop.

Known bugs:

- Indentation after "functor toto() where type foo = bar ="
  Because the last is treated as an equality comparison.
- indentation of a declaration after a long `datatype' can be slow.
