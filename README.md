# Mammal

Mammal (MML) is a markup language intended to provide a simplified
method for specifying HTML documents. The following table shows the
relation between the syntax of MML and HTML:

| HTML                    | MML                    |
| :---------------------- | :--------------------- |
| `<br/>`                 | `{br}`                 |
| `<em></em>`             | `{em->}`               |
| `<a href=cat>meow</a>`  | `{a{href->cat}->meow}` |
| `two&nbsp;&nbsp;spaces` | `two~~spaces`          |

Unlike HTML or XML, whitespace reduction within a tag body is part
of the syntax. The rule is simple: spaces occurring between two
tags which are not list as [phrasing content][2] in the HTML standard
is eliminated completely. All other whitespace is reduced to a single
non-breaking space (U+00A0). Whitespace that is escaped using a
backslash is not considered whitespace for the purposes
of whitespace handling. In this approach whitespace being used to
neatly arrange the source markup is eliminated before the rendering
stage is reached. Additional non-breaking spaces can be introduced
using the tilde character ('~').

C-style comments are available (but not C++-style single-line
comments). Characters are escaped either by preceding them with a
backslash ('\') or surrounding them with backticks ('\`'). Literal
characters are passed on as literal to HTML, but no attempt is made
to prevent the renderer from doing things like collapsing whitespace.
However, the syntax is designed to deal with these issues at the syntax
level so that users don't usually have to worry about HTML's whitespace
rules.

Mammal is inspired partly by Erik Naggum's unreleased [Enamel][1]
(NML) markup language.

[1]: http://www.schnada.de/grapt/eriknaggum-enamel.html
[2]: https://html.spec.whatwg.org/multipage/dom.html#phrasing-content