# Mammal

**Author:** Michael Olney &lt;michael@spectralforms.com&gt;<br/>
**Version:** 0.3<br/>
**Status:** Unstable<br/>

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
of the syntax, though the renderer is free to reduce the emitted
whitespace further.

C-style comments ('/* */') are available. Characters are escaped either
by preceding them with a backslash ('\') or surrounding them with
backticks ('\`'). Literal characters are passed on as literal to HTML,
but no attempt is made to prevent the renderer from doing things like
collapsing whitespace. However, the syntax is designed to deal with these
issues at the syntax level so that users don't usually have to worry about
HTML's whitespace rules.

Mammal is inspired partly by Erik Naggum's unreleased [Enamel][1]
(NML) markup language.

## Templates

In addition to the pure syntax, there is also additional, optional
syntax for basic templates and raw text inclusion. Templates are defined
by placing files into the path of the MML processor and placing tags
with names that begin with `$` as variable usages. For example, a
template might be created by placing the following into a file called
`field-entry.mtag`

    {div{class->field-entry}->
        {label{for->{$name}}->{$friendly-name}}
        {input{type->text}{name->{$name}}}
    }

This template could then be invoked with the following syntax:

    {@field-entry{name->first_name}{friendly-name->First Name}}

The substitution performed is syntactic and the results are
guaranteed to be valid MML. Using this mechanism it is possible to
abstract away some of your HTML tags in order to achieve a more
semantic style of markup, e.g.:

    {@band-list{name->The Beatles}{members->
        {@band-member{name->John Lennon}{portrait->john.jpg}}
        {@band-member{name->Paul McCartney}{portrait->paul.jpg}}
        {@band-member{name->Ringo Starr}{portrait->ringo.jpg}}
        {@band-member{name->George Harrison}{portrait->george.jpg}}
    }

## Limitations

Some limitations that might be useful to fix in a future version:

* Template files cannot control whether or not they are treated as
  phrasing content for the purposes of whitespace reduction, which
  makes it impossible to define new phrasing tags.
* Template names are coupled to a fixed template regardless of
  context, so (CSS aside) the same information always has to be
  presented in the same way.
* No control structures aside from invocation of other templates
  are provided for use within templates.
* CSS is not encapsulated by the templates. Ideally, templates
  would provide their own custom CSS attributes and selectors and
  all the HTML tags in the expansion would be hidden from any
  external CSS selectors.

## Running

To install the package, use cabal:

    $ cabal install mml

To convert an MML file to HTML, run the `mml` command:

    $ mml --infile=index.mml --infmt=mml --outfile=index.html --outfmt=html

[1]: http://www.schnada.de/grapt/eriknaggum-enamel.html
[2]: https://html.spec.whatwg.org/multipage/dom.html#phrasing-content
