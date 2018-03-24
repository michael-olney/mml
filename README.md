# MML

**Author:** Michael Olney &lt;michael@spectralforms.com&gt;<br/>
**Version:** 0.3<br/>
**Status:** Unstable<br/>

MML is a markup language intended to provide a better
way to specify HTML documents. The main advantages are a simplified
syntax and a template system that makes it easier to achieve
abstraction when specifying document contents.
The following table shows the
relation between the syntax of MML and HTML:

| HTML                    | MML                    |
| :---------------------- | :--------------------- |
| `<br/>`                 | `{br}`                 |
| `<em></em>`             | `{em->}`               |
| `<a href=cat>meow</a>`  | `{a{href->cat}->meow}` |
| `two&nbsp;&nbsp;spaces` | `two~~spaces`          |

Unlike HTML, whitespace reduction within a tag body is part
of the syntax, though the renderer is free to reduce the emitted
whitespace further.

C-style comments ('/* */') are available. Characters are escaped either
by preceding them with a backslash ('\') or surrounding them with
backticks ('\`'). Literal characters are passed on as literal to HTML,
but no attempt is made to prevent the renderer from doing things like
collapsing whitespace. However, the syntax is designed to deal with these
issues at the syntax level so that users don't usually have to worry about
HTML's whitespace rules.

An important distiction between MML and HTML is that MML attribute
values can contain the document structures (tags, text etc) that are
allowed inside tag bodies. HTML allows only strings. This is important
for the custom tag system.

Mammal is inspired partly by Erik Naggum's unreleased [Enamel][1]
(NML) markup language.

## Tags as a Form of Abstraction

MML also supports the creation of custom tags through a template syntax.
Tags are defined by placing MML files (with the extension .mtag) into the
path of the MML processor containing tags with names beginning with `$` inside
them. These special tags are treated as variables; when the tag is included in
an MML file the attributes in the specific instance of the tag are
substituted into the variables in the .mtag file.  For example, a
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

In programming terms, you can think of a custom tag as a function
that takes in some arguments and returns document structure.
Under this analogy, the attributes are named arguments and the child
elements are the the positional arguments.

## Future Work

So far MML is just a system I designed to build my personal website with.
A more complete tool would need lots of extra features, including:

* Control structures and other programming constructs for defining
  custom tags.
* Encapsulation of CSS such that selectors cannot be used to cross
  abstraction boundaries.
* Custom CSS attributes, so that client CSS can usefully customize
  any custom elements.
* A module system. It would be useful to organise tags into modules
  that can be imported and reused.
* Encapsulation of DOM elements and other JavaScript structures.

## Running

To install the package, use cabal:

    $ stack install

To convert an MML file to HTML, run the `mml` command:

    $ mml --infile=index.mml --infmt=mml --outfile=index.html --outfmt=html

[1]: http://www.schnada.de/grapt/eriknaggum-enamel.html
[2]: https://html.spec.whatwg.org/multipage/dom.html#phrasing-content
