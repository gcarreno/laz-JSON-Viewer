# How to contribute

I'm very happy that anyone wants to contribute!

## Indentation

Use two `<space>`.

No `<tab>` please.

## Assignment

No `<space>` before `:=` and one after.

Example
```pascal
var
  Name: String;
begin
  Name:= 'My Name';
end.
```

## Constant assignment

One `<space>` before `=` and one after.

Example
```pascal
const
  cName = 'My Name';
```

## Comparison

One `<space>` before `=` and one after.

Example
```pascal
var
  index: Integer;
begin
  if index = 1 then
  begin
    // Do something
  end;
end.
```

## `begin..end` blocks

All `if`, `for`, etc... will have a `begin..end` block.

No one liners please.

Example
```pascal
begin
  if FValue = aValue then
  begin
    exit;
  end;
end.
```

## Uses clause format

- No trailing commas
- Indent first unit by two `<space>`
- Subsequent unit are preceded by a comma and a `<space>`
- Separate the system units from the project units with a blank line
- Finish with a line containing the  `;`

Example
```pascal
uses
  Unit1
, Unit2
, Unit3

, Name.Space.Unit2
;
```

## Classes interface declaration

Always include `TObject` in the `class()`.

Always include `private`, `protected`, `public` and `published` even if there is nothing to declare.

Property's `read` and `write` are indented by two `<space>`.

Example
```pascal
type
  TSomeClass = class(TObject)
  private
    FName: String;
  protected
  public
    property Name: String
      read FName
      write FName;
  published
  end;
```

## Commit Messages

(Originally from the [Udacity Git Commit Message Style Guide](https://udacity.github.io/git-styleguide/index.html))

### Message Structure

A commit messages consists of three distinct parts separated by a blank line: the title, an optional body and an optional footer.

The layout looks like this:
```
type: subject

body

footer
```

The title consists of the type of the message and subject.

### The Type

The type is contained within the title and can be one of these types:

* feat: a new feature
* fix: a bug fix
* docs: changes to documentation
* i18n: changes to translation files
* style: formatting, missing semi colons, etc; no code change
* refactor: refactoring production code
* test: adding tests, refactoring test; no production code change
* chore: updating build tasks, package manager configs, etc; no production code change

### The Subject

Subjects should be no greater than 50 characters, should begin with a capital letter and do not end with a period.

Use an imperative tone to describe what a commit does, rather than what it did. For example, use change; not changed or changes.
The Body

Not all commits are complex enough to warrant a body, therefore it is optional and only used when a commit requires a bit of explanation and context. Use the body to explain the what and why of a commit, not the how.

When writing a body, the blank line between the title and the body is required and you should limit the length of each line to no more than 72 characters.

### The Footer

The footer is optional and is used to reference issue tracker IDs.

### Example Commit Message

```
feat: Summarise changes in around 50 characters or less

More detailed explanatory text, if necessary. Wrap it to about 72
characters or so. In some contexts, the first line is treated as the
subject of the commit and the rest of the text as the body. The
blank line separating the summary from the body is critical (unless
you omit the body entirely); various tools like `log`, `shortlog`
and `rebase` can get confused if you run the two together.

Explain the problem that this commit is solving. Focus on why you
are making this change as opposed to how (the code explains that).
Are there side effects or other non intuitive consequences of this
change? Here's the place to explain them.

Further paragraphs come after blank lines.

 - Bullet points are okay, too

 - Typically a hyphen or asterisk is used for the bullet, preceded
   by a single space, with blank lines in between, but conventions
   vary here

If you use an issue tracker, put references to them at the bottom,
like this:

Resolves: #123
See also: #456, #789
```
