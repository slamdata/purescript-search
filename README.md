[![Build Status](https://travis-ci.org/slamdata/purescript-search.svg?branch=master)](https://travis-ci.org/slamdata/purescript-search)

# purescript-search

A grammar and parser for Google-style searches on unstructured or semistructured data

## Installation

```shell
bower install purescript-search
```

## Documentation

Module documentation is published on Pursuit: [http://pursuit.purescript.org/packages/purescript-search](http://pursuit.purescript.org/packages/purescript-search)

## Examples

### Simple Searches

1. Search for everything containing the text "foo":

    ```
    foo
    +foo
    ```
2. Search for everything **not** containing the text "foo":

    ```
    -foo
    ```
3. Search for anything which is tagged with the `foo` tag:

    ````
    #foo
    ````

4. Search for anything:

    ```
    *
    ```
5. Search for anything containing text that starts with "uni":

    ```
   uni*
    ```

### Predicate Searches

1. Search for everything containing a `foo` field whose value is greater than `2`:

    ```
    foo:>2
    ```
2. Search for everything containing a `foo` field whose value falls inside the range 0..2:

    ```
    foo:0..2
    ```
3. Search for everything that does **not** contain a `foo` field whose value falls inside the range 0..2:

    ```
    -foo:0..2
    ```
4. Search for everything that contains a `foo` field which contains a `bar` field which contains the text `baz`:

    ```
    foo:bar:baz
    ```
6. Search for everything that contains a `baz` field matching the SQL `LIKE` pattern `_foo%bar`:

    ```
    baz:~"_foo%bar"
    ```
7. Search for everything that contains anything matching the glob pattern `?foo*bar`:

    ```
    ~?foo*bar
    ```
8. Search for everything that contains anything greater than `2`:

    ```
    >2
    ```
9. Search for anything containing a `foo` that starts with the text "uni":

    ```
    foo:uni*
    ```
10. Search for anything containing a metadata attribute called `path` that contains `/foo/bar`:

    ```
    @path:/foo/bar
    ```
