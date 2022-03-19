# Niffin

A tiny statically typed scripting language.

## Why the name?

Because I'm a fan of [The Magicians](https://www.imdb.com/title/tt4254242/) and `niffin` is a cool name for a language.

## Based on ...

The language syntax is based on:

- Nim
- C
- Rust
- [Lox](https://craftinginterpreters.com/)

## Samples

Hello world:

```niffin
# this is a comment
print("Hello world!");
```
Constants:

```niffin
discard 0b110;
discard 0o777;
discard 0x1f;
discard 23;
discard 23.32;
discard 6.02e23;
discard "Hello";
```
Declarations:

```niffin
var x = 0;
# vars can be reassigned
x = 1;

let y = 2;
# let bindings cannot be reassigned
# y = 3 # <- error

fun add(a: int, b: int): int {
    return a + b;
}

```

UFCS:

```niffin

fun add(a: int, b: int): int {
    return a + b;
}

# The following are all equivalent
discard 1.add(2);
discar add(1, 2);
```
# To do

- [X] Lexer
- [X] Parser (in progress)
- [ ] Resolver
- [ ] Type checker
- [ ] Virtual machine