# TinyHaskell

TinyHaskell is a programming language for practice of Haskell.

## Usage

Please build.

```
make
```

Note: require [Stack](https://docs.haskellstack.org/en/stable/README/) for build. And add following alias to your config file.

`alias ghc='stack ghc --'`

### Run!

```
./main ./script/fibo.txt
```

## example

### basic function

```sh
# factorial
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial 10

result:
3628800

# fibonacci
fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n - 1) + fibo (n - 2)

fibo 10

result:
89
```
