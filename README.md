# TinyHaskell

TinyHaskell is a programming language for practice of Haskell.

## Usage

Please build.

```
# in top level
make

# in vm dir
make
```

Note: require [Stack](https://docs.haskellstack.org/en/stable/README/) for build. And add following alias to your config file.

`alias ghc='stack ghc --'`

### Run!

```
echo "y=1;y;" | ./compiler | ./vm/main
```

## Interpreter

```
chmod +x ./interpreter/interpreter.sh
./interpreter/interpreter.sh
```

## example

### basic function

```sh
# no args
y = 1; y;
result:
1

# single args
y a = a; y 1;
result:
1

# multiple call
y a = a; y 1; y 2; y 3;
result:
1
2
3

# factorial
f 0 = 1; f n = n * f (n-1); f 5;
result:
120
```

**Not support multiple args.**
