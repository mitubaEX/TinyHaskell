# tinyHaskell

## Usage

Please build.

```
# in top level
make

# in vm dir
make
```

Run!

```
echo "y=1;y" | ./compiler | ./vm/main
```

## Interpreter

```
chmod +x ./interpreter/interpreter.sh
./interpreter/interpreter.sh
```

## example

### basic function

```
y = 1; y;
>> 1
```
