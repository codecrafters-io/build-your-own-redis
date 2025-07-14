In this stage, you'll add support for retrieving multiple locations using a single `GEOPOS` command.

### Tests
The tester will execute your program like this:

```
./your_program.sh
```

It will add multiple locations using the `GEOADD` command.

```
$ redis-cli
> GEOADD location_key 19.0872 33.5026 "Foo" 49.125 72.991 "Bar"
> GEOADD location_key 10.0872 34.5026 "Baz" 41.125 73.991 "Caz"
```

The tester will then send multiple `GEOPOS` commands, each specifying a multiple locations that may or may not have been added. For example, for the following command

```
> GEOPOS location_key Foo Caz non_existent
```

```
1) 1) "19.0872"
   2) "33.5026"
2) 1) "41.125"
   2) "73.991"
3) (nil)
```

which is RESP-encoded as 

```
*2\r\n
*2\r\n
$18\r\n
19.087197482585907\r\n
$17\r\n
33.50259961456723\r\n
*2\r\n
$18\r\n
41.12499922513962\r\n
$17\r\n
73.99100100464304\r\n
```