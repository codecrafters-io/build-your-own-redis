In this stage, you'll add support for different units of measure in the `GEODIST` command.

### Unit Conversion
The `GEODIST` command supports an optional fourth argument, which is the unit in which the distance should be returned. The valid units are meters(`m`), kilometers(`km`), miles(`mi`) and feet(`ft`). If the unit is specified, the syntax for `GEODIST` is:
```
GEODIST key location1 location2 <unit>
```

Example Usage:
```
> GEODIST places Catania Rome mi
"333.8108"
```

### Tests
The tester will execute your program like this:
```
./your_program.sh
```

It will then add multiple locations using the `GEOADD` command.

```
> GEOADD places 15.087269 37.502669 "Catania"
> GEOADD places 12.496365 41.902783 "Rome"
```

The tester will then send multiple `GEODIST` commands specifying two locations and a unit of measure. For example, in this case,

```
> GEODIST places Catania Rome km
```

It will expect the response to be "537.2151", which is RESP bulk string encoded as

```
$8\r\n
537.2151\r\n
```