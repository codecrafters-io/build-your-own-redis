In this stage, you'll add support for different units of measure in the `GEODIST` command.

### Tests
The tester will execute your program like this:
```
./your_program.sh
```

It will then add multiple locations using the `GEOADD` command.

```
GEOADD places 15.087269 37.502669 "Catania" 12.496365 41.902783 "Rome"
```

The tester will then send multiple `GEODIST` commands specifying two locations and a unit of measure. For example, in this case,

```
GEODIST places Palermo Catania km
```

It will expect the response to be "537.2151", which is RESP bulk string encoded as

```
$8\r\n
537.2151\r\n
```

### Notes
- For greatest accuracy, it is recommended to calculate the distance in feet, since it is the unit with smallest precision, and convert that into other units.