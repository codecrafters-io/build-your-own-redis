In this stage, you'll add support for inserting multiple locations in one `GEOADD` command.


### Tests
The tester will execute your program like this:
It will then send multiple `GEOADD` commands specifying one or more locations with random coordinates.

```
./your_program.sh
```

It will then send multiple GEOADD commands specifying one or more locations with random coordinates.

```
$ redis-cli
> GEOADD location_key 15.087269 37.502669 "Catania" 12.496365 41.902783 "Rome"
# Expect: 2 (Resp Encoded Integer)

> GEOADD location_key 10.0 20.0 "Foo"
# Expect: 1 (Resp Encoded Integer)

> GEOADD location_key 20.0 10.0 "Bar"
# Expect: 1 (Resp Encoded Integer)
```

