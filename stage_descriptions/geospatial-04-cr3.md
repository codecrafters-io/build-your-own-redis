In this stage, you'll add support for calculating the score of a location.

### Calculating location score

The algorithm for calculating the score from latitude and longitude involves two major steps:

1. Normalizing the latitude and longitude values
2. Interleaving the bits of normalized values of latitude and longitude

#### Normalization

1. The values of latitude and longitude are converted in the range of [0, 1). This is done using the following formula:

```
normalized_latitude = (latitude - LATITUDE_MIN) / (LATITUDE_MAX - LATITUDE_MIN)
normalized_longitude = (longitude - LONGITUDE_MIN) / (LONGITUDE_MAX - LONGITUDE_MIN)
```

The values of the constants are the boundary values for the latitude and longitude. Specifically,

```
LATITUDE_MIN = -85.05112878
LATITUDE_MAX = 85.05112878
LONGITUDE_MIN = -180
LONGITUDE_MAX = 180
```

You can see how the official Redis implementation does this [here](https://github.com/redis/redis/blob/ff2f0b092c24d5cc590ff1eb596fc0865e0fb721/src/geohash.c#L141)

2. The normalized values are then scaled in the range of [0, 2^26). This can be achieved by multiplying the normalized values by 2^26.

```
scaled_latitude = normalized_latitude * (2^26)
scaled_longitude = normalized_longitude * (2^26)
```

You can see how the official Redis implementation does this [here](https://github.com/redis/redis/blob/ff2f0b092c24d5cc590ff1eb596fc0865e0fb721/src/geohash.c#L147)

#### Interleaving bits

1. The scaled values of latitude and longitude are converted to unsigned 32 bit unsigned integers. This is equivalent to dropping off the digits after the decimal point so that these values are now integers instead of floating point numbers.

In the official Redis implementation, the scaled values of latitude and longitude (stored as `double`) are casted implicitly to `uint32_t` while calling `interleave64(uint32_t, uint32_t)`. You can view how this is done [here](https://github.com/redis/redis/blob/ff2f0b092c24d5cc590ff1eb596fc0865e0fb721/src/geohash.c#L149).


2. The bits of latitude and longitude are interleaved to form a 64 bit unsigned integer.

Let's consider the bits of latitude are `X31 X30 X29 ... X2 X1 X0` (32 bit integer), and the bits of longitude are `Y31 Y30 Y29 ... Y2 Y1 Y0` (32 bit integer). The end goal here is to construct a 64 bit integer whose digits will look like this:

```
score = Y31 X31 Y30 X30 Y29 X29 ... Y2 X2 Y1 X1 Y0 X0
```

The pseudocode below shows how this is done.

```
function interleave_bits(lat: uint32, lon: uint32) -> uint64:
    # Cast the values to 64 bits unsigned integers
    lat = uint_64(lat)
    lon = uint_64(lon)

    # Interleave-16
    lat = (lat | (lat << 16)) & 0x0000FFFF0000FFFF
    lon = (lon | (lon << 16)) & 0x0000FFFF0000FFFF

    # Interleave-8
    lat = (lat | (lat << 8))  & 0x00FF00FF00FF00FF
    lon = (lon | (lon << 8))  & 0x00FF00FF00FF00FF

    # Interleave-4
    lat = (lat | (lat << 4))  & 0x0F0F0F0F0F0F0F0F
    lon = (lon | (lon << 4))  & 0x0F0F0F0F0F0F0F0F

    # Interleave-2
    lat = (lat | (lat << 2))  & 0x3333333333333333
    lon = (lon | (lon << 2))  & 0x3333333333333333

    # Interleave-1
    lat = (lat | (lat << 1))  & 0x5555555555555555
    lon = (lon | (lon << 1))  & 0x5555555555555555

    # Interleave latitude and lonngitude: latitude in even bits, longitude in odd bits
    return lat | (lon << 1)
```

Let's focus only on the latitude value for now and call it x. This is a 32-bit integer made up of bits like X31, X30, ..., X0. The end goal here is to spread these bits out so that each one is separated by a zero. In other words, the goal here is to turn x into a 64-bit value: 0 X31 0 X30 0 X29 ... 0 X1 0 X0.

This step prepares the data for interleaving with longitude. Once the same spreading for longitude is done, the longitude bits are shifted to the left once, so its bits move to the odd positions. Then both are combined using bitwise OR. The result is a 64-bit integer where longitude bits are in the odd positions, and latitude bits are in the even positions, achieving bit interleaving. This is done in the last step of the pseudocode above.

##### Interleave-16

`x` is a 64 bit unsigned integer after casting. It is made up of four 16-bit blocks. Since the number was originally a 32 bit integer before being casted to 64 bit integer, the 32 most significant bits are all zeros. The 32 least significant bits are the original bits of the number. Let's call the two 16 bits blocks which make up that number P and Q.

Consider x = 48348958 after normalization. Here's the application of first step on the integer.
```
                                                              P                 Q
x                   = 0000000000000000 0000000000000000 0000001011100001 1011111100011110
                                              P                Q
x << 16             = 0000000000000000 0000001011100001 1011111100011110 0000000000000000

x | (x << 16)       = 0000000000000000 0000001011100001 1011111111111111 1011111100011110
0x0000FFFF0000FFFF  = 0000000000000000 1111111111111111 0000000000000000 1111111111111111
-----------------------------------------------------------------------------------------
                                              P                                 Q
x (after bitwise &) = 0000000000000000 0000001011100001 0000000000000000 1011111100011110
```

This step spreads out the digits of `x` (P and Q) in two different 32 bit groups. The pattern that appears after the first step is `0P0Q`, where each digit in the pattern is a 16-bit block, and P and Q are the original digits of `x`. If they're arranged in the order `PQ`, they'll form `x`. 

Let's re-write for clarity. Consider that P and Q are made up of (W, X), and (Y, Z) respectively.

##### Interleave-8
```                                         W        X                          Y        Z
x                    = 00000000 00000000 00000010 11100001 00000000 00000000 10111111 00011110
                                   W        X                          Y        Z    
x << 8               = 00000000 00000010 11100001 00000000 00000000 10111111 00011110 00000000

x | x << 8           = 00000000 00000010 11100011 11100001 00000000 10111111 10111111 00011110
0x00FF00FF00FF00FF   = 00000000 11111111 00000000 11111111 00000000 11111111 00000000 11111111
----------------------------------------------------------------------------------------------
                                   W                 X                 Y                 Z
x (after bitwise &)  = 00000000 00000010 00000000 11100001 00000000 10111111 00000000 00011110
```

After step 2, the bits of `x` are spread out in 8 8-bit groups. The pattern that appears after this step is `0W0X0Y0Z`, where each digit in the pattern is a 8-bit block. `W`, `X`, `Y`, and `Z` are the original digits that make up `x`. If they're arranged in the order `WXYZ`, they'll form `x`.

Let's re-write for clarity. Consider that W, X, Y and Z are made up of (A,B), (C,D), (E,F) and (G,H) respectively.

##### Interleave-4
```
                                 A    B              C    D              E    F              G    H
x                   = 0000 0000 0000 0010 0000 0000 1110 0001 0000 0000 1011 1111 0000 0000 0001 1110
                            A    B              C    D              E    F              G    H
x << 4              = 0000 0000 0010 0000 0000 1110 0001 0000 0000 1011 1111 0000 0000 0001 1110 0000

x | (x << 4)        = 0000 0000 0010 0010 0000 1110 1111 0001 0000 1011 1111 1111 0000 0001 1111 1110
0x0F0F0F0F0F0F0F0F  = 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111 0000 1111
-----------------------------------------------------------------------------------------------------
                            A         B         C         D         E         F         G         H
x (after bitwise &) = 0000 0000 0000 0010 0000 1110 0000 0001 0000 1011 0000 1111 0000 0001 0000 1110
```

After this step, the digits of the original number have been spread out in 16 4-bit groups. The pattern that appears after this step is `0A0B0C0D0E0F0G0H`, where each digit in the pattern is a 4-bit block. If the blocks are arranged as `ABCDEFGH`, they'll for the orignal number `x`.

##### Interleave-2
A asterisk above a bit means that it is a part of the original bit sequence of `x`.

```
                            ** **       ** **       ** **       ** **       ** **       ** **       ** **       ** **      
x                   = 00 00 00 00 00 00 00 10 00 00 11 10 00 00 00 01 00 00 10 11 00 00 11 11 00 00 00 01 00 00 11 10
                         ** **       ** **       ** **       ** **       ** **       ** **       ** **       ** **
x << 2              = 00 00 00 00 00 00 10 00 00 11 10 00 00 00 01 00 00 10 11 00 00 11 11 00 00 00 01 00 00 11 10 00

x | (x << 2)        = 00 00 00 00 00 00 10 10 00 11 11 10 00 00 01 01 00 10 11 11 00 11 11 11 00 00 01 01 00 11 11 10
0x3333333333333333  = 00 11 00 11 00 11 00 11 00 11 00 11 00 11 00 11 00 11 00 11 00 11 00 11 00 11 00 11 00 11 00 11
---------------------------------------------------------------------------------------------------------------------
                         **    **    **    **    **    **    **    **    **    **    **    **    **    **    **    **
x (after bitwise &) = 00 00 00 00 00 00 00 10 00 00 00 11 00 10 00 01 00 00 00 10 00 11 00 11 00 00 00 00 00 01 00 10
```

After this step, the result is be 32 2-bit blocks, where every second block constitute of the digits of `x`.

After one more step, the block size will be 1, which means, every second bit is the orignal bit in `x`. If the zeros in between are ignored, those bits form the orignal number `x`.


- The left shift (<<) operations are used to create space between the bits of the original number. Each shift duplicates the bits and moves them into higher positions, effectively spreading them apart.

- The bitwise AND (&) with a constant is then used to mask out unwanted bits and retain only the original bits of `x`. These constants (eg. `0x00FF00FF00FF00FF`) are carefully chosen masks that preserve every n-th bit group after the shift, ensuring bits from the original value are positioned correctly for final interleaving.


This step is done for both `latitude` and `longitude` and at last, the longitude is shifted by 1 bit to the right so that its digits take up all the odd places. After bitwise OR operation between the interleaved latitude and interleaved longitude, you get the final result.

You can view the official Redis implementation of this step [here](https://github.com/redis/redis/blob/ff2f0b092c24d5cc590ff1eb596fc0865e0fb721/src/geohash.c#L52).


### Tests

The tester will execute your program like this:

```bash
$ ./your_program.sh
```

It will then send multiple `GEOADD` commands each specifying a key, laitude, longitude, and location name.

For example, the tester might send your program a command like this.

```bash
$ redis-cli GEOADD places 15.09 37.50 Catania
```

The tester will then send multiple `ZSCORE` commands to your program specifying the key and locations used in `GEOADD` command. For example, the tester might send your program a command like this.

```bash
$ redis-cli ZSCORE places Catania
# Expecting bulk string: "3479447193062238" 
```