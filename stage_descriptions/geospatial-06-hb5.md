In this stage, you'll add support for decoding the coordinates of a location.

### Calculating latitude and longitude from score

The algorithm to get back the latitude and longitude is essentially the reverse of the one used to compute the score from them. In other words, we undo the bit interleaving process to extract the original latitude and longitude values. This involves two major steps:

1. De-interleaving the bits of score
2. De-normalizing the latitude and longitude


#### De-interleaving bits

The `score` is a 64-bit unsigned integer, whose bits are the interleaved bits of two 32-bit unsigned integers, which represent latitude and longitude. The digits at the odd place are the digits of longitude and those at the even place are the digits of latitude.

For example, if `x` is the score, it is made up of

```
x = B63 B62 B61 .... B3 B2 B1 B0
```

The bits `B63 B61 B59 ... B5 B3 B1` make up the longitude and the bits `B62 B60 B58 ... B4 B2 B0` make up the latitude. The end goal here is to de-interleave the bits so that the final result looks like this:

```
longitude = B63 B61 B59 ... B5 B3 B1 
latitude = B62 B60 B58 ... B4 B2 B0
```

The pseudocode below shows how this is done.

```python
def deinterleave_bits(score: uint64) -> (uint64, uint64):

# Right shift
    uint64_t lat = score
    uint64_t lon = score >> 1

# Let's see how the bits of latitude are extracted. Suppose the score is 3781709020344510. Let us refer the latitude as x for simplicity.


    lat = (lat | (lat >> 0)) & 0x5555555555555555
    lon = (lon | (lon >> 0)) & 0x5555555555555555
# Step-1
# Let us see the application of the first step

# x                     = 00 00 00 00 00 00 11 01 01 10 11 11 01 11 00 01 10 11 11 10 11 00 11 01 00 11 00 00 10 11 11 10
# x >> 0                = 00 00 00 00 00 00 11 01 01 10 11 11 01 11 00 01 10 11 11 10 11 00 11 01 00 11 00 00 10 11 11 10

# x | (x >> 0)          = 00 00 00 00 00 00 11 01 01 10 11 11 01 11 00 01 10 11 11 10 11 00 11 01 00 11 00 00 10 11 11 10
# 0x5555555555555555    = 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01 01
# -----------------------------------------------------------------------------------------------------------------------
# BITWISE AND           = 00 00 00 00 00 00 01 01 01 00 01 01 01 01 00 01 00 01 01 00 01 00 01 01 00 01 00 00 00 01 01 00

# In the first step, x is right shifted by 0, which means up until the stage x | (x >> 0), the original number stays the same. Now, the constant 0x5555555555555555 is carefully picked so that all of its bits at the even position are 1 and the odd position are 0. So, when x is operated with this constant using bitwise AND, only the bits of even position(bits of latitude) are retained. The bits of latitude haven't changed their position, but the bits of longitude has been set to 0 after this step.


    lat = (lat | (lat >> 1)) & 0x3333333333333333
    lon = (lon | (lon >> 1)) & 0x3333333333333333
# Step-2
# Let's re-write this in groups of 4 to make it easier, and see the application of the second step. A asterisk above a bit indicates that it is the digit of latitude, and a empty space means the bit below it is zero.

#                          * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *
# x                     = 0000 0000 0000 0101 0100 0101 0101 0001 0001 0100 0100 0101 0001 0000 0001 0100
#                           *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *  * *
# x >> 1                = 0000 0000 0000 0010 1010 0010 1010 1000 1000 1010 0010 0010 1000 1000 0000 1010

# x | (x >> 1)          = 0000 0000 0000 0111 1110 0111 1111 1001 1001 1110 0110 0111 1001 1000 0001 1110
# 0x3333333333333333    = 0011 0011 0011 0011 0011 0011 0011 0011 0011 0011 0011 0011 0011 0011 0011 0011
# -------------------------------------------------------------------------------------------------------
#                           **   **   **   **   **   **   **   **   **   **   **   **   **   **   **   **
# BITWISE AND           = 0000 0000 0000 0011 0010 0011 0011 0001 0001 0010 0010 0011 0001 0000 0001 0010

# In this step, x is right-shifted by 1 and operated with bitwisse OR with the original value of x. This ensures that bits which were originally one positions apart (i.e., bit 0 and bit 2, bit 1 and bit 3, etc.) are grouped together. This is because (x | 0) = x. This applies to every odd positioned bit. It ensure that the signifcant bit after that is moved to its position because of the right shift operator. The constant has been carefully chosen to convinently ignore values which are not the original bits of x.

# Then, bitwise AND is applied with 0x3333333333333333. This constant has the pattern 0011 0011... 0011, which isolates pairs of bits at every 4-bit block by setting other bits to 0.

# The result of this steps consists of 4 16-bit blocks. The 2 least significant bits of each 4-bit block are the digits of latitude. The result is one step closer to the full re-construction. 


    lat = (lat | (lat >> 2)) & 0x0F0F0F0F0F0F0F0F
    lon = (lon | (lon >> 2)) & 0x0F0F0F0F0F0F0F0F
##### Step-3
# Let's re-write the result into 8 8-bit groups for clarity and see the application of the third step. A asterisk above a bit indicates that it is the digit of latitude, and a empty space means the bit below it is zero.

#                            **  **   **  **   **  **   **  **   **  **   **  **   **  **   **  **
# x                      = 00000000 00000011 00100011 00110001 00010010 00100011 00010000 00010010
#                              **   **  **   **  **   **  **   **  **   **  **   **  **   **  **
# x >> 2                 = 00000000 00000000 11001000 11001100 01000100 10001000 11000100 00000100

# (x | (x >> 2))         = 00000000 00000011 11101011 11111101 01010110 10101011 11010100 00010110
# 0x0F0F0F0F0F0F0F0F     = 00001111 00001111 00001111 00001111 00001111 00001111 00001111 00001111
# ------------------------------------------------------------------------------------------------
#                              ****     ****     ****     ****     ****     ****     ****     ****
# BITWISE AND            = 00000000 00000000 00001000 00001100 00000100 00001000 00000100 00000100

# In this step, the process of collapsing the scattered bits of latitude closer to their original positions continues. The bits that were originally two positions apart are re-grouped. To do this, x is right-shift by 2 and operated using bitwise OR with the original value of x. Here, the constant 0x0F0F0F0F0F0F0F0F has the pattern 00001111 00001111 .... 00001111 repeated across all bytes. This selects only the lowest 4 bits of each 8-bit block and masks out the rest. Since two bits at a time were grouped previously, this step now gathers 4 bits that originally belonged together into a single nibble (4-bit group).

# As a result, the bits of latitude are now packed one nibble per byte, with the lowest significant nibble of each 8 bit-block representing the digits of latitude.


    lat = (lat | (lat >> 4)) & 0x00FF00FF00FF00FF
    lon = (lon | (lon >> 4)) & 0x00FF00FF00FF00FF
# Step-4
# Let's re-write the result into 4 16-bit groups for clarity and see the application of the third step. A asterisk above a bit indicates that it is the digit of latitude, and a empty space means the bit below it is zero.

#                              ****    ****     ****    ****     ****    ****     ****    ****
# x                      = 0000000000000011 0010001100110001 0001001000100011 0001000000010010
#                                  ****     ****    ****     ****    ****     ****    ****
# x >> 4                 = 0000000000000000 0011001000110011 0001000100100010 0011000100000001

# (x | (x >> 4))         = 0000000000000011 0011001100110011 0001001100100011 0011000100010011
# 0x0F0F0F0F0F0F0F0F     = 0000000011111111 0000000011111111 0000000011111111 0000000011111111
# --------------------------------------------------------------------------------------------
#                                  ********         ********         ********         ********
# BITWISE AND            = 0000000000000011 0000000000110011 0000000000100011 0000000000010011

# In this step the bits that were originally four positions apart are grouped into a single 8-bit block. To do this, x is right right-shift by 4 and operated using bitwise OR with the original value of x. Here, the constant 0x0F0F0F0F0F0F0F0F has the pattern 0000000011111111 ... 0000000011111111 repeated across all bytes. This selects only the lowest 8 bits of each 16-bit block and masks out the rest. Since four bits were grouped at a time previously, this step now gathers 8 bits that originally belonged together into a 16 bit block. As a result, the bits of latitude are now packed in 8-bit block per 16-bit block, with the lowest significant 8 bits of each 16 bit-block representing the digits of latitude, and the upper 8 bits all being 0.


# Each step shifts and combines progressively wider chunks, essentially "unzipping" the interleaved bits by peeling off odd bits and regrouping even ones back into their original layout.

# After two more steps, the result will essentially be a 64 bit integer, whose 32 most significant bits are all zeros and 32 least significant digits are the digits of the latitude.

# The same steps are repeated for longitude, except that at the very first step, the digits of longitude are shifted by 1 because the digits of longitude are stored in odd positions. Shifting it to the right by 1 brings all the digits of longitude in the even position. This implies that the same steps can be applied to longitude as that are applied for the latitude.
    lat = (lat | (lat >> 8)) & 0x0000FFFF0000FFFF
    lon = (lon | (lon >> 8)) & 0x0000FFFF0000FFFF

    lat = (lat | (lat >> 16)) & 0x00000000FFFFFFFF
    lon = (lon | (lon >> 16)) & 0x00000000FFFFFFFF

    return lat, lon
```

You can see how this step is implemented in the official Redis source [here](https://github.com/redis/redis/blob/ff2f0b092c24d5cc590ff1eb596fc0865e0fb721/src/geohash.c#L82).


#### De-normalizing the latitude and longitude

After the computation of the previous step, both latitude and longitude will have been obtained. But, they are in the range of \[0, 2^26\). To convert them to valid values, they should be normalized to the range of \[0, 1\) and multiplied by their respective ranges, and added with respective offsets to obtain the actual values of latitude and longitude.

The pseudocode below shows how this is done:

```python
def convert_to_lat_lon(latitude: uint64, longitude: uint64)
    double lat_scale = LATITUDE_MAX - LATITUDE_MIN
    double long_scale = LONGITUDE_MAX - LONGITUDE_MIN

    # Calculate lower boundary of grid
    latitude_min = LATITUDE_MIN + (latitude * 1.0 / (1 << 26)) * lat_scale
    latitude_max = LATITUDE_MAX + ((latitude + 1) * 1.0 / (1 << 26)) * lat_scale

    # Calculate upper boundary of grid
    longitude_min = LONGITUDE_MIN + (longitude * 1.0 / (1 << 26)) * long_scale
    longitude_max = LONGITUDE_MAX + ((longitude + 1) * 1.0 / (1 << 26)) * long_scale

    # Calculate average for best approximation
    lat = (latitude_min + latitude_max) / 2
    lon = (longitude_min + longitude_max) / 2

    return lat, lon
```

The values of the constants are given below:

```
LATITUDE_MIN = -85.05112878
LATITUDE_MAX = 85.05112878
LONGITUDE_MIN = -180
LONGITUDE_MAX = 180
```

The minimum and maximum values of latitude and longitude are averaged because the encoded score represents a rectangular region on the Earth's surface, not a single point. This region is a cell in a grid formed by subdividing the full geographic coordinate space. Averaging the boundaries gives the center of that cell — the best approximation of the original coordinate that was encoded. You can learn more about this visually by using the [GeoHash Explorer](https://geohash.softeng.co/).


You an see how this is implemented in the official Redis source [here](https://github.com/redis/redis/blob/ff2f0b092c24d5cc590ff1eb596fc0865e0fb721/src/geohash.c#L181).


### Tests
The tester will execute your program like this:
```bash
$ ./your_program.sh
```

It will add multiple locations using the `ZADD` command. It will use a score which will be equivalent to a latitude and longitude.

```bash
$ redis-cli
> ZADD location_key 3663832614298053 "Foo"
> ZADD location_key 3876464048901851 "Bar"
> ZADD location_key 3468915414364476 "Baz"
> ZADD location_key 3781709020344510 "Caz"
```

The tester will then send multiple `GEOPOS` commands, each specifying a single location that may or may not have been added. For example, the tester might send your program a command like this:

```bash
> GEOPOS location_key Foo
# Expecting [["2.294471561908722", "48.85846255040141"]]
```

The value is a RESP array, which is encoded as
```
*1\r\n
*2\r\n
$17\r\n
2.294471561908722\r\n
$17\r\n
48.85846255040141\r\n
```

### Notes

- The tester will be lenient in checking the coordinates provided. The latitude and longitude returned by the server should match the values provided in the `GEOADD` command with a precision of up to 4 decimal places when rounded off.

   - For example, for the response of the example shown above, any of the following will be accepted:

      - `2.2945 48.8585`
      - `2.2945001 48.8585002`
      - `2.29447156190 48.858462550`
      - `2.29449 48.858453`