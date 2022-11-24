In stage 6 you add the `set` and `get` commands. This implies that a database is required to store `key`s and `value`s.

With the parser having been built in the previous stage, adding support for `set` and `get` is not that much of a hassle anymore. Building the database is somewhat trickier, since Haskell has no mutable states and we also need concurrent access by multiple clients to the database. In this solution, we explain and show how to implement the necessary parts to pass this stage.

# 1. Setting up the database
There are two requirements for the database:
1. Mutability - changing the state of an entity
2. Concurrency - this implies atomicity, consistency and isolation

## 1.1. Mutability
You may think of using a list that contains a tuple of keys and values for a simple database.
You could actually define such a database and add values to it throughout your program.
But in fact this just creates a different database every time you change it (even if you use the same name) where the previous versions are still around somewhere.
Since your program just uses the latest one around there should not be a problem, right?

The problem with this approach is, that Haskell does not allow mutable states in a pure form.
When you define a function that uses an element from your database, it gets bound to the version of the database at that specific moment.
If you call that function, it will always refer to this moment in time, and every subsequent update to the database is not known to that function.
It also prevents interactive interactions with such a database, which is not helpful in our case.

Haskell does have a solution for it, and it involves the impure `IO monad`.
We will see in the next section how this implementation looks like.
Basically, the program you write that contains an impure database "connects" to it through the IO monad, where the database processes all inputs and only returns the latest state to the program whenever it accesses it.
Although this is a rather advanced topic, you may find some useful [information about mutable objects in this source](https://en.wikibooks.org/wiki/Haskell/Mutable_objects).

## 1.2. Concurrency
When you throw concurrent access into the mix of mutability in Haskell, things get a bit more complicated.
Again, the `IO monad` is to the rescue, but it is joined by a concept called `STM` which is short for `Software Transactional Memory`.

If you are familiar with database concepts in general, you may have heard about transactions that atomic, consistent, isolated and durable ([ACID](https://en.wikipedia.org/wiki/ACID)).
A similar concepts exists in `STM` with transactions when concurrent interactions with a database are required.
The Haskell library [stm](https://hackage.haskell.org/package/stm) exposes functions which handle concurrency in a way that no conflicts arise.
You can also read more about [concurrency in Haskell](https://en.wikibooks.org/wiki/Haskell/Concurrency).

We are specifically interested in a simple concept of transactional variables, or `TVar`s, which are implemented in the package [STM.TVar](https://hackage.haskell.org/package/stm/docs/Control-Concurrent-STM-TVar.html).
Such a `TVar` can hold a single piece of data.
When using a single list of tuples (remember, like the initial idea) we can construct that single piece of data with it, but we get the benefit of mutability and concurrency.

## 1.3. Database implementation
Since we are interested in storing a `Key`-`Value` pair in this database, it is a good idea to use a mapping function, such as `Map` from the `containers` library.
This has the benefit that we can use abstract functions to insert and find elements without having to write our own implementations for it (but feel free to do so).

We first define a `type` alias for the database to easily refer to it throughout the program.
Additionally, we define two new type synonyms, `Key` and `Value` which are both a `ByteString`.

Then, using the `newTVarIO` function from the `STM` package we create a new instance of the database.
We add to it some initial values in a list of tuples of type `(Key, Value)`, which the `fromList` function transforms into the desired `Map Key Value` format.

Finally, we can initialize the database in our main routine. 

```haskell
type Key = ByteString
type Value = ByteString
type DB = Map Key Value

setupDB :: IO (TVar DB)
setupDB = newTVarIO $ fromList [("__version__", "1.0.0")]

main :: IO ()
main = do
    ...
    redisDB <- setupDB
    serve ...
```

Within the main program we can now reference the database by `redisDB`.

# 2. Implementing set and get
Now that we have the database in place, we can focus on implementing `set` and `get`.
Let us start with `set` to get a feeling for how to add an element to the database.

## 2.1. set
Similar to the echo and ping functions, we start with the parsing to extract all the relevant inputs for `set` out of a `Request`.

We check also that the request contains `set` in its first element.
Since set requires two additional inputs, a `key` and a `value`, we expect the number of elements of that array to be three.

When both of these checks were successful, we extract a key and a value and return them as part of the `set` function.
This is similar to echo, just with an additional element.

```haskell
parseSet :: Parser Command
parseSet = do
    (n, _) <- commandCheck "set"
    guard $ n == 3
    key <- crlfAlt *> redisBulkString
    value <- crlfAlt *> redisBulkString
    return $ set key value
```

Since we want to add a key-value pair to the database, we have to also take the database as an input parameter.
Therefore, we have to add the input of type `TVar DB` to the function's signature.
As a consequence, we have to extend the `Command` type by the TVar as well, since each `Parser` returns a function that also takes a database (this also applies to echo and ping, see section 3).

The implementation of the `set` functionality looks slim, however the writing to the database is somewhat involved.
To modify an `STM` in general, the library provides the `atomically` function, which ensures either writing everything or nothing to prevent only partial updates.

We then use `modifyTVar` to change the database by `insert`ing a new (or even existing) `key` and its `value`.
The `insert` function is from the `Data.Map` library since we actually modify the map.
As per the Redis definition, a successful set command simply returns "OK" to the client.

```haskell
type Command = TVar DB -> IO Response

set :: Key -> Value -> TVar DB -> IO Response
set key val db = do
    _ <- atomically $ modifyTVar db $ insert key val
    return "OK"  -- see the section 3 for an improvement
```

In section 3 we discuss a small improvement to avoid using hard-coded constants within functions.

## 2.2. get
The parsing of `get` happens in the same way as all other functions.
You can look at the solution code to see our implementation of `parseGet`.

Similar to set, we have to add the `TVar DB` type to get's signature so that we can read from the database.
Reading from the database is now such an action as described earlier, where we access the database's current value through the impure `IO` "interface".
The `readTVarIO` always returns the latest value of our `TVar` database, i.e. the keys and values stored in our map.
By storing the current readout of the database in the `out` variable, we get the most recent state from which we can read the desired key and its corresponding value.

Since we are reading from our map, we can use another map function, `findWithDefault`, also from the `Data.Map` package.
The Redis standard defines that if no key is found, get should return `(nil)`.
`findWithDefault` accepts a default value in such a case, which we happily use.
The other two parameters are the `key` and the map, i.e. `out`.

```haskell
get :: Key -> TVar DB -> IO Response
get key db = do
    out <- readTVarIO db
    return $ findWithDefault "(nil)" key out
    -- see the section 3 for an improvement
```

# 3. Other improvements

The parser expects now a database to be present, so does each function it returns.
Therefore, we need to add the `TVar DB` type to `echo` and `ping`, too, even if we do nothing with it.
Otherwise, we would get a type mismatch error.

Also, to be consistent with set and get, we improve the readability of `echo` and `ping` by defining yet another type synonym, `Message`.

```haskell
type Message = ByteString

echo :: Message -> TVar DB -> IO Response
echo x _ = return x
```

By now we are using many constants in the program
This could get confused, especially if they are used in multiple different functions.
We can improve this situation by adding an abstract data structure (`ADT`).
The naming of such an `ADT` is up to our liking, so we simply call it `Configuration`.

In there, we add the constants that we want to have in a single, central place.
You may add more constants to it, but for now we chose the following ones.

```haskell
data Configuration = Configuration {
    port :: String,
    recvBytes :: Int,
    pingDefault :: ByteString,
    setSuccess :: ByteString,
    nilString :: ByteString
}
```

Once you have a structure defined, you can create a function with type `Configuration` and add the values one by one.

```haskell
redisConfig :: Configuration
redisConfig = Configuration "6379" 2048 "PONG" "OK" "(nil)"
```

Within the program, you can refer to a single constant by its name.
You will notice that it is of type `Configuration -> 'constant type'`, for example: `Configuration -> String` in the case of `port`.

As mentioned previously, we can improve, amongst others, the `set` and `get` functions by calling the constants.

```haskell
set :: Key -> Value -> TVar DB -> IO Response
set key val db = do
    _ <- atomically $ modifyTVar db $ insert key val
    return $ setSuccess redisConfig

get :: Key -> TVar DB -> IO Response
get x db = do
    let err = nilString redisConfig
    out <- readTVarIO db
    return $ findWithDefault err x out
```

This should be done for all other constants as well.
The solution code contains the other replacements.

With all this in place, you should now have a working database and be able to write and read to it by using the `set` and `get` functions.
