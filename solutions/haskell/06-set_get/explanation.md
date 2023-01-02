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
Since we are interested in storing a `Key`-`Value` pair in this database, it is a good idea to use a mapping function, such as the `Map` type from the `containers` library.
This has the benefit that we can use abstract functions to insert and find elements without having to write our own implementations for it (but feel free to do so).

We first define a `type` alias for the database to easily refer to it throughout the program.
Additionally, we define two new type synonyms, `Key` and `Value` which are both a `ByteString`.

Then, using the `newTVarIO` function from the `STM` package we create a new, empty instance of the database.
From the `Data.Map` package of the `containers` library we use the handy `empty` function for this.

Finally, we can initialize the database in our main routine. 

```haskell
type Key = ByteString
type Value = ByteString
type DB = Map Key Value

setupDB :: IO (TVar DB)
setupDB = newTVarIO empty

main :: IO ()
main = do
    ...
    redisDB <- setupDB
    serve ...
```

Within the main program we can now reference the database by `redisDB`.
We also added a debugging aid which prints the content of the whole database to the server command line so that you can verify what is happening.

# 2. Implementing set and get
Now that we have the database in place, we can focus on implementing `set` and `get`.

As a preparation, we can extend the `Command` type with two new constructors, `Set` and `Get`.
They take as values a `Key` and `Set` an additional `Value`.
```haskell
data Command = Ping
             | Echo Message
             | Set Key Value
             | Get Key
```

Let us start with `set` to get a feeling for how to add an element to the database.

## 2.1. set
Similar to the echo and ping functions, we start with the parsing to extract all the relevant inputs for `set` out of a `Request`.

We check also that the request contains `set` in its first element.
Since set requires two additional inputs, a `key` and a `value`, we expect the number of elements of that array to be three.

When both of these checks were successful, we extract a key and a value and return the type `Command` with its corresponding `Set key value`.
This is similar to echo, just with an additional element.

```haskell
parseSet :: Parser Command
parseSet = do
    (n, _) <- commandCheck "set"
    guard $ n == 3
    key <- crlfAlt *> redisBulkString
    value <- crlfAlt *> redisBulkString
    return $ Set key value
```

The `set` command is a bit more complicated than the simple ping and echo commands, hence we decide carve the functionality out in a separate function called `set`.
Since we want to add a key-value pair to the database, we have to also take the database as an input parameter to `set`.
Therefore, we have to add the input of type `TVar DB` to the function's signature.

The implementation of the `set` functionality looks slim, however the writing to the database is somewhat involved.
To modify an `STM` in general, the library provides the `atomically` function, which ensures either writing everything or nothing to prevent only partial updates.

We then use `modifyTVar` to change the database by `insert`ing a new or existing `key` and add or replace its `value`, respectively.
The `insert` function is from the `Data.Map` package because we actually modify the map.
As per the Redis definition, a successful set command simply returns `"OK"` to the client.

```haskell
type Command = TVar DB -> IO Response

set :: Key -> Value -> TVar DB -> IO Response
set key val db = do
    _ <- atomically $ modifyTVar db $ insert key val
    return "OK"
```

## 2.2. get
The parsing of `get` happens in the same way as all other functions.
You can look at the solution code to see our implementation of `parseGet`.

Similar to set, we have to add the `TVar DB` type to get's signature so that we can read from the database.
Reading from the database is now such an action as described earlier, where we access the database's current value through the impure `IO` "interface".
The `readTVarIO` always returns the latest value of our `TVar` database, i.e. the keys and values stored in our map.

Since we are reading from our map, we can use another map function, `findWithDefault`, also from the `Data.Map` package.
The Redis standard defines that if no key is found, get should return `(nil)`.
`findWithDefault` accepts a default value in such a case, which we happily use.
The other two parameters are the `key` and the map, i.e. `out`.

To end up with a concise implementation we can leverage an applicative functor, here.
Basically, we can apply the `findWithDefault` function to the function that reads the database, `readTVarIO`.
We could do this by either using `pure (findWithDefault "(nil)" key) <*> readTVarIO db`, or as an alternative, use `findWithDefault "(nil)" key <$> readTVarIO db`, the latter being the infix version of the former.

```haskell
get :: Key -> TVar DB -> IO Response
get key db = findWithDefault "(nil)" key <$> readTVarIO db
```

Such an operation works because `findWithDefault` is an instance of `Functor` which provides the function `fmap` that lets you apply a function to another function.
You can read more about [the Functor class](https://en.wikibooks.org/wiki/Haskell/The_Functor_class) and [the Applicative class (for applicative functors)](https://en.wikibooks.org/wiki/Haskell/Applicative_functors) following these two links.

With the `exec` function expecting now a database to be consumed for `set` and `get`, each of the other pattern matches does so, too.
Therefore, we need to add the `TVar DB` type to `Echo` and `Ping`, even if we do nothing with it.
Otherwise, we would get an error for having different number of arguments.

You should now have a working database and be able to write to and read from it by using the `set` and `get` commands.
