In stage 5 you implement a second command, `echo`.

This stage requires the most work of all since you have to build the foundations for handling different commands.

It is composed of three parts:
1. **RESP and Concepts** - some key concepts are introduced
2. **Parser** - the implementation of the parser function is described
3. **Process Information** Information - execute a command and return a response to the client

This solution should help you understand why each implementation is required.

# 1. RESP and Concepts
Let us start with a high-level view of what should happen with our Redis implementation.
The course of events, after a client is connected to the server, is as following:
1. The client sends a request
2. The server receives the request, which is encoded in the RESP format
3. The request is decoded
4. The decoded request is parsed, so that the appropriate action can be taken
5. An action, likely a Redis function that is implemented, is carried out
6. This action produces a result, i.e. a response
7. The response from that action is encoded into the RESP format
8. The encoded result is sent back to the client

The next sections highlight some important concepts so that you can comprehend and re-implement these steps yourself.

## 1.1. RESP Arrays
You learned in previous stages that Redis uses a custom protocol, RESP, mainly to encode and decode messages.
The Simple String format can only handle one element at a time, for example `ping` is encoded as `+ping\r\n`.
When we want to pack more elements into a single message, we need a more flexible format.
This is where the [RESP Arrays](https://redis.io/docs/reference/protocol-spec/#resp-arrays) format comes into play.
It would be good to familiarize yourself with how such an array is composed, since we have to decode it quite early in the process.

## 1.2. RESP Bulk Strings
Another RESP format that is common in Redis are the [Bulk Strings](https://redis.io/docs/reference/protocol-spec/#resp-bulk-strings).
They are mainly used within RESP Arrays and contain, as the name implies, strings.
The key difference to Simple Strings is that Bulk Strings also indicate the number of bytes of the string they contain which makes them binary-safe.
We assume that this format is used to receive requests, while Simple Strings are used for sending responses (with one exception in stage 7).

An example of a RESP Bulk Strings is:
```
$5\r\nhello\r\n
```
Every section is terminated by a carriage return `\r` and a newline `\n`.

## 1.3. Types
You may have come across types in Haskell before, since it is a static and strongly-typed language which does not compile unless all type dependencies are satisfied.
If you are not familiar with the Haskell Type System you may find [this introduction](https://en.wikibooks.org/wiki/Haskell/Type_basics) helpful. 

In this solution we call the input from a Redis user a `Request` and the answer that the server sends a `Response`.
This will help us to think in familiar terms rather than with abstract concepts like bytes and strings.
In Haskell, you can define your own types using `type`.
Here, we use it mainly to improve the readability of our program.
For example, we define `Request` and `Response` of type `ByteString` (we learn about ByteStrings in the next section).

```haskell
type Request = ByteString
type Response = ByteString
```

This notation and usage is quite arbitrary and we could also use `ByteString` throughout the code, or use Request and Response interchangeably without it having an effect on the outcome.
Feel free to use your own naming.

## 1.4. ByteString
The `ByteString` type is primarily used when sending data over a network.
A single `ByteString` is 1 byte in size and represents a single character as a vector, which is not easily human-readable.
However, there exists a Haskell library that does the transformation for you and it is recommended to read about the exposed functions in the [Data.ByteString](https://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html) description.

Since some functions may overlap with functions from other libraries, it is a common practice to name the import of such a package.
A specific example we encounter in this solution is the `concat` function, which conflicts with the one from `Prelude`.
To solve this conflict we can also hide the one we do not want.

```haskell
import qualified Data.ByteString.Char8 as B
import Prelude hiding (concat)
```

This allows us to refer to ByteString's concat function as `B.concat`.

## 1.5. Algebra-Driven Design
Until now, we did not have to write large and involved pieces of code.
As with every programming language, things can quickly become confusing if one just starts developing without having an idea how to structure the code.

When we start building the functionality of this Redis implementation, wouldn't it be good if we can start with small, elementary pieces and use them to build more and more abstract levels?

There exists a general concept of this approach, which is thinking in algebraic terms, aptly named `Algebra-Driven Design`.
No, we do not need any math for this, but the concept is akin to using simple functions and build more complex abstractions with it.
This concepts exists also in a similar form under the name of `Denotational Design`.

An example would be to use the `plus` function to construct `multiplication` with it.
And then use `multiplication` to build the `power` function, and so on.
Haskell is actually well suited to think in algebraic terms due to its lack of mutable variables and its static type system, but this goes beyond what we can cover here.
If you are interested in learning more about Algebra-Driven Design, you can search for it online as there are some good books and blogs on this topic.

The following section on parsing is using this concept to build the parsing functionality for Redis.

# 2. Parser
Parsing an input, or `Request` in our case, is the key element for the server to understand what it should do.

Something that every `Request` has in common, is that the input arrives in the RESP format.
We need a way to decode this format to extract the information from it.
Hence, we can build the basic blocks to do this in a generic way, and then use these for every function that we implement on top of them.

In this context the requests are encoded in a `RESP Array` that contains `Bulk Strings`.
Each Array starts with a `*` followed by a number indicating the number of *elements* this array contains.
All Bulk Strings begin with a `$` followed by a number indicating the number of *bytes* each string has.

Hence, it makes sense to distinguish these two cases and to write a function for each.
But first, a short section on parser libraries in Haskell.

## 2.1. Parser Libraries
To parse the input we use a library.
We decided to use [Megaparsec](https://hackage.haskell.org/package/megaparsec) since it can parse [ByteStrings](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte.html) and it has some high-level implementations that make our lives a bit easier.

Another popular parsing library is `Attoparsec` which you are free to use as well.
Their general functionality does not differ much, only the latter requires some more specifications and it is optimized for speed, which is not relevant in our case.

### 2.1.1. `Parser` Type
To define our parser we have to specify its type.
`Parsec` is the type of the Megaparsec library which takes three more arguments.
The first one is to handle errors, where `Void` is commonly used as an empty return type.
The second one is the type we want the parser to process, in our case we want it to process the type `Request`, i.e. a `ByteString`.
The third one is what we actually want `Parsec` to return, which are mainly of type `Response`, but are later defined for each parsing function.

```haskell
type Parser = Parsec Void Request
```

Another benefit of Megaparsec is that it returns something of type `Either`.
By convention, `Left` represents the error while `Right` indicates success (see the [Data.Either package description](https://hackage.haskell.org/package/base/docs/Data-Either.html)).
This allows us to easily account for these two cases and implement appropriate actions.

We now tackle the implementation using our new parser type.

## 2.2. Parse Bulk Strings
Let us start with the `Bulk String` first.
Since an Array can contain more than one Bulk String we have some sort of dependency between these elements.
Also, a Bulk String indicates the number of bytes the string has, which is another dependency.
While we ignore the Array dependency for simplicity, we can ensure that the Bulk String contains exactly the number of bytes it indicates.

To do this, we need `monadic parsing` where we can build different sections of input being parsed.
As another benefit, we can use the `do` notation.

We call the function `redisBulkString` and have it return the type `Parser ByteString`, where `ByteString` is the third type that `Parsec` expects.

```haskell
redisBulkString :: Parser ByteString
redisBulkString = do
    _ <- "$"
    n <- decimal
    guard $ n >= 0
    _ <- crlfAlt
    s <- count n printChar
    return $ pack s
```

If you look at the `redisBulkString` code snippet you observe that we first process the `$` token which indicates the Bulk String.
After that we use a helper function from the Megaparsec library to identify and process `decimal`.
This number indicates the number of bytes that the following string has.
We keep this number and check if it is actually non-negative using the `guard` function to throw an error otherwise.

If this check was successful, we know now that we are dealing with a Bulk String for sure.
As per the Bulk String definition the next element is a `\r\n`.
Depending on how you test your implementation locally, it can be the case that backslashes are double escaped, i.e. `\\r\\n`.
For this case we defined an alternative implementation `crlfAlt` that is based on the `crlf` function.
You can look at the solution code for this particular implementation.

After parsing the `\r\n` we expect the actual string of length or size `n`.
There exists another helpful function, `count`, from the Megaparsec library.
It takes an integer and a monadic expression, which is in our case a token.
Since the request we process could contain any printable character (i.e. including space and most symbols), we are interested in the token [printChar](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte.html#v:printChar).
Counting down from the integer `n`, the `count` function repeatedly consumes characters (or single ByteStrings in our case) and stores them in a list `s`.

Since we want a single string rather than individual elements, like this list `s` contains at the moment, we use the `pack` function to produce a single ByteString, or `Response`. By now, you should notice the benefits of using these libraries as we can simply re-use such functions for our purpose.

With this basic function in place we can now build other functions that need to parse Bulk Strings.

## 2.3. Parse Arrays
With Bulk String parsing in place, we can move one abstraction level up and parse a whole Array.

### 2.3.1. Extract a Command
This is similar to the Bulk String, but we can modify this function to tell us also which command is embedded in the Array.
Therefore, we call this function `commandCheck`.

```haskell
commandCheck :: Text -> Parser (Integer, ByteString)
commandCheck c = do
    _ <- "*"
    n <- decimal
    guard $ n > 0
    cmd <- crlfAlt *> redisBulkString
    guard $ cmpIgnoreCase (decodeUtf8 cmd) c
    return (n, cmd)
```
Since every array starts the same way with a `*` and a number indicating the number of elements, we expect it to contain a command in the first element.
This element is assumed to be a Bulk String, for which we can leverage the already implemented function.

To ensure that we only process functions that are implemented, you may want to provide such a check.
When we then implement each function, we simply pass the corresponding name of that Redis function to the `commandCheck` (for example `commandCheck "echo"`).
The `guard` ensures this is indeed the case.
We will later deal with the case how to check for different functions.

The command is extracted into the variable `cmd`.
Since `\r\n\` follows after the Array's number, it is first consumed, but for our purposes it is not relevant to keep.
To discard such an element, you can use the `*>` combinator, which only keeps the element it points towards, in this case the one to its right (the opposite combinator exists as well).
Afterwards, the remaining element is consumed using the `redisBulkString` function and stored in `cmd`.

Since a user does not have to worry about lower and upper case when typing a request, we implement a custom function `cmpIgnoreCase` that compares two elements ignoring the case.
The details of this simple implementation are in the solution code.
To compare actual `Text` rather than `ByteString`s, a conversion from ByteString to Text is necessary, which is achieved by using the library-provided `decodeUtf8` function.

To return two values of different types, a `Tuple` is commonly used.
The `commandCheck` function returns the number of elements in the Array (of type `Integer`) along with the identified command (of type `ByteString`) in such a tuple.

### 2.3.2. Extract the Rest
We can now build another function on top that extracts and structures all information contained in an Array.
This stage is about the `echo` command, which we use an example here, but the `ping` command is even easier to implement and provided in the solution code.

```haskell
parseEcho :: Parser Command
parseEcho = do
    (n, _) <- commandCheck "echo"
    guard $ n == 2
    message <- crlfAlt *> redisBulkString
    return $ Echo message
```

As you can see, we use the `commandCheck` to check if the Array contains the `echo` command.
Per the Redis specification, `echo` requires another parameter, which is the message that the server has to return
Therefore, another check is performed to ensure that the Array consists of exactly two elements.
To extract the second element (the message) from the Array, the `redisBulkString` function is used and the message is stored in the `message` variable.

### 2.3.3. `Command` Type
This function returns a type `Command` which we have not encountered yet:
```haskell
data Command = Ping
             | Echo Message
```

To define a pure return from the parsing we can define a type where all possible return values are referenced.
This allows us to better structure the parsing so that the implementation of what happens with the outcome of the parsing can be handled by each corresponding function.
We will encounter in the section, where we return a response, how to deal with the different `Command` types.

The `Command` type also explains where `Echo` in the last expression originates from.
`Echo` is actually a constructor that takes a value, `Message`.
We defined message as another type synonym `type Message = ByteString`.
The same applies to `Ping` bar the value.

If the parsing fails, that means when a command other than `ping` or `echo` is requested, ideally an error should be thrown that informs the user.
You can do this by defining another type that handles errors.
```haskell
data ApplicationError = UnknownCommand
```
This error is returned if the parsing could not find a matching `Command`.

## 2.4. Combine Parsers
Before we go back to our main function, we need to piece together the two parsing functions we just built, `parsePing` and `parseEcho`.

### 2.4.1. The `<|>` Operator
When we receive a request, we naturally do not know what it contains.
Hence, it would be great to have something akin to `switch case` to try different options, for example try `ping` first, then try `echo`, and so on.
In Haskell there exists an elegant implementation in the `Alternative` type class which is the `<|>` operator.
Because `Alternative` is of type `Applicative` (i.e. `Applicative Functor`), `Megaparsec` is a `Monad`ic parser, and all `Monad`s are based on `Applicative Functor`s, we can use the `<|>` operator.

`<|>` can be used between alternative options, where it starts to parse the first, and if it does not match continues with the second option, and so on.

### 2.4.2. Backtrack Failed Attempts
Unfortunately, when using `<|>` alone, the parser already consumes parts of the input to decide if the first option is a match.
If not, it moves on to the second option, where it would fail in our case if we do not get the full input back.
To avoid this, we can use `try` which backtracks a failed attempt.
You can [read about try](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#v:try) in the Megaparsec documentation.

With that, we can construct a function `parseToCommand` which can parse the different requests for us and only then call the appropriate `Command` that is contained in the input.

```haskell
parseToCommand :: Parser Command
parseToCommand = try parseEcho
             <|> try parsePing
```

### 2.4.3. Megaparsec's `parse` Function
Until now, we have not really used the parsing functionality of Megaparsec.
The library contains a `parse` function, which can be used to actually start the parsing.
This function accepts three inputs:
1. The function with the parsing logic, in our case `parseToCommand`
2. An identifier that is returned if there is a failed match - this is useful if multiple parsers are in place, but for us it can be an empty string, i.e. `""`
3. The input to be parsed, i.e. the `Request`

Since the Megaparsec `parse` function returns the `Either` type, we have to handle the two outcomes.
The parser's task is to return either a command or, in case of failure, to indicate an error.
The `Either` type is again useful here to distinguish two cases, especially since one means success and the other is an error.

```haskell
parseRequest :: Request -> Either ApplicationError Command
parseRequest req = case parseResult of
                       Left _    -> Left UnknownCommand
                       Right cmd -> Right cmd
                   where parseResult = parse parseToCommand "" req
```

If the parsing was a success, we return the type `Command` that comprises the actual command and, if present, any keys, values, messages, etc.
When a user sent a request that is not implemented, it is not recognized by the parser.
In that case, it returns the type `ApplicationError`, specifically the `UnknownCommand` value.

# 3. Process Information
So far, we have only extracted information from the request, but we have not done anything yet with it.

## 3.1. Execute Commands
The `parseEcho` function returns a value `Echo Message`.
Your goal is to get the server to return the message from the value.
This also applies to `ping`, where the message in the value is simply `PONG`, rather than a user specified message.

It may be surprising, but we do not have to implement a specific function just for `echo` or for `ping`.
Since they are rather simple and have no inherent logic, we can simplify our program from the start.

However, we have to do something with the `Command` type that the two parsers, `parseEcho` and `parsePing` return.

### 3.1.1. Interactive Interactions
First, a little detour on interactive interactions.
Since the whole Redis implementation is an 'impure' interaction with the outside world, it is of type `IO`.
You may want to [read about IO](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/IO) first to understand why it is relevant here.

Our goal is to return a `Response` at the end to the client.
Therefore, each function that returns something to the main function has to be of type `IO`, and in our case of type `IO Response`.
The function that takes the `Command` as an input should therefore produce the `IO Response`.

### 3.1.2. `exec` Function
For this we define a function `exec` which basically executes the command and produces a response.

```haskell
exec :: Command -> IO Response
exec Ping       = return "PONG"
exec (Echo msg) = return msg
```

It performs a pattern matching with the `Command` type and for each match we define an action.
Since `ping` simply returns `PONG`, we can just let it do that without the need for a separate ping function.
The same applies to `echo`, we just return the message that we extracted before.

With `exec` in place, we have a function that returns a response and you can now return the `Response` to the user.



## 3.2. Response
Now, that we have implemented the full parsing logic and we get a return value in any case, we can start putting everything together.
For this, we go back to our main function.

### 3.2.1. Parse the Request
Previously, we discarded the input from the user (or client) since we assumed only `ping` is sent.
This assumption no longer holds as we also want to process the `echo` command.
Therefore, we can exchange the underscore `_` with a variable name of our choice, `request` in our case.

Since this variable contains all relevant information that is encoded in RESP, we want to parse it using our previously defined function `parseRequest`.
The result from `parseRequest` is something of type `Either ApplicationError Command`.
Therefore, we have to first distinguish between the error and the command, before we pass the actual `Command` to the `exec` function for execution.

### 3.2.2. Return a Response
The result from the `exec` function, which ultimately returns a type `IO Response`, should be sent back to the client, also in a RESP format.

```haskell
_ <- forever $ do
    request <- recv socket 2048
    response <- do
        case parseRequest request of
            Left _ -> return "-ERR Unknown Command"
            Right cmd -> exec cmd
    send socket (encodeRESP response)
putStrLn $ "disconnected client: " ++ show address
```

This is done using the `send` function, but this time we add the response that was returned from either `ping` or `echo` (or an error message if neither of both).

### 3.2.3. Encode Response
Before sending the response we need to transform it back into the RESP format.
One last function in this section, the `encodeRESP` function, will handle this for us.
It takes the `Response` and transforms it to a RESP Simple String by putting it between `+` and `\r\n`.
We assume, based on the Redis specification and our implementation of `ping` and `echo`, that only a single, simple string is returned.

```haskell
encodeRESP :: Response -> Response
encodeRESP s = B.concat ["+", s, "\r\n"]
```

This concludes this rather long, but hopefully informative and helpful solution write-up.
The next stages will be a bit easier to implement since we have the main parsing functions already in place.
