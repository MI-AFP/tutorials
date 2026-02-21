# IO in Real Programs

In the previous lecture, we explored `Functor`, `Applicative`, and `Monad`. This time, we finally use them for what they are most famous for: *real programs*.

Until now, almost everything we wrote was pure. Functions always returned the same result for the same input. That's beautiful — but real applications must:

* Read files
* Talk to the network
* Accept command-line arguments
* Print results
* Handle failures

All of this happens in `IO`. This lecture is about writing practical programs, not just understanding typeclasses in isolation. We will gradually build pieces of a small CLI application and use that as motivation for:

* IO
* Exception handling
* JSON parsing (Aeson)
* CLI arguments and environment variables
* Application context (ReaderT pattern)
* Traversable in practice
* Lens with JSON

## Working with IO

We can easily check what is actually `IO` type:

```haskell
Prelude> :info IO
newtype IO a
  = GHC.Types.IO (GHC.Prim.State# GHC.Prim.RealWorld
                  -> (# GHC.Prim.State# GHC.Prim.RealWorld, a #))
        -- Defined in ‘GHC.Types’
instance Monad IO -- Defined in ‘GHC.Base’
instance Functor IO -- Defined in ‘GHC.Base’
instance Applicative IO -- Defined in ‘GHC.Base’
instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
```

The important takeaway:

> `IO` is just another type constructor that happens to represent actions interacting with the outside world. It is not a magical thing, it is not a special language construct, it is just a type.

`IO` is not (about):

* a special language construct,
* *impure Haskell*,
* *imperative style in Haskell*,
* *magic*.

It is a type that describes a **computation which, when executed by the runtime, may perform side effects**.

### `main` = the entry point

Every executable Haskell program must define:

```haskell
main :: IO ()
```

This means:

* the program performs some `IO` actions, and
* it produces no meaningful result (the `()` type is a type with a single value, also called *unit*).

If you need to exit with a non-zero code, you can use `System.Exit`:

```haskell
import System.Exit

main :: IO ()
main = do
  -- ...
  die "Something went wrong"

-- or

main :: IO ()
main = do
  -- ...
  exitWith (ExitFailure 1)
```

### Basic `IO` actions

The `IO` type is a *monad*, which means we can use `do` notation to sequence actions.

To **print something to the console**, we can use `putStrLn` or `print` (there are also other functions for printing such as `putStr` or `putChar`, but these are the most common ones):

```haskell
main :: IO ()
main = do
  putStrLn "Hello, world!"
  print (1 + 2)
```

Types:

```haskell
putStrLn :: String -> IO ()
print :: Show a => a -> IO ()
```

Notice that `putStrLn` takes a `String` and returns an `IO ()`, while `print` takes any value that is an instance of the `Show` typeclass and also returns an `IO ()`. This means that both functions perform some side effect (printing to the console) and do not produce any meaningful result.

To **read input from the console**, we can use `getLine`:

```haskell
main :: IO ()
main = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn ("Hello, " ++ name ++ "!")
```

Types:

```haskell
getLine :: IO String
```

Notice that `getLine` returns an `IO String`, which means it performs some side effect (reading from the console) and produces a `String` as a result. We can use the `<-` syntax in `do` notation to extract the `String` value from the `IO` action.

Here `<-` is syntactic sugar for `>>=`:

```haskell
main :: IO ()
main = putStrLn "What is your name?" >> getLine >>= \name ->
       putStrLn ("Hello, " ++ name ++ "!")
```

To **interact via stdin and stdout** is the most basic form of `IO` and you can use function `interact` easily to create simple programs that read from stdin and write to stdout:

```haskell
main :: IO ()
main = interact (unlines . reverse . lines)
```

`interact` takes a function of type `(String -> String)` and returns an `IO ()` action that reads from standard input, applies the function to the input, and then writes the result to standard output. In this example, we are reversing the lines of input:

```haskell
interact :: (String -> String) -> IO ()
```

### `return` is not "return"

This might be a bit confusing for people coming from other languages, but `return` in Haskell does not mean "exit the function and return a value". Instead, it is a function that takes a pure value and wraps it in the `IO` type:

```haskell
return :: a -> IO a
```

`return`:

- does not cause the function to exit,
- does not have to be the last statement in a `do` block,
- does not have to be used at all,
- simply does not behave like `return` in other languages (C, Java, Python, ...).

It simply wraps a pure value in the `IO` type, which allows us to use it in a `do` block where we are working with `IO` actions. For example:

```haskell
promptInt :: IO Int
promptInt = do
  putStrLn "Please enter an integer:"
  input <- getLine
  return (read input :: Int)
```

`return` here takes the pure `Int` value produced by `read input` and wraps it in `IO Int`, which is the type of the `promptInt` function (if you know `pure` from `Applicative`, you can also use it instead of `return` since they are essentially the same thing).


### Composition of IO actions

Because `IO` is a monad, we can compose `IO` actions using `do` notation or using the monadic operators (`>>`, `>>=`, etc.). This allows us to build more complex interactions with the outside world. For example, we can read a line from the console, reverse it, and print it back:

```haskell
main :: IO ()
main = do
  reversed <- reverse <$> getLine
  print reversed
```

Using what we know about `Functor` and `Applicative`, we can also write this without `do` notation:

```haskell
import System.Random

randomInt :: IO Int
randomInt = randomRIO (1, 10)

main :: IO ()
main = do
  result <- (+) <$> randomInt <*> randomInt
  print result
```

This demonstrates:

* We are composing IO actions.
* The computation structure remains pure.
* Only the execution happens in IO.

### Sequencing without binding results

Sometimes we want to perform an `IO` action but we don't care about its result. In that case, we can use the `>>` operator to sequence actions without binding their results:

```haskell
main :: IO ()
main = print 1 *> print 2 *> print 3
```

We simply use sequencing operators to perform multiple `IO` actions in a row, and we don't have to worry about the results of those actions if we don't need them.

```haskell
(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad m => m a -> m b -> m b
```

Both mean:

> Perform the first action, ignore its result, and then perform the second action.

### `do` notation is just syntax sugar

While the `do` "block" may look like imperative code, it is actually just syntactic sugar for chaining monadic operations. The `do` notation allows us to write code that looks more sequential and easier to read, but under the hood, it is still just a series of function applications and monadic bindings. For example, the following `do` block:

```haskell
main :: IO ()
main = do
  x <- getLine
  y <- getLine
  putStrLn (x ++ " " ++ y)
```

It is still pure functional composition of `IO` actions (which are having side effects), and it can be rewritten without `do` notation as:

```haskell
main :: IO ()
main = getLine >>= \x -> getLine >>= \y -> putStrLn (x ++ " " ++ y)
```

This is equivalent to the `do` block version, but without the syntactic sugar. The `>>=` operator is the monadic bind operator, which sequences monadic actions and passes their results to the next action.

So, `do` = combination of `>>=` and `>>` + lambda functions. No magic, just syntax sugar.

### IO with files

Having fun with console input and output is great, but real applications often need to read from and write to files. Haskell provides a rich set of functions for working with files in the `System.IO` module. For example, to read the contents of a file, we can use `readFile`:

```haskell
import System.IO

main :: IO ()
main = do
  contents <- readFile "input.txt"
  putStrLn contents
```

Type:

```haskell
readFile :: FilePath -> IO String
```

It is important to remember that Haskell is lazy. When we call `readFile`, it does not immediately read the entire file into memory. Instead, it returns an `IO String` that represents the action of reading the file. The actual reading happens when we try to use the contents (e.g., when we print it). This means that if the file is very large, we might run into memory issues if we try to read it all at once. In such cases, we can use functions like `hGetContents` along with `withFile` to read the file in a more controlled manner.

To write to a file, we can use `writeFile`:

```haskell
import System.IO

main :: IO ()
main = do
  writeFile "output.txt" "Hello, file!"
```

You can find more functions for working with files in the [`System.IO`](https://hackage-content.haskell.org/package/base/docs/System-IO.html) module, such as `appendFile`, `hPutStrLn`, and many others that allow you to work with file handles for more complex file operations.

Typically, you want to use `withFile` to ensure that file handles are properly closed after their use, even if an error occurs:

```haskell
import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStrLn contents
```

When working with various file formats (e.g., JSON, CSV, etc.), you can use libraries like `aeson` for JSON parsing or `cassava` for CSV parsing. These libraries provide convenient functions for reading and writing structured data to and from files. As always, do not reinvent the wheel — check Hackage for existing libraries that can help you with your specific use case.

### Command-line arguments and environment variables

In addition to reading from and writing to files, real applications often need to interact with the command line and environment variables. Haskell provides functions for accessing command-line arguments and environment variables in the `System.Environment` module. For example, to access command-line arguments, we can use `getArgs`:

```haskell
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Command-line arguments: " ++ show args)
```

Type:

```haskell
getArgs :: IO [String]
```

From other languages that have something like `argv`, you might expect `args` to contain the name of the program as the first element, but in Haskell, `getArgs` returns only the arguments passed to the program, not including the program name itself. If you need the program name, you can use `getProgName`:

```haskell
import System.Environment

main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs
  putStrLn ("Program name: " ++ progName)
  putStrLn ("Command-line arguments: " ++ show args)
```

In real-world applications, you might want to use a specific library for parsing command-line arguments, such as `cmdargs`, `optparse-applicative`, or `parseargs`. These libraries provide more powerful and flexible ways to define and parse command-line options, flags, and arguments. There is even a dedicated wiki page [Command line option parsers](https://wiki.haskell.org/Command_line_option_parsers) but you can consult Hackage.

Environment variables can be accessed using `getEnv`:

```haskell
import System.Environment

main :: IO ()
main = do
  home <- getEnv "HOME"
  putStrLn ("Home directory: " ++ home)
```

### IO with network

For network programming as simple IO, you can use low-level networking libraries like `network` or higher-level libraries such as `http-conduit` or `wreq` for making HTTP requests. These libraries provide functions for creating sockets, sending and receiving data over the network, and handling various network protocols.

Basic example using `network` and `Network.Socket`:

```haskell
import Network.Socket

main :: IO ()
main = do
  -- Create a socket
  sock <- socket AF_INET Stream defaultProtocol
  -- Connect to a server (e.g., localhost on port 10666)
  connect sock (SockAddrInet 10666 (tupleToHostAddress (127, 0, 0, 1)))
  -- Send a message
  send sock "Hello, server!"
  -- Close the socket
  close sock
```

Similarly, you can use it to create a simple server that listens for incoming connections and handles them (prints received messages, etc.):

```haskell
import Network.Socket

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 10666 iNADDR_ANY)
  listen sock 5
  putStrLn "Server is listening on port 10666..."
  forever $ do
    (conn, _) <- accept sock
    putStrLn "Client connected!"
    msg <- recv conn 1024
    putStrLn ("Received message: " ++ show msg)
    close conn
```

We will go into HTTP servers (web applications) later in the course, but this is just to show that you can do network programming with `IO` as well.

### Separate IO from pure code

When writing real applications, it is a good practice to separate the pure logic of your program from the `IO` actions. This makes your code easier to test and reason about. You can write pure functions that take regular data types as input and produce regular data types as output, and then have a thin layer of `IO` code that interacts with the outside world and calls these pure functions.

For example, this is bad practice:

```haskell
main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let number = read input :: Int
  putStrLn ("The square of the number is: " ++ show (number * number))
```

Better solution is to separate the pure logic:

```haskell
square :: Int -> Int
square x = x * x

main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let number = read input :: Int
  putStrLn ("The square of the number is: " ++ show (square number))
```

This code is easier to test because we can write unit tests for the `square` function without worrying about `IO`. The `main` function is now just a thin layer that handles the interaction with the user, while the core logic of squaring a number is contained in a pure function. This separation of concerns is a fundamental principle in functional programming and helps to keep your code clean and maintainable.

> Keep IO at the edges of your program.

## Error Handling in Haskell

In pure functional programming, errors are usually handled explicitly using types:

* `Maybe a` — something may be missing
* `Either e a` — something may fail with an error value

However, once we step into IO, things get more complicated:

* Files may not exist
* Network requests may fail
* Permissions may be missing
* JSON may be malformed

These failures are represented using exceptions. Understanding when to use types and when to use exceptions is essential for writing good Haskell programs.

### Errors in pure code

Before we dive into `IO`, let's quickly review how we handle errors in pure code. The most common way to represent a failure is to use the `Maybe` type:

```haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
```

The caller must check the result:

```haskell
main :: IO ()
main = do
  let result = safeDiv 10 0
  case result of
    Nothing -> putStrLn "Division by zero!"
    Just value -> putStrLn ("Result: " ++ show value)
```

Or using `Either` for more informative errors:

```haskell
safeDiv :: Int -> Int -> Either String Int
safeDiv _ 0 = Left "Division by zero!"
safeDiv x y = Right (x `div` y)

main :: IO ()
main = do
  let result = safeDiv 10 0
  case result of
    Left err -> putStrLn ("Error: " ++ err)
    Right value -> putStrLn ("Result: " ++ show value)
```

### `error` and `undefined`

You may encounter and use `error` and `undefined` in Haskell. These are special functions that cause the program to crash when evaluated:

```haskell
error :: String -> a
undefined :: a
```

In both cases, the program will terminate with an error message. These functions are useful for stubbing out code during development or for indicating that a certain code path should never be reached. However, they should be used with caution in production code, as they can lead to unexpected crashes if not handled properly.

Example you may know very well:

```haskell
head :: [a] -> a
head [] = error "Empty list"
head (x:_) = x

main :: IO ()
main = print (head [])

-- This will crash with "Empty list" error:
-- *** Exception: Prelude.head: empty list
```

These errors are not recoverable. They are not meant to be caught and handled; they indicate a bug in the program that should be fixed. In contrast, exceptions in `IO` can be caught and handled gracefully, allowing the program to continue running even when something goes wrong.

You should avoid using `error` and `undefined` in production code. Instead, use proper error handling with `Maybe`, `Either`, or exceptions in `IO` to ensure that your program can handle failures gracefully.

### Exceptions in `IO`

In `IO`, we can use exceptions to represent failures that occur during side-effecting operations. For example, when reading a file, if the file does not exist, an exception will be thrown. We can catch and handle these exceptions using the [`Control.Exception`](https://hackage.haskell.org/package/base/docs/Control-Exception.html) module.

#### `try` = convert exceptions to `Either`

The `try` function allows us to catch exceptions and convert them into an `Either` type:

```haskell
try
  :: Exception e
  => IO a             -- action that may throw an exception (of type `e`)
  -> IO (Either e a)  -- produces either an exception (Left) or a successful result (Right)
```

```haskell
import Control.Exception
import System.IO

readConfig :: FilePath -> IO (Either IOException String)
readConfig path = try (readFile path)

main :: IO ()
main = do
  result <- readConfig "config.json"
  case result of
    Left err -> putStrLn ("Failed to read config: " ++ show err)
    Right contents -> putStrLn ("Config contents: " ++ contents)
```

#### `catch` = handle exceptions directly

The `catch` function allows us to handle exceptions directly in the `IO` monad:

```haskell
catch
  :: Exception e
  => IO a         -- main action that may throw an exception
  -> (e -> IO a)  -- handler function (takes an exception and returns an IO action)
  -> IO a         -- either the result of the main action or the handler
```


```haskell
import Control.Exception
import System.IO

main :: IO ()
main = do
readFile "config.json"
    >>= putStrLn
    `catch` handler
  where
    handler :: IOException -> IO ()
    handler e =
      putStrLn ("Error reading file: " ++ show e)
```

#### `handle` = handle exceptions in a more composable way

The `handle` function is similar to `catch`, but it allows us to write exception handling in a more composable way:

```haskell
handle
  :: Exception e
  => (e -> IO a)  -- handler function (takes an exception and returns an IO action)
  -> IO a         -- main action that may throw an exception
  -> IO a         -- either the result of the main action or the handler
```

```haskell
import Control.Exception
import System.IO

main :: IO ()
main = handle handler (readFile "config.json" >>= putStrLn)
  where
    handler :: IOException -> IO ()
    handler e = putStrLn ("Error reading file: " ++ show e)
```

Basically, the two are equivalent, but `handle` allows us to write the exception handling logic separately from the main action, which can make the code cleaner and easier to read:

```haskell
handle handler action = action `catch` handler
```

#### `bracket` = safe resource management

A very important pattern for working with resources (like file handles, network connections, etc.) is to use the `bracket` function, which ensures that resources are properly released even if an exception occurs:

```haskell
bracket
  :: IO a         -- action to acquire the resource `a`
  -> (a -> IO b)  -- action to release the resource `a`
  -> (a -> IO c)  -- action that uses the resource `a` and produces a result of type `c`
  -> IO c         -- the result of the action that uses the resource
```

Typical resource is a file handle:

```haskell
import Control.Exception
import System.IO

main :: IO ()
main =
  bracket (openFile "input.txt" ReadMode) hClose $ \handle -> do
    contents <- hGetContents handle
    putStrLn contents
```

1. We acquire the resource by opening a file handle (`openFile "input.txt" ReadMode`).
2. We release the resource by closing the handle (`hClose`).
3. We use the resource in the action that is passed to `bracket` (`\handle -> do ...`).

The `bracket` function ensures that the file handle is properly closed even if an exception occurs while reading the file. This is crucial for preventing resource leaks and ensuring that your program behaves correctly in the face of errors.

#### `throw` and `throwIO` = throwing exceptions

To throw an exception in Haskell, we can use the `throw` function:

```haskell
throw :: Exception e => e -> a
```

However, `throw` can be used in pure code, which means that it can lead to unexpected exceptions if not used carefully. In `IO`, it is generally better to use `throwIO`, which ensures that the exception is thrown in the `IO` monad and can be caught using the standard exception handling functions:

```haskell
throwIO :: Exception e => e -> IO a
```

Using `throwIO` ensures that the exception is properly handled in the context of `IO` and allows us to write more robust error handling logic.

Example:

```haskell
import Control.Exception

main :: IO ()
main = do
  putStrLn "Enter a number:"
  input <- getLine
  let number = read input :: Int
  if number < 0
    then throwIO (userError "Negative number is not allowed")
    else putStrLn ("The square of the number is: " ++ show (number * number))

-- `userError` is a helper function that creates an `IOException` with a custom error message
-- it comes from Prelude: userError :: String -> IOError
```

#### Typing exceptions

Haskell exceptions are typed. This means that you can have different types of exceptions for different kinds of errors, and you can catch them separately. For example, you might have a `FileNotFoundException` for when a file is missing, and a `ParseException` for when JSON parsing fails. This allows you to write more specific error handling logic based on the type of exception that was thrown.

You can define your own exception types by creating a new data type and making it an instance of the `Exception` typeclass. This allows you to throw and catch your custom exceptions in a type-safe way:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception
import Data.Typeable

data ConfigError = InvalidFormat
  deriving (Show, Typeable, Exception)
```

Now you can throw a `ConfigError` when something goes wrong with your configuration:

```haskell
import Control.Exception

verifyConfig :: IO ()
verifyConfig = do
  putStrLn "Enter config value:"
  input <- getLine
  if not (isValidConfig input)
    then throwIO InvalidFormat
    else putStrLn "Config is valid!"
```

This could be eventually caught and handled in a way that is specific to `ConfigError`:

```haskell
import Control.Exception

main :: IO ()
main = handle handler verifyConfig
  where
    handler :: ConfigError -> IO ()
    handler InvalidFormat = putStrLn "The configuration format is invalid. Please check your input."
    handler _ = putStrLn "Oopsie! An unknown error occurred."
```

#### When to use what?

This is the most important conceptual part of this section. When should you use `Either` / `Maybe` and when should you use exceptions?

1) Use `Either` / `Maybe` when:

  * The failure is part of your domain logic
  * It is expected and recoverable
  * You want explicit error handling
  * Examples: invalid user input, validation failure, parsing error

2) Use exceptions when:

  * The failure is due to an external factor (e.g., file not found, network error)
  * It is unexpected and not part of your domain logic
  * You want to separate error handling from the main logic
  * Examples: file I/O errors, database connection failures, network timeouts, resource leaks

For creating custom exceptions, it makes sense to do so when you have specific error conditions that you want to represent in a type-safe way. This allows you to catch and handle those exceptions separately from other types of errors, and it can make your error handling logic more robust and easier to maintain.

#### Combining `IO` and pure error handling

Very common pattern is to combine `IO` with pure error handling using `Either`. For example, you might have a function that reads a file and parses its contents, and you want to represent parsing errors using `Either` while still performing the file I/O in `IO`:

```haskell
loadTasks :: FilePath -> IO (Either String [Task])
```

Inside this function, you would perform the file reading in `IO`, and then use `Either` to represent any parsing errors that might occur. This allows you to keep your error handling logic separate from your I/O logic, and it makes it easier to test the pure parsing function independently of the file I/O.

```haskell
loadFileSafe :: FilePath -> IO (Either String String)
loadFileSafe path = do
  result <- try (readFile path)
  case result of
    Left (e :: IOException) ->
      return (Left (show e))
    Right content ->
      return (Right content)
```

We convert the `IOException` into a `String` and return it as a `Left` value in the `Either`, while the successful file contents are returned as a `Right` value. This way, the caller of `loadFileSafe` can handle both the success and failure cases in a clean and type-safe manner.

#### Final note: fail fast vs recover gracefully

Two common philosophies for error handling:

1) **Fail fast**: throw exceptions **immediately when something goes wrong**, and let the program crash. This is often used during development to quickly identify and fix bugs.
2) **Recover gracefully**: catch exceptions and handle them in a way that **allows the program to continue running**. This is important for production code to ensure a good user experience and to prevent crashes.

### Example: Aeson and JSON parsing

JSON is everywhere: config files, APIs, logs, data exchange. In Haskell, the standard library for JSON is **Aeson**. While you might want to use other file formats, this is a very common one and it is a good example of how to use typeclasses in practice. Other formats (YAML, CSV, XML, ...) have similar libraries with similar APIs, so learning Aeson will give you a good foundation for working with structured data in Haskell.

Aeson works through typeclasses:

* `FromJSON a` — decode JSON into a Haskell value
* `ToJSON a` — encode a Haskell value into JSON

This fits perfectly with what you already know about typeclasses: **we define instances**, and then generic functions like `decode` / `encode` work for our types.

You typically combine Aeson with:

* `ByteString` (not `String`) for `IO`
* `Either` for errors (malformed JSON, wrong shape)
* exception handling for file `IO` errors

#### Setup

Typically, you want to use these when working with Aeson:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
```

`OverloadedStrings` allows us to write string literals that can be interpreted as `ByteString`, which is what Aeson uses for JSON data. `DeriveGeneric` allows us to automatically derive instances of the `Generic` typeclass, which is required for Aeson's generic encoding and decoding.

#### Parsing a Custom Type

```haskell
data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person
```

With these (default) instances, we can now easily encode and decode `Person` values to and from JSON:

```json
{
  "name": "Alice",
  "age": 30
}
```

```haskell
main :: IO ()
main =
  withFile "person.json" ReadMode $ \handle -> do
    contents <- BL.hGetContents handle
    case eitherDecode contents of
      Left err -> putStrLn ("Failed to parse JSON: " ++ err)
      Right person -> putStrLn ("Parsed person: " ++ show (person :: Person))
```

#### Parsing lists and nested structures

It is possible to easily parse more complex JSON structures, such as lists of people or nested objects. For example, if we have a JSON file that contains a list of people:

```haskell
data Task = Task
  { title :: String
  , done  :: Bool
  } deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

data Project = Project
  { projectName :: String
  , tasks       :: [Task]
  } deriving (Show, Generic)

instance FromJSON Project
instance ToJSON Project
```

```json
{
  "projectName": "AFP",
  "tasks": [
    { "title": "Finish IO lecture", "done": false },
    { "title": "Add Aeson example", "done": true }
  ]
}
```

Then simply:

```haskell
eitherDecode jsonBytes :: Either String Project
```

#### Customizing JSON parsing

By default, Aeson uses the field names of your data types as the keys in the JSON. However, you can customize this behavior using options. For example, if you want to use camelCase in your Haskell code but snake_case in your JSON, you can use the `defaultOptions` with a custom `fieldLabelModifier`:

```haskell
import Data.Aeson.Types (defaultOptions, fieldLabelModifier)

data Person = Person
  { _name :: String
  , _age  :: Int
  } deriving (Show, Generic)

instance FromJSON Person where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = drop 1 }  -- drop leading '_'

instance ToJSON Person where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = drop 1 }
```

So `_name` in Haskell will correspond to `name` in JSON, and `_age` will correspond to `age`. This allows you to have more control over the JSON structure while still keeping your Haskell code clean and idiomatic.

#### Optional fields and default values

Naturally, JSON often has optional fields. In Haskell, we can represent optional fields using `Maybe`:

```haskell
data Person = Person
  { name :: String
  , age  :: Int
  , email :: Maybe String
  } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person
```

#### Full-manual parsing

If you need more control over the parsing process (e.g., to handle different JSON formats, to provide better error messages, or to perform validation), you can write a custom `FromJSON` instance using the `parseJSON` function:

```haskell
instance FromJSON Person where
  parseJSON = withObject "Person" $ \obj -> do
    name <- obj .: "name"
    age <- obj .: "age"
    email <- obj .:? "email"  -- optional field
    when (age < 0) $
      fail "Age cannot be negative"
    return (Person name age email)
```

#### Encoding to JSON

Encoding a Haskell value to JSON is just as easy. You can use the `encode` function, which takes a value that is an instance of `ToJSON` and produces a `ByteString` containing the JSON representation:

```haskell
main :: IO ()
main = do
  let person = Person "Alice" 30 (Just "alice@acme.ltd")
  BL.writeFile "person.json" (encode person)
```

Alternatively, there is `aeson-pretty` library that provides a `encodePretty` function for producing more human-readable JSON output.

```haskell
import Data.Aeson.Encode.Pretty (encodePretty)

main :: IO ()
main = do
  let person = Person "Alice" 30 (Just "alice@acme.ltd")
  BL.writeFile "person.json" (encodePretty person)
```

## Other useful typeclasses

Now we look at several typeclasses that appear frequently in real programs: `Foldable`, `Traversable`, `State`, `Reader`, `Parser`, monad transformers, and `Lens`. We will not go into details of these typeclasses, but we will give you a brief overview and some resources for further reading. There are also more that you might find useful in your projects, so feel free to explore Hackage and find the ones that fit your needs (e.g., `MonadError`, `MonadWriter`, `MonadReader`, `MonadState`, `Arrow`, `Category`, `Profunctor`, `Contravariant`, `Bifunctor`, etc.).

### Foldable

A Foldable structure is something that can be collapsed into a summary value.

Mathematically, a fold is:

```math
\text{foldr} :: (a \to b \to b) \to b \to t a \to b
```

> Given a way to combine one element with an accumulator, reduce the entire structure.

We have seen `foldr` and `foldl` for lists, but the `Foldable` typeclass allows us to use folding operations on any data structure that implements it (e.g., `Maybe`, `Either`, `Tree`, etc.). This is very powerful because it allows us to write generic code that can work with any foldable structure. Many known functions like `sum`, `product`, `length`, `elem`, etc. are defined in terms of `Foldable`.

There is also a hidden gem called `foldMap` that allows us to map each element to a monoid and then combine them using the monoid operation. This is a very powerful abstraction that allows us to do all sorts of things with foldable structures.

```math
\text{foldMap} :: (a \to m) \to t a \to m
```

where `m` is a monoid. This means that we can use `foldMap` to perform all sorts of operations on foldable structures, such as counting elements, finding the maximum, concatenating strings, etc., by simply providing the appropriate monoid.

```haskell
import Data.Monoid (Sum(..))

countDone :: Foldable t => t Task -> Int
countDone =
  getSum . foldMap (\t -> if done t then Sum 1 else Sum 0)
```

This pattern typically appears in real code such as: logging, validation, counting, finding maximum/minimum or other aggregations, etc.

### Traversable

If `Foldable` is about collapsing a structure, then `Traversable` is about traversing a structure while applying an effect. It allows us to map each element to an effectful computation and then combine those computations in a way that preserves the structure.

```math
\text{traverse} :: (a \to f b) \to t a \to f (t b)
```

We can compare it with known `fmap` from `Functor`:

```math
\text{fmap} :: (a \to b) \to f a \to f b
```

Essentially, `traverse` is like `fmap`, but it works with effects (e.g., `IO`, `Either`, `Maybe`, etc.) and it preserves the structure of the data. This allows us to perform operations that have side effects while still working with our data in a pure way.

Imagine you have a function to validate a single `Task`:

```haskell
validateTask :: Task -> Either String Task
```

Then, simply `traverse` helps to validate a list of tasks:

```haskell
validateTasks :: [Task] -> Either String [Task]
validateTasks = traverse validateTask
```

If a single task is invalid, the entire validation will fail with an error message. If all tasks are valid, we get back a list of valid tasks. This pattern is very common in real code when you want to perform some effectful operation on each element of a data structure while preserving the overall structure.

### State

Sometimes computation carries evolving state. However, this is not about mutable state or side effects. It is about pure functional composition of computations that have some state that changes over time. The `State` monad allows us to model this kind of computation in a pure way.

We can model such behavior as:

```math
s \to (a, s')
```

where `s` is the state, `a` is the result of the computation, and `s'` is the new state after the computation.

You may know that from state machines, parsers, simulations, etc. The `State` monad provides a way to thread state through a sequence of computations without having to pass the state explicitly as an argument to each function. The `State` type is defined as follows:

```haskell
newtype State s a = State { runState :: s -> (a, s) }
```

Like this we can create a simple counter:

```haskell
import Control.Monad.State

type Counter = State Int

tick :: Counter ()
tick = modify (+1)

main :: IO ()
main = do
  let (result, finalState) = runState (tick >> tick >> tick)
  putStrLn ("Final state: " ++ show finalState)  -- Final state: 3
```

Similarly, you could implement random number generation, parsers, simulations, or any other computation that involves evolving state over time.

### Reader

The `Reader` monad is another useful abstraction for computations that depend on some shared environment or configuration. It allows us to model computations that read from a shared environment without having to pass the environment explicitly as an argument to each function.

```math
r \to a
```

where `r` is the environment and `a` is the result of the computation. The `Reader` type is defined as follows:

```haskell
newtype Reader r a = Reader { runReader :: r -> a }
```

Typical example from real-world applications is configuration management. You can have a `Config` type that contains all the configuration values for your application, and then use the `Reader` monad to access those values throughout your code without having to pass the `Config` around explicitly.

```haskell
import Control.Monad.Reader

data Config = Config
  { dbHost :: String
  , dbPort :: Int
  } deriving (Show)

type App = Reader Config

getDbHost :: App String
getDbHost = asks dbHost

main :: IO ()
main = do
  let config = Config "localhost" 5432
  let dbHost = runReader getDbHost config
  putStrLn ("Database host: " ++ dbHost)  -- Database host: localhost
```

Modern Haskell applications often use a combination of `State`, `Reader`, and other monads to manage state, configuration, and effects in a clean and composable way. This is often done using **monad transformers** (such as `ReaderT`, `StateT`), which allow us to combine multiple monads into a single monad that has the capabilities of all of them.

### Parser

Parsing is a common task in many applications, whether it's parsing command-line arguments, configuration files, or structured data formats like JSON or XML. The `Parser` typeclass (or more commonly, parser combinator libraries) provides a way to model parsers as composable functions that can be combined to build complex parsers from simpler ones.

```haskell
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
```

Parser combinator libraries (e.g., `parsec`, `megaparsec`, `attoparsec`) allow us to define parsers in a declarative way, using combinators to combine smaller parsers into larger ones. This makes it easy to write parsers that are both powerful and easy to read.

### Monad Transformers

Monad transformers are a powerful tool for combining **multiple monads into a single monad** that has the capabilities of all of them. For example, you might want to combine `Reader` for configuration, and `IO` for side effects. This is where `ReaderT` comes in:

```haskell
newtype App a = App
  { runApp :: ReaderT Config IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)
```

With this we can now write code that has access to both the configuration and the ability to perform `IO` actions, all within a single monad. This allows us to write clean and composable code that can handle complex interactions between different effects.

```haskell
getDbHost :: App String
getDbHost = asks dbHost

main :: IO ()
main = do
  let config = Config "localhost" 5432
  runReaderT (runApp getDbHost) config
```

We can add more, for example, `Except` for error handling, `State` for state management, etc., and combine them all together using monad transformers to create a powerful and flexible application architecture.

```haskell
newtype App a = App
  { runApp :: ReaderT Config (ExceptT String IO) a
  }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError String)
```

Typical example is to have some `AppContext` that contains configuration, state, and other shared resources, and then use monad transformers to access and manipulate that context throughout your application.

```haskell
data AppContext = AppContext
  { config :: Config
  , appName :: String
  } deriving (Show)

type App = ReaderT AppContext IO  -- or more complex stack with State, Except, Logging etc.

logInfo :: String -> App ()
logInfo msg = do
  name <- asks appName
  liftIO $ putStrLn ("[" ++ name ++ "] " ++ msg)
```

### Lens

Working with deeply nested data structures can be cumbersome in Haskell, especially when you want to update a nested field. The `Lens` library provides a powerful way to access and modify nested data structures in a composable and elegant way. Luckily, we already know the `OverloadedRecordDot` extension that allows us to use dot notation for accessing fields, and `Lens` builds on top of that to provide a way to update fields as well.

Without lenses, updating a nested field can be verbose and error-prone:

```haskell
data Config = Config
  { database :: DatabaseConfig
  } deriving (Show)

data DatabaseConfig = DatabaseConfig
  { host :: String
  , port :: Int
  } deriving (Show)

updateHost :: Config -> String -> Config
updateHost config newHost =
  config { database = (database config) { host = newHost } }
```

With lenses, we can define a lens for the `host` field and then use it to update the value in a more concise and composable way:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

makeLenses ''Config
makeLenses ''DatabaseConfig

updateHost :: Config -> String -> Config
updateHost config newHost =
  config & database . host .~ newHost
```

There are various operators provided by the `Lens` library for working with lenses, such as `.~` for setting a value, `%~` for modifying a value, and `^.` for accessing a value. Lenses can be composed together to work with deeply nested structures, making it much easier to read and write code that manipulates complex data. Key functions (and operators) are:

* `view` (or `^.`) — to access a value through a lens
* `set` (or `.~`) — to set a value through a lens
* `over` (or `%~`) — to modify a value through a lens

### Aeson Lens

Aeson provides a way to work with JSON data using lenses. This allows us to access and modify JSON values in a composable way without having to parse the entire JSON structure into a Haskell data type. This can be very useful when working with dynamic or unknown JSON structures.

```haskell
import Data.Aeson.Lens
import Control.Lens
```

Supposive we have a JSON value that represents a user:

```json
{
  "user": {
    "name": "Alice",
    "age": 30,
    "email": "alice@acme.ltd"
  }
}
```

With Aeson Lens, we can access the user's name like this:

```haskell
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens

main :: IO ()
main = do
  withFile "user.json" ReadMode $ \handle -> do
    contents <- BL.hGetContents handle
    let name = contents ^? key "user" . key "name" . _String
    case name of
      Just n -> putStrLn ("User's name: " ++ n)
      Nothing -> putStrLn "Name not found in JSON"
```

For example:

```haskell
preview (key "user" . key "name" . _String) jsonValue
```

returns `Just "Alice"` if the JSON structure matches, or `Nothing` if it doesn't. This allows us to safely access nested fields in a JSON object without having to worry about parsing errors or missing fields. We can also use lenses to modify JSON values in a composable way, which can be very powerful when working with dynamic JSON data.

Similarly:

```haskell
over (key "user" . key "age" . _Number) (+1) jsonValue
```

increments the user's age by 1, returning a new JSON value with the updated age. This allows us to manipulate JSON data in a functional way, without having to parse it into a Haskell data type and then convert it back to JSON.

## Task assignment

For the assignment, navigate to the `hw06` project and follow the instructions in the `README.md` file there as usual.

## Further reading

* [Haskell: Introduction to IO](https://wiki.haskell.org/Introduction_to_IO)
* [Haskell: Handling errors in Haskell](https://wiki.haskell.org/Handling_errors_in_Haskell)
* [Haskell: Foldable](https://en.wikibooks.org/wiki/Haskell/Foldable)
* [Haskell: Traversable](https://en.wikibooks.org/wiki/Haskell/Traversable)
* [Haskell: State monad](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State)
* [Haskell: Monad transformers](https://en.wikibooks.org/wiki/Haskell/Monad_transformers)
* [Haskell: Arrow tutorial](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)
* [Haskell: Lenses and functional references](https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references)
* [Haskell Wiki: Foldable and Traversable](https://wiki.haskell.org/Foldable_and_Traversable)
* [Haskell Wiki: State monad](https://wiki.haskell.org/State_Monad)
* [Monadic parsing combinators](http://eprints.nottingham.ac.uk/223/1/pearl.pdf)
* [LYAH: For a Few Monads More](http://learnyouahaskell.com/for-a-few-monads-more)
* [Arrows](https://www.haskell.org/arrows/)
* [Lens](http://lens.github.io/tutorial.html)
* [Control.Lens.Tutorial](https://hackage.haskell.org/package/lens-tutorial/docs/Control-Lens-Tutorial.html)
* [SchoolOfHaskell: Lens tutorial](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial)
* [Lenses In Pictures](http://adit.io/posts/2013-07-22-lenses-in-pictures.html)
* [Next Level MTL, George Wilson: BFPG 2016-06 (Lens, Monad transformers)](https://www.youtube.com/watch?v=GZPup5Iuaqw)
