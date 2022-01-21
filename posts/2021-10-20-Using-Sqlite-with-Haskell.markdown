---
title: Using SQLite with Haskell
author: Nitin Prakash
---

As a Haskell programmer, it's quite natural to think about _type safety_ almost always. Even when it comes to doing simplest of the things. There's a whole bunch of debate on why dead simple database packages like [postgres-simple](https://github.com/haskellari/postgresql-simple) or [sqlite-simple](https://github.com/nurpax/sqlite-simple) should be avoided generally. The inlcination is towards libraries which offer more type safety (not to mention complexity comes along with it) like [Persistent](https://github.com/yesodweb/persistent). It offers a lot of features which guarantees type safety with SQL queries. Under the hood, it makes a ton of template haskell usage to produce a large amount of types and type classes to simplify serialization to and from the database. But the reality is, `persistent` is not as easy to use as `*-simple` libraries mentioned earlier. And it's not always worth it to make things complex for a bit of type safety. If our use case is simple enough (and even in most of the code bases with heavy database usage, it really is) we can probably get away with writing plain SQL queries with enough type safety for things to not break at runtime.

All that being said, it's a nice idea to have type safe queries floating around in your codebase. Or you can do what I do, start with something simple and then iterate to make things better.

The idea of this blog is the present a nice and concise way of writing SQL queries using Haskell without having to fight the type system unnecessarily. And these queries do not deviate a lot from writing standard SQL either.

Let's first create a new experimental table in an existing database schema. We'll define a few data types based on top of this and carry on from there.

```haskell
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}

import Database.SQLite.Simple (Connection (..), execute_)
import Database.SQLite.Simple.QQ (sql)

import qualified Data.Text as T

createPersonTable :: Connection -> IO ()
createPersonTable conn = execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS person
        ( id           INTEGER  PRIMARY KEY AUTOINCREMENT
        , name         TEXT     NOT NULL
        , email        TEXT     UNIQUE NOT NULL
        , phone_number TEXT     UNIQUE NOT NULL
        , role         TEXT     NOT NULL
        -- ^ This could even be an enum type at the DB layer
        )
    |]
```

This essentially captures some basic information of a person. The catch here would be to define `role` in our haskell code base as a sum type. The reasoning behind this is we don't want random user roles to corrupt our database. We need to set a bound on the the values allowed on it. Once we have it, the real trouble is inserting and fetching the data itself. That is, navigation of data to and from the database.

If we were to define data type `Person` which corresponds to the person table, it would look like:

```haskell
import qualified Data.Text as T

data Person = Person
    { pUUID      :: !Integer
    , pName      :: !T.Text
    , pEmail     :: !T.Text
    , pRole      :: !Role
    , pNumber    :: !T.Text
    } deriving stock (Eq, Show)
```

Note that the above is not going to compile just yet because we still need to go ahead and implement `Role` type. So let's do that first.

```haskell
{-# LANGUAGE LambdaCase #-}

-- We are going to limit to only 3 user roles
data Role
    = Hashira
    | Kakushi
    | DemonLord
    deriving (Eq, Show, Read, Enum, Bounded)

roleToText :: Role -> T.Text
roleToText = \case
    Hashira    -> "hashira"
    Kakushi    -> "kakushi"
    DemondLord -> "demon lord
```

<p align="center">
  <img src="/images/6_post/rengoku.jpeg" style="width:350px;height:300px;">
</p>

Cool! What now? Well, we've only setup the required data types that represent a person. We haven't implemented anything that can let `person` communicate with the database itself. We need something (function, instance, miracle etc) in order to convert a **sequence** of fields (that we will fetch from the DB) to our haskell data type. Similary, we'll need something to convert our haskell data type to a **sequnce** of fields that sqlite can understand while inserting data into the `person` table. Fortunately, `sqlite-simple` offers `FromRow` and `ToRow` typeclasses which is build exactly for this purpose!

```haskell
import Control.Applicative
import Database.SQLite.Simple.FromRow (FromRow (..))
import Database.SQLite.Simple.Internal (Field (..))
import Database.SQLite.Simple.ToRow (ToRow (..))

-- Why so many @field@ though?
-- According to the docs,
-- The number of calls to @field@ must match the number of fields returned
-- in a single row of the query result. Otherwise, a @ConversionFailed@
-- exception will be thrown.
instance FromRow Person where
    fromRow = Person <$> field <*> field <*> field <*> field <*> field

-- We ignore pUUID as it is set to auto increment at the DB layer and is already
-- managed by SQLite. Ideally, you should let UUIDs be handled at the DB layer
-- itself unless you have a very strong reason not to
instance ToRow Person where
    toRow (Person _pUUID pName pEmail pRole pNumber) =
        toRow (pName, pEmail, pRole, pNumber)
```

At this point, we should have an understanding of how data conversions are taking place between the code and database. Let's write a function real quick to see if we are able insert and fetch a person or not.

```haskell
import Database.SQLite.Simple (Connection (..), NamedParam (..), executeNamed_, query_)

createNewPerson :: Connection -> IO ()
createNewPerson conn = executeNamed conn [sql|
    INSERT INTO person
        (name, email, phone_number, role)
    VALUES
        (:name, :email, :phoneNumber, :userRole)
    |] [ ":name"        := ("Rengoku" :: T.Text)
       , ":phoneNumber" := ("+91234567899" :: T.Text)
       , ":email"       := ("rengoku@demonslayer.com" :: T.Text)
       , ":userRole"    := Hashira
       ]

getAllPerson :: Connection -> IO [Person]
getAllPerson conn = query_ conn [sql|
    SELECT id
         , name
         , email
         , role
         , phone_number
      FROM person
|]
```

<p align="center">
  <img src="/images/6_post/wtf-confused.gif" style="width:350px;height:300px;">
</p>

`getAllPerson` is very straight forward. But what in the name of sweet type classes is going on inside `createNewPerson` !?

Let's go step by step. There are 3 main components to the function:

- `Connection` type in the type signature
- Named Parameters ( Weird looking equalities)
- Structure of the query itself

And this is going to be the case for most of the queries you write which needs some sort of parameter substitution. At least, I'd recommend writing queries using named parameters in order to keep things clean and much more readable. We could've written `createNewPerson` without named parameters like so:

```haskell
import Database.SQLite.Simple (execute)

createNewPerson :: Connection -> IO ()
createNewPerson conn = execute conn [sql|
    INSERT INTO person
        (name, email, phone_number, role)
    VALUES
        (?, ?, ?, ?)
    |] ( "Rengoku" :: T.Text
       , "+91234567899" :: T.Text
       , "rengoku@demonslayer.com" :: T.Text
       , Hashira
       )
```

But the readability reduces as soon as the query grows and there are bunch of substitution happening. Also, there is a slight chance of messing up the order of the arguments (eg: type safety won't save you if two consecute `Text` type substitutions are happening) leading to a corrupt DB state. Using named parameters remove these shortcomings.

Alright enough talk, let's actually see if this works or not. We'll write a `main` method and fire up the repl to test and spam the code.

```haskell
import Database.SQLite.Simple (close, open)

flushPersonTable :: Connection -> IO ()
flushPersonTable conn =
    execute_ conn [sql|
        DROP TABLE IF EXISTS person;
    |]

-- Drops the person table and creates a fresh new one
-- This is done in order to play with fresh data every time we
-- run @main@ in the repl. Not that it's a rule.
resetDb :: Connection -> IO ()
resetDb conn = do
    flushPersonTable conn
    createPersonTable conn

main :: IO ()
main = do
  -- Open up a connection to the database and this will be passed to each query we run.
  -- In practice, we should maintain a pool of connections so that we can run SQl
  -- queries in parallel. Also, this practice enables you to use queries inside a
  -- DB transaction.
  conn <- open "test.db"
  resetDb conn
  createNewPerson conn
  rows <- getAllPerson conn
  mapM_ print rows
  close conn
```

Aw snap! We've run into errors. Out of all the things GHC is complaining, we focus ourselves on the following two first

```haskell
 src/SqliteSimple.hs:54:56: error:
    • No instance for (FromField Role) arising from a use of ‘field’
    • In the second argument of ‘(<*>)’, namely ‘field’
      In the first argument of ‘(<*>)’, namely
        ‘Person <$> field <*> field <*> field <*> field’
      In the expression:
        Person <$> field <*> field <*> field <*> field <*> field
   |
54 |     fromRow = Person <$> field <*> field <*> field <*> field <*> field
   |                                                        ^^^^^
```

```haskell
src/SqliteSimple.hs:58:56: error:
    • No instance for (ToField Role) arising from a use of ‘toRow’
    • In the expression: toRow (pName, pEmail, pRole, pNumber)
      In an equation for ‘toRow’:
          toRow (Person _pUUID pName pEmail pRole pNumber)
            = toRow (pName, pEmail, pRole, pNumber)
      In the instance declaration for ‘ToRow Person’
   |
58 |     toRow (Person _pUUID pName pEmail pRole pNumber) = toRow (pName, pEmail, pRole, pNumber)
   |                                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

Previously, we defined a `FromRow` and `ToRow` instance when we wanted to convert a **collection** of Haskell types to and from a **collection** of SQL types. But what happens when we introduce a new data type which is not understood by GHC natively. In our case, when we defined `Role`, we were good as GHC knew how to interpret sum types but it doesn't know the intentions behind it's usage. In other words, how we want `Role` to intereact with other data types (internally and from other packages) is left entirely to us. We _tell_ GHC our intentions by impementing different instances for it. Therefore, we'll have to define `ToField` and `FromField` instances so that the compiler knows how to convert `Role` to an SQL data type. Also, these implementations are used by `ToRow` and `FromRow`. What we had earlier was just a step in the right direction but it was incomplete.

```haskell
import Database.SQLite.Simple.FromField (Field (..), FromField (..), returnError)
import Database.SQLite.Simple.Ok (Ok (..))
import Database.SQLite.Simple.ToField (ToField (..))

-- This prepares a value for substitution into a query string.
instance ToField Role where
    toField = SQLText . roleToText

instance FromField Role where
    fromField (Field (SQLText "hashira") _) = Ok Hahsira
    fromField (Field (SQLText "kakushi") _) = Ok Kakushi
    fromField (Field (SQLText "demon lord") _) = Ok DemonLord
    fromField f = returnError ConversionFailed f "role does not exist"
```

One of the question from above can be regarding the definition of `ToField` and `FromField` instances of `Role`. How did we come up with that? Well, the secret ingredient here is reading the docs. According to the [docs](https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/src/Database.SQLite.Simple.ToField.html#local-6989586621679115476), The way `ToField` is defined for `Text` types are:

```haskell
instance ToField T.Text where
    toField = SQLText
    {-# INLINE toField #-}

instance FromField T.Text where
    fromField (Field (SQLText txt) _) = Ok txt
    fromField f = returnError ConversionFailed f "need a text"
```

It is quite clear from the above on how we can define our own version of `instance ToField Role` given that we want a `text` type in the database. If we try reloading our REPL now and run `main`, we can see that we have a successful transaction between Haskell layer and DB layer:

```haskell
*SqliteSimple> main
Person {pUUID = 1, pName = "Rengoku", pEmail = "rengoku@demonslayer.com", pRole = Hashira, pNumber = "+91234567899"}
*SqliteSimple>
```

This was a high level tutorial on how to use `sqlite-simple` (or any database like `postgres-simple` and `mysql-simple`) for that matter. Note that `postgres-simple` does not ship named parameters natively. [postgres-simple-named](https://github.com/Holmusk/postgresql-simple-named) would be a suitable chouce for that as it is built on top of `postgres-simple`. Anyways, we've only seen how to play with very basic data types. Let's get a bit more creative. We can play with --

- timestamps
- Joins
- JSON structures

to see the usefulness and ease of writing SQL queries with haskell.

Let's introduce a `created_at` value for a person. Let's make the following changes,

```haskell
import Data.Time (UTCTime (..))

createPersonTable :: Connection -> IO ()
createPersonTable conn = execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS person
        ( id           INTEGER  PRIMARY KEY AUTOINCREMENT
        , name         TEXT     NOT NULL
        , email        TEXT     UNIQUE NOT NULL
        , phone_number TEXT     UNIQUE NOT NULL
        , role         TEXT     NOT NULL -- This could even be an enum type at the DB layer
        -- Changed --
        , created_at   IMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
        ------------
        )
    |]

data Person = Person
    { pUUID      :: !Integer
    , pName      :: !T.Text
    , pEmail     :: !T.Text
    , pRole      :: !Role
    , pNumber    :: !T.Text
    -- Changed --
    , pCreatedAt :: !UTCTime
    -------------
    } deriving stock (Eq, Show)

-- Now we need to handle the added field in our instances
-- Compiler will scream at you that certain fields are missing
instance FromRow Person where
    fromRow = Person <$> field <*> field <*> field <*> field <*> field <*> field

-- when inserting a new person, ignore UUID and created_at (Handled by SQLite).
instance ToRow Person where
    toRow (Person _pUUID pName pEmail pRole pNumber _pJoinedAt) =
        toRow (pName, pEmail, pRole, pNumber)
```

Now if we reload the REPL and run `main`, we get:

```haskell
*SqliteSimple> :r
[1 of 1] Compiling SqliteSimple     ( src/SqliteSimple.hs, interpreted )
Ok, one module loaded.
*SqliteSimple> main
Person {pUUID = 1, pName = "Rengoku", pEmail = "rengoku@demonslayer.com", pRole = Hashira, pNumber = "+91234567899", pJoinedAt = 2021-10-25 07:21:54 UTC}
*SqliteSimple>
```

Notice the `pJoinedAt` at the end and the fact that we didn't have to write a custom `FromField` instance for a timestamp value. That's because `sqlite-simple` ships `instance FromField UTCTime` which gets imported while importing `FromField (..)`. This reasoning holds true even for other `Text` and `Int` types.

Now let's create another entity so we can experiment with SQL joins.

```haskell
-- A data type that represent the favourite anime and character of a user
data FavouriteAnime = FavouriteAnime
    { faUUID               :: !Integer
    , faAnimeName          :: !T.Text
    , faFavouriteCharacter :: !T.Text
    , faUserName           :: !T.Text
    } deriving (Eq, Show)

instance FromRow FavouriteAnime where
    fromRow = FavouriteAnime <$> field <*> field <*> field <*> field

createAnimeTable :: Connection -> IO ()
createAnimeTable conn = execute_ conn [sql|
    CREATE TABLE IF NOT EXISTS favourite_anime
        ( id                  INTEGER PRIMARY KEY AUTOINCREMENT
        , anime_name          TEXT    NOT NULL
        , favourite_character TEXT    UNIQUE
        , person_id           INTEGER NOT NULL

        -- We reference the person_id but we need to fetch the
        -- person name as per our Haskell data type
        , FOREIGN KEY(person_id) REFERENCES person(id)
        )
    |]

insertFavouriteAnime :: Connection -> IO ()
insertFavouriteAnime conn = executeNamed conn [sql|
    INSERT INTO favourite_anime
        (anime_name, favourite_character, person_id)
    VALUES
        (:animeName, :favouriteCharacter, :personId)
    |] [ ":animeName"          := ("Attack on titans" :: T.Text)
       , ":favouriteCharacter" := ("Levi Ackerman" :: T.Text)
       , ":personId"           := (1 :: Int)
       ]

-- This example demonstrates just one LEFT JOIN for simplicity purposes.
-- But, if you can write a plain SQL query in SQLite, you can write it here.
-- So it can be as complicated as you want it to be.
-- RECOMMENDED: Keep things simple.
getAllFavAnime :: Connection -> IO [FavouriteAnime]
getAllFavAnime conn = query_ conn [sql|
    SELECT fa.id
         , fa.anime_name
         , fa.favourite_character
         , p.name
    FROM favourite_anime fa
    LEFT JOIN person p ON p.id = fa.person_id
    |]

flushAnimeTable :: Connection -> IO ()
flushAnimeTable conn =
    execute_ conn [sql|
        DROP TABLE IF EXISTS favourite_anime;
    |]

-- And the corresponding changes to @resetDb@ and @main@
resetDb :: Connection -> IO ()
resetDb conn = do
    -- drop all the tables
    flushPersonTable conn
    flushAnimeTable conn

    -- create fresh ones
    createPersonTable conn
    createAnimeTable conn

main :: IO ()
main = do
    conn <- open "test.db"
    resetDb conn

    createNewPerson conn
    insertFavouriteAnime conn

    pRows <- getAllPerson conn
    faRows <- getAllFavAnime conn
    mapM_ print pRows
    mapM_ print faRows
    close conn
```

<p align="center">
  <img src="/images/6_post/anime-nerd.png" style="width:350px;height:300px;">
</p>

Let's reload the REPL and run the `main` function,

```haskell
*SqliteSimple> :r
[1 of 1] Compiling SqliteSimple     ( src/SqliteSimple.hs, interpreted )
Ok, one module loaded.
*SqliteSimple> main
Person {pUUID = 1, pName = "Rengoku", pEmail = "rengoku@demonslayer.com", pRole = Hashira, pNumber = "+91234567899", pJoinedAt = 2021-10-25 10:46:47 UTC}
FavouriteAnime {faUUID = 1, faAnimeName = "Attack on titans", faFavouriteCharacter = "Levi Ackerman", faUserName = "Rengoku"}
```

Just as expected!! These are just some basic stuff I wanted to write about. These can surely be extended to do a lot of complicated stuff as well
if it fits the relational data model. So feel free to experiment as much as you want.

One concern now is that these queries are unsafe in nature. These will result into runtime errors and we want to avoid that. One of the ways of doing that is to wrap
the queries provided by `sqlite-simple` into some `Maybe` or `Either` types to catch these kind of errors when running tests.

p.s. Always write tests.
