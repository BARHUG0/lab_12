This document provides a consolidated explanation of the concepts required to solve Problem 1.

### 1. Representing the Data: A List of Dictionaries

The problem requires sorting a list of dictionaries. In Haskell, a "dictionary" with mixed value types can be represented as an **association list** (a list of key-value pairs).

**The Data Structure:** `[[(String, StringOrNum)]]`
*   The outer list `[]` represents the list of dictionaries.
*   The inner list `[]` represents a single dictionary.
*   `(String, StringOrNum)` is a key-value pair (a tuple).
*   `String` is the type of our keys.
*   `StringOrNum` is a custom type we'll define to handle values that can be either a `String` or a number.

### 2. Handling Mixed Types: Algebraic Data Types (ADTs)

To handle values that can be either a `String` or an `Int`, we define a custom **Algebraic Data Type (ADT)**.

```haskell
data StringOrNum = StringValue String | IntValue Int
  deriving (Show, Eq, Ord)
```

*   `data StringOrNum = ...`: Declares a new type named `StringOrNum`.
*   `StringValue String | IntValue Int`: Defines the two possible "constructors" for this type.
    *   `StringValue` is a constructor that "wraps" a `String`.
    *   `IntValue` is a constructor that "wraps" an `Int`.
*   `deriving (Show, Eq, Ord)`: Automatically provides default implementations for:
    *   `Show`: Allows values of this type to be converted to a string (for printing).
    *   `Eq`: Allows values to be compared for equality (`==`).
    *   `Ord`: Allows values to be ordered (`<`, `>`, `compare`). This is crucial for sorting.

**Why use constructors like `StringValue`?**
Constructors act as "tags" that tell the compiler what kind of data is inside the `StringOrNum` wrapper. This ensures type safety, making it impossible to accidentally perform math on a `StringValue` or treat an `IntValue` as text.

### 3. Finding a Value: The `lookup` Function and `Maybe`

To find a value for a given key in a dictionary, we use the standard `lookup` function.

`lookup :: Eq a => a -> [(a, b)] -> Maybe b`

`lookup` returns a `Maybe` value, which is an ADT defined as `data Maybe a = Nothing | Just a`.
*   `Just value`: Returned if the key is found. `value` is the corresponding value from the dictionary.
*   `Nothing`: Returned if the key is not found.

This is Haskell's safe way to handle potentially missing values.

### 4. Sorting: The `sortBy` Function

To perform a custom sort, we use `sortBy` from the `Data.List` module.

`sortBy :: (a -> a -> Ordering) -> [a] -> [a]`

*   It takes a **comparison function** and a list to sort.
*   The comparison function must have the type `a -> a -> Ordering`. It takes two elements from the list and returns `LT` (less than), `EQ` (equal), or `GT` (greater than).

**Important:** To use `sortBy`, you must add `import Data.List` to the top of your file.

### 5. The Comparison Function: `let` and `case`

This is the core of the solution. We need a function that compares two dictionaries based on a specific key.

```haskell
-- Type alias for readability
type Dictionary = [(String, StringOrNum)]

compareByKey :: String -> Dictionary -> Dictionary -> Ordering
compareByKey key dict1 dict2 =
  -- 'let' defines local variables. It's our "scratchpad".
  let
    maybeValue1 = lookup key dict1 -- Result is 'Maybe StringOrNum'
    maybeValue2 = lookup key dict2
  in
    -- 'case...of' inspects the structure of our variables and chooses a code path.
    case (maybeValue1, maybeValue2) of
      -- Path 1: Both lookups succeeded.
      (Just val1, Just val2) -> compare val1 val2 -- 'compare' works because of 'deriving (Ord)'

      -- Path 2: First succeeded, second failed. dict1 comes first.
      (Just _, Nothing) -> LT

      -- Path 3: First failed, second succeeded. dict2 comes first.
      (Nothing, Just _) -> GT

      -- Path 4: Both failed. They are equal.
      (Nothing, Nothing) -> EQ
```

### 6. Currying and Partial Application

Our `compareByKey` function takes three arguments. `sortBy` expects a function with only two. This works because of **currying**.

When you provide only some of the arguments to a function, you get back a new function that "remembers" the arguments you provided and is waiting for the rest. This is called **partial application**.

`sortBy (compareByKey "model") myDictionaries`

1.  `(compareByKey "model")` is evaluated first. This is a partial application.
2.  It returns a *new function* of type `Dictionary -> Dictionary -> Ordering`, which is exactly what `sortBy` needs.
3.  `sortBy` then uses this new, specialized function to sort the list.

### 7. The Importance of Parentheses

In Haskell, function application is left-associative. The expression `sortBy compareByKey "model" myDictionaries` would be parsed as `((sortBy compareByKey) "model") myDictionaries`, which would cause a type error.

The parentheses in `(compareByKey "model")` are essential. They ensure that the partial application happens *first*, creating the correct function to be passed as an argument to `sortBy`.
