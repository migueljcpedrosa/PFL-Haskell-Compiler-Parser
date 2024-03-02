{-
PFL - Practical Assignment 2

Delivery Date: 2024/01/02

Authors:
    - Miguel Pedrosa (up202108809), collaboration (50%)
    - António Rama (up202108801), collaboration (50%)
-}

import Debug.Trace
import Data.Ord (comparing)
import Data.List
import Text.Read (readMaybe)
import Control.DeepSeq (deepseq)
import Data.Char
import Data.Maybe (fromJust)

-- Part 1

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

--------
-- a) --
--------

-- Define a StackItem type that can be either an IntItem holding an Integer or a BoolItem holding a Bool
data StackItem = IntItem Integer | BoolItem Bool

-- Define the Stack (a list of StackItems)
type Stack = [StackItem]

--------
-- b) --
--------

-- Define the Storage type
type State = [(String, StackItem)]

--------
-- c) --
--------

{-
createEmptyStack:
Type: Stack

Purpose: createEmptyStack creates an empty stack.
Input: None.
Output: An empty stack.
-}
createEmptyStack :: Stack
createEmptyStack = []

--------
-- e) --
--------

{-
item2Str:
Type: StackItem -> String

Purpose: Converts a single StackItem to a String.
Input: A StackItem.
Output: A String representing the StackItem.
-}
item2Str :: StackItem -> String
item2Str (IntItem i) = show i
item2Str (BoolItem b) = show b

stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (x:xs) = item2Str x ++ concatMap ((',' :) . item2Str) xs

--------
-- d) --
--------

{-
createEmptyState:
Type: State

Purpose: createEmptyState creates an empty storage (State).
Input: None.
Output: An empty storage (State).
-}
createEmptyState :: State
createEmptyState = []

--------
-- f) --
--------

{-
storageTuple2Str:
Type: (String, StackItem) -> String

Purpose: Converts a storage tuple to a String.
Input: A tuple containing a String and a StackItem.
Output: A String representing the storage tuple.
-}
storageTuple2Str :: (String, StackItem) -> String
storageTuple2Str (a, b) = a ++ "=" ++ item2Str b 


{-
state2Str:
Type: State -> String

Purpose: Converts a storage (State) to a String.
Input: A storage (State).
Output: A String representing the storage.
-}
state2Str :: State -> String
state2Str [] = ""
state2Str (x:xs) =
    let sortedStorage = sortBy (comparing fst) (x:xs)
    in case sortedStorage of
        [] -> ""
        (x:xs) -> storageTuple2Str x ++ concatMap ((',' :) . storageTuple2Str) xs


{-
run:
Type: (Code, Stack, State) -> (Code, Stack, State)

Purpose: run takes a tuple containing a list of instructions (Code), a stack (Stack), and a storage (State) and returns a tuple containing the updated list of instructions, stack, and storage.
Input: A tuple containing a list of instructions (Code), a stack (Stack), and a storage (State).
Output: A tuple containing the updated list of instructions, stack, and storage.
-}
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state) -- Base case, no Code left
run ((inst:rest), stack, state) = case inst of

  Push n -> 
    run (rest, IntItem n : stack, state)

  Add -> 
    case stack of 
      (IntItem n1 : IntItem n2 : stackRest) -> run (rest, IntItem (n1 + n2) : stackRest, state) 
      _ -> error "Run-time error"

  Sub -> 
    case stack of 
      (IntItem n1 : IntItem n2 : stackRest) -> run (rest, IntItem (n1 - n2) : stackRest, state) 
      _ -> error "Run-time error"

  Mult -> 
    case stack of 
      (IntItem n1 : IntItem n2 : stackRest) ->  run (rest, IntItem (n1 * n2) : stackRest, state) 
      _ -> error "Run-time error"

  Fals -> 
    run (rest, BoolItem False : stack, state)

  Tru -> 
    run (rest, BoolItem True : stack, state)

  Store var -> 
    case stack of
      (val : stackRest) -> 
        let updatedState = case lookup var state of
              Just _  -> map (\(k, v) -> if k == var then (k, val) else (k, v)) state
              Nothing -> (var, val) : state
        in run (rest, stackRest, updatedState)
      _ -> error "Run-time error"

  Fetch var -> 
    case lookup var state of
      Just val -> run (rest, val : stack, state)
      Nothing -> error "Run-time error"

  Neg -> 
    case stack of
      (BoolItem b : stackRest) -> run (rest, BoolItem (not b) : stackRest, state)
      _ -> error "Run-time error"

  Equ -> 
    case stack of
      (IntItem a : IntItem b : stackRest) -> run (rest, BoolItem (a == b) : stackRest, state)
      (BoolItem a : BoolItem b : stackRest) -> run (rest, BoolItem (a == b) : stackRest, state)
      _ -> error "Run-time error"

  Le -> 
    case stack of
      (IntItem a : IntItem b : stackRest) -> run (rest, BoolItem (a <= b) : stackRest, state)
      _ -> error "Run-time error"

  Noop -> 
    run (rest, stack, state)

  Branch code1 code2 -> 
    case stack of
      (BoolItem True : stackRest) -> run (code1 ++ rest, stackRest, state)
      (BoolItem False : stackRest) -> run (code2 ++ rest, stackRest, state)
      _ -> error "Run-time error"

  Loop code1 code2 -> 
    run (code1 ++ [Branch (code2 ++ [Loop code1 code2]) [Noop]] ++ rest, stack, state)

  And -> 
    case stack of
      (BoolItem b1 : BoolItem b2 : stackRest) -> run (rest, BoolItem (b1 && b2) : stackRest, state)
      _ -> error "Run-time error"

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples (working):
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")

-- Examples (developed by us):
-- testAssembler [Tru, Fals, And] == ("False", "")
-- testAssembler [Tru, Tru, And] == ("True", "")

-- Part 2

--------
-- a) --
--------

data Aexp = Var String             -- Variable
          | Num Integer            -- Constant
          | AAdd Aexp Aexp         -- Addition
          | ASub Aexp Aexp         -- Subtraction
          | AMult Aexp Aexp        -- Multiplication
          deriving (Show, Eq)

data Bexp = AEq Aexp Aexp    
          | BTrue                  -- True
          | BFals                  -- False
          | BLe Aexp Aexp          -- Less than or equal to  
          | BAnd Bexp Bexp         -- Logical AND
          | BEqBool Bexp Bexp      -- Boolean equality
          | BNot Bexp              -- Logical NOT
          deriving (Show, Eq)

data Stm = AssignStm String Aexp   -- Assignment
         | IfStm Bexp [Stm] [Stm]  -- 'If' condition, 'then' branch, 'else' branch
         | WhileStm Bexp [Stm]     -- 'While' condition and loop body (list of statements)
         deriving (Show, Eq)

type Program = [Stm]


--------
-- b) --
--------


{-
compA:
Type: Aexp -> Code

Purpose: compA takes an arithmetic expression (Aexp) and returns a list of instructions (Code).
The order of operations ensures the operands are correctly placed on the stack before the operation is performed.
Input: An arithmetic expression (Aexp).
Output: A list of instructions (Code).
-}
compA :: Aexp -> Code

compA (Var x) = [Fetch x]
compA (Num n) = [Push n]
compA (AMult a1 a2) = compA a2 ++ compA a1 ++ [Mult]
compA (AAdd a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (ASub a1 a2) = compA a2 ++ compA a1 ++ [Sub]


{-
compB:
Type: Bexp -> Code

Purpose: compB takes a boolean expression (Bexp) and returns a list of instructions (Code).
The order of operations ensures the operands are correctly placed on the stack before the operation is performed.
Input: A boolean expression (Bexp).
Output: A list of instructions (Code).
-}
compB :: Bexp -> Code

compB (AEq b1 b2) = compA b2 ++ compA b1 ++ [Equ]
compB (BLe b1 b2) = compA b2 ++ compA b1 ++ [Le]
compB (BNot b1) = compB b1 ++ [Neg]
compB (BAnd b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (BEqBool b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB BTrue = [Tru]
compB BFals = [Fals]


{-
compile:
Type: Program -> Code

Purpose: compile takes a program (list of statements) and returns a list of instructions (Code).
Input: A program (list of statements).
Output: A list of instructions (Code).
-}
compile :: Program -> Code
compile program = foldr (\stm acc -> compileSingle stm ++ acc) [] program
  where
    compileSingle (AssignStm var aexp) = compA aexp ++ [Store var]
    compileSingle (IfStm bexp stm1 stm2) = compB bexp ++ [Branch (compile stm1) (compile stm2)]
    compileSingle (WhileStm bexp stm) = [Loop (compB bexp) (compile stm)]


--------
-- c) --
--------

{-
lexer
Input: A String (inputString), which is the program text to be tokenized.
Output: A list of String tokens representing the lexical elements of the input program.

Purpose:
The function processes the input string using pattern matching and recursion to build the list of tokens.

Tokenization Process:
Whitespace Handling: If the current character is a space, it's ignored, and lexer continues with the rest of the characters.
Number Handling: If the current character is a digit, lexer captures the entire number (sequence of digits) as a token and proceeds with the rest.
Word Handling: If the current character is a letter, lexer captures the entire word. It then:
Converts the word to lowercase for case-insensitive keyword matching.
Checks if the word is a language keyword like "if", "then", "else", etc., and treats it as a separate token.
If it's not a keyword, the word is treated as an identifier (like a variable name).
Special Characters and Operators: The lexer recognizes specific single and double characters as tokens, such as:
Semicolons (;), parentheses, and operators like +, -, *, :=, ==, <=, and =.

Error Handling:
The function includes a catch-all case at the end. If it encounters an unrecognized character, it generates an error message indicating the unrecognized character.
-}
type Lexer = String -> [String]

lexer :: String -> [String]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isDigit c = let (number, rest) = span isDigit (c:cs) in number : lexer rest
    | isAlpha c = 
        let (word, rest) = span isAlphaNum (c:cs)
            lowerWord = map toLower word
            in case lowerWord of
                 -- Match and separate keywords
                 "if" -> "if" : lexer rest
                 "then" -> "then" : lexer rest
                 "else" -> "else" : lexer rest
                 "not" -> "not" : lexer rest
                 "true" -> "True" : lexer rest
                 "false" -> "False" : lexer rest
                 "while" -> "while" : lexer rest
                 "do" -> "do" : lexer rest
                 "and" -> "and" : lexer rest
                 -- Handle non-keyword identifiers
                 _ -> word : lexer rest
    | c == ';' = ";" : lexer cs
    | c == ':' && not (null cs) && head cs == '=' = ":=" : lexer (tail cs)  -- Recognize ':=' as an assignment operator
    | c == '+' = "+" : lexer cs
    | c == '-' = "-" : lexer cs
    | c == '*' = "*" : lexer cs
    | c == '=' && (not (null cs) && head cs == '=') = "==" : lexer (tail cs) -- Recognize '==' as a comparison operator
    | c == '=' = "=" : lexer cs
    | c == '(' = "(" : lexer cs
    | c == ')' = ")" : lexer cs
    | c == '{' = "{" : lexer cs
    | c == '}' = "}" : lexer cs
    | c == '<' && (not (null cs) && head cs == '=') = "<=" : lexer (tail cs) -- Recognize '<=' as a comparison operator
    | c == '<' = "<" : lexer cs
    | otherwise = error $ "lexer: Unrecognized character '" ++ [c] ++ "'"



--Aexp Parser

{-
parseInt:
Type: [String] -> Maybe (Aexp, [String])

Purpose: Parses an integer token from the input list.
Input: A list of string tokens.
Output: A Maybe tuple containing an Aexp (arithmetic expression) representing the parsed integer (or variable if not an integer) and the remaining tokens.
-}
parseInt :: [String] -> Maybe (Aexp, [String])
parseInt [] = Nothing
parseInt (token:remainingTokens) = 
    Just $ maybe (Var token, remainingTokens) (\numValue -> (Num numValue, remainingTokens)) (readMaybe token)


{-
parseProdOrInt:
Type: [String] -> Maybe (Aexp, [String])

Purpose: Parses expressions involving multiplication or just an integer.
Input: A list of string tokens.
Output: A Maybe tuple containing an Aexp representing the product or integer and the remaining tokens.
-}
parseProdOrInt :: [String] -> Maybe (Aexp, [String])
parseProdOrInt tokens = do
  (leftExpr, remainingTokens) <- parseInt tokens
  parseMultExpr leftExpr remainingTokens


{-
parseMultExpr:
Type: Aexp -> [String] -> Maybe (Aexp, [String])

Purpose: Helper function to parse multiplication expressions.
Input: An Aexp representing the left expression and a list of remaining tokens.
Output: A Maybe tuple with an Aexp representing the multiplication and the remaining tokens.
-}
parseMultExpr :: Aexp -> [String] -> Maybe (Aexp, [String])
parseMultExpr leftExpr tokens = case tokens of
    "*":rest -> do
      (rightExpr, remainingTokens) <- parseProdOrInt rest
      Just (AMult leftExpr rightExpr, remainingTokens)
    _ -> Just (leftExpr, tokens)


{-
parseSumSubOrProdOrInt:
Type: [String] -> Maybe (Aexp, [String])

Purpose: Parses expressions involving addition, subtraction, or multiplication.
Input: A list of string tokens.
Output: A Maybe tuple with an Aexp and the remaining tokens.
-}
parseSumSubOrProdOrInt :: [String] -> Maybe (Aexp, [String])
parseSumSubOrProdOrInt tokens = do
  (leftExpr, remainingTokens) <- parseProdOrInt tokens
  parseAddSubOrReturn leftExpr remainingTokens


{-
parseIntOrParentExpr:
Type: [String] -> Maybe (Aexp, [String])

Purpose: Parses an integer or a parenthesized expression.
Input: A list of string tokens.
Output: A Maybe tuple with an Aexp for the parsed integer/parenthesized expression and the remaining tokens.
-}
parseIntOrParentExpr :: [String] -> Maybe (Aexp, [String])
parseIntOrParentExpr ("(":tokens) = parseParenExpr tokens
parseIntOrParentExpr (token:tokens) = parseNumOrVar token tokens
parseIntOrParentExpr [] = Nothing


{-
parseParenExpr:
Type: [String] -> Maybe (Aexp, [String])

Purpose: Parses a parenthesized expression.
Input: A list of string tokens.
Output: A Maybe tuple with an Aexp for the parenthesized expression and the remaining tokens.
-}
parseParenExpr :: [String] -> Maybe (Aexp, [String])
parseParenExpr tokens =
  case parseSumSubOrProdOrIntOrParen tokens of
    Just (expr, ")":remainingTokens) -> Just (expr, remainingTokens)
    _ -> Nothing


{-
parseNumOrVar:
Type: String -> [String] -> Maybe (Aexp, [String])

Purpose: Parses a numeric literal or a variable.
Input: A string token and a list of remaining tokens.
Output: A Maybe tuple with an Aexp for the number/variable and the remaining tokens.
-}
parseNumOrVar :: String -> [String] -> Maybe (Aexp, [String])
parseNumOrVar token tokens =
  case readMaybe token :: Maybe Integer of
    Just num -> Just (Num num, tokens)
    Nothing -> Just (Var token, tokens)


{-
parseProdOrIntOrParen:
Type: [String] -> Maybe (Aexp, [String])

Purpose: Parses products, integers, or parenthesized expressions.
Input: A list of string tokens.
Output: A Maybe tuple with an Aexp and the remaining tokens.
-}
parseProdOrIntOrParen :: [String] -> Maybe (Aexp, [String])
parseProdOrIntOrParen tokens = do
  (leftExpr, remainingAfterFirstParse) <- parseIntOrParentExpr tokens
  case remainingAfterFirstParse of
    "*":remainingAfterMult -> do
      (rightExpr, remainingAfterProd) <- parseProdOrIntOrParen remainingAfterMult
      return (AMult leftExpr rightExpr, remainingAfterProd)
    _ -> Just (leftExpr, remainingAfterFirstParse)


{-
parseSumSubOrProdOrIntOrParen:
Type: [String] -> Maybe (Aexp, [String])

Purpose: Defines a parser for expressions involving addition, subtraction, multiplication, or parentheses.
Input: A list of string tokens.
Output: A Maybe tuple with an Aexp and the remaining tokens.
-}
parseSumSubOrProdOrIntOrParen :: [String] -> Maybe (Aexp, [String])
parseSumSubOrProdOrIntOrParen tokens = do
  (leftExpr, remainingTokens) <- parseProdOrIntOrParen tokens
  parseAddSubOrReturn leftExpr remainingTokens


{-
parseAddSubOrReturn:
Type: Aexp -> [String] -> Maybe (Aexp, [String])

Purpose: Handles addition or subtraction, or returns the expression if neither is found.
Input: An Aexp representing the left expression and a list of tokens.
Output: A Maybe tuple with an Aexp and the remaining tokens.
-}
parseAddSubOrReturn :: Aexp -> [String] -> Maybe (Aexp, [String])
parseAddSubOrReturn leftExpr tokens =
  case tokens of
    "+":rest -> parseSumSubExpr AAdd leftExpr rest
    "-":rest -> parseSumSubExpr ASub leftExpr rest
    _ -> Just (leftExpr, tokens)


{-
parseSumSubExpr:
Type: (Aexp -> Aexp -> Aexp) -> Aexp -> [String] -> Maybe (Aexp, [String])

Purpose: Parses an operation (addition or subtraction) and continues parsing the rest of the expression.
Input: A function representing the operation, an Aexp for the left expression, and a list of tokens.
Output: A Maybe tuple with an Aexp and the remaining tokens.
-}
parseSumSubExpr :: (Aexp -> Aexp -> Aexp) -> Aexp -> [String] -> Maybe (Aexp, [String])
parseSumSubExpr op leftExpr tokens = do
  (rightExpr, remainingTokens) <- parseSumSubOrProdOrIntOrParen tokens
  return (op leftExpr rightExpr, remainingTokens)




--BExp Parser

{-
parseParenthesizedExpr:
Type: [String] -> Maybe (Bexp, [String])

Purpose: Parses parenthesized boolean expressions.
Input: A list of string tokens.
Output: A Maybe tuple containing a Bexp for the parsed expression within parentheses and the remaining tokens.
-}
parseParenthesizedExpr :: [String] -> Maybe (Bexp, [String])
parseParenthesizedExpr tokens =
  case parseAndOrBoolEqOrNotOrBasicBoolExpr tokens of
    Just (expr, ")":remainingTokens) -> Just (expr, remainingTokens)
    _ -> Nothing


{-
parseComparisonExpr:
Type: [String] -> Maybe (Bexp, [String])

Purpose: Parses comparison expressions involving == for equality and <= for less than or equal.
Input: A list of string tokens.
Output: A Maybe tuple with a Bexp representing the comparison and the remaining tokens.
-}
parseComparisonExpr :: [String] -> Maybe (Bexp, [String])
parseComparisonExpr tokens = do
  (leftExpr, remainingTokens) <- parseSumSubOrProdOrIntOrParen tokens
  case remainingTokens of
    "==":rest -> parseComparison AEq leftExpr rest
    "<=":rest -> parseComparison BLe leftExpr rest
    _ -> Nothing


{-
parseComparison:
Type: (Aexp -> Aexp -> Bexp) -> Aexp -> [String] -> Maybe (Bexp, [String])

Purpose: Handles the logic for parsing a comparison operation.
Input: A function representing the comparison operation, an Aexp for the left expression, and a list of tokens.
Output: A Maybe tuple with a Bexp for the comparison operation and the remaining tokens.
-}
parseComparison :: (Aexp -> Aexp -> Bexp) -> Aexp -> [String] -> Maybe (Bexp, [String])
parseComparison compOp leftExpr tokens = do
  (rightExpr, tokensAfterRightExpr) <- parseSumSubOrProdOrIntOrParen tokens
  return (compOp leftExpr rightExpr, tokensAfterRightExpr)


{-
parseBasicBoolExpr:
Type: [String] -> Maybe (Bexp, [String])

Purpose: Defines a basic boolean expression parser for literals ("True", "False"), parenthesized expressions, and comparisons.
Input: A list of string tokens.
Output: A Maybe tuple with a Bexp for the basic boolean expression and the remaining tokens.
-}
parseBasicBoolExpr :: [String] -> Maybe (Bexp, [String])
parseBasicBoolExpr ("True":remainingTokens)  = Just (BTrue, remainingTokens)
parseBasicBoolExpr ("False":remainingTokens) = Just (BFals, remainingTokens)
parseBasicBoolExpr ("(":tokens)              = parseParenthesizedExpr tokens
parseBasicBoolExpr tokens                    = parseComparisonExpr tokens


{-
parseNotOrBasicBoolExpr:
Type: [String] -> Maybe (Bexp, [String])

Purpose: Parses expressions involving the "not" operator or falls back to parsing basic boolean expressions.
Input: A list of string tokens.
Output: A Maybe tuple with a Bexp and the remaining tokens.
-}
parseNotOrBasicBoolExpr :: [String] -> Maybe (Bexp, [String])
parseNotOrBasicBoolExpr tokens = case tokens of
    "not":rest -> parseNotExpr rest
    _ -> parseBasicBoolExpr tokens


{-
parseNotExpr:
Type: [String] -> Maybe (Bexp, [String])

Purpose: Helper function to parse the "not" expression.
Input: A list of string tokens.
Output: A Maybe tuple with a Bexp for the negated expression and the remaining tokens.
-}
parseNotExpr :: [String] -> Maybe (Bexp, [String])
parseNotExpr tokens = do
  (parsedExpr, remainingTokens) <- parseBasicBoolExpr tokens
  Just (BNot parsedExpr, remainingTokens)


{-
parseBoolEqOrNotOrBasicBoolExpr:
Type: [String] -> Maybe (Bexp, [String])

Purpose: Parses expressions involving "equal", "not", or more basic boolean expressions.
Input: A list of string tokens.
Output: A Maybe tuple with a Bexp and the remaining tokens.
-}
parseBoolEqOrNotOrBasicBoolExpr :: [String] -> Maybe (Bexp, [String])
parseBoolEqOrNotOrBasicBoolExpr tokens = do
  (parsedLeftExpr, remainingTokens) <- parseNotOrBasicBoolExpr tokens
  parseEqExpr parsedLeftExpr remainingTokens


{-
parseEqExpr:
Type: Bexp -> [String] -> Maybe (Bexp, [String])

Purpose: Helper function to parse the "equal" boolean expression.
Input: A Bexp for the left expression and a list of tokens.
Output: A Maybe tuple with a Bexp for the equality expression and the remaining tokens.
-}
parseEqExpr :: Bexp -> [String] -> Maybe (Bexp, [String])
parseEqExpr leftExpr tokens = case tokens of
    "=":rest -> do
      (parsedRightExpr, remainingTokens) <- parseBoolEqOrNotOrBasicBoolExpr rest
      Just (BEqBool leftExpr parsedRightExpr, remainingTokens)
    _ -> Just (leftExpr, tokens)


{-
parseAndOrBoolEqOrNotOrBasicBoolExpr:
Type: [String] -> Maybe (Bexp, [String])

Purpose: Extends the parser to handle expressions involving the "and" operator, building upon the previous parsers.
Input: A list of string tokens.
Output: A Maybe tuple with a Bexp for the combined expression and the remaining tokens.
-}
parseAndOrBoolEqOrNotOrBasicBoolExpr :: [String] -> Maybe (Bexp, [String])
parseAndOrBoolEqOrNotOrBasicBoolExpr tokens = do
  (parsedLeftExpr, remainingTokens) <- parseBoolEqOrNotOrBasicBoolExpr tokens
  parseAndExpr parsedLeftExpr remainingTokens


{-
parseAndExpr:
Type: Bexp -> [String] -> Maybe (Bexp, [String])

Purpose: Helper function to parse the "and" expression.
Input: A Bexp for the left expression and a list of tokens.
Output: A Maybe tuple with a Bexp for the conjunction and the remaining tokens.
-}
parseAndExpr :: Bexp -> [String] -> Maybe (Bexp, [String])
parseAndExpr leftExpr tokens =
  case tokens of
    "and":rest -> do
      (parsedRightExpr, remainingTokens) <- parseAndOrBoolEqOrNotOrBasicBoolExpr rest
      Just (BAnd leftExpr parsedRightExpr, remainingTokens)
    _ -> Just (leftExpr, tokens)




--Statements Parser

{-
parseParenthesizedStm:
Type: [String] -> [Stm] -> ([Stm], [String])

Purpose: Parses statements within parentheses, managing the extraction and parsing of inner tokens.
Input: A list of string tokens and the current list of accumulated statements.
Output: A tuple with updated accumulated statements and remaining tokens.
-}
parseParenthesizedStm :: [String] -> [Stm] -> ([Stm], [String])
parseParenthesizedStm ("(":tokens) currentStm = 
    case findClosingParen tokens 0 of
        Just (innerTokens, remainingTokens) -> 
            let parsedInnerStms = parseStm innerTokens []
            in (currentStm ++ parsedInnerStms, tail remainingTokens)  -- tail to skip the closing ")"
        Nothing -> 
            error "ParseStm Error: Expected closing ')' but none was found."
parseParenthesizedStm _ _ = 
    error "ParseStm Error: Expected opening '('."


{-
findClosingParen:
Type: [String] -> Int -> Maybe ([String], [String])

Purpose: Helper function to find the closing parenthesis for a parenthesized expression.
Input: A list of string tokens and the current depth of nested parentheses.
Output: A tuple with the inner tokens and remaining tokens after the closing parenthesis.
-}
findClosingParen :: [String] -> Int -> Maybe ([String], [String])
findClosingParen tokens depth = 
    case tokens of
        [] -> Nothing
        (")":_) | depth == 1 -> Just ([], tokens)
        (")":xs) -> Just (splitAt (length tokens - length xs - 1) tokens)
        ("(":xs) -> findClosingParen xs (depth + 1)
        (x:xs) -> 
            let (innerTokens, restTokens) = fromJust (findClosingParen xs depth)
            in Just (x : innerTokens, restTokens)



{-
parseAssignment:
Type: [String] -> [Stm] -> ([Stm], [String])

Purpose: Parses assignment statements, extracting the variable name and the expression to be assigned.
Input: A list of string tokens and the current list of accumulated statements.
Output: A tuple with updated accumulated statements and remaining tokens after processing the assignment.
-}
parseAssignment :: [String] -> [Stm] -> ([Stm], [String])
parseAssignment tokens stmAccumulator =
    case tokens of
        (varName : ":=" : rest) -> 
            let (expressionTokens, ";":remainingTokens) = break (== ";") rest
            in case parseSumSubOrProdOrIntOrParen expressionTokens of
                Just (expr, []) -> 
                    let newAssignStm = AssignStm varName expr
                    in (stmAccumulator ++ [newAssignStm], remainingTokens)
                Just _ -> error "ParseStm Error: Extra tokens after expression."
                Nothing -> error "ParseStm Error: Invalid expression after ':='."
        _ -> error "ParseStm Error: Expected a variable name followed by ':='."


{-
parseExpression:
Type: String -> [String] -> [Stm] -> ([Stm], [String])

Purpose: Parses expressions, extracting the variable name and the expression to be assigned.
Input: A variable name, a list of string tokens, and the current list of accumulated statements.
Output: A tuple with updated accumulated statements and remaining tokens after processing the expression.
-}
parseExpression :: String -> [String] -> [Stm] -> ([Stm], [String])
parseExpression varName tokens stmAccumulator =
    let (expressionTokens, remainingTokens) = break (== ";") tokens
    in case parseSumSubOrProdOrIntOrParen expressionTokens of
        Just (expr, []) -> 
            let newAssignStm = AssignStm varName expr
                (_, rest) = splitAt 1 remainingTokens -- safely discard the ";" which is the first element
            in (stmAccumulator ++ [newAssignStm], rest)
        Just _ -> error "ParseStm Error: Extra tokens after expression."
        Nothing -> error "ParseStm Error: Invalid expression after ':='."



{-
splitWhileDo:
Type: [String] -> ([String], [String], [String])

Purpose: Helper function to split the tokens for a while loop into the condition and body.
Input: A list of string tokens.
Output: A tuple with the tokens before "while", the condition tokens, and the tokens after "do".
-}
splitWhileDo :: [String] -> ([String], [String], [String])
splitWhileDo tokens = 
    let (beforeWhile, rest) = break (== "while") tokens
        (_, afterWhile) = splitAt 1 rest 
        (condition, afterDoWithRest) = break (== "do") afterWhile
        (_, afterDo) = splitAt 1 afterDoWithRest 
    in (beforeWhile, condition, afterDo)

{-
parseWhileLoop:
Type: [String] -> [Stm] -> ([Stm], [String])

Purpose: Parses while loop constructs by identifying the condition and body of the loop.
Input: A list of string tokens and the current list of accumulated statements.
Output: A tuple with updated accumulated statements and remaining tokens after parsing the while loop.

-}
parseWhileLoop :: [String] -> [Stm] -> ([Stm], [String])
parseWhileLoop tokens currentStm = 
    let (beforeWhile, conditionTokensRaw, afterDo) = splitWhileDo tokens
        cleanedConditionTokens = if not (null conditionTokensRaw) && head conditionTokensRaw == "(" then tail (init conditionTokensRaw) else conditionTokensRaw
        parsedBooleanExpr = parseAndOrBoolEqOrNotOrBasicBoolExpr cleanedConditionTokens
    in case afterDo of
          ("(":_) -> 
              case parsedBooleanExpr of
                  Just (boolExpr, [")"]) -> 
                      let (whileBodyTokens, restTokens) = span (/= ")") afterDo
                          (_, bodyTokens) = splitAt 1 whileBodyTokens 
                          whileBody = parseStm bodyTokens []
                      in (currentStm ++ [WhileStm boolExpr whileBody], tail restTokens)
                  Just (boolExpr, []) -> 
                      let (whileBodyTokens, restTokens) = span (/= ")") afterDo
                          (_, bodyTokens) = splitAt 1 whileBodyTokens 
                          whileBody = parseStm bodyTokens []
                      in (currentStm ++ [WhileStm boolExpr whileBody], tail restTokens)
                  Nothing -> error "ParseStm Error: Failed to parse boolean expression in 'while' statement."
                  _ -> error "ParseStm Error: Unexpected tokens after parsing boolean expression in 'while' statement."

          (x:_) -> 
              case parsedBooleanExpr of
                  Just (boolExpr, [")"]) -> 
                      let (whileBodyTokens, restTokens) = span (/= ";") afterDo
                          whileBody = parseStm whileBodyTokens []
                      in (currentStm ++ [WhileStm boolExpr whileBody], tail restTokens)
                  Just (boolExpr, []) -> 
                      let (whileBodyTokens, restTokens) = span (/= ";") afterDo
                          whileBody = parseStm whileBodyTokens []
                      in (currentStm ++ [WhileStm boolExpr whileBody], tail restTokens)
                  Nothing -> error "ParseStm Error: Failed to parse boolean expression in 'while' statement."
                  _ -> error "ParseStm Error: Unexpected tokens after parsing boolean expression in 'while' statement."

          [] -> error "ParseStm While Loop Error: No statements after 'do'."


{-
splitIfThenElse:
Type: [String] -> ([String], [String], [String], [String], [String])

Purpose: Helper function to split the tokens for an if-then-else construct into tokens before if, the condition, then-branch, else-branch, and remaining tokens.
Input: A list of string tokens.
Output: A tuple with the tokens before "if", the condition tokens, the then-branch tokens, the else-branch tokens, and the remaining tokens.
-}
splitIfThenElse :: [String] -> ([String], [String], [String], [String], [String])
splitIfThenElse tokens = 
    let (beforeIf, rest) = break (== "if") tokens
        (_, afterIf) = splitAt 1 rest 
        (condition, afterThenWithRest) = break (== "then") afterIf
        (_, afterThen) = splitAt 1 afterThenWithRest 
        (thenBranch, afterElseWithRest) = break (== "else") afterThen
        (_, afterElse) = splitAt 1 afterElseWithRest
        (elseBranchWithoutSemi, outOfConditionStatementsRaw) = breakAtOutCondition afterElse 0
        elseBranch = if not (null elseBranchWithoutSemi) && last elseBranchWithoutSemi /= ";" then elseBranchWithoutSemi ++ [";"] else elseBranchWithoutSemi
        outOfConditionStatements = if not (null outOfConditionStatementsRaw) && head outOfConditionStatementsRaw == ";" then tail outOfConditionStatementsRaw else outOfConditionStatementsRaw
    in (beforeIf, condition, thenBranch, elseBranch, outOfConditionStatements)


{-
breakAtOutCondition:
Type: [String] -> Int -> ([String], [String])

Purpose: Helper function to break at the end of the 'else' block or at the first ';' outside of parentheses.
Input: A list of string tokens and the current level of nested parentheses.
Output: A tuple with the tokens before the end of the 'else' block and the remaining tokens.
-}
breakAtOutCondition :: [String] -> Int -> ([String], [String])
breakAtOutCondition [] _ = ([], [])
breakAtOutCondition (t:ts) level = 
    case t of
        ";" | level == 0 -> ([], ts)
        "(" -> let (body, rest) = breakAtOutCondition ts (level + 1) 
               in (t : body, rest)
        ")" -> let (body, rest) = breakAtOutCondition ts (level - 1) 
               in (t : body, rest)
        _ -> let (body, rest) = breakAtOutCondition ts level 
             in (t : body, rest)


{-
parseIfThenElse:
Type: [String] -> [Stm] -> ([Stm], [String])

Purpose: Parses if-then-else constructs, identifying and parsing the condition, then-branch, and else-branch.
Input: A list of string tokens and the current list of accumulated statements.
Output: A tuple with updated accumulated statements and remaining tokens after parsing the if-then-else construct.
-}
parseIfThenElse :: [String] -> [Stm] -> ([Stm], [String])
parseIfThenElse tokens currentStm = 
    let (beforeIf, conditionTokensRaw, thenBranchTokensRaw, elseBranchTokensRaw, outOfConditionTokens) = splitIfThenElse tokens
        cleanedConditionTokens = if not (null conditionTokensRaw) && head conditionTokensRaw == "(" then tail (init conditionTokensRaw) else conditionTokensRaw
        cleanedThenBranchTokens = if not (null thenBranchTokensRaw) && head thenBranchTokensRaw == "(" then tail (init thenBranchTokensRaw) else thenBranchTokensRaw
        parsedBooleanExpr = parseAndOrBoolEqOrNotOrBasicBoolExpr cleanedConditionTokens
    in case parsedBooleanExpr of
        Just (boolExpr, []) ->
            let thenBody = parseStm cleanedThenBranchTokens []
                (elseBody, _) = parseElse elseBranchTokensRaw  -- No need to use remainingTokens here as it's captured in outOfConditionTokens
            in (currentStm ++ [IfStm boolExpr thenBody elseBody], outOfConditionTokens)
        Just (boolExpr, _) -> error "ParseStm Error: Unexpected tokens after parsing boolean expression in 'if' statement."
        Nothing -> error "ParseStm Error: Failed to parse boolean expression in 'if' statement."


{-
parseElse:
Type: [String] -> ([Stm], [String])

Purpose: Parses the 'else' block of an if-then-else construct.
Input: A list of string tokens.
Output: A tuple with the parsed statements in the 'else' block and the remaining tokens.
-}
parseElse :: [String] -> ([Stm], [String])
parseElse tokens = 
    if null tokens then
        -- Handle case where there are no 'else' statements.
        ([], [])
    else 
        case head tokens of
            "(" -> 
                -- Handle case where 'else' block is enclosed in parentheses.
                let (bodyTokens, remainingTokens) = span (/= ")") tokens
                    (_, bodyTokensTail) = splitAt 1 bodyTokens 
                    body = parseStm bodyTokensTail []
                in (body, tail remainingTokens)
            _ ->
                -- Directly handle 'else' block tokens.
                -- No need to check for semicolons as the tokens are already part of the 'else' block.
                let body = parseStm tokens []
                in (body, [])


{-
parseStm:
Type: [String] -> [Stm] -> [Stm]

Purpose: The main parsing function that dispatches to specific parsers based on the current token, managing the overall parsing process of statements.
Input: A list of string tokens and the current list of accumulated statements.
Output: The final list of parsed statements after processing all tokens.
-}
parseStm :: [String] -> [Stm] -> [Stm]
parseStm [] stm = stm

parseStm (";":remainingTokens) accumulatedStatements = parseStm remainingTokens accumulatedStatements

parseStm (varName:":=":tokens) stmAccumulator =
    let (newStms, remainingTokens) = parseAssignment (varName:":=":tokens) stmAccumulator
    in parseStm remainingTokens newStms

parseStm ("(":remainingTokens) currentStm =
    let (newStms, remainingTokensAfterParen) = parseParenthesizedStm ("(":remainingTokens) currentStm
    in parseStm remainingTokensAfterParen newStms
    
parseStm ("if":remainingTokens) currentStm =
    let (newIfStm, remainingTokensAfterIf) = parseIfThenElse ("if":remainingTokens) currentStm
    in parseStm remainingTokensAfterIf (currentStm ++ newIfStm)


parseStm ("while":tokens) currentStm =
    let (newWhileStm, remainingTokensAfterWhile) = parseWhileLoop ("while":tokens) currentStm
    in parseStm remainingTokensAfterWhile (currentStm ++ newWhileStm)


{-
parse:
Type: String -> Program

Purpose: To parse a program written in the language defined in the assignment. 
Input: A String (inputString), which is the text of the program to be parsed.
Output: A Program, which is a structured representation of the input program.
-}
parse :: String -> Program
parse inputString = parseStm (lexer inputString) []


-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:

-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "x := 0 - 2;" == ("","x=-2")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2") 
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")