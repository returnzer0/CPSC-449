data TriviaQuestion = NumericQ String Float | MCQ String String String String String Char

instance Eq (TriviaQuestion) where

    MCQ q _ _ _ _ _   == MCQ r _ _ _ _ _ = q == r
    NumericQ q _      == NumericQ r _ _ = q == r
    (NumericQ x _)    == (MCQ y _ _ _ _ _) = x == y
    (MCQ y _ _ _ _ _) == (NumericQ x _)    = x == y
                    _ == _ = False
