-- Definition of Expr.

data Expr = S | K | I | B 
          | Expr :$ Expr 
          | X | Z | V Int  
          deriving (Show, Read)


-- Test Expr cases.

test1 = S :$ K :$ K :$ X
twoB = S :$B :$ I
threeB = S :$ B :$ (S :$B :$ I)
test3 = threeB :$ X :$ Z
omega = ((S :$ I) :$ I) :$ ((S :$ I) :$ I)
kio = K :$ I :$ omega
add = (B :$ S) :$ (B :$ B)


-- Turning expr to string.

prettyExpr expr = remove(simple expr)
    where 
        simple (a :$ (b :$ c)) = simple a ++ " (" ++ remove (simple (b :$ c)) ++ ")"  -- If right Expr is in brackets it should stay in it.
        simple (a :$ b) = simple a ++ simple b  -- Simplify both Expr.
        simple (V a) = " v" ++ show a
        simple (X) = " x"
        simple (Z) = " z"
        simple (S) = " S"
        simple (K) = " K"
        simple (I) = " I"
        simple (B) = " B"
        remove (x:xs) = xs  -- Removing redundant whitespace from the beginning of the bracket.


-- Performs one reduction step. Pref is a saved prefix which can't be reduced. Expr is expression left to reduce.

rstep pref expr = reduce pref expr
    where
    reduce pref (S:a:I:c:ts) = (pref, (a:c:c:ts))         -- Shortening redex S.
    reduce pref (S:a:b:c:ts) = (pref, (a:c:(b:$c):ts))
    reduce pref (K:a:b:ts) = (pref, (a:ts))
    reduce pref (I:a:ts) = (pref, (a:ts))
    reduce pref (B:a:I:c:ts) = (pref, (a:c:ts))           -- Shortening redex B.
    reduce pref (B:a:b:c:ts) = (pref, (a:(b:$c):ts))
    reduce pref ((a :$ b):ts) = reduce pref (a:b:ts)      -- If front of expression is in bracktes, then unpacking.
    
    reduce Nothing (h:ts) = reduce (Just h) ts            -- If there is no match in patterns above, then front of the expression is not a redex.
    reduce (Just ex) (h:ts) = reduce (Just (ex :$ h)) ts  -- If there was some prefix before.
    reduce _ [] = (Nothing, [])  -- If there is no more expression to reduce.


-- Performes max n steps of reduction. 

rpath expr = steps Nothing (dig expr) [prettyExpr expr] 0
    where
    steps pref expr list count = 
        let (n_pref, seqe) = rstep pref expr  -- Perform a step
        in 
        if not (null seqe) && count < 29      -- If number of steps is not exceeded and there is a sequence to reduce.
        then steps n_pref seqe ((normalize n_pref seqe) : list) (count + 1)  
        else rev list []  -- Return reversed list of steps.

    -- Changes List to Expr.
    normalize pref (h : m : ts) = normalize pref ((h :$ m) : ts)        -- Change rest of the exspression to Expr.
    normalize (Just (a :$ b)) (h : ts) = normalize (Just a) [b :$ (h)]  -- Add whole prefix.
    normalize (Just a) (h : ts) = prettyExpr (a :$ h)
    normalize Nothing (h : ts) = prettyExpr h

    -- Changes Expr to List
    dig (h :$ t) = ((dig h) ++ [t])
    dig h = [h]

    -- Reverts the list.
    rev (h:ts) out = rev ts (h:out) 
    rev [] out = out
 

printPath expr = mapM_ putStrLn (rpath expr) 
