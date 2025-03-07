data Expr = S | K | I | B 
          | Expr :$ Expr 
          | X | Z | V Int  
          deriving (Show, Read)

test1 = S :$ K :$ K :$ X
twoB = S :$B :$ I
threeB = S :$ B :$ (S :$B :$ I)
test3 = threeB :$ X :$ Z
omega = ((S :$ I) :$ I) :$ ((S :$ I) :$ I)
kio = K :$ I :$ omega
add = (B :$ S) :$ (B :$ B)

prettyExpr expr = remove(simple expr)
    where 
        simple (a :$ (b :$ c)) = simple a ++ " (" ++ remove (simple (b :$ c)) ++ ")"
        simple (a :$ b) = simple a ++ simple b
        simple (V a) = " v" ++ show a
        simple (X) = " x"
        simple (Z) = " z"
        simple (S) = " S"
        simple (K) = " K"
        simple (I) = " I"
        simple (B) = " B"
        remove (x:xs) = xs

rstep expr = reduce Nothing [expr] (dig expr)
    where
    dig (h :$ t) = ((dig h) ++ [t])
    dig h = [h]
    reduce pref steps (S:a:b:c:ts) = reduce pref (normalize pref (a:c:(b:$c):ts) : steps) (a:c:(b:$c):ts)
    reduce pref steps (K:a:b:ts) = reduce pref (normalize pref (a:ts) : steps) (a:ts)
    reduce pref steps (I:a:ts) = reduce pref (normalize pref (a:ts) : steps) (a:ts)
    reduce pref steps (B:a:b:c:ts) = reduce pref (normalize pref (a:(b:$c):ts) : steps) (a:(b:$c):ts)
    reduce pref steps ( (a :$ b):ts ) = reduce pref steps (a:b:ts)
    reduce Nothing steps (h:ts) = reduce (Just h) steps ts
    reduce (Just ex) steps (h:ts) = reduce (Just (ex :$ h)) steps ts  -- Tutaj nawiasy na odwrót prefiksu.
    reduce _ steps [] = rev_pretty [] steps

    normalize pref (h : m : ts) = normalize pref ((h :$ m) : ts)
    normalize (Just (a :$ b)) (h : ts) = normalize (Just a) [b :$ (h)]
    normalize (Just a) (h : ts) = (a :$ h)
    normalize Nothing (h : ts) = h

    rev_pretty els (h : ts) = rev_pretty ((prettyExpr h) : els) ts
    rev_pretty els [] = els 


printPath expr = mapM_ putStrLn (rstep expr) 