-- this is a chapter exercise

-- Given the following definitions, tell us what value results
-- from further applications.

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

-- fill in the types

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. what is the value of appedCatty "woohoo!"? Use REPL to verify.
-- "woops mrow woohoo!"

-- 2. frappe "1"
-- "1 mrow haha"

-- 3. frappe (appedCatty "2")
-- frappe (cattyConny "woops" "2")
-- frappe "woops mrow 2"
-- flippy "haha" "woops mrow 2"
-- flip cattyConny "haha" "woops mrow 2"
-- cattyConny "woops mrow 2" "haha"
-- "woops mrow 2 mrow haha"

-- 4. appedCatty (frappe "blue")
-- "woops mrow blue mrow haha"

-- 5. cattyConny (frappe "pink")(cattyConny "green" (appedCatty "blue"))
-- cattyConny "pink mrow haha" (cattyConny "green" "woops mrow blue")
-- cattyConny "pink mrow haha" "green mrow woops mrow blue"
-- "pink mrow haha mrow green mrow woops mrow blue"

-- 6. cattyConny (flippy "Pugs" "are") "awesome"
-- cattyConny "are mrow pugs" "awesome"
-- "are mrow pugs mrow awesome"
