data Days = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving(Eq, Ord, Show, Read, Bounded, Enum)

main =
    putStrLn $ show $ Days !! 1
