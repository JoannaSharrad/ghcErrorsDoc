module Tclass where
    import System.Environment

    class Console a where
        writeLine::a->IO()
        readLine::IO a

    instance Console Int where
        writeLine= putStrLn . show 

        readLine = do
            a <- getLine
            let b= (read  a)::Int
            return b

    useInt::IO()
    useInt =putStrLn . show $ (2+readLine)
