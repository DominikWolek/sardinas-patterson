module Main (
    main
) where
    
import Menu
import System.Environment

main = do
    args <- getArgs

    if args /= []
        then Menu.checkFile (head args)
        else Menu.showMenu
