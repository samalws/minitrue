import CoC.DeBruijn
import CoC.Named

mainRoutine = do
  putStrLn "enter a:"
  a <- readLn
  putStrLn "enter b:"
  b <- readLn
  putStrLn $ if (hasType [] a b) then "a :: b" else "a !:: b"

main = mainRoutine >> main
