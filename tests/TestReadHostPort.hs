import Database.MongoDB.Connection
import System.Exit
import Control.Monad

testList = [
  ("Simple host", readHostPort "host" == Host "host" (PortNumber 27017)),
  ("Simple host with port", readHostPort "host:123" == Host "host" (PortNumber 123)),
  ("Pathological ::1:1234 case", readHostPort "::1:1234" == Host "::1" (PortNumber 1234)),
  ("Full IPv6 with port", readHostPort "1:2:3:4:5:6:7:8:1234" == Host "1:2:3:4:5:6:7:8" (PortNumber 1234)),
  ("Full IPv6 without port", readHostPort "1:2:3:4:5:6:7:8" == Host "1:2:3:4:5:6:7:8" (PortNumber 27017)),
  ("Partial IPv6 with port", readHostPort "1:2:3::4:12" == Host "1:2:3::4" (PortNumber 12)),
  ("Partial IPv6 with hex at end", readHostPort "1:2:3::4:a" == Host "1:2:3::4:a" (PortNumber 27017))
           ]
main =
  let
    failedTests = filter (not . snd) testList
  in
   if null failedTests then
     putStrLn "All tests passed"
   else
     do
       putStrLn "The following tests failed:"
       forM_ failedTests $ \(descr, _) -> putStrLn $ "  * " ++ descr
       exitWith (ExitFailure 1)
