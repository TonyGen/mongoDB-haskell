import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Data.Text (pack)
import Data.Bson

(==:) :: (Val a) => String -> a -> Field
a ==: b = pack a =: b

main = do
    pipe <- runIOE $ connect (host "127.0.0.1")
    e <- access pipe master (pack "baseball") run
    close pipe
    print e

run = do
    clearTeams
    insertTeams
    allTeams >>= printDocs "All Teams"
    nationalLeagueTeams >>= printDocs "National League Teams"
    newYorkTeams >>= printDocs "New York Teams"

clearTeams = delete (select [] (pack "team"))

insertTeams = insertMany (pack "team") [
    ["name" ==: "Yankees", "home" ==: ["city" ==: "New York", "state" ==: "NY"], "league" ==: "American"],
    ["name" ==: "Mets", "home" ==: ["city" ==: "New York", "state" ==: "NY"], "league" ==: "National"],
    ["name" ==: "Phillies", "home" ==: ["city" ==: "Philadelphia", "state" ==: "PA"], "league" ==: "National"],
    ["name" ==: "Red Sox", "home" ==: ["city" ==: "Boston", "state" ==: "MA"], "league" ==: "American"] ]

allTeams = rest =<< find (select [] $ pack "team") {sort = ["home.city" ==: (1 :: Int)]}

nationalLeagueTeams = rest =<< find (select ["league" ==: "National"] $ pack "team")

newYorkTeams = rest =<< find (select ["home.state" ==: "NY"] $ pack "team") {project = ["name" ==: (1 :: Int), "league" ==: (1 :: Int)]}

printDocs title docs = liftIO $ putStrLn title >> mapM_ (print . exclude [pack "_id"]) docs