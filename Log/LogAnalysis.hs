module LogAnalysis where
import Log

parse :: String -> [LogMessage]
parse logText = map parseMessage . lines $ logText

parseMessage :: String -> LogMessage
parseMessage msg = 
  let parts = words msg
      mtResult = parseMessageType parts
  in  case mtResult
      of (Nothing, _) -> Unknown msg
         (Just messageType, rest) -> 
           let tsResult = parseTimeStamp rest
           in case tsResult 
              of  (Nothing, _) -> Unknown msg
                  (Just timeStamp, rest) -> LogMessage messageType timeStamp $ unwords rest

isInt :: String -> Bool
isInt = all (`elem` ['0'..'9'])

parseMessageType :: [String] -> (Maybe MessageType, [String])
parseMessageType [] = (Nothing, [])
parseMessageType lst@(x:[]) = (Nothing, lst)
parseMessageType lst@(err:severity:_) = 
  case err
  of "I" -> (Just Info, drop 1 lst)
     "W" -> (Just Warning, drop 1 lst)
     "E" -> if isInt severity then (Just $ Error (read severity), drop 2 lst) else (Nothing, lst)
     _   -> (Nothing, lst)

parseTimeStamp :: [String] -> (Maybe TimeStamp, [String])
parseTimeStamp [] = (Nothing, [])
parseTimeStamp lst@(ts:rest) = if isInt ts then (Just (read ts), rest) else (Nothing, lst)


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown msg) mTree = mTree
insert msg Leaf = Node Leaf msg Leaf
insert lm@(LogMessage _ timeStamp _) (Node left tlm@(LogMessage _ treeTs _) right) =
  case timeStamp < treeTs
  of True   -> Node (insert lm left) tlm right
     False  -> Node left tlm (insert lm right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build msgs = foldl (\a x -> insert x a) Leaf msgs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ msg:(inOrder right)

isSerious :: LogMessage -> Bool
isSerious (LogMessage (Error severity) _ _) = severity >= 50
isSerious _                                 = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = 
  let seriousMsgs = filter isSerious msgs
      seriousTree = build seriousMsgs
  in map (\(LogMessage _ _ msg) -> msg) . inOrder $ seriousTree
