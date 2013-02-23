import Data.Aeson
import Control.Applicative
import qualified Data.Text as T
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Base ()
import Data.Aeson.TH ()

data Test = Test { z :: Maybe [String]
                 , a :: Maybe T.Text
                 , b :: T.Text
                 , c :: ()
                 , d :: [String]
                 }
          deriving (Show)

{-
instance ToJSON Test where
    toJSON (Test z a b c d) = object [ T.pack "z" .= z
                                   , T.pack "a" .= a
                                   , T.pack "b" .= b
                                   , T.pack "c" .= c
                                   , T.pack "d" .= d
                                   ]
-}

-- $(deriveJSON id ''Test)

instance ToJSON Test where
    { toJSON
        = \ value[a8qB]
            -> case value[a8qB] of {
                 Test arg1[a8qC] arg2[a8qD] arg3[a8qE] arg4[a8qF] arg5[a8qG]
                   -> object
                        [(T.pack "z" .= arg1[a8qC]), (T.pack "a" .= arg2[a8qD]),
                         (T.pack "b" .= arg3[a8qE]), (T.pack "c" .= arg4[a8qF]),
                         (T.pack "d" .= arg5[a8qG])] } }
instance FromJSON Test where
    { parseJSON
        = \ value[a8qH]
            -> case value[a8qH] of 
                 Object recObj[a8qI]
                   -> if (Data.HashMap.Base.size
                            recObj[a8qI]
                        ==
                          5)
                      then
                          (((((Test
                             <$>
                               Data.Aeson.TH.lookupField
                                 "Main.Test" "Test" recObj[a8qI] (T.pack "z"))
                            <*>
                              Data.Aeson.TH.lookupField
                                "Main.Test" "Test" recObj[a8qI] (T.pack "a"))
                           <*>
                              Data.Aeson.TH.lookupField
                               "Main.Test" "Test" recObj[a8qI] (T.pack "b"))
                          <*>
                            Data.Aeson.TH.lookupField
                              "Main.Test" "Test" recObj[a8qI] (T.pack "c"))
                          <*>
                           Data.Aeson.TH.lookupField
                             "Main.Test" "Test" recObj[a8qI] (T.pack "d"))
                      else
                          Data.Aeson.TH.parseTypeMismatch'
                            "Test"
                            "Main.Test"
                            "Object with 5 name/value pairs"
                            ((show . unordered-containers-0.2.1.0:Data.HashMap.Base.size)
                               recObj[a8qI]
                           ++
                             " name/value pairs")
                 other ->
                      Data.Aeson.TH.parseTypeMismatch'
                        "Test"
                        "Main.Test"
                        "Object"
                        (Data.Aeson.TH.valueConName other) }

main = do print $ encode $ Test Nothing (Nothing) (T.pack "hi") () ["hello"]
          print $ (decode $ (L.pack "{\"b_\":\"hi\",\"d_\":[\"hello\"]}") :: Maybe Test)

