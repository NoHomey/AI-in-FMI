module Data.AssocList ( AssocList
                      , empty
                      , fromList
                      , list
                      , find
                      , insert
                      , update
                      , delete
                      , keys
                      , Data.AssocList.null
                      ) where
                        
newtype AssocList key value = AssocList { list :: [(key, value)] }

instance (Show key, Show value) => Show (AssocList key value) where
    show = show . list

empty :: AssocList key value
empty = AssocList []

fromList :: [(key, value)] -> AssocList key value
fromList = AssocList

find :: Eq key => key -> AssocList key value -> Maybe value
find key assocList = findInList key $ list assocList
    where findInList _         []                  = Nothing
          findInList searchKey ((key, value):rest) = if searchKey == key
                                                       then Just value
                                                       else findInList searchKey rest

insert :: Eq key => key -> value -> AssocList key value -> AssocList key value
insert key value assocList = AssocList $ insertInList key value $ list assocList
    where insertInList key value []          = [(key, value)]
          insertInList key value (head:tail) = if (fst head) == key
                                                 then (key, value):tail
                                                 else head:(insertInList key value tail)

update :: Eq key => key -> value -> AssocList key value -> AssocList key value
update = insert

delete :: Eq key => key -> AssocList key value -> AssocList key value
delete key assocList = AssocList $ deleteFromList key $ list assocList
    where deleteFromList _         []                       = []
          deleteFromList searchKey (head@(key, value):tail) = if searchKey == key
                                                                then tail
                                                                else head:(deleteFromList searchKey tail)

keys :: AssocList key value -> [key]
keys = (map fst) . list

null :: AssocList key value -> Bool
null = Prelude.null . list