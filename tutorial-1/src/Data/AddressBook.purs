module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByAddress :: Address -> AddressBook -> Maybe Entry
findEntryByAddress address = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry =
    entry.address.street == address.street &&
    entry.address.city == address.city &&
    entry.address.state == address.state

existsName :: String -> AddressBook -> Boolean
existsName name addressBook =
  not null $ filter filterEntry addressBook
    where
      filterEntry entry = entry.firstName == name

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates =
  nubBy (\a b -> a.firstName == b.firstName && a.lastName == b.lastName)
