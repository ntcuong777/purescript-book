module Main where

import Data.AddressBook
import Prelude

import Data.List (filter, head, null, nubByEq)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "🍝"

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName = head <<< filter filterStreet
  where
    filterStreet :: Entry -> Boolean
    filterStreet entry = _.address.street entry == streetName

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = not null <<< filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq isNameEq
  where
    isNameEq :: Entry -> Entry -> Boolean
    isNameEq e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
