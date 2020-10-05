-- https://kowainik.github.io/posts/haskell-mini-patterns#newtype-task

module NewtypeTask where

newtype PlayerHealth = PlayerHealth
  { unPlayerHealth :: Int
  }

newtype PlayerArmor = PlayerArmor 
  { unPlayerArmor :: Int
  }

newtype PlayerAttack = PlayerAttack 
  { unPlayerAttack :: Int
  }

newtype PlayerDexterity = PlayerDexterity
  { unPlayerDexterity :: Int
  }

newtype PlayerStrength = PlayerStrength
  { unPlayerStrength :: Int
  }

data Player = Player
  { playerHealth :: PlayerHealth,
    playerArmor :: PlayerArmor,
    playerAttack :: PlayerAttack,
    playerDexterity :: PlayerDexterity,
    playerStrength :: PlayerStrength
  }

newtype PlayerDamage = PlayerDamage
  { unPlayerDamage :: Int
  }

newtype PlayerDefense = PlayerDefense
  { unPlayerDefense :: Int
  }

calculatePlayerDamage :: PlayerAttack -> PlayerStrength -> PlayerDamage
calculatePlayerDamage attack strength = PlayerDamage $ unPlayerAttack attack + unPlayerStrength strength

calculatePlayerDefense :: PlayerArmor -> PlayerDexterity -> PlayerDefense
calculatePlayerDefense armor dexterity = PlayerDefense $ unPlayerArmor armor * unPlayerDexterity dexterity

calculateHit :: PlayerDamage -> PlayerDefense -> PlayerHealth -> PlayerHealth
calculateHit damage defense health = PlayerHealth $ unPlayerHealth health + unPlayerDefense defense - unPlayerDamage damage

-- The second player hits first player and the new first player is returned
hitPlayer :: Player -> Player -> Player
hitPlayer player1 player2 =
  let damage =
        calculatePlayerDamage
          (playerAttack player2)
          (playerStrength player2)
      defense =
        calculatePlayerDefense
          (playerArmor player1)
          (playerDexterity player1)
      newHealth =
        calculateHit
          damage
          defense
          (playerHealth player1)
   in player1 {playerHealth = newHealth}