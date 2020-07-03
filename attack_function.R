
sim_damage_roll <- function(num_attacks, STR, PROF, sides, num_dice,
                            AC, crit, runs, WEAP, crit_thresh) {
  
  # STR = Strength modifier
  # PROF = Proficiency modifier
  # sides = Num of sides of your damage dice, e.g. if roll 3d6 then 6 for d6
  # num_dice = Num of dice you roll on damage roll, e.g. if roll 3d6 then 3 for 3 dice
  # AC = AC of hypothetical opponent
  # crit = Num of dice you roll on critical hit damage roll,
  #   e.g. 4 if crit during sneak attack
  # runs = Num of times to run simulation
  # WEAP = weapon bonus. i.e., if the weapon is a +1...then add +1 to your
  #   attack & damage rolls
  # num_attacks = The number of attacks taken in a turn by the character
  
  damage_roll <- vector(mode = "numeric")
  
  # Two attacks -------------------------------------------------------------
  
  if(num_attacks == 2) {
    
    # For each simulation run...
    for(i in 1:runs){
      
      # Show progress
      incProgress(amount = 1 / runs, detail = paste("Doing run", i))
      
      
      # First of two attacks ----------------------------------------------------
      
      # Prep for results of attack 1
      attack_roll1 <- vector(mode = "numeric")
      damage_roll1 <- NULL
      
      # Does the attack hit?
      attack_roll1 <- sum(sample(x = 1:20, size = 1, replace = T),
                          STR, PROF, WEAP)
      
      # If it hits roll damage
      if(AC <= attack_roll1 & (attack_roll1 - (STR + PROF + WEAP)) < crit_thresh){
        
        damage_roll1 <- sum(sample(x = 1:sides, size = num_dice, replace = T),
                            STR, WEAP)
        
        # If it doesn't hit, 0 damage
      }else if(attack_roll1 < AC){
        
        damage_roll1 <- 0
        
        # If it is a critical hit...
      }else if((attack_roll1 - (STR + PROF + WEAP)) >= crit_thresh){
        
        damage_roll1 <- sum(sample(x = 1:sides, size = crit, replace = T),
                            STR, WEAP)
        
      }
      
      
      # Second of two attacks ---------------------------------------------------
      
      # Prep for results of attack 2
      attack_roll2 <- vector(mode = "numeric")
      damage_roll2 <- NULL
      
      # Does the attack hit?
      attack_roll2 <- sum(sample(x = 1:20, size = 1, replace = T),
                          STR, PROF, WEAP)
      
      # If it hits roll damage
      if(AC <= attack_roll2 & (attack_roll2 - (STR + PROF + WEAP)) < crit_thresh){
        
        # It hits
        damage_roll2 <- sum(sample(x = 1:sides, size = num_dice, replace = T),
                            STR, WEAP)
        
        # If it doesn't hit, 0 damage
      }else if(attack_roll2 < AC){
        
        damage_roll2 <- 0
        
        # If it is a critical hit...
      }else if((attack_roll2 - (STR + PROF + WEAP)) >= crit_thresh){
        
        damage_roll2 <- sum(sample(x = 1:sides, size = crit, replace = T),
                            STR, WEAP)
        
      }
      
      # Compile all damage
      damage_roll[i] <- damage_roll1 + damage_roll2
      
    }
    
    
    # Single attack -----------------------------------------------------------
    
  } else if(num_attacks == 1){
    
    # For each simulation run...
    for(i in 1:runs){
      
      # Show progress
      incProgress(amount = 1 / runs, detail = paste("Doing run", i))
      
      # Prep for results of attack
      attack_roll1 <- vector(mode = "numeric")
      damage_roll1 <- NULL
      
      # Does the attack hit?
      attack_roll1 <- sum(sample(x = 1:20, size = 1, replace = T),
                          STR, PROF, WEAP)
      
      # If it hits roll damage
      if(AC <= attack_roll1 & (attack_roll1 - (STR + PROF + WEAP)) < crit_thresh){
        
        # It hits
        damage_roll1 <- sum(sample(x = 1:sides, size = num_dice, replace = T),
                            STR, WEAP)
        
        # If it doesn't hit, 0 damage
      }else if(attack_roll1 < AC){
        
        damage_roll1 <- 0
        
        # If it is a critical hit...
      }else if((attack_roll1 - (STR + PROF)) >= crit_thresh){
        
        damage_roll1 <- sum(sample(x = 1:sides, size = crit, replace = T),
                            STR, WEAP)
        
      }
      
      # Compile damage
      damage_roll[i] <- damage_roll1
      
    }
    
    
  }
  
  return(damage_roll)
  
}