---
title: "Dota 2 Oracle in depth"
---

## Content
This post contains my findings and information from other sources on how Oracle in Dota 2 works and his weird interactions. This is not a general guide. I'm an Oracle one-trick with 200+ games though I'm new to Dota 2 overall. https://www.opendota.com/players/260253919/heroes

## Purifying Flames
Cannot kill allies directly including minions.

Always more mana efficient for healing to use with Fate's Edict but it will use more mana overall.

Here are some efficiency calculations:

**Without Fate's Edict:**
- Purifying Flames at lvl 1 heals 60 using 75 mana. Eff: 0.80
- Purifying Flames at lvl 2 heals 70 using 75 mana. Eff: 0.93
- Purifying Flames at lvl 3 heals 80 using 75 mana. Eff: 1.07
- Purifying Flames at lvl 4 heals 90 using 75 mana. Eff: 1.20

**Without Fate's Edict + 30% magic resist:**
- Purifying Flames at lvl 1 heals 87 using 75 mana. Eff: 1.16
- Purifying Flames at lvl 2 heals 124 using 75 mana. Eff: 1.65
- Purifying Flames at lvl 3 heals 161 using 75 mana. Eff: 2.15
- Purifying Flames at lvl 4 heals 198 using 75 mana. Eff: 2.64

**With Fate's Edict:**
- Purifying Flames at lvl 1 heals 150 using 145 mana. Eff: 1.03
- Purifying Flames at lvl 2 heals 250 using 145 mana. Eff: 1.72
- Purifying Flames at lvl 3 heals 350 using 145 mana. Eff: 2.41
- Purifying Flames at lvl 4 heals 450 using 145 mana. Eff: 3.10

Purifying Flames has great synergy with Glimmer Cape as it can tank 1.5~ Purifying Flames' casts while allowing your carry to attack.

It's okay to use this without any sort of protection to conserve your mana pool short term or in fights where you know that a person will live long enough to get healed instead of just damaged. This is especially the case in late game when high magic resistance and BKBs are common.

## Fortune's End
The projectile emitted by this always aims at the target or its last location if it dies. The flying projectile provides vision and has an infinite flying distance. You can use this property to "extend" the range by for example casting it on an ally minion wave that is pushing forward. The minion will move forward as you are casting and enemies might come to hit the creeps. You can then stop the channel and hit them at a very long range without them expecting it.

You can also cast it on an ally teleporting across lanes. The flying projectile will scout the area using its vision while flying to this teleported ally. Of course this works on enemies too but is rarely useful since it's better to interrupt the channel early to disturb TP with a root.

The ally cleanse only works on an ally that you are channeling on, it's not AoE. The purge on enemies however is and works regardless of who the Q is cast on. Therefore when your ally and enemy are both close, it is more optimal to cast Fortune's End on an ally.

Only the root duration scales with channel time, the damage does not change.

## Fate's Edict
Contrary to some sources the magic resist and disarm cannot be cleansed with Fortune's End on enemies.

Can be cleansed from allies. Both the MR and disarm get cleansed.

## False Promise
False Promise collects all healing and damage taken, keeping the damage in order. Then it checks if there is more damage than healing. If there is more healing the excess becomes its own healing instance. Healing during and the excess both get amped by outgoing and incoming heal amp, making it very effective with False Promise.

Example calculation of False Promise heal:
Oracle with Holy Locket (1.1 outgoing amp) ults an ally. This ally then gets buffed with Chen's Divine Favor (1.2 incoming amp) and healed once by Dazzle's E (85 heal). Chen's favor lasts longer than False Promise here so it gets applied on the excess heal.

healValue (204) = heal (85) * falsePromiseFactor (2) * incomingAmp (1.2) * outgoingAmp (1.1)
healValueFinal (270) = (healValue (204) - damage (0)) * incomingAmp (1.2) * Oracle's outgoing amp (1.1)

### Quirks
Debuff immunity makes **self-casted** False Promise deal 0 damage no matter what. Sources include Black King Bar and Omniknight's Repel.

## Hero interactions
### Medusa
As foe Purifying Flames can be spammed on her as most of her effective health is mana which does not get healed.

### Doom
Can spam Purifying Flames on Doomed targets to deal damage without healing.

### Ancient Apparition
Can spam Purifying Flames on targets affected by ult to deal damage without healing.

### Abaddon
As ally be careful to not pop his ult while healing him.

### Necrophos
His scythe ult is very predictable and can be easily blocked using Fate's Edict.

Ghost Shroud can be purged.
