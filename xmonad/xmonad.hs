import XMonad

main = xmonad defaultConfig
       { modMask = mod4Mask -- use the Super key as mod
       , borderWidth = 3
       }
