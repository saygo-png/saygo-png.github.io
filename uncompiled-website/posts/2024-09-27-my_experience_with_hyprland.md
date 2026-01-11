---
title: "Why AwesomeWM is just better than Hyprland"
---

I first started using Linux with a minimal Debian setup, using AwesomeWM and Picom as the window manager. After thoroughly considering other options, I found that this setup served me well, as I could configure it to my needs and even extend it later with a plugin.

Although I was satisfied with AwesomeWM, I kept hearing about Hyprland — a project that caught my interest due to Wayland’s goal of being "frame perfect." This was in contrast to my AwesomeWM setup, which had plenty of visual artifacts when resizing windows. However, this interest remained distant since Debian Stable had no hope of running bleeding-edge software like Hyprland, which relies on the latest libraries. So, I continued using AwesomeWM.

## NixOS

After becoming frustrated with the imperative style of Debian, I switched to NixOS. Thanks to NixOS's more robust dependency handling and availability of newer packages, I was finally able to use Hyprland!

I had the choice of using three versions: the NixOS stable version, the unstable version, and the flake version. I ruled out the flake version because I didn’t want to add third-party flakes to my config. I also avoided the unstable version, as I dislike frequent changes and prefer my updates to consist only of security fixes, as they were on Debian. So, I went with the stable version.

After setting up the basic configurations, I was amazed — Hyprland provided beautiful animations, and everything was buttery smooth with no artifacts! Switching windows, workspaces, and resizing — all actions were animated just as I had hoped. But soon, the issues started to reveal themselves.

Here they are, in order of most to least significant:

## Buggy drag and drop

When using [Krita](https://github.com/hyprwm/Hyprland/issues/6450) and Inkscape, shifting the position of layers with the mouse was nearly impossible. It’s hard to describe exactly what Hyprland was doing — the layers would often go to the wrong places or switch to a different layer while I was dragging one. This made Inkscape’s already unusual layer system nearly unusable. It was somewhat better in Krita, possibly because I like Krita’s layer system more. Still, the layers would often select each other, drop selections, or move to incorrect positions.

I’m placing this issue at the top because I use these two applications frequently for art, and this completely breaks my workflow.

## Games

Games would turn into a black screen if I switched workspaces, moved them, fullscreened them, toggled floating, or resized them. This was true with Overwatch 2, basically the only game I play now, and Overcooked 2.

The wiki recommends using `gamescope`, stating that "Using gamescope tends to fix any and all issues with Wayland/Hyprland." So, I tried it. However, when I ran `gamescope` from Steam, it simply never showed up. Running `gamescope-steam` from the terminal resulted in a black window with working audio. Running `gamescope -W 1920 -H 1080 -r 70 -- alacritty` in a terminal emulator opened it, but it was surrounded by a large black border, at a much lower resolution than specified, and frozen.

Eventually, I worked around this by running gamescope in TTY without nesting it in Hyprland. This was far from ideal because, for example, my mouse sensitivity from libinput was not properly applied, nor could I do things like take screenshots or quickly switch to my browser or terminal. But using this workaround revealed yet another issue with Hyprland: when an application was focused and I switched to the TTY and then back, Hyprland would send some sort of wrong message to the client. Now, some clients like [Neovide](https://github.com/neovide/neovide/issues/2795) and [Alacritty](https://github.com/alacritty/alacritty/issues/8065) would just crash due to this malformed message. I had to switch to Foot just to use this workaround.

## Minimize

The basic "Minimize" function, known from Windows, macOS, Android, and many other platforms, [does not exist](https://github.com/hyprwm/Hyprland/issues/995) in Hyprland! When I first encountered this, I thought I was simply missing something — that the functionality was there but just named differently. But no, it’s genuinely not supported.

Why is this an issue? Minimize is an essential feature that allows you to hide a window you might not need right now but will need later. Currently, in Hyprland, if you want to hide a window, your only options are to close it or move it to another workspace.

Closing is obviously not ideal. But what about the other solution? If you have windows in workspaces 1 and 2 that you want to minimize, where are you going to put them? Your only option is to move them to workspace 3. Now, when you want to bring a window back, you have to go to workspace 3, identify the window you moved from workspace 2, and then explicitly bring it back to workspace 2. This gets increasingly complicated as you increase the number of workspaces and minimized windows.

AwesomeWM solves this by binding minimized windows to the workspace they were minimized in. If you want to bring back a window minimized in workspace 2, you simply use the unminimize shortcut — no need to remember where each window was or use up a workspace just for minimized windows.

As far as I’m aware, there’s no other solution. Yes, there are special workspaces, but they function the same as a normal workspace regarding minimizing. The wiki page recommends using them but adds this remark: “Note that one keybind can only handle one window,” which makes this approach impractical.

Vaxry, the core Hyprland developer, closed the issue saying, "no, this is not kde," despite other window managers having this feature and it clearly being something that users want. (His comment got a ratio of 6 likes to 52 dislikes)

## Visual glitches

Yes, the very thing I was promised I wouldn’t have to deal with. Visual glitches still happen, albeit less frequently and inconsistently. Hyprland creates "ghosts" of some windows, especially when dragging items (e.g., during drag and drop). It also often happens with notifications — if I receive too many notifications in dunst too quickly, I frequently see ghosting artifacts. I use AMD, so this is not an NVIDIA-specific issue.

## Crashes

Rarely, I experienced random crashes that had not happened to me in AwesomeWM. For example, switching to a TTY, waiting, and returning to Hyprland would sometimes cause a crash. I’m putting this issue low on the list because these crashes were infrequent.

## Buggy window swallow

When window swallow was enabled, windows in complex layouts would be positioned unpredictably. By "complex layout," I mean something like having four terminals grouped together, one of them opening a GUI application that should appear to the right but instead goes to the left because swallow was enabled. This broke my workflow since that application was my program that I kept recompiling and testing, and I had to constantly rearrange windows.
