# Notice Avoid
An Atari 2600 game to help people understand social anxiety.

# Ever want to help on a real 2600 game?
I have wanted to make a game for the 2600 for a very long time. I finally found the right idea.  Notice Avoid is a game that lets you take a Gemba Walk in the shoes of someone with social anxiety.


## Gameplay
You are the dot, like in 2600 Adventure.  Your friend Chad didn't make it to class today.  You have to bring him his book and his homework assignment.  This might seem easy for most people, but with social anxiety, some things are difficult.

### Lookie-loos and being noticed
As you begin the journey to Chad's house, you will see people looking around. They might notice you.  The bullets coming from the people aren't bullets but where they are looking.  If they see you, you need to listen to your anxiety and do things to turn down their attention.  Maybe looking at your cell phone, throwing up your hoodie, or hanging out against the wall might help.

### Game 1- A Simple Delivery
You must take Chad his homework and book. 
You can wear your hoodie, use your iPhone, and/or stand against the wall to avoid people noticing you. In game 1, their interaction doesn't matter much.

You see what you are using or wearing in the top left corner.
You move the dot with the joystick.
You select your hoodie or your phone to be in or out of use with the player A and player B difficulty switches respectively.  Changing the difficulty switch alters your use of the hoodie or phone.  You can do this in the middle of the game.

You drive the dot that represents you from your house to Chad's.  If you do this in the minimum amount of time, you win the max delivery points of 1000.  Take longer, you get fewer points.  Delivery points are shown in black on the left top of the screen.

The goal of game 1 is to make your 1000 points.

Your anxiety isn't considered in game 1. In this game, you have no anxiety.

### Game 2 - Anxiety Driver
Play as game 1.  There are no points for delivery. Instead, you get points for listening to your anxiety.

Anxiety is represented by a static-like noise. The noise gets louder when you are more anxious.  Anxiety is driven by random situations and lookie-loos who notice you. Anxiety is mostly random, but even that randomness has a distribution model that resembles my good friend's situation.  Sometimes, anxiety increases with no apparent driver.

If you do not listen to your anxiety, you will get no points.
If you listen and act, you will get anxiety listening points.

You can use your hoodie, the cell phone, or hang out against the wall. All of those actions will reduce the attention you get from the lookie-loos. The actions might help with listening to your anxiety. Getting to Chad's place or even returning to your own home will end the game with your current anxiety listening score.  The maximum anxiety listening score is random.  It's up to you how you want to deal with your anxiety.  Anxiety listening points are shown in red on right top of the screen.

The goal of game 2 is to get the most anxiety listening points.

Your delivery goal is not considered in game 2. In this game, you are wandering to understand what people look at and how to go under the radar.

### Game 3 - Notice Avoid
Both scores are considered in game 3.  You might not make the maximum delivery points while trying to deal with your anxiety. 

Your final score is a combination of delivery points and anxiety listening points.

## Building
Get the game source and throw it in DASM. Run the result in Stella.
Or, take the raw noticeavoid.asm file and paste it into an online emulation/authoring tool like https://8bitworkshop.com/v3.10.0/?platform=vcs&file=examples%2Fmissiles.a

Have a good time. If this game helps you understand others, then it is a wild success.

## Kernel Features
### Hardware Usage
#### TIA assignments:
* Ball: player- changes color when back is against wall to avoid notice
* Missiles 0 & 1: the eyeline from the Lookie-Loos
* Players 0 & 1:
  * Score letters
  * Your picture that shows what you are doing
  * Lookie-Loos
* Background: just black
* Playfield: shows where you are on your journey to experience social anxiety and try to help Chad.
* Audio Channel 0: Some beats, yo
* Audio Channel 1: Anxiety Static

#### The Game field is drawn:
1. Play music & static; determine Lookie-loos strategy
2. Score
3. Your picture
4. Playfield
5. Your dot, Lookie-Loos, their eyelines.

#### Features:
* joystick movement input: moving the ball, which represents you
* joystick fire button input: when you are against the wall, this makes you stand with your back to the wall
* difficulty switch input
  1. Difficulty A puts up (expert) or lowers (novice) your hoodie
  2. Difficulty B makes you use (expert) or lower (novice) your cell phone
* audio out
  1. Left channel has cool beats
  2. Right channel has the noise of anxiety


## Updates
  3/4/2023: Adding some few initial files with some sample/test code  
  3/6/2023: Added one lookie loo on top of playfield  
  3/7/2023: Fixed a minor timing issue for the lookie loo versus the playfield  
  3/8/2023: Added credits section to README.md and added 2nd lookie loo. Fixed CRT Probe timing errors  
  3/9/2023: Added the player as the ball and moved joystick control to the ball. 
  3/11/2023: Cleaned up screen timing  
  3/13/2023: Added music and a static demo to ensure timing will work. Added rests to alienbill's call at the expense of the 0 divisor. I'm okay with that.   
  3/13/2023: Added 6-digit score and split results on both sides of the screen.  


## Credits:
These sites encourage coders to learn and use their works. I appreciate the boosts and want to give back. Please use their ideas and tools. There would be no Notice Avoid without these contributions.  
8Blit for his [awesome efforts](https://www.8blit.com/) and his [awesome Github site](https://github.com/kreiach/).  I'm a Patreon patron- you should check this out immediately!  
8bitworkshop for their [site](https://8bitworkshop.com/) and Javascript CRT Probe debugger  
alienbill for their [2600 programming site and tooling](https://alienbill.com/2600/)  
[DASM 6502 assember](https://dasm-assembler.github.io/)  
[Stella Manual](http://atarihq.com/danb/files/stella.pdf)  
Chunkypixel's Atari Dev Studio [tooling in Visual Studio Code](https://github.com/chunkypixel/atari-dev-studio)  

## Bio:
  Tony Reynolds got his Atari VCS in 1977. With his family, they had lots of cool games. Just now found the right game to write for the 2600.

