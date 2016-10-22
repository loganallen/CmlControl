## CS 3110: Final Project

### Milestone 0: Charter

---

**Team Members:** <br>

> Logan Allen (lga24)<br>
  Andrew Grossfeld (amg445)<br>
  Michael Gingras (mcg79)<br>
  Brandon Walker (bcw58)

**Meeting Plan:** <br>
	
> We plan to meet for 45 minutes after every Tuesday/Thursday lecture for brief team checkpoints. These are simply to touch base and make sure the team is on the same page for weekly goals. We will spend big blocks of time on Friday to hammer through code together, bouncing off ideas and helping one another. If necessary we may meet up other times during the week to discuss more complex implementations.
	
**System Proposal:** <br>
	
*Core Vision:* 
> Create an exciting, interactive combat game similar in nature to Super Smash Bros.

*Key Features:* <br>
> ● Proper in-game physics <br>
  ● Strong, custom graphics <br>
  ● Keyboard interactivity for player movement <br>
  ● A.I. computer player <br>

*Descriptive Narrative:* <br>
> Our system intends to be a higly-functional graphical combat engine. At a high level, this game involves animated characters who fight one another in an arena. The first iteration involves player-vs-cpu combat where the player's actions are dictated by keyboard input and that of the cpu are generated with AI. Character actions involve moving horizontaly and vertically throughout an arena, dealing damage with attack moves, blocking and dodging opponent's attacks, and collection and using temporary in-game items. Whichever character loses 100% of its health first loses, and thus the standing character wins.
> 
> We plan to implement characters with a custom `Sprite Kit` that will model a graphical character, all physical movements associated with a sprite, and how health is managed. This will allow for code reusability and streamlined sprite-on-sprite combat handling.
> 
> The combat arena will consist of a 2-D platforms that facilitate vertical and horizontal sprite movement. The GUI will expose the the area of the arena containing the main sprite, following the characters movements when appropriate.