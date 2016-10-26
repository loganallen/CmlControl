## CS 3110: Final Project

### Milestone 0: Charter for CmlControl
---

**Team Members:**
> Logan Allen (lga24) <br>
> Andrew Grossfeld (amg445) <br>
> Michael Gingras (mcg79) <br>
> Brandon Walker (bcw58) <br>

**Meeting Plan:**
> We plan to meet for 45 minutes after every Tuesday/Thursday lecture for brief team checkpoints. Theseare simply to touch base and make sure the team is on the same page for weekly goals. We will spendbig blocks of time on Friday to hammer through code together, bouncing off ideas and helping oneanother. If necessary we may meet up other times during the week to discuss more compleximplementations.

**System Proposal:**

*Core Vision:*
> Create a fundamental version control system in OCaml.

*Key Features:*
> ● Basic local version control commands<br>
> ● Intermediate local version control commands <br>
> ● (Given extra time): advanced version control with remote repository access via `cmlhub.com`

*Descriptive Narrative:*
> Our `CmlControl` system intends to be a fundamental version control system built in OCaml. It will contain all of the core local version control commands that exists in git. The basic commands include `init`, `add`, `rm`, `commit` and `status`. The intermediate commands include `branch`, `checkout`, `diff`, `merge` and `stash`. 
> 
> Depending on how successful we are in implementing the fundamentals in `CmlControl` we may extend the project to include remote version control and simulation on `cmlhub.com`. Commands for this would include `remote`, `fetch`, `push`, `pull` and `clone`.