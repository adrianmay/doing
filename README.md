# doing - A Time Tracker

## Concept

Doing (pronounced "do-ing") is a command line app for linux.  

It stores a list of transitions between activities.

You enter a transition to an activity with `doing Something` optionally appending a number of minutes ago that the transition happened.

It will then wait for you to add some details of the activity that just *finished* followed by Ctrl-D. This can be an empty string. You can change to the current activity to enter different details.

The activity `0` means you are doing nothing worth tracking.

There's a list of declared activities to guard against typos. You can enter a substring of an activity name and it'll work if there's exactly one match in that list.

An activity is billable if its name does not begin with _. 

Activity names may contain spaces.

If you enter `doing` with no parameters it'll print a report organised by month, then by activity, with a row for each stint showing the start, the finish, the duration and the details. Activity `0` is excluded from the report. Billable activities are listed first in each month.

The activity header line shows the total time on that activity in that month and the month header line shows the total for all billable activities.

## Installation

1. Clone this repo.
1. Install Haskell's `stack`.
1. Run `make` which will install the Haskell executable (`did`) and a script (`doing`) in `/usr/bin`.
1. Create a directory `~/.doing`.
1. Create a file in there called `acts` with a line per activity containing simply its name. `0` must be included.
1. Your transitions will be stored in `~/.doing/transitions`. The format is self-explanatory so you can edit them.
1. You should occasionally delete old transitions to keep performance up.

