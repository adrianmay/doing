# doing - A Time Tracker

## Concept

Doing (pronounced "do-ing") is a command line app for linux.  

It stores a list of transitions between activities.

You enter a transition to an activity with `doing Something` optionally appending a number of minutes ago that the transition happened.

It will then wait for you to add some details of the activity that just *finished* followed by Ctrl-D. This can be an empty string. You can change to the current activity to enter different details.

The activity `0` means you are doing nothing worth tracking.

There's a list of declared activities to guard against typos. You can enter a substring of an activity name and it'll work if there's exactly one match in that list.

If you enter `doing` with no parameters it'll print a report organised by month, then by activity, with a row for each stint showing start and finishing times, the duration and the details. The project header line shows the total time on that activity in than month. 0 is excluded from the report.

If you don't report an activity to your client, start the name with a space in /var/doing/acts. These will be grouped together at the end of the month so you can easily copy-paste the billable projects.

## Installation

1. Clone the repo
1. Install Haskell's `stack`
1. `make`
1. Create and take ownership of a directory `/var/doing`
1. Create a file in there called `acts` with a line per activity containing simply its name. `0` must be included.
1. Your transitions will be stored in `/var/doing/transitions`.
1. You should occasionally delete old transitions to keep performance up.








