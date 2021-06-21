# Makers BBS: Redux

In this module the goals are:

* Learn to modify existing software for new purposes.
* Learn to build maintainable software in a team.

You'll learn these by building new functionality on your projects from teamwork
week. The catch is that you'll be building on another team's work. In doing this,
you will learn what makes code easy to work with, both first-hand through working
with someone else's code, and second-hand by seeing what challenges emerge for
the team inheriting your code.

In your roles, you will both inherit code from the past, and build code for the
future, and so this empathy and sound engineering practice will serve you well.

## How will we know if our software is maintainable?

Organisations want to deliver effective software to users in a timely fashion.
Maintainable software preserves the ability of the team to do this, rather than
getting harder and harder to change over time.

* **Testing.**  
  Tests help by making it easier to verify nothing has broken when you make changes. Untested code hinders this by making it easy to break things, resulting in going back over things and rework.

* **Readable code.**  
  Readability helps this by making it easier for developers to understand what is happening in the code, so that they can then change it. Hard-to-understand code means regularly having to ask your colleagues to unpick what they've written so that you can understand it, slowing down the delivery of the software.

* **Single Responsibility Principle.**  
  Modules following the single responsibility principle make it easier for developers to define and isolate the change to be made in a way that doesn't break anything else. Large, complex code modules are slow to change and hard to verify, slowing down the team and the software delivery.

So you will know if your software is production-quality if, at the end of the module, you find it pleasant, easy, and quick to make changes to.

## How will we know if we're working well in a team?

You will know your team is working well if:

* **You're shipping features.**  
  Software, deployed, getting into the hands of the users — that is the aim of your work! But as you may discover: it's easy to start off fast, but staying fast is the real challenge (and often involves working at a steady pace).

* **Everyone is contributing.**  
  No one should be sitting around waiting, no one should be passively letting the team get on with things. If this happens, you are wasting energy that can be going into building software.

* **You feel good.**  
  That means you're well-rested, taking care of yourselves, speaking and listening to each other. Treat frustration like thirst — not a problem to suppress, but a helpful signal from yourself to surface and figure out how to fix. Don't stew!

* **You're proud of your work.**  
  If you know you're working together effectively, if you know you're building production-quality software, if you know you're learning and taking care of yourselves, you should be proud.

## Team Process

Follow the setup and team process from your previous [Makers BBS](https://github.com/makersacademy/makersbbs) module, with the following additional practices:

### Estimation

For effective team coordination, it's useful to know how long things are likely to take. After you've broken down your team's work into tickets, estimate it.

To do this, go through each ticket and determine collectively how long you think it will take. Engineers often use t-shirt sizing (XS, S, M, L, XL) rather than time, to capture how 'complex' a task is.

To do this, you'll need to think in a bit of detail what you'll need to do to achieve it. You might need to make some design decisions — note these on the card. You might find the ticket is better be split up into smaller tickets — feel free to do so.

### Retrospection

Spend some more time in your retrospective on how your team is working. Here are some prompts:

* How much fun did you have today? 1-10
* What have we implemented? Demo for each other!
* Are we wasting time anywhere?
* Do we have any quality concerns to address?
* Were our estimates accurate? What did we get forget about?

## What are we going to build?

The internet has become very popular, and so have BBSs. However, they need resources, which means they need to make money. Your team have been hired to develop an existing BBS to enable this.

Your coach will share with you an existing BBS codebase from another group.

The client has identified the following essential features. Your task is to both add these, and to add new features both to attract new users and ensure the current users are enjoying and engaging with the BBS.

### User Sign-up and Log-in

If this functionality is not already present, you will need to add it. If it is, you will need to modify it.

You will need to capture:

* Username
* Password
* Bank account number

### Credits

The service will make money by selling credits. Credits can then be used (for example) to play games, post messages, or do other useful things.

The users will want to:

* See their credit balance in the top right of every screen.
* Spend their credits on certain things (you decide).
* Understand how much credits cost, and how to get them.

### Payments

Each day an administrator will download the latest bank statement. They will then enter these details into the BBS system, including how much and from which accounts, so that the system can then credit the right users with the right amount of credits.

### Other features

The user hasn't specified what the exciting new features the users will spend their credits on, so it's up to you to decide this and then implement some cool stuff!

## Presentations

At the end of the project, each team will present the great work done at our demo day.

You will talk through a slide deck and do a quick demo of your project. You'll share your learning journey, technical achievements, and show off your cool project to your cohort, coaches, and invited guests.

[Read the guidance for these presentations here.](https://github.com/makersacademy/course/blob/master/pills/final_project_presentations.md)
