# State Machine Compiler

This is a _State Machine Compiler (SMC)_ that generates code for state machines in various programming languages. It allows developers to define state machines in a high-level language and generate the corresponding code, making it easier to implement complex _state-based_ logic in applications.

## Syntax

First draft of the syntax for defining state machines. `coffeeMachine.sm`:

### Examples

```sm
$machine "CoffeeMachine" {
  $initial "Selecting"
  $state "Selecting" => "ChooseDrink" => "Brewing"
  $state "Brewing" => "Finish" => "Selecting" => "dispenseCup"
}
```

In the above example, we define a state machine called `CoffeeMachine` with an _initial state_ of `Selecting`. The machine has two states: `Selecting` and `Brewing`. Each state has _transitions_ defined by _events_, for example:

Given that we are in the `Selecting` state, when we get a `ChooseDrink` event, then we transition to the `Brewing` state.

Notice that some transitions can have actions associated with them. Given that we are in the `Brewing` state, when we get a `Finish` event, then we transition to the `Selecting` state and invoke the `dispenseCup` action.

This can also be defined using a verbose definition of states and events, which can be useful if the goal is to succinctly describe a more complicated state machine. For example, we can group _several transitions_ related to the same state, that way we don't have to repeat the root state several times. We also have the opportunity to add _entry_ and _exit_ actions to the states as well:


```sm
$machine "CoffeeMachine" {
  $initial "Selecting"
  $state "Selecting" {
    $entry "DisplayMenu"
    $event "ChooseDrink" => "Brewing"
    $event "NoMoreCoffee" => "Selecting"
  }
  $state "Brewing" {
    $entry "StartHeating"
    $exit "StopHeating"
    $event "Finish" => "Selecting" => "dispenseCup"
  }
}
```

It is also possible to define superstates, which allows substates to _inherit common events and actions_. This is useful to avoid duplication of events and actions that are shared across multiple states.

Here is an example that extends the previous `CoffeeMachine` state machine to include a superstate called `Operational`, which contains a common event for handling power outages. We also add a new state called `Off` to represent the powered-off state of the machine.

```sm
$machine "CoffeeMachine" {
  $initial "Selecting"
  $superstate "Operational" {
    $event "PowerOutage" => "Off"
  }
  $state "Selecting" $inherits "Operational" {
    $entry "DisplayMenu"
    $event "ChooseDrink" => "Brewing"
    $event "NoMoreCoffee" => "Selecting"
  }
  $state "Brewing" $inherits "Operational" {
    $entry "StartHeating"
    $exit "StopHeating"
    $event "Finish" => "Selecting" => "dispenseCup"
  }
  $state "Off" {
    $entry "PowerDownComponents"
    $event "PowerRestored" => "Selecting"
  }
}
```

In the above example, the states `Selecting` and `Brewing` inherit the `PowerOutage` event from the superstate `Operational`. Inheritance is defined by specifying the superstate after the state name using the `$inherits` keyword.

### Comments

Comments can be placed in the state machine source code using either a hashtag `#` or two slashes `//` for single-line comments. Multi-line comments are supported through the `/* ... */` syntax.

```sm
// My Finite State Machine for a door
$machine "Door" {
  # Could also start in "Opened" state
  $initial "Closed"
  $state "Closed" => "Open" => "Opened"
  $state "Open" => "Close" => "Closed"
  /*
    This is a multi-line comment
    describing the Door state machine.
  */
}
```

### Syntactical Sugar 

To make things succinct, there are several _syntactical sugars_ built into the SMC. For example it is optional to define actions or just leave them out in case none are needed.

Another neat syntax shortcut is that you can reduce the verbosity of state definitions by omitting the `$initial` keyword and specifying the initial state directly in the `$machine` declaration by using the `=>` syntax.

You can also use a hyphen to indicate that a transition will leave the state unchanged, even though you _still might execute an action_. To define _multiple actions_ for a _single transition_, group them with curly braces `{}`. Here is an example using the mentioned syntax sugars:

```sm
$machine "Printer" => "Idle" {
  $superstate "Operational" {
    $event "PowerLoss" => "Offline"
  }
  $state "Idle" $inherits "Operational" {
    $event "Print" => "Printing"
    $event "Cancel" => - => {"logCancelWithoutJob" "beep"}
  }
  $state "Printing" $inherits "Operational" {
    $event "Finish" => "Idle" => "ejectPage"
  }
  $state "Offline" {
    $entry "PowerDown"
    $event "PowerRestored" => "Idle"
  }
}
```

First thing to notice is that the arrow `=>` syntax after the machine declaration indicates the initial state of the state machine. We see this act as the inheritance declaration a bit later as well. The hyphen `-` indicates that the state _remains unchanged_, though it still triggers _two actions_. Multiple actions are grouped using curly braces `{}`.

### Keywords

Here is an overview of the keywords used in the state machine definition language:

- **$machine** - Declares a state machine with a name.
- **$initial** - Declares the target state of the initial pseudostate.
- **$state** - Defines a state within the state machine.
- **$event** - Defines an event that triggers a transition from one state to another, optionally specifying actions to be executed during the transition.
- **$superstate** - Defines a superstate that can contain common events and actions for its substates.
- **$inherits** - Specifies that a state inherits events and actions from a superstate.
- **$entry** - Defines an action to be executed when entering a state.
- **$exit** - Defines an action to be executed when exiting a state.

Additional syntax includes `#`, `//`, `/* */`, `=>`, `{}`, and `-` for various purposes as described in the above sections.

Extra whitespace and newlines are _ignored_, so you can format the state machine definitions in a way that is most readable to _you_.

### EBNF

Below you can see a formalized description of the syntax expressed in Extended Backus-Naur Form (EBNF)

```ebnf
file ::= whitespace* comment* stateMachine whitespace* comment*

stateMachine ::= "$machine" string ( "=>" stateName )? "{" machineItem* "}"
machineItem  ::= initialState | superstate | state | comment | whitespace
initialState ::= "$initial" stateName

superstate ::= "$superstate" string "{" superItem* "}"
superItem  ::= entryAction | exitAction | event | comment | whitespace

state ::= "$state" stateName ( inheritsClause )?
          ( "{" stateItem* "}" | terseTransition )

inheritsClause ::= "$inherits" string

stateItem ::= entryAction | exitAction | event | comment | whitespace
terseTransition ::= "=>" eventName "=>" destination ( "=>" action )?
event ::= "$event" eventName "=>" destination ( "=>" action )?

destination ::= stateName | "-"
entryAction ::= "$entry" action
exitAction  ::= "$exit" action

action ::= string | "{" string+ "}"

eventName ::= string
stateName ::= string

string ::= '"' { characterExceptQuote } '"'
characterExceptQuote ::= ? any character except " or newline ? 
whitespace ::= { " " | "\t" | "\n" | "\r" }

comment ::= singleLineComment | multiLineComment
singleLineComment ::= "#" { notNewlineChar } | "//" { notNewlineChar }
multiLineComment ::= "/*" { anyCharExceptClosingComment } "*/"
```
