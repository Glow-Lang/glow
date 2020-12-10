# *Glow* - reference manual

Welcome to *Glow*'s reference manual. 

Here you will learn everything about this mighty language and its associated toolset and perform feats previously inaccessible to mere mortals!



This document was updated on December 10th, 2020.





# Intro

## Hello, there! Who are you?

Hi, and welcome to Glow's reference manual.

To be as useful as possible, this manual has been designed with multiple layers of complexity, depending on your level of proficiency in programming and in computer science: it means you can re-read it as you get better at using Glow. This manual has been written both by nearly total-beginners, and computer science experts, so that *Glow* can be learnt without being confusing

And if that's not enough, you can always:

- join us on [our discord channel](https://discord.com/channels/655606984514469899/655606984967585832)

- contact us directly: support@glow-lang.org



So here's a little rundown...



### Level 1: I am new to computer programming

Great! Your presence here means our product is becoming mainstream

For now, though, *Glow* is a bit tricky to start programming, but in the future, we plan to make tutorials that will take you from zero knowledge to a point where you can code DApps with *Glow* without any uncertainty. We want to make *Glow* as accessible as possible, and we'll do everything for you to reach this goal.

In the meantime, you can Browse the Internet to learn the two prerequisites for *Glow*-programming: 

- the general notions of computer science and app-development
- a basic understanding of how blockchain-related programming works



You can still read this documentation for business purposes and skip the parts that contain code.



### Level 2: I can make standard apps

You're a developer with knowledge of the production of standard apps: you know how to make one for your users, but you lack the knowledge and experience in the field of DApps: *Glow* comes with many typical smart-contracts that you can use in your DApps.

With these pre-made templates, you just have to edit the settings of these contracts, and use tools that we certified.

You'll be sure that your DApp's transactions are secure, so that you only have to take care of what matters the most in any app: its design, its front-end, its functionalities...

While not needing to program in *Glow*, you can read this manual step by step to understand how *Glow* works, to ultimately use an efficient tool to save time, money, and a lot of a worrying uncertainty while building your DApp.



### Level 3: I can make DApps, and want to make my own smart-contracts

You have read this manual and tried *Glow* for a while, or you are already a bit experienced in blockchain-related programming.

This documentation will allow you to make your own smart-contracts: you will develop your own versions of our tools, for your own unique vision of smart-contracts. The team behind *Glow* can audit your smart-contracts for a fee, if you want to make sure they are safe.

Each chapter of this manual links to sub-documents for Level-3 users. Each time you want to be more autonomous and make the best use of glow, those links will give you advanced tips about what our community and development-team understood over time.

It'll save you a precious tinkering-time, and you'll be able to grasp *Glow* (and DApps) like never before.



### Level 4: I want to help develop *Glow* and understand all the details

A contributor, are we? Well, welcome backstage: each chapter of this manual contains sub-documents for level-4 users. 

These sub-documents can contain either the source code of *Glow* (written in other languages) or details about the thinking process we went through (you'll find some high-level mathematical proofs, links to our previous versions, and many other things)

In the end, these documents will allow you to contribute to *Glow*, make your own version of it, or just audit it and know it as well as we do. 

Feel free to share your progress with the community.



## Why use *GLOW*?

Regular apps and computer programs are safe and reliable when meant to be used by only one person: ie solo video games, text editors, calculators, graphic design softwares, or anything, really...

But when you have to interact with other people with a regular app, it needs to be regulated by a centralized entity (a state, a company, or a person). This entity will use servers physically located somewhere in the world, to ensure the safety of the interactions: ie AirBNB, Facebook, Uber, Amazon, any bank account or wallet, sending emails, etc...

A server-less version of these now classic Apps is called a DApp (it stands for Decentralized Application, and is pronounced either "dap" or "D-app").

A DApp is distributed around all the users of a network, and compared to its centralized ancestor, it has many advantages:

- Uncensorable and ad-less social networks
- Uberized services where service-providers are paid more (because no fee for the centralized server)
- Increased safety of your asset transfers
- Complete traceability of an asset, for supply chain managers, stock-management, or even lawyers
- Secured private life



**Problem**: 
Compared to centralized programs, DApps are much more prone to bugs that can make you lose all your money and data (and it happened before). They are harder to program, and if you want them to be safe, it'll require more resources, and more time.



**Solution**: 
To make DApps more mainstream, the world needed a toolset to automate all the critical mathematical functions for safety and drastically simplify the way a DApp is programmed.

This is precisely what *Glow* is...



## What is *Glow*?

### A programming language for DApps

*Glow* is a programming language used to make DApps. It comes with a wide set of tools.



*Glow's* three main specificities are:

- **Safety**: each newly implemented function is thoroughly audited so that a programmer using the *Glow* language or its associated toolset won't have to worry about securing interactions between users
- **User-friendliness**: we want to make DApps mainstream, and Glow's development team always aims 
- ***Portable* to any blockchain**: Glow can produce DApps that use the resources of multiple blockchains at once if you want to. 



In other words: what usually takes a huge amount of lines of code, and many experts in various fields has been automated. Some people will use contracts pre-made in *Glow*, some others will make their contracts with a very safe language.



### What *Glow* can and can't do

*Glow* is a language dedicated to safe interactions, asset-protection, and trust.

Hence, *Glow* is specialized in smart-contracts. At the moment, it cannot be used to program every type of software, and isn't meant to do so. Just like some languages are dedicated to specialized tasks (*eg* R for data science and statistics), *Glow*'s main focus is safe smart contracts.

For that reason, every time we add a new feature, we carefully check if its addition is safe in the whole *Glow* ecosystem. Maybe one day you will be able to program complete apps, full video games and everything in *Glow*, but for the moment, we focus on its specialty, so that DApps developed with *Glow* stay safe.



### More information

#### Official pages and documents

Official website: https://glow-lang.org 

Social media pages:

- Twitter: [@MutualKnowledge](https://twitter.com/MutualKnowledge)
- Facebook: [facebook.com/MuKnSys/](https://facebook.com/MuKnSys/)
- Telegram: [t.me/mukn_io](https://t.me/mukn_io)
- Linkedin: [linkedin.com/company/mukn-io/](https://linkedin.com/company/mukn-io/)
- Youtube: [www.youtube.com/playlist?list=PLiEa4i6H-qcdosr5x1jPkGJRUVPB1GOlL](https://www.youtube.com/playlist?list=PLiEa4i6H-qcdosr5x1jPkGJRUVPB1GOlL)

- Discord: https://discord.com/channels/655606984514469899/655606984967585832



And of course, to get more information about glow, you can read our [whitepaper](https://docs.google.com/document/d/1nBmI28yISX2HynodZnYWW0Px6re4JyYNNw2ncaFfJSg/edit)



> Social pages in Russian and Chinese (VK and Wechat) will come later
>
> The documentation will be translated to French and perhaps other languages before June 2021
>
> Russian, Chinese and other translations will come late 2021





#### Team and license

*Glow* is free and Open Source and will stay like that

*Glow* is distributed under the Apache License, version 2.0 (see the file [LICENSE]()). 

Copyright 2019 Mutual Knowledge Systems, Inc. All rights reserved. 



US-based company Mutual Knowledge Systems Inc. keeps developing it, and doesn't want to have a legal monopoly over it: we just want to keep it accessible. 

Check our [website](https://mukn.io/) to discover the human beings behind the code



#### Gitlab repo

You can check our [gitlab repository](https://gitlab.com/mukn/glow) to know the code as well as we do.



#### Our history

Check our history to learn more about how we arrived there.

We also thoroughly keep [track](https://gitlab.com/mukn/glow/-/blob/master/docs/bibliography.md) of every source talking about *Glow* online.



#### *Glow's* predecessor

For the sake of history, you can learn more about our previous attempt to make a safe and reliable tool for DApp-development: [Alacrity](https://gitlab.com/mukn/glow/-/blob/master/docs/Alacrity.md).



#### Philosophical and technical basis 

We compiled and extensive list of what you should read to understand our thought-process during the development of *Glow*.

We hope you'll enjoy reading our [bibliography about relevant related subjects](https://gitlab.com/mukn/glow/-/blob/master/docs/extra-bibliography.md).

Also, if you are keen on reading them, check our [mathematical proofs](https://gitlab.com/mukn/glow/-/blob/master/docs/proofs.md) (we're currently publishing them)



### Environments

#### Use *Glow* in any browser

In the near future, our code will run on any browser on desktop with a nice user interface, but this is still work in progress.



#### Use *Glow* locally on Linux and Mac OS

At the moment, *Glow* can run on Linux distributions and Mac OS



#### Other platforms (levels 3 & 4)

*Glow* builds in a reproducible way using [the Nix package manager](https://nixos.org/). Hence, you can also use Glow on NixOS, by directly installing Nix as an operating system. It'll require to dual boot your computer, or to have a separate computer for this use.

Also, you can install *Glow* the hard way on other systems: check the following chapter for more information.



# Getting started

## Simplified install on Linux or Mac OS

#### Install Glow with only one command line

To install Glow as a user or developer, go to the terminal (or any app on your system that allows you to enter command lines).

Once there, type the following command line:

> sh <(curl -L https://glow-lang.org/install/glow-install)



Note that until we provide a caching server, the first-time installation will take some time (about an hour). We will be working on making it faster *after we launch*.



#### Notes

##### Safety

This installation process will require you to manually type "yes", or "y" from time to time, to authorize parts of the installation. It is a normal part of the process.

Of course *we* trust the installation code, but *you* shouldn't until *you* audit it, and so you should run it inside a virtual machine. For extra security, you might want to use [Qubes OS](https://www.qubes-os.org/).



##### Why the installation is that long (levels 3 and 4)

The reason the installation is that long is that it is going to recompile the entire compiler suite, starting with Gambit Scheme. 

We'll correct that in a later version of Glow :)



## Install on NixOS (levels 3 and 4)

If you have [Nix](https://nixos.org/nix/) as an operating system (either as a dual-boot, or on a dedicated computer), then configure it to use the [same repo as we do](https://github.com/fare-patches/nixpkgs) AND the same branch: `fare` 

```
export NIX_PATH=nixpkgs=http://github.com/fare-patches/nixpkgs/archive/fare.tar.gz
```

Note that these will change when we have stable releases.

Once you have configured Nix, you can build and install Glow together with a coherent set of its dependencies, from source, yet with a binary cache, by running this command from the current directory:

```
nix-build
```



## Trying it out with Docker (levels 3 and 4)

When we have a stable release, we'll directly provide an image `mukn/glow:stable` on [Docker](https://hub.docker.com/repository/docker/mukn/glow).

In the meantime, you can build your own with:

```
docker build -t mukn/glow -f scripts/Dockerfile 

```



## Install Glow the hard way on other systems (levels 3 and 4)

If you don't use either Linux or macOS, and can't use Docker, you will have to build and install Glow the hard way: installing GCC, on top of it Gambit Scheme, on top of it Gerbil Scheme, on top of it Glow.

If you really want to try it this way, you can check our [hardcore installation file]().



# Languages basics

## Overview

The base language is a statically typed pure functional dialect of JavaScript, with some inspiration from ReasonML.

If you are used to programming in imperative languages, you will also find common concepts. 

If you are a seasoned developer, you can check our less-detailed and straight-to-the-point grammar, our [front-end syntax](https://gitlab.com/mukn/glow/-/blob/master/docs/internal/syntax/glow-sexpr.scrbl) and [surface grammar](https://gitlab.com/mukn/glow/-/blob/master/docs/internal/syntax/glow-surface.scrbl).

Otherwise the next chapters are designed to make you grasp it quickly.



## Fundamentals: Hello world!

Once *Glow* is installed, you can code in any local text editor on Linux or Mac OS. Later, we will implement a user-interface for *Glow* that will allow you to code directly in your browser.

As many languages dedicated to smart contracts, *Glow* is a language focused on interactions between users.

 Hence, there is no typical "hello world!" program like in most languages, but there is something alike: a "hello world!" for two...



Let's have a look at this very simple code:

> \#lang glow
>
> `@interaction([A, B])`

> `let publishHello = () => {`

> ` @A let ha = "Hello world! uh... I mean... Hello B!";`

> ` publish! A -> ha;`

> ` @B let hb = "Hello A! I'm world! Uh... I mean... I'm B! Nice to see you.";`

> ` publish! B -> hb;`

> `};`



Any smart contract or program that you write in *Glow* starts with the mandatory line:

> `#lang glow`
>
> 

Interactions are defined with an arobase:`@` 

Note the brackets `{}` at the beginning and the end of the function, just like in some other languages.

Also, regarding line-breaking, note that white spaces and interlines don't matter.



**The first line** of this code defines that the following function will be an interaction between `A` and `B`.

You can see that semicolons are used to end expressions. They can be used to end every statement, but are mandatory only for expressions.

**The second line** defines a publication function. 

**The third and fifth lines** define texts respectively named `ha` and `hb` using the command `let`

**The fourth and sixth** lines basically use the `publish!` command to say that the sentences `ha` and `hb` will be sent respectively on behalf of users  `A`  and  `B` .

Note that these instructions will be executed line by line, so the dialog will happen like a real one.



## Other syntactic and general rules

### Basic comparisons

Equal sign `=` is for binding values only with the command `let`

Double equal sign `==` is for comparisons

You can also already use the following comparators: 

-  `<`
- `>`
- `<=`
- `=>`



### Basic operations

You can also already use the following operators: 

-  `+`add

- `-` substract

- `*` multiply

- `/` divide

- `%` to get the rest of the euclidian division

- `**` for exponentiation

  

Unlike in Javascript, ++ and -- aren't in use.

Addition and substraction mustn't go beyond the boundaries, otherwise it'll get an overflow error (division can't divide by zero, obviously, and at the moment, *Glow* doesn't support negative numbers)

Also note that you can't perform operations between strings and numbers: you have to explicitly convert one type to another



### Commenting your code

Remember: *"badass programmers don't comment their code: it was hard to write, it has to be hard to read"*

No, seriously, do comment your code in *Glow*: it works just like in ReasonML and JavaScript:

> `// This is a comment` 



> `/* This is
> a comment */`



### Naming objects in *Glow*

A variable name is made one or more of the characters `[_a-zA-Z0-9]` and doesn't start with a digit, and the variable `_` is reserved to denote a pattern variable that matches anything but whose value is ignored.

Capitalization is conventionally reserved for types and constructors, but isn't mandatory.

There are terms that can't be used to name objects, as they are already used as Keywords by either *Glow* or other languages that interact with it:



- `abstract`
- `arguments`
- `assert`
- `boolean`
- `break`
- `byte`
- `case`
- `catch`
- `class`
- `char`
- `const`
- `continue`
- `data`
- `debugger`
- `default`
- `delete`
- `deposit`
- `do`
- `double`
- `else`
- `enum`
- `eval`
- `export`
- `extends`
- `false`
- `final`
- `finally`
- `float`
- `for`
- `function`
- `goto`
- `if`
- `implements`
- `import`
- `in`
- `instanceof`
- `int`
- `interface`
- `let`
- `long`
- `native`
- `new`
- `null`
- `package`
- `private`
- `protected`
- `public`
- `rec`
- `require`
- `return`
- `short`
- `static`
- `sum`
- `super`
- `switch`
- `synchronized`
- `this`
- `throw`
- `throws`
- `transient`
- `true`
- `try`
- `type`
- `typeof`
- `var`
- `verify`
- `void`
- `volatile`
- `while`
- `with`
- `yield`
- `withdraw`





## Basic features

### Primitive types

*Glow* can define several types:

- Units: `Unit`
- Booleans: `Bool`
- Integers: `Nat` 
- Byte strings: `Bytes`



#### Units

`Unit` contains a single value `()`.



#### Booleans

The most basic data type in *Glow* is `Bool`, the type of booleans, containing the logical values `true` and `false`.

Both values have to be typed in lowercase. 



##### Operators involving booleans

- and: &&

- or: ||
- not: not

> `<a> && <b> boolean "and" operator—only evaluates the right-hand-side expression <b> if <a> evaluates to true.`



> `<a> || <b> boolean short-circuiting "or" operator—only evaluates the right-hand-side expression <b> if <a> evaluates to false.`



> ```
> ! true == false
> ```

Note that short-circuit evaluation works in *Glow* just like in Javascript



#### Integers

The next most common data type in *Glow* is the integer.

At this moment, *Glow* only supports the type `Nat` which contains non-negative natural integers. 

In practice, integers have to fit in 256 bits, or 32 bytes, the same as `UInt256` of Ethereum. Integers greater or equal to 2^256 or less than zero will cause a runtime error.

The syntax is just decimal for the moment: just write a non-empty sequence of digits from `0` to `9`.

We will support a richer set of integer types in the near future, and will add hexadecimal syntax.



#### Byte strings

Finally, the last primitive type in *Glow* is the type `Bytes`for byte strings of finite sequences (or array) of bytes (integers from `0` to `255` included). 

For now, the syntax for byte strings is to write arbitrary ASCII characters between double quotes.

As in:

> `"hello world!"`



Our syntax is currently limited to ASCII characters, but we will soon add an escape syntax for arbitrary bytes.

Currently available escape sequences are :

- `\"` for a double-quote (ASCII 34)
- `\n` for a newline (ASCII 10)
- `\\`for a backslash (ASCII 92)

The syntax for a string is a double quote `"` followed by unescaped ASCII characters or escape sequences.

Unescaped ASCII characters are any character in the ASCII table from space (ASCII 32) to tilde (ASCII 126), except double-quote `"` (ASCII 34) and backslash (ASCII 92) that must be espaced.

Recognized escape sequences currently include `\"` to represent the double-quote (ASCII 34) and `\\` to represent a backslash (ASCII 92). We will support more escape sequences and an alternate byte-oriented syntax in the near future.



##### Important note

Note that the choice of characters as non-negative 8-bit bytes, is deliberate: DApps have to deal with digesting exactly well-defined sequences of bytes, and concepts like Unicode, UTF-8, etc., only get in the way. When interfacing between *Glow* and other languages, make sure that you represent *Glow* bytestrings as sequences bytes in your language, and not "native" strings of Unicode or any such dangerously misfit type.



### Composite types

At the moment, composite types are:

- Aliases: `Alias`
- Tuples: `Tuple`
- Sums: `Sum`



##### Aliases 

You can define a new type alias with `type <name> = <formula>` for instance the definition: `type RGB = Nat * Nat * Nat;` defines a type whose elements are triplets of natural integers.



##### Tuples

Tuple-types are simple cartesian products of types containing tuples made of one element of each of the types in the product, in order.



###### Tuple types

The type `Bool` * `Nat` * `Bytes` contains such elements as `(true, 4, "hello")` and `(false, 42, "world")`

The syntax for tuple types is `(A,B,C)` ... 



###### Tuple values

The syntax for tuple values is `(a , b , c)` and is expressed with the command `let`

For example, say you have a type named `RGB`, and that you want to define a data (here, we'll arbitrarily say that it's a color).

Let's call that data `mySoftBlue` and say that it is from the `RGC` type:

>`let mySoftBlue : RGB = (204, 238, 255);`



This previous line has set the triplet of values within a tuple.

More precisely, at the moment, what you will type in *Glow* will be:

> type Byte = Nat; 
> let doubleQuote = 34; 
> let backslash : Byte = 92; 

> type RGB = (Byte, Byte, Byte); 
> let mySoftBlue : RGB = (204, 238, 255);



**Explanation**

Let's say that we want to deal with a lot of byte values. The current type system only knows about `Nat` and all bytes will be natural integers and live in the type `Nat`

Therefore to declare that some values are supposed to be bytes, the most precise type we can give them in *Glow*'s type system is `Nat`.

Nevertheless, we might want to make the *intent* of the type to be clear to other programmers, if not to the compiler. Thus we define a type alias `type Byte = Nat;` where the compiler won't *enforce* that you cannot erroneously pass an overly large value where a byte expected, but at least the intent to the reader is clearer and a bug will be easier to spot. In the future, the language and its compiler will support refinement types....

Type aliases can convey *intent*, but also some *abstraction*: today a `Form48` could be a `(Bool,Nat,Nat)` and in the future may change to `(Bool,Nat,Bytes)` but if you wrote `Form48` you won't have to modify your program everywhere that passed values of type `Form48` without looking inside.



###### A few warnings

- Do note that `(A,B,C)` is different from both  `(A,(B,C))` and  `((A,B),C)`: these types syntactically different and semantically disjoint, 
- Technically, our primitive `Unit` is a `Tuple` with zero element.



##### Sum types

Sum types contain elements that are each one of many alternatives

The syntax to define a new sum type is to type the command `data` followed by the name of your sum type, followed by the sign `=`, and followed by the possible alternatives, that we'll name constructors, <constructor>, each being separated by a vertical bar `|`



> `data <name> = | <constructor1> | <constructor2> ...` 



For example, if you want to start programming a Rock-Paper-Scissors game, here's how you would name your data `Hand` and list the possible values for it (namely `Rock`, `Paper`,`Scissors`)

> `data Hand = | Rock | Paper | Scissors;`



Then, `Hand`, `Rock` and `Paper` are constructors for the type.

Constructors can themselves have arguments as in `data Primitive = | Boolean (Bool) | Natural (Nat) | Bytestring (Bytes)`.

A data type can have no constructor as in `data Empty = ;`This type does not contain any value.

The type `Example` includes values such as `NoParam`, `OneParam(true)` and `TwoParams(23, "foo")`





### Variables

##### Defining variables

Defining a variable is done similarly as in Javascript or ReasonML, with the keyword  `let` .

You can bind a value <val> to a variable <var> with:

> `let <var> = <val>`



So if your variable <var> is named x and its value <val> is 2, it'll look like that:

> let x = 2



Note that a variable's previous value can be used to re-define its new value. For example, when you write:

> let x = x + 2

It'll define x's value on the basis of its previous value.

Still, all the bindings are lexically scoped. This means that the variable is visible strictly in the part of the program that lies below it, in the same or nested function scopes.

In other words, `let`bindings may shadow a previous mention of a variable within the current function's scope, but will not remove the variable's value in other parts of the program.



**Example**

> `let a = 10; `

> `let f = (x) => x + a; `

> `let a = a + 5; `

> `let b = f(32);`



At the end: `a` is bound to `15`, it's a *new* variable that shadows the previous variable named `a` that is not visible anymore in the scope below. The definition of the *new* `a` uses the value bound to the *old* `a`. `b` is `42` because `f` still refers to the *old* variable `a` defined above.

Note that `let` does not allow the definition of the new variable to recursively refer to itself and depend on its own value.

In the future, we will introduce a distinct construct `let rec` as in ReasonML for recursive definitions.





### Functions

Functions are also defined with the command `let`. 

As you can see below, you can have one argument, no argument, or multiple arguments:

> let f = (x) => x + 1;

> let f = () => 23;

> let f = (x, y) => 23;

You can also use undefined arguments:

> let f = (_, _) => 23;



The underscore symbol `_`can only be used on the left side of a function arrow. So this type of code below doesn't work in *Glow*:

> `let f = _ => _`

But this one does:

> `let f = _ => 23`



### Conditions

Like most languages, *Glow* uses conditionality with `if `

For three elements `<a>`, `<b>` and `<c>`, the common *Glow* syntax is:

> `if <a> then <b> else <c>`



But it is also possible to obtain the same result with:

> `<a> ? <b> : <c>`



Or, when <b> and <c> are expressions and not statements, you can use `if` this way:

> ``if (a) { <b> } else { <c> }``



Note that this particular syntax is only allowed with `if`



### Keywords specific to *Glow*

- `assert!` checks at compile-time that the predicate always hold no matter what, as long as this point in the program was reached while the current user and the contract indeed run code generated by the Glow compiler, and the underlying infrastructure runs within the allowed parameters. Informally, `assert!` means "if this fails, the program won't even start."
- `assume!` means that either the active user (within an `@A`) or everyone (outside an `@A`) assume the given hypothesis. Informally, `assume!` means "you must not run this program unless you know this to be true."
- `data` defines sum types
- `else` is an expression to define conditional execution of a routine of your program. Unlike some other languages, in *Glow*, `else` is not optional
- `digest`is an expression that takes any data from any data structure, and calculates its hash. It transforms your data structure in a reasonably small fixed size (typically 32 bytes) sequence of numbers via a one way cryptographic process, that ensures it is humanly impossible to tamper with.
- `false`

- `deposit!` is used to deposit funds to an address. See our examples of smart contracts for more details regarding proper syntax
- `if` is an expression to define conditional execution of a routine of your program. See conditionality.

- `let` defines variables
- `publish` is used to display any value in the front end

- `require!` checks at runtime a predicate holds and if not fails the active user's transaction. The predicate must be computable at runtime by all the parties using their common knowledge, so that the contract may verify it. The Glow compiler will reject the program unless it can prove that the assertion is indeed satisfiable for the active user, and generate code that satisfies the assertion. If some of the data comes from user input, it is the user's responsibility to provide data satisfying the predicate in a timely fashion. Informally, `require!` means "if this fails, the active user is at fault".
-  `switch`  COMME REASONML

-  `true`
- `type` defines aliases types
- `verify`

- `withdraw!`  is used to deposit funds to an address. See our examples of smart contracts for more details regarding proper syntax



## Compiling your program

At the moment, *Glow* can't directly compile a program with it's own compiler, but it'll be doable Q12021



# Example of smart-contracts

If you are at the beginning of your journey in DApp programming, you might want to just program DApps buy importing ready-made smart contracts.

We have plenty of them at [this address](https://gitlab.com/mukn/glow/-/tree/master/future#dead-man-switch), where you'll find a list of our contracts (those already done and our works-in-progress) with a list of their features, and, of course, .glow files for you to import in your DApp. Each of these files is open-source and auditable.

But of course, if you want to program your smart-contracts yourself

Some of Glow's functionalities will be hard to grasp if you already need to focus on the main functions of your DApp. All the examples below are copy-pastable in Glow, but if you want to use a clean version of them in your DApp, you can just replicate them.



## Rock, Paper, Scissors (RPS)

Here is a minimal variant of an interaction for Rock, Paper, Scissors.

In this well-known game, two participants play by each choosing one of three possible hands, that they reveal at the same time. 

Depending on the choices, one wins, or the other wins, or it’s a draw. 

However, there is no such thing as “at the same time” on the blockchain: it is built on top of an *asynchronous* message passing network, and simultaneity isn't available as a primitive. 

Therefore, in a naive attempt to implement that interaction, whichever participant show his hand first is sure to lose, as the other participant may make a last minute change to his hand so as to win, [like this real-life robot](https://www.youtube.com/watch?v=ZVNnoOcohaU).

In our version of RPS we simulate simultaneity using a simple cryptographic technique called a [*commitment scheme*](https://en.wikipedia.org/wiki/Commitment_scheme). 

The first participant, say Alice, chooses her hand, but does not reveal it. 

Instead, she reveals a *commitment*: a digest of his hand concatenated to a random salt by a cryptographic one-way hash function. 

This commitment ensures that she cannot change her mind later, but can only play the hand that she committed to. 

The second participant, Bob, can then safely reveal his hand, confident that Alice cannot cheat. 

Finally, Alice reveals her hand, the outcome is computed, and winner takes the pot of money, while the loser gets nothing, unless it’s a draw at which point they each get their wager back. As for the salt, it ensures that Bob cannot guess the hand merely by trying each of the three possibilities and compare them to the commitment. 

The interaction is in thefeore in three steps, as follows:



> `@interaction([A, B])`
> `let rockPaperScissors = (wagerAmount) => {`
> ` @A let handA = Hand.input("First player, pick your hand");`
> ` @A let salt = randomUInt256();`
> ` @A let salt = randomUInt256();`

> ` publish! A -> commitment; deposit! A -> wagerAmount;`

> `@B let handB = Hand.input("Second player, pick your hand");`
> `publish! B -> handB; deposit! B -> wagerAmount;`

> `publish! A -> salt, handA;`
> `verify! commitment;`
> `  let outcome = winner(handA, handB);`
> `  switch(outcome) {`
> `  | A_Wins => withdraw! A <- 2 * wagerAmount   `
> `  | B_Wins => withdraw! B <- 2 * wagerAmount`
> `  | Draw => withdraw! A <- wagerAmount; withdraw! B <- wagerAmount }}`



The contract starts with the familiar and mandatory line #lang glow.

The first two statements define two simple data types: 

- The first one, `Hand`, is a `sum` type describing the choices that either participant can make for a hand to pay. 

- The second one, `Outcome`, is a `sum` type describing the potential outcomes of the interaction. 

  

Each of these types happens to have three *constructors*: 

- a Hand can be Rock, Paper or Scissors 

- an outcome can be B_Wins, Draw or A_Wins.



> `data Hand = | Rock | Paper | Scissors;`
>
> `data Outcome = | B_Wins | Draw | A_Wins;`



Implicitly defined are four functions:

1. Hand.toNat

2. Hand.ofNat

3. Outcome.toNat

4. Outcome.ofNat 

   

These functions map between the constructors of each type and natural integers, in order. Thus: 

- Hand.toNat(Rock) is 0
- Hand.toNat(Paper) is 1
- etc...



These implicitly defined functions and the carefully chosen order of the outcomes make it possible to compute the outcome with this “magic”-looking function:

> `let winner = (handA : Hand, handB : Hand) : Outcome => {  Outcome.ofNat((Hand.toNat(handA) + (4 - Hand.toNat(handB))) % 3) }`



Without using the conversion to integers, we could have used a more verbose definition using pattern-matching as follows:

> `let winner = (handA : Hand, handB : Hand) : Outcome => { `
> `switch([handA, handB]) {  `

>`| [Rock, Rock]   => Draw`  
` | [Paper, Rock]   => A_Wins `
`| [Scissors, Rock]   => B_Wins  `
`| [Rock, Paper]  => B_Wins `
`| [Paper, Paper]  => Draw  `
`| [Scissors, Paper]  => A_Wins ` 
`| [Rock, Scissors] => A_Wins `
`| [Paper, Scissors] => B_Wins 
`
`| [Scissors, Scissors] => Draw }`



Now comes the definition of the interaction RPS. As the annotation specifies, it has two roles called `A` and `B` in the code, and a single parameter, the `wagerAmount`.

The first step is as follows:

> `@A let handA = Hand.input("First player, pick your hand");`
> `@A let salt = randomUInt256();`
> `@A @verifiably let commitment = digest([salt, handA]);`
> `publish! A -> commitment; deposit! A -> wagerAmount;`

Note that the variable definitions preceded by `@A` only take place on Alice’s computer. 



The variables are not published on the network (so far), but jealously kept on Alice’s private database. 

Nevertheless, the commitment definition is annotated by @verifiably, which ensures that it can be verified later with `verify!. `

In the beginning, the `input` special form means that Alice chooses a hand, which invokes a private interaction between the agent that handles the blockchain layer, her her user interface. In the middle, the special form `digest` computes a cryptographic digest of the salt and hand. 

A `digest` is computed by a “one-way” function such that the odds that two different values of the same type have the same digest is astronomically unlikely. In the end, Alice publishes her commitment and deposits her wager into the contract, that manages “the pot”.



The second step is simpler:

> `@B let handB = Hand.input("Second player, pick your hand");  `

> `publish! B -> handB; deposit! B -> wagerAmount;`



Bob just reveals his hand and puts his share of the pot.



The third step in only slightly involved:

> `publish! A -> salt, handA;  `
> `verify! commitment;  `
> `let outcome = winner(handA, handB);  switch(outcome) {   `

> `| A_Wins => withdraw! A <- 2 * wagerAmount  `
> `| B_Wins => withdraw! B <- 2 * wagerAmount   `
> `| Draw => withdraw! A <- wagerAmount; withdraw! B <- wagerAmount }}`



Alice reveals her hand and salt. 

Everybody verifies that this is indeed correct (or else Alice’s transaction attempt fails and it is as if she did nothing). The outcome is computed, and depending on it, the pot is redistributed.

Now, what if Alice never starts the interaction? Then she times out, and she just wasted a bit of her and Bob’s time.

What if Alice does her first step, but Bob never replies? Then Bob times out, Alice invokes the contract to get her money back, and all Bob did was waste a bit of her time and some gas she spent to create the contract and get her money back.

What if Alice does the first step, but does the second step, but Alice never does the third step? Then she times out, and Bob can invoke the contract to get the entire pot since she’s in default.

But, if Alice, after Bob reveals his hand, realizes that she has lost, why would she bother to publish the last message? She loses anyway, she could just let Bob waste his time and pay the transaction fees to collect his winnings after she times out.

To disincentivize Alice from behaving in this way, *Glow* can (and in the near future, will) automatically transform the above contract in the below, where Alice has to deposit an additional amount of money as escrow to ensure she loses more by timing out than by participating, and that Bob receives more this way than he would have wasted while waiting.



> `@interaction([A, B])`
> `let rockPaperScissors = (wagerAmount, escrowAmount) => {  `
> `@A let handA = Hand.input("First player, pick your hand");  `
> `@A let salt = randomUInt256();  `
> `@A @verifiably let commitment = digest([salt, handA]);  `
> `publish! A -> commitment; deposit! A -> wagerAmount + escrowAmount;`

> `@B let handB = Hand.input("Second player, pick your hand");  `
> `publish! B -> handB; deposit! B -> wagerAmount;`

> `publish! A -> salt, handA;  `
> `verify! commitment;  `
> `let outcome = winner(handA, handB);  `
> `switch(outcome) {   `
> `| A_Wins => withdraw! A <- 2 * wagerAmount + escrowAmount`
> `| B_Wins => withdraw! A <- escrowAmount; withdraw! B <- 2wagerAmount   `
> `| Draw => withdraw! A <- wagerAmount + escrowAmount; withdraw! B <- wagerAmount }}`



Notice how Bob does NOT need to deposit an escrow amount, because his first message is also his last message, so there is no opportunity for him to stop cooperating after he started.

**Details on contract execution**

To invoke the contract at runtime:

Let’s suppose that two participant Alice and Bob agree that Alice will buy a signature by Bob of a string that says: "I, Bob, sell item #101 to Alice", for 100 ethers (10^20 wei) on the Cardano KEVM testnet.

First they they construct an agreement, and exchange it off-chain encoded in JSON, where the ethereum addresses of the respective users are used:

> `["interaction-agreement", 
> 	`{"glow-version": "v0.0-383-g83524b1",  `
> 	"interaction": "mukn/glow/examples/buy_sig#payForSignature",  
> 	"participants": {"Buyer": "0xC54e86DFFb87B9736E2E35DD85c775358F1c31CE",         
> 						"Seller": "0x9CcaEd210CE8c0Cb49c5Ad1C4f583406c264BA69"},  
> 	"parameters": {"digest": 
> "0x7a33c718fe7f3b9c56bd67b7b3e20fec6e3edf083626a7e11a10bba90243e405"         
> 			"price": "0x246ddf97976680000"},  
> "reference": {"Buyer": "Purchase #42", "Seller": "Sale #101"}},  
> "parameters": {"wagerAmount": {"TestEther": 100}}  
> "options": {"blockchain": "Cardano KEVM testnet",        
> 		"escrowAmount": {"TestEther": 10},        
> 		"timeoutInBlocks": 100,        
> 		"maxInitialBlock": 61247}  
> 	"Code-digest": 
> "0xaac1265d31e58390f2971bb58004f1944082116908ddb9c4a64be8b7d495c757"}]



Then, as the buyer and first participant, Alice creates the contract and sends a handshake with the agreement as context and easily verifiable information about the contract to accompany it.



> ["agreement-handshake",
> [... copy of the interaction agreement …] 
> {"contract-address": "0xaf0FdEA3C5eF883dB314fb8d5c5cf2892c8efC30",  
> "code-hash": 
> "0xaac1265d31e58390f2971bb58004f1944082116908ddb9c4a64be8b7d495c757",  
> "creation-hash": 
> "0xdffd466220658c75bf7a300babd981599b0fd1c268a073239605b391bf3b396e",  
> "creation-block": 61240}]



Of course, both can possibly sign off-chain once they agreed.



## Buy-Sig

Buying a signature, or buy-sig, has many uses for DApps:

- "swaps" where you sign a message for another blockchain

- rental where you sign a key access message that some door's / some car's reader will accept

- "Diploma" or other affidavit- Usually for purely digital goods. 

  

For physical ones you will want the last step to be sending the money to a multisig escrow contract.



> `#lang glow`
> `@interaction([Buyer, Seller])let payForSignature = (digest : Digest, price : Nat) => {`
>
> ` deposit! Buyer -> price;`

> ` @Seller @publicly let signature = sign(digest);`
> ` withdraw! Seller <- price;`
> };



This contract starts with a special line #lang glow to indicate that it is a glow program indeed.

Then we define a function payForSignature in a syntax familiar to JavaScript or ReasonML programmers. 

However, this definition is preceded by an *annotation* `@interaction([Buyer, Seller])` than indicates that it corresponds to a blockchain interaction with two participant roles. 

The first role is that of a Buyer, the second [ETC]

Note that the line `@Seller @publicly let signature = sign(digest);` is equivalent to the three lines below:



> `@Seller @verifiably let signature = sign(digest); 
> publish! Seller -> signature; 
> verify! signature;`



And in turn, `verify! Signature;` will be replaced by:

> `require! isValidSignature(Seller, signature, digest);`



If the formula to define the signature ever changes, the verification will always exactly match the definition.
This interaction involves *two* transactions between the participants and the contract.

In the first step:

> ` deposit! Buyer -> price;`



The Buyer deposits the agreed amount into the contract.
In the second step, the Seller publishes the signature for the document, then withdraws the amount deposited.



> `@Seller @publicly let signature = sign(digest); 
> withdraw! Seller <- price;`



The language and the contract know that there two transactions because the active user has changed: when depositing, the *buyer* is active. 

When publishing the signature, the *seller* is active. 

This requires a separate blockchain transaction. By contrast, the withdrawal step at the end is done by the contract, which can be and is included as part of the same blockchain transaction as the previous step.

In practice, the two participants must agree OFF-CHAIN on the terms of the interaction. 



Then, the first participant, in this case the buyer, creates the contract as part of enacting his first transaction. He then communicates the contract address to the seller as part of an off-chain handshake. 

The seller verifies that the terms of the handshake indeed correspond to a previous agreement, and then enacts his transaction. At the end, the contract concludes, the buyer can read the signature, and the seller has been paid.



What if the buyer never creates the contract or fails to send the handshake? He times out, and the seller drops the interaction from his active set.

What if the seller never publishes the signature? Then *he* times out, and the buyer invokes the contract to get his money back.





# Contribute to Glow (level 4)

### Programmation standards

[If you have installed](https://gitlab.com/mukn/glow/-/blob/master/INSTALL.md) Glow and want to experiment with the Glow compiler, please read our document about [how to hack Glow](https://gitlab.com/mukn/glow/-/blob/master/HACKING.md).





## Upcoming functionalities

### Turing-completeness

We value safety over versatility: our holy grail is to make Glow Turing-complete, but not by accepting a compromise about safety. We think we can reach that goal and make a difference in the world of DApps, but it'll be at the cost of a development-process tainted with permanent and extreme caution.

Thus we don't have a set deadline, but be assured that we'll spend time, energy and resources to do that.



### Web-user interface 

**Features we want for early 2021**

- A nice Web Editor at the level of ide.zilliqa.com, perhaps by implementing it with [Quasar](https://quasar.dev/)
- A compiler, compiling for Cardano, deploying on Cardano “Plutus DApp Framework”
- Also compiling for Ethereum, deploying on Ethereum test network 
- Omniscient debugger at the granularity of blockchain transactions
- Run Buy Sig with a “graphical user interface”, see the Maybe Signature result
- Run RPS with a “graphical user interface”.
- Easily input parameters found off-chain (forms or expressions)
- Be able to create an easy template for buy_sig, RPS, etc., so end-users don’t even have to see the code.
- Available examples on the side easily selectable (like on ide.zilliqa.com) 
- Awesome tutorials for many other examples
  - emails and chat
  - crypto wallet
  - video chat service



**Features we want for summer 2021**

- On-line help

- Minimal “chat” in which to negotiate the parameters of contracts.

- Minimal contact management for chat.

- UI to find the other party off-chain

- Abstract comparison of traces so we can compare the various backends

- Running in the UI on an abstract blockchain without having to wait for block confirmations

- Running test cases in the UI that illustrate race conditions and failures

- Real-time feedback

- Syntax highlighting

- Background coloring depending on which role is active & whether the action is private/public

- Full multilevel omniscient debugger--from the EVM to the UI via source language (raw or transformed), BEPP language, user-configurable presentation, etc...

- Full wallet integration, for as many crypto-currencies as possible

- Remembering by hash old versions of contracts as compiled by previous versions of the compiler (especially after they’ve been deployed in production)



### Portability to other blockchains

Ultimately, Glow's purpose is to work on any blockchain.

Some well-established blockchains receive a special attention from our team, and we plan to carefully ensure that DApps developed with Glow can perfectly run on the following networks:

- Ethereum

- Tezos
- Bitcoin Cash
- Algorand
- Block.is
- Decred

- EOS
- ...and a few more!



### Other functions

Hexadecimal will be supported



