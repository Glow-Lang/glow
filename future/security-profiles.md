# Security Profiles

Different users have different needs in terms of security, performance, costs.
Different *security profiles* can accommodate these different users.
Often a very same person my partake in several activities, each requiring
its own *security profile*. But careful! Activities with different profiles
may not be freely executed on the same computer systems without imposing
costs on the less secure one and/or risks on the more secure one.

There is a continuum of possible security profiles, and identifying the
best practices and the best parameters for any given user requires
case-by-case analysis of the user's activities and their context,
what attack model he is facing, etc. Still, we'll try to simplify our
reasoning by drafting three broad classes of users: small time amateurs,
seasoned users, and high-stake companies.


## Typical Security Profiles

### Small time amateurs

A small time user doesn't do anything risky. He only sends payment or partakes
in decentralized applications with people, partners and platforms he trusts, or
who are otherwise accountable through centralized justice. At stake are only
relatively small amounts that he can afford to lose, and that will not attract
targeted attacks by sophisticated enemies.

Such a small time user can probably afford to "just" connect to networks with
their phone or laptop, with simple default settings, without taking extra steps
to hide their identity and location, without redundancy in multiple data
centers. They can accept relatively short timeouts in their applications
(in a matter of a few confirmation times). If attacked, they can connect
to an alternate WiFi network from a café or library to post their messages,
or use their cellular network — at which point only they will pay for security
services such as regularly used by seasoned users below.

* Attack model:
  Fishing, trivial DDoS, bad app or remote code execution vulnerability that steals keys,
  bad naming or confusing UI that causes the user to use wrong transaction parameters
  (amounts, recipients, rates, etc.).
* Defense model:
  UI with good user experience and good defaults and reasonable limits.
  Reasonably safe network hygiene, with fallback to even more secure measures.
  A bit of education (but there's a cap to what is possible).
  Minimizing alerts and reminders, and providing good explanations about them.
  Preparing the users to a backup plan, with someone to call in case of trouble.

### Seasoned users

A seasoned user understands the concepts well, and handles amounts of money that
are large for an individual. A mishap could have significant consequences on his personal wealth,
though not for the economy at large.

Attack model:
* In addition to the above, spear fishing by moderately proficient enemies with off-the-shelf tools.
  Swatting. More technical DoS attempts.

### High Stake Companies

Regular high-stake users shouldn't hope to be able to hide their network and
physical locations indefinitely from attackers. To avoid DoS attacks, they need
a lot of redundancy between multiple data centers on several continents, each
of them securely defended by armed guards, in countries whose governments will
not easily and suddenly turn against them. That's a tall order.
It's much easier if the users are a well-established institution or otherwise
have an activity fully condoned by centralized authorities, in which case big
established providers like Amazon can step forward.

Attack model:
* High stake enemies. Professionals. Mathematicians.
* Coordinated multifactor attacks, including physical attacks.
* Targeted break-ins, robberies, maybe even abductions.
