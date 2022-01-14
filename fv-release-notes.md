
# Outline of work performed

## Secondary research

  

The functionality of the assertion construct of Glow language was included in the early design stage of Glow. One of the key design goals behind the assertion mechanism was to enable programmers to perform automated verification of correctness of the assumption about the program in the adversarial environment. From the beginning, it was obvious that the ultimate goal of domain-specific logic for Glow will require combining multiple modalities into one consistent system.

  

Since reliance on existing theories and established technologies is our strategy of choice, we began our project with in-depth desk research:

  

-   We performed a review of relevant work in the area.
    
-   For each requirement posed for Glow Formal Verification, we identified the set of prior works most relevant to a particular functionality.
    
-   We performed a simplified cost-benefit analysis of applying each approach to the Glow ecosystem.
    

  

## Design decisions



  

In this stage of the project, we focused on investigating different possible ways of limiting the powerful syntax of our assertion system. Our goal was to have a working system as soon as possible since we firmly believe that even limited implementation will help us in collecting feedback from developers and will give us a way to test sometimes elusive theoretical concepts.

  

The challenge of this stage was to impose the right amount of limitations:

  

-   We performed a cost-benefit analysis of the implementation of different techniques and integration of different existing tools with Glow Compiler. To avoid foundational problems of integrating different theories into a common model, for our proof of concept we decided to integrate only one external tool:
    

-   Satisfiability modulo theories solver - Z3. The current version of glow is Turing complete, and possible execution paths are easily enumerable. Since we can easily generate a relevant model for most of the Glow programs, we decided to base our proof of concept implementation on this tool.  
      
    

We postponed the integration of followed tools:

-   Model checkers based on state machines like Computation Tree Logic and Linear Temporal Logic. Those techniques are most beneficial while dealing with programs making use of recursion and loops. Since we excluded those constructs from the current version of Glow, we decided to postpone this integration.
    
-   Symbolic computation, computer algebra system. While recognizing the necessity of integrating tools like this in the longer run, we decided not to include it in our proof of concept and instead use the limited capability of SMT solver in this area.
    
-   We decided not to use the Proof assistant to prove the correctness of our implementation since it would require us to freeze the design early what would stand in conflict with the exploratory nature of this phase of the project.
    

  

-   We investigated dependencies between different modalities and semantic constructs, which we plan to include in our system in the long run.  
    This lets us identify key aspects of our system, which we must implement from the start:
    

-   Game-theoretic semantics is crucial since it frames every assumption into the context of cooperating and adversarial participants. Since proving the viability of such an approach is crucial for our project, we set up delivering this functionality as one of the key goals of our proof of concept.
    
-   Reasoning about the reachability of specific parts of Glow programs
    
-   Combining assertions with logical connectives to express properties of the programs in a fine-grained way.
    

  
On the other hand, we decided to leave the following capabilities out of the scope of our proof of concept:

-   Reasoning about randomness and probability. Our proof of concept cannot prove the fairness and correctness of the programs involving generating random values; we are confident that we will be able to add this functionality later since it can be done with an already established theory.
    
-   Properties of cryptographic functions are ignored by our proof of concept while verifying assertions. We intent our proof of concept to serve as a testbed to develop a proper way of including those properties in our model in the future.
    

  
  

## Exploratory implementation



  

Since the goal of this iteration was exploratory implementation, following the strategy laid down in our proposal, we restricted the scope of Glow constructs implemented in FV Engine. Because of this, not every Glow program can be augmented with the assertions from our proof of concept, but we were able to spend more resources on experiments and evaluating different versions of syntax and semantics of assertion.

  
  

## Evaluation

Project KPI: Evluate the proof-of-conept implementation of the Formal Verification functionality of Glow.

  

-   performance of FV system was evaluated and was satisfactory even for complicated examples
    
-   proof of concepts implementation correctly verifies or rejects all test Glow programs in the test suite developed as a part of the project
    
-   we evaluated the utility of game-theoretic directives by applying them to real-world example program. We showed an example of an assertion about business logic that can’t be expressed without game semantics and demonstrated how our proof of concept made it verifiable.
    
-   some subtle bugs were discovered (and fixed) in the implementation of generating input for Z3
    
-   TODO: describe issues with the current syntax of assertions
    
-   implementing verification inside glow compiler negatively impacted its code clarity
    

-   for such critical code as Glow compiler, this is an important concern
    

  

# Key achievements

-   Our team developed a simplified model of Glow Formal Verification Engine by carefully limiting its scope and modularizing its architecture.
    
-   We validated the idea of decorating assertions about programs with game-theoretic directives. We provided a working implementation of this concept. The impact of those directives on the verification of assertions was evaluated and proved to be consistent with designed semantics.
    
-   With our proof of concept implementation of Formal Verification in Glow published, we are ready to involve developers in the design process of its production version since it's easier to give feedback about even very basic implementation than for even most detailed specifications.
    

# Key learnings

Given the exploratory nature of this work, we consider the following insight to be a major part of value resulting from the project:

  

-   A design of syntax of assertions is not obvious, and when done incorrectly, can be dangerously ambiguous. That's why it is important to decouple it from an internal model of the FV system. An additional layer of syntactic sugar may become necessary in the future to allow fine-tuning of user-accessible syntax without modifying the underlying theory.
    
-   The process of translating assertions about programs into a format specific for a particular theorem prover is error-prone and complicated. Details of this process are specific for language, so implementation cannot be readily hand-checked for adhering to some established theory. Formalization of this translation process inside proof assistant may be required to deploy a system like this in production.
    
-   Since assertions inside Glow program code are designed not to impact the execution of the DApp, the Formal Verification system should be maximally decupled from the existing parts of the Glow compiler. MuKn may realize this by implementing a separate parser and type checker for assertions or even by implementing the FV system as a separate process.
    

# Next steps for the product or service developed

Completing this stage is an important step towards the long-term goal of developing a production-ready implementation of a powerful Formal Verification Engine for Glow.

  

We also shared with the community our long-term plan regarding the application of formal methods to Glow Language.

  

# Final thoughts/comments

In our Catalyst5 proposal, we already declared that we plan to continue this line of work given enough support from the Cardano community via catalyst project votes. And with adherence to this plan, we proposed the continuation of this work in our Catalyst7 [proposal.](https://cardano.ideascale.com/a/dtd/Glow-Formal-Verification-Stage-2/384296-48088)
