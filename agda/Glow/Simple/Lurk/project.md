

   Glow on Lurk TODO

   To have Glow on Lurk, it is suffcient and nessary that we have

   * Host contract
     - written in FVM
     - universal? (same for every glow program)
          (for now Marcin is in favor of this) 
     - stores participants founds
     - verifies state changes
     - stores published data

   * Compiler backend
     - function translating Glow IR(LastLeg.hs) into lurk code
     - some container format to store lurk code, client code and all metadata necesary to deploy and run contract 

   * Client runtime
     - accepts data from compiler output
     - proivide functionality of deploying a contract and running it interactively

   
   
   
