![eris logo](imgs/logo_grey.png)
# eris

Eris is a haskell library that being designed to provide recommendation functionality. It will basically follow the structure of the [Surprise](https://github.com/NicolasHug/Surprise) a python recommendation library.

At currently stage it is a note for learning Haskell, to record all necessary application structure and all kinds of usage of stack in real world.

Development schedule:
  1. Basic functionality of Surprise. (Doing)
  2. Refactor to be able to use Haskell cocurrent. (TODO)
  3. Modify the implementation of algorithms to be able to integrate with cloud haskell. (Is is really possible?)

- ### source file
  - src/Eris/Compute  
    - Metric function
    - others : TODO
  - src/Eris/Meta
    Definitions of data type
  - src/Eris/Predict
    - KNNbased
    - others: TODO
  - src/Eris/Typhon
    provide all kinds of helper functions which cannot be categoried as any perticular module.
- ### stack test
  more description about stack test,links to note  

- ### stack benchmark
  more description about benchmark, links to note

- ### note
  cotent list
  links to be added
- ### examples
  TODO

- Acknowledgement:
  - [Dat Le](https://github.com/lenguyenthedat)
  - [Nicolas](https://github.com/NicolasHug)
