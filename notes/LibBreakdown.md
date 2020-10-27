# Clojure compilation, libraries, unifcation for lang, project structure

Since lang need increasing number of functionality, currently avaiable only for compiler (and potentionally interpret), I need to divide compiler into several libraries. Also current way of generating clojure code is very basic and I should create a project structure for code compiled into clojure. THis also allows me to use .jar libraries in the clojure and remove clojure specific implementations from cluttering code (like int2roman and roman2int algorithms).

This modification will consist of following phases:

* Clean up the compiler/interpret project
* Divide the project into libraries and setup dependencies
* in clojure compilation switch to using code from libs and allow access to additional functions
* drop Main class and replace it with custom Interpret and Compile classes
* Write wrapper script for compiler and interpret and use it to create project structure during compilation

## Clean up the compiler/interpret project
Make it all work again. During research on how to use libs and such a lot of experimental code was deployed and should be trimmed.

## Divide the project into libraries and setup dependencies
I will divide the project into several libraries:

* Base package: This is package that will be used by all other packages (if needed), should be as minimal as possible and only contain minimum of shared code. 
* Types library: Supposedly contents of Types package, if there is any dependency on other parts of project move those to Base package. This lib should contain unification algorithm and will be used by compiled clojure code.
* Util library: Tools and utilities not directly related to compilation, roman number conversion. Mostly contents of Util package. Will be used by compiled clojure code.
* Core library: Main library for compilation and interpretation. Will contain entry points for compilation and interpretation. Also will contain Expression and its subclasses.
* Test library: Contains unit tests.

## Switch to using lib code in clojure instead of custom native code
Investigation in this field was concluded. Use deps.edn file to specify the dependant Java .jar files to get them on clojure classpath and use fully qualified names in compilation. In this phase putting jar or deps.edn file into clojure compilation will have no automation.

## Drop main and replace it with Interert and Compile classes
Old Main class will be removed and will be replaced with use-specific entry point classes. Script will be used to lauch jar file with correct configuration.

## Write wrapper script for compiler and interpret
I will write wrapper script for calling compiler and interpret jars. Also script for compiler will create clojure project structure, deps.edn file and will copy required jar libraries.