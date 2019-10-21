# Functionnal programming report

##### Lucas Sardois

https://github.com/loustak/sgit

## Instructions

To compile and run the program you will need the following software installed:

* Java JDK >= 8 [download](https://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html)
* Scala version >= 2.13 [download](https://www.scala-lang.org/download/)
* Sbt version >= 1.3 [download](https://www.scala-sbt.org/download.html)

Once they are installed and functional, we can move to the next step.
From here I will assume that you are using Linux, but it should also works on Windows and Mac just use the appropriate commands.
clone the git project: `git clone https://github.com/loustak/sgit`.
Go inside the cloned repository: `cd sgit`.
And now build the executable:`./build.sh` (it just run `sbt assembly` then copy the binary in the `target/scala-2.13/sgit` directory to the current directory). An `sgit` executable will be generated in your current directory. If you want to be able to use sgit as any other commands add it to your path.

## Architecture

During the development of the project I made two distinct architectures. The first one can be found in the branch `old` on my github repository. This architecture was developed from 1 Tuesday to 17 Thursday of this month. The second architecture in the `master` branch was developed from 18 Friday to 20 Sunday.

I made two different architectures because I didn't like the first one. Indeed, it was too close to imperative programming and not very functional, plus I found myself often write the same code again and again. The error management consisted of one giant try-catch at the start of the program, not very functional...

Because of those reasons, I chooses to rewrite everything from scratch even trough the timing was hard. And I think, this new architecture is way better. To give an idea I was able to code more features in 3 days than in 17 days with the previous architecture. 

I will now present the resulting architecture. My main entry point is the `Main.scala` file, it parse arguments given to the executable using Scopt, a library to make CLI. Once I'm able to determine the user command and arguments I call an HOF wich, find the repository, call the given command and handle potential errors ([ref]([https://github.com/loustak/sgit/blob/master/src/main/scala/com/sardois/sgit/Main.scala#L26])). Commands can be found in the `Command.scala` file. They all have the same signature, because they are passed to the HOF. 

Commands mainly manipulate a repository instance. The repository class is defined in the `Repository.scala` file. It contains all the attributes and functions necessary to handle a git repository. Many attributes are defined using `lazy var`, this allow fields to be initialized only when needed ([ref](https://github.com/loustak/sgit/blob/master/src/main/scala/com/sardois/sgit/Repository.scala#L22)). Since we are recreating a repository instance all the time it makes the program execution faster. Almost all my`lazy var` fields makes **I/O** calls.

Many concepts in git are stored and minupulated by the same functions. Especially writing and reading. To handle this I created a trait in the `IO.scala` file wich represents a class that can be writen and read.  It use a HOF to handle **IOException** ([ref](https://github.com/loustak/sgit/blob/master/src/main/scala/com/sardois/sgit/IO.scala#L16)) and returns an `Either[String, T]` to represents wether there was an exception or not, this makes **I/O** calls pure. Speaking about purity, I created an annotation in the `impure.scala` file wich allow me to annotate all the functions that arent pure by using the `@impure` annotation. The rest of the IO object contains HOF functions to read and write classes that extends the IO trait.

The `UI.scala` file contains function to makes commands string output easly. I wanted to separate function computing and output generation to write reusable functions.

To make the development easier I chooses to not make a tree of blobs like git but instead to store the index file for each commits. This allowed me to reuse previously created function for **I/O**. I also choosed to implement my own diff algorithm wich can be found in the `Diff.scala` file instead of using the one used by git ([ref](https://github.com/loustak/sgit/blob/master/src/main/scala/com/sardois/sgit/Diff.scala)).

The following dequence diagram summarize the previous explanation.

![Untitled Diagram.png](https://github.com/loustak/sgit/raw/master/diagram.png)

### Pros

* This architecture allow me to reuse functions thanks to HOF.

* To handle error in a pure way.

* To only load whats needed for the given command and so reduce **I/O**.

* Thanks to the `@impure` annotation it allows me to see clearly wich functions have side effects.

* Separate output rendering from function computing allowing to produce complex output.

### Cons

* This architecture makes the writing of commands complex because they often need to load many differents parts of the repository to work.

* The error handling is limited. Indeed, it should had be done trough an error class passed to the UI object.

* The `Checkable.scala` file contains obviously duplicated code.

## Tests

I made tests only for the first architecture I developped because of the delay limit. They can be found in the test folder on the `old` branch ([ref](https://github.com/loustak/sgit/tree/old/src/test/scala/com/sardois/sgit)).

Since it only contains test for the first architecture developped I will try to explain my approach instead of what I did. My idea was to test desired functions in a separeted folder using real **I/O**. I made helper functions to generate test folder. Each test using a sgit repository was made in a separated folder. This allowed me to make tests concurrent. They were also encapsulated, they doesn't affect each others. And finally one a test failed, it didn't deleted the files, so I was able to manually inspect what really happend during the test. A test exemple can be found [here](https://github.com/loustak/sgit/blob/old/src/test/scala/com/sardois/sgit/CommitSpec.scala#L56)).

The drawback of this method is the number of **I/O** performed. Since each test require the repository recreation it make a lots of **I/O**. Moreover, if the number of test start to increase the time needed to run them increase drasticly.

## Post mortem

### What went right

I was able to understand the basics concepts of git and how it works internally. 

I was able to use better files, scopt and scala test easly.

And in the end, I was able to produce a Scala program executable and working with units tests. The most importants features are working (only the merging & rebasing features were not developped).

### What went wrong

I started to code before having a global vision of the project and because of that my first architecture was very poor and inneficient. At the start of the project I tried too much to copy git, it made me lose time because you don't develop a C application the same as you would develop a Scala application.

As the development continued I became too focused on adding functionalities and lost the ability to keep clean and functionnal code. Thats why my first architecture looks really poor and seems to look more like imperative than functional.

As a results of the previous points I was so unhappy with the quality of my code that I choosed to make a new branch on my repository with a clean restart of the project (the `refactoring` branch).

### Lessons learned

To not reproduce the same mistakes I will try to get a better overview of a project before starting to develop and architecture my code. I will also try to discover common pattern for a given programming language when dealing with one I don't master. And finally, I will try to take a step back when developping to check code quality from times to times.
