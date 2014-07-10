Perlembagaan Persekutuan
========================

The constitution of Malaysia in plaintext. Converted from a PDF, so there are a lot of typos. Useful for grepping.

Source: http://cpps.org.my/resource_centre/Perlembagaan_Persekutuan.pdf

### Running Scala scripts

All of the `.scala` files here are written as Scala scripts. To run them, use `scala markov.scala`. They don't have 
to be compiled. They also take training set input from standard input, and prints results to standard output. Pipe 
the training set into the scripts like so `cat sample.txt | scala markov.scala`. The results will be printed out in 
the terminal, or they can be piped into a file: `cat sample.txt | scala markov.scala > results.txt`

### The code

##### `WordIterator(in: InputStream) extends Iterator[String]`

This class converts a stream of bytes. `InputStream`, into a stream of words, `Iterator[String]`, using the 
simplistic "[a-zA-Z]+" regex. The `InputStream` is usually the standard input `System.in`.

##### Matrices

The scripts use matrices of different dimensions for the analysis. The `starting` matrix represents the probability 
that a sequence of characters begins a word in the training set. For a markov chain of degree n, the starting matrix 
has n dimensions. For a markov chain of degree 2, `starting(1)(0) == 0.5` means that half of the words in the 
training set begins with "ba".

The `transition` matrix represents the probability that a character follows a sequence of characters. For a markov 
chain of degree n, the transition matrix has n+1 dimensions. For a markov chain of degree 2, `transition(1)(0)(3) == 
0.5` means that whenever the substring "ba" appears in the text, half of the time it is followed by "d". This 
excludes the instances where it appears at the end of a word.

