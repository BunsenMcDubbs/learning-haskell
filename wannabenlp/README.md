# Welcome to `wannabenlp`

**1-gram Markov chain has been implemented!**

Note that the Markov chain implementation in `Markov/MarkovChain.hs` is
actually generic and only requires states and transitions are instances
of `Ord` and `Show` (maybe and probably `Read` in the future).

Sample output (see the magic!):
```
$ ./ParseText big.txt
Creating 1-Gram Markov Chain from big.txt...
" if youll see she had dreams and strangely only to warn me ill send for defense of military and neisser who was always warm wellventilated room clutching her head "
Press ENTER to continue...

" organisms are you know people of the standard and by law went up to go "
Press ENTER to continue...

" in dissensions under the diet must be to reign but said the ground floor natasha lay down beside vereshchagin "
Press ENTER to continue...

" in this minister count grey cloth for the swelling in the spotted manchester and yet unknown what petya decided upon and irritating tax shall go "
Press ENTER to continue...

" theres no doubt to the dowager empress field "
Press ENTER to continue...
```

Steps to running code!
 1. Download a text file into this directory (Peter Norvig has some
    [good ones](http://norvig.com/ngrams/))
 2. Compile the code `ghc ParseText.hs`
 3. Run! `./ParseText <filename>`

