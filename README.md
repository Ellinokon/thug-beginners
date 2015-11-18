Trondheim Haskell Users' Group beginners' workshoppe
====================================================

This repository is a collection of exercises for the THUG beginners' 
workshoppe series.

LessonA covers the very basics. LessonB will cover more advanced types and 
features. LessonC will focus on using everything covered in LessonA and 
LessonB to solve problems.

You'll need a computer with GHC and cabal.

Start off by making a sandbox (isolated environment) where you install the 
dependencies:

```
$ cabal sandbox init
$ cabal install --only-dependencies
```

Then you should be good to go. I suggest using GHCi as you work.

```
$ cd lessonA
$ ghci A1Nat.hs
```

Now you can open A1Nat.hs in your favourite editor. You can also do this from 
GHCi:

```
λ :e
```

When you have made changes, close the text editor and GHCi will reload the 
file automatically. If you have the file open yourself (not via GHCi), you'll 
have to reload it.

```
λ :r
```

GHCi is very powerful and useful. See its help output for a glimpse of what it 
is capable of.:

```
λ :h
```


There is a per-lesson answer guide you can consult to see if you've gotten it 
right. You can access it by running cabal from the top level directory:

```
$ cabal run a
```

The answers are based on property laws. This means that you can write a 
function that achieves the "correct answer" without it necessarily being 
correct. I would advice you not to try to cheat the answer guide, as it would 
only be detrimental to your learning. For the same reason I would avoid 
looking at the source code for the tests, or searching the Web for the exact 
solution to the problems presented.


Presentations:   
 LessonA: https://secure.plaimi.net/~alexander/tmp/2015-10-06-thug-beginners-1.html   

Suggested material for after LessonA:   
 https://www.youtube.com/watch?v=3bjXGrycMhQ   
 http://www.iro.umontreal.ca/~monnier/2035/history.pdf   
 http://learnyouahaskell.com/starting-out   
 http://learnyouahaskell.com/syntax-in-functions   
 http://www.haskellforall.com/2013/12/equational-reasoning.html

Suggested material for after LessonB:
 https://wiki.haskell.org/Typeclassopedia
 http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html
 http://www.cs.nott.ac.uk/~gmh/fold.pdf
 http://202.3.77.10/users/karkare/courses/2010/cs653/Papers/ad-hoc-polymorphism.pdf
 http://ipaper.googlecode.com/git-history/8070869c59470de474515000e3af74f8958b2161/John-Hughes/The%20Computer%20Journal-1989-Hughes-98-107.pdf
