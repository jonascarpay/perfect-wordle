# perfect-wordle

Solver for [Wordle](https://www.powerlanguage.co.uk/wordle/).
The bot never loses, and on average wins in 3.50 moves.

### Demo

The lines starting with `>` are user input.

```
  ROATE?          '.' for grey, 'y'/'Y' for yellow, 'g'/'G' for green
> .y..y
  PYLON?          '.' for grey, 'y'/'Y' for yellow, 'g'/'G' for green
> ..gy.
  CELLO?          '.' for grey, 'y'/'Y' for yellow, 'g'/'G' for green
> .gggg
  HELLO?          '.' for grey, 'y'/'Y' for yellow, 'g'/'G' for green
> ggggg

  ROATE?          '.' for grey, 'y'/'Y' for yellow, 'g'/'G' for green
> yg...
  CUNDY?          '.' for grey, 'y'/'Y' for yellow, 'g'/'G' for green
> ...y.
  WORLD?          '.' for grey, 'y'/'Y' for yellow, 'g'/'G' for green
> ggggg
```

### Explanation

Wordle uses two lists; a list of valid words _v_, and a subset of those that are actually used as answers _a_.

In any given round, call the list of answers that are still possible given the knowledge obtained so far _a'_.
The bot guesses the word from _v_ that, on average (i.e. summed over all possible answers in _a'_), removes the most remaining answers from _a'_.

### Performance

The bot uses a brute-force algorithm, which places the complexity of selecting the first move in the order of `(length v) * (length a)^2`, or roughly 60,000,000,000.
The selection of candidates is run in parallel, but other than that, the code is entirely unoptimized.
There's probably a ton of low-hanging fruit in terms of optimization[^quirk], but I have spent about as much time on this as I wanted to, so I'm not going to bother

In practice it doesn't really matter anyway, the first word is the most expensive to calculate, but it's also the same every time.
So, I just calculated it once, and now the first guess is firm-coded to `ROATE`.

[^quirk]:
For some reason the average utilization is only about 230%, but if we select our answers from _a_ or _a'_ we get the expected 1200%.
I'm not sure what's going on there.

### Running

#### Cabal

```
> cabal new-run perfect-wordle
```

#### Nix

Only attempt if you have [the IOHK cache/my cachix set up](https://jonascarpay.com/posts/2021-01-28-haskell-project-template.html).

```
nix build
result/bin/perfect-wordle
```
