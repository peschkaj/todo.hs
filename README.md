# todo-hs

PSU CS557 final project.

## Getting Started

1. Install [Haskell Stack][1]
2. Clone this repository
3. Go to the repository folder in your terminal
4. `stack init`
5. `stack build`
6. `stack exec` (to run the program)

## Code Layout/Architecture

Libraries end up in the `src` folder.
The application is in the `app` folder.

## Adding a new library

To add a new library:

1. Open `package.yaml` in your favorite editor
2. Add the library under `dependencies`. If you're feeling really lazy, you don't even need to add a version, just dump in the library name that you want.
3. Run `stack build`

## What about GHCI?

Just run `stack ghci`




[1]: https://docs.haskellstack.org/en/stable/install_and_upgrade/#macos
