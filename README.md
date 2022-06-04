# sokoban

Шаблон проекта для локальной работы с CodeWorld.

Для работы нужно установить утилиту [GHCup](https://www.haskell.org/ghcup/), с помощью неё установить последнюю версию Cabal и GHC 8.10.

Команда `cabal build` собирает проект, `cabal run` запускает приложение CodeWorld, которое после запуска доступно по адресу http://localhost:3000/.

Для удобства можно установить утилиту [entr](https://github.com/eradman/entr) (`brew install entr`, `sudo apt install entr`, `sudo pacman -S entr`),
и запускать приложение командой `find app -name '*.hs' | entr -r cabal run`, тогда оно будет автоматически перезапускаться при изменении кода.
