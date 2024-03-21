# dotifles

公私での dotfiles

## gitconfig_global

[include]
  path = ~/.gitconfig_global

を .gitconfig に追加すれば良い
.gitconfig を dotfiles で管理すると機微情報が含まれてしまうため、includeで読み込む形にした

## Brewfile

```sh
brew bundle # Brewbundle 内で定義された依存関係をインストールしたりアップグレードする
brew bundle dump # 現環境で Brewfile を出力
brew bundle cleanujp # Brewfileに記載されていない依存関係をアンインストールする
```
